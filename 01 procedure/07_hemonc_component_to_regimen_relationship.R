##############################################
### INTRODUCTION
##############################################
# Clear env
# clean_env()

# Source function and vars
 source("startup.R")

# Set search parameters
# type <- "Pivot"

# Create output variables
# new_col_name <- "HemOnc Component to Regimen"
# path_to_output_fn <- create_path_to_output_fn()

# Temporary stop if the output file exists
# brake_if_output_exists()

##############################################
### PRE-PROCEDURE
##############################################
# Read input
input <- read_input()
path_to_output_fn <- create_path_to_output_fn()
brake_if_output_exists()

input_map <-
        input %>%
        dplyr::mutate_all(as.character) %>%
        rubix::normalize_all_to_na() %>%
        dplyr::filter_at(vars(Component), any_vars(!is.na(.))) %>%
        dplyr::filter_at(vars(Regimen), any_vars(is.na(.))) %>%
        dplyr::select(routine_id,
                      Component)

# Create final input object to join with final output object before export
final_input <- input_map




input_map2 <- input_map %>%
        #separate by carriage return
        tidyr::separate_rows(Component,
                             sep = "\n") %>%
        # remove excess carriage returns
        dplyr::mutate_at(vars(!routine_id), ~stringr::str_remove_all(., "\r"))


#####################
# New Regimens may be needed from existing components if such regimens don't exist
# -All component concept ids
# -No NA component concept id sets
# -Pivot longer for component concept id sets of 1 and more (separated by carriage return)
# -Remove "{search tearm}[:]" prefix to the concept strip
# -
input_b <-
        input_map2 %>%
        rubix::filter_at_grepl(Component,
                               grepl_phrase = "NEW ",
                               evaluates_to = FALSE,
                               ignore.case = FALSE
        ) %>%
        # unmerge to capture concept_id
        chariot::unmerge_concepts(Component)


qa <-
        input_b %>%
        rubix::filter_for(concept_class_id, c("Regimen", "Component"), invert = TRUE)

if (nrow(qa) > 0) {

        stop('Unrecognized concept_class_id: ', paste(qa$concept_class_id, collapse = ", "))

}

input_b2 <-
        input_b %>%
        dplyr::select(component_concept_id = concept_id,
                      routine_id) %>%
        dplyr::group_by(routine_id) %>%
        dplyr::mutate(component_count = n()) %>%
        dplyr::ungroup()

# Split input by routine_id to isolate groupings of components
input_b3 <-
        input_b2 %>%
        dplyr::select(-component_count) %>%
        split(input_b2$routine_id) %>%
        purrr::map(dplyr::select, -routine_id) %>%
        purrr::map(unlist)

# Querying first antineoplastic
output <- list()
for (i in 1:length(input_b3)) {
        routine <- input_b3[[i]]

        output[[i]] <-
        chariot::query_athena(
        paste0("SELECT concept_id_1,concept_id_2 FROM concept_relationship WHERE relationship_id = 'Has antineoplastic' AND concept_id_2 IN ", seagull::write_where_in_string(routine[1]))) %>%
                dplyr::rename(regimen_concept_id = concept_id_1,
                              component_concept_id = concept_id_2)
}
names(output) <- names(input_b3)

# Filtering resultsets for all components
output2 <- list()
for (i in 1:length(output)) {

        output2[[i]] <- output[[i]]

        for (j in 1:length(input_b3[[i]])) {
                output2[[i]] <-
                        output2[[i]] %>%
                        dplyr::filter(component_concept_id %in% input_b3[[i]][[j]])

                #secretary::press_enter()
        }
}
names(output2) <- names(output)


output3_part_a <-
        output2 %>%
        purrr::keep(~nrow(.) == 1)


if (length(output3_part_a) > 0) {
        output3_part_a <-
                output3_part_a %>%
                dplyr::bind_rows(.id = "routine_id") %>%
                dplyr::select(routine_id, regimen_concept_id) %>%
                rubix::mutate_to_integer(regimen_concept_id) %>%
                chariot::left_join_concept(column = "regimen_concept_id") %>%
                chariot::merge_concepts(into = "NewRegimen") %>%
                rubix::group_by_unique_aggregate(routine_id,
                                                 agg.col = NewRegimen,
                                                 collapse = "\n")
} else {
        output3_part_a <-
                dplyr::bind_rows(output3_part_a)
}


output3_part_b <-
        output2 %>%
        purrr::keep(~nrow(.) > 1)
output3_part_b_components <- input_b3[names(output3_part_b)]

output3_part_b2 <- list()
if (length(output3_part_b) > 0) {
        routine_ids <- names(output3_part_b)
        output3_part_b_components <-
                input_b3[routine_ids] %>%
                purrr::map(~length(.))

        for (i in 1:length(output3_part_b_components)) {
                component_length <- output3_part_b_components[[i]]
                if (component_length == 1) {
                        component_name <-
                        output3_part_b[[i]] %>%
                                dplyr::select(component_concept_id) %>%
                                dplyr::distinct() %>%
                                rubix::mutate_to_integer(component_concept_id) %>%
                                chariot::left_join_concept() %>%
                                dplyr::select(concept_name) %>%
                                unlist() %>%
                                unname()

                        output3_part_b2[[i]] <-
                                chariot::hemonc_monotherapy(component = component_name) %>%
                                rubix::filter_first_row()

                        if (nrow(output3_part_b2[[i]]) == 0) {

                                output3_part_b2[[i]] <-
                                tibble(concept_id = "NEW",
                                       concept_name = paste0(component_name, " monotherapy"))

                        }

                        names(output3_part_b2)[i] <- names(output3_part_b)[i]

                }

        }
}

if (length(output3_part_b2) > 0) {

        output3_part_b2 <-
                output3_part_b2 %>%
                purrr::map(mutate_all, as.character) %>%
                dplyr::bind_rows(.id = "routine_id") %>%
                chariot::merge_concepts(into = "NewRegimen") %>%
                rubix::group_by_unique_aggregate(routine_id,
                                                 agg.col = NewRegimen,
                                                 collapse = "\n")
} else {
        output3_part_b2 <-
                dplyr::bind_rows(output3_part_b2)
}

output3_part_c <-
        output2 %>%
        purrr::keep(~nrow(.) == 0)

# C: create NEW Regimen labels
output3_part_c2 <- input_b3[names(output3_part_c)]
output3_part_c3 <-
        output3_part_c2 %>%
        rubix::map_names_set(rubix::vector_to_tibble, new_col = "component_concept_id") %>%
        purrr::map(rubix::mutate_to_integer, component_concept_id) %>%
        purrr::map(chariot::left_join_concept) %>%
        purrr::map(dplyr::arrange, concept_name)

output3_part_c4 <- list()
for (i in 1:length(output3_part_c3)) {

        component_set <- output3_part_c3[[i]]

        if (nrow(component_set) == 1) {
                output3_part_c4[[i]] <-
                        paste0(
                        component_set %>%
                        dplyr::select(concept_name) %>%
                        unlist(),
                        " monotherapy")
        }

        if (nrow(component_set) == 2) {
                output3_part_c4[[i]] <-
                        paste(
                                component_set %>%
                                        dplyr::select(concept_name) %>%
                                        unlist(),
                                collapse = " and ")
        }

        if (nrow(component_set) > 2) {
                output3_part_c4[[i]] <-
                        paste(
                                component_set %>%
                                        dplyr::select(concept_name) %>%
                                        unlist(),
                                collapse = ", ")
        }
}
names(output3_part_c4) <- names(output3_part_c3)

output3_part_c5 <-
        output3_part_c4 %>%
        rubix::map_names_set(rubix::vector_to_tibble, "concept_name") %>%
        dplyr::bind_rows(.id = "routine_id") %>%
        dplyr::mutate(concept_id = "NEW") %>%
        dplyr::mutate(NewRegimen = paste0(concept_id, " ", concept_name)) %>%
        dplyr::select(routine_id, NewRegimen)



final_output <-
        list(output3_part_a,
             output3_part_b2,
             output3_part_c5) %>%
        dplyr::bind_rows()


final_routine_output <-
        dplyr::left_join(input,
                         final_output,
                         by = "routine_id")

qa1 <- nrow(final_routine_output) != nrow(input)
if (qa1) {
        stop("rows don't match")
}

qa2 <- length(unique(final_routine_output$routine_id)) != length(unique(input$routine_id))

if (qa2) {
        stop("missing routine ids")
}

broca::simply_write_csv(final_routine_output,
                        path_to_output_fn)


#         output %>%
#         rubix::map_names_set(function(x) x %>%
#                                                 dplyr::group_by(concept_id_1) %>%
#                                                 dplyr::mutate(component_count = length(unique(concept_id_2))) %>%
#                                      ungroup())
#
# output3 <-
#         output2 %>%
#         purrr::map2(output, function(x,y) x %>%
#                                                 dplyr::filter(component_count == length(y)))
#
# outpuoutput_b <-
#         dplyr::inner_join(input_b2,
#                          hemonc_regimen_concept2_3,
#                          by = c("component_concept_id", "component_count"),
#                          suffix = c(".map", ".hemonc"))
#
# # Each routine_id represents a single source regimen and `output_b` is at the component-level, meaning it will have a 1-to-many relationship between hemonc_regimen_concept_id and component_concept_id. For multiple possible regimens identified by component_concept_id to hemonc_regimen_concept_ids with matching counts, filtering further for grouped hemonc_regimen_concept_id counts to find regimens that map to the same set of components
# output_b2_i <-
#         output_b %>%
#         #dplyr::filter(!is.na(hemonc_regimen_concept_id)) %>%
#         dplyr::group_by(routine_id) %>%
#         dplyr::mutate(possible_hemonc_regimen_count = length(unique(hemonc_regimen_concept_id))) %>%
#         ungroup()
#
#
# output <-
#         output_b2_i %>%
#         chariot::left_join_concept(column = "hemonc_regimen_concept_id") %>%
#         chariot::merge_concepts(into = "NewRegimen") %>%
#         group_by(routine_id, component_count, possible_hemonc_regimen_count) %>%
#         summarize_at(vars(NewRegimen), function(x) paste0(x, collapse = "\n")) %>%
#         ungroup()
#
# final_routine_output <-
#         dplyr::left_join(input,
#                          output)
#
# qa1 <- nrow(final_routine_output) != nrow(input)
# if (qa1) {
#         stop("rows don't match")
# }
#
# qa2 <- length(unique(final_routine_output$routine_id)) != length(unique(input$routine_id))
#
# if (qa2) {
#         stop("missing routine ids")
# }
#
# broca::simply_write_csv(final_routine_output,
#                         path_to_output_fn)

# # Reincorporate routine_ids what have been completely filtered out because there isn't an instance of counts matching
# output_b2_ii <-
#         output_b[!(output_b$routine_id %in% output_b2_i$routine_id),] %>%
#         # All regimen and their counts are NA because there wasn't a match %>%
#         dplyr::mutate(hemonc_regimen_concept_id = NA) %>%
#         dplyr::mutate(hemonc_regimen_count = NA) %>%
#         # Deduplicating because the hemonc_regimen data has been normalized to a 1:1 relationship to the routine_id
#         dplyr::distinct() %>%
#         # Adding a "No Match" flag to output so I know that the input was run through this algorithm
#         dplyr::mutate(component_to_regimen_status = "No Match") %>%
#         # Returning concept_name to construct a NEW label
#         dplyr::mutate(component_concept_id = as.integer(component_concept_id)) %>%
#         chariot::left_join_concept(column = "component_concept_id",
#                                    include_synonyms = FALSE) %>%
#         dplyr::select(routine_id,
#                       component_to_regimen_status,
#                       component_concept_id,
#                       component_count,
#                       hemonc_regimen_concept_id,
#                       hemonc_regimen_count,
#                       component_concept_name = concept_name
#         )
#
# # Creating NEWRegimenConcept label
# output_b2_ii2 <-
#         output_b2_ii %>%
#         dplyr::group_by(routine_id) %>%
#         # Alphabetical order by routine_id
#         dplyr::arrange(component_concept_name)
#
# # Split by component count to generate the labels
# output_b2_ii3 <- split(output_b2_ii2,
#                        output_b2_ii2$component_count)
#
# # Are there any names in the object that could not be interpreted as a component count?
# qa <- any(is.na(names(output_b2_ii3) %>% as.integer()))
# if (qa) {
#         stop("Invalid component count")
# }
#
# if ("0" %in% names(output_b2_ii3)) {
#         stop("Cannot make a NEW regimen with component of length 0")
# }
#
#
# if ("1" %in% names(output_b2_ii3)) {
#         output_b2_ii3$`1` <-
#                 output_b2_ii3$`1` %>%
#                 dplyr::mutate(NewRegimenConcept = paste0("NEW ", component_concept_name, " monotherapy")) %>%
#                 dplyr::select(routine_id,NewRegimenConcept) %>%
#                 dplyr::distinct()
# }
#
#
# if ("2" %in% names(output_b2_ii3)) {
#         output_b2_ii3$`2` <-
#                 output_b2_ii3$`2` %>%
#                 dplyr::select(routine_id,
#                               component_concept_name) %>%
#                 rubix::group_by_unique_aggregate(routine_id,
#                                                  agg.col = component_concept_name,
#                                                  collapse = " and ") %>%
#                 dplyr::rename(NewRegimenConcept = component_concept_name) %>%
#                 dplyr::mutate(NewRegimenConcept = paste0("NEW ", NewRegimenConcept)) %>%
#                 dplyr::select(routine_id,NewRegimenConcept)
# }
#
# if ("3" %in% names(output_b2_ii3)) {
#         output_b2_ii3$`3` <-
#                 output_b2_ii3$`3` %>%
#                 dplyr::select(routine_id,
#                               component_concept_name) %>%
#                 rubix::group_by_unique_aggregate(routine_id,
#                                                  agg.col = component_concept_name,
#                                                  collapse = ", ") %>%
#                 dplyr::rename(NewRegimenConcept = component_concept_name) %>%
#                 dplyr::mutate(NewRegimenConcept = paste0("NEW ", NewRegimenConcept)) %>%
#                 dplyr::select(routine_id,NewRegimenConcept)
# }
#
# if ("4" %in% names(output_b2_ii3)) {
#         output_b2_ii3$`4` <-
#                 output_b2_ii3$`4` %>%
#                 dplyr::select(routine_id,
#                               component_concept_name) %>%
#                 rubix::group_by_unique_aggregate(routine_id,
#                                                  agg.col = component_concept_name,
#                                                  collapse = ", ") %>%
#                 dplyr::rename(NewRegimenConcept = component_concept_name) %>%
#                 dplyr::mutate(NewRegimenConcept = paste0("NEW ", NewRegimenConcept)) %>%
#                 dplyr::select(routine_id,NewRegimenConcept)
# }
#
# if ("5" %in% names(output_b2_ii3)) {
#         output_b2_ii3$`5` <-
#                 output_b2_ii3$`5` %>%
#                 dplyr::select(routine_id,
#                               component_concept_name) %>%
#                 rubix::group_by_unique_aggregate(routine_id,
#                                                  agg.col = component_concept_name,
#                                                  collapse = ", ") %>%
#                 dplyr::rename(NewRegimenConcept = component_concept_name) %>%
#                 dplyr::mutate(NewRegimenConcept = paste0("NEW ", NewRegimenConcept)) %>%
#                 dplyr::select(routine_id,NewRegimenConcept)
# }
#
# if (length(output_b2_ii3)) {
#         output_b2_ii3 <- dplyr::bind_rows(output_b2_ii3, .id = "component_count") %>%
#                 dplyr::mutate(component_count = as.integer(component_count))
#
#         output_b2_ii4 <-
#                 dplyr::left_join(output_b2_ii2,
#                                  output_b2_ii3)
#
#         output_b2 <- list(output_b2_i,
#                           output_b2_ii4)
# } else {
#         output_b2 <- list(output_b2_i)
# }
#
#
#
#
#
# output_b3 <-
#         output_b2 %>%
#         rubix::map_names_set(function(x) x %>%
#                                      dplyr::mutate_at(vars(contains("concept_id"), contains("count")),
#                                                       as.integer))
# output_b4 <-
#         dplyr::bind_rows(output_b3)
#
#
# # Add RegimenConcept
# output_b4_concept <-
#         chariot::left_join_concept(output_b4 %>%
#                                            dplyr::select(hemonc_regimen_concept_id),
#                                    include_synonyms = FALSE) %>%
#         chariot::merge_concepts(into = "HemoncRegimenConcept") %>%
#         dplyr::select(hemonc_regimen_concept_id,
#                       HemoncRegimenConcept) %>%
#         dplyr::distinct()
#
#
# # Add concept strips to output
# output_b5 <-
#         dplyr::left_join(output_b4,
#                          output_b4_concept)
#
# # Aggregate back to routine_id level
# output_b6 <-
#         output_b5 %>%
#         rubix::group_by_unique_aggregate(routine_id, component_to_regimen_status, component_count, hemonc_regimen_count, NewRegimenConcept,
#                                          agg.col = HemoncRegimenConcept)
#
# # QA
# nrow(output_b6)
# length(unique(output_b6$routine_id))
#
# final_output_b <-
#         dplyr::left_join(input,
#                          output_b6)
#
# nrow(input_map) == nrow(final_output_b)
# length(unique(input_map$routine_id)) == length(unique(final_output_b$routine_id))
# broca::view_as_csv(final_output_b)
#
#
#
# ###################
# #####################
# #
# input_c <-
#         input_map2 %>%
#         rubix::filter_at_grepl(Component,
#                                grepl_phrase = "NEW ",
#                                evaluates_to = FALSE,
#                                ignore.case = FALSE
#         ) %>%
#
#         # pivot longer
#         tidyr::separate_rows(Component,
#                              sep = "\n") %>%
#
#         # trimws to remove any trailing \r\r
#         dplyr::mutate_at(vars(Component), trimws) %>%
#
#         # renormalizing to na because trim introduces blank values
#         rubix::normalize_all_to_na() %>%
#
#         # remove all NA due to multiple spaces in 1:many mappings
#         dplyr::filter(!is.na(Component)) %>%
#
#         # unmerge to capture concept_id
#         chariot::unmerge_concepts(Component)
#
#
# qa <-
#         input_c %>%
#         rubix::filter_for(concept_class_id, c("Regimen", "Component"), invert = TRUE)
#
# if (nrow(qa) > 0) {
#
#         stop('Unrecognized concept_class_id: ', paste(qa$concept_class_id, collapse = ", "))
#
# }
#
# # Difference between Part B and C is that for the input, any Regimens in the source Component list needs to have their components isolated
# input_c_i <-
#         input_c %>%
#         rubix::filter_for(concept_class_id,
#                           inclusion_vector = "Regimen",
#                           invert = TRUE) %>%
#         rubix::rename_at_prefix(!routine_id,
#                                 prefix = "component_")
#
# input_c_ii <-
#         input_c %>%
#         rubix::filter_for(concept_class_id,
#                           inclusion_vector = "Regimen",
#                           invert = FALSE) %>%
#         rubix::rename_at_prefix(!routine_id,
#                                 prefix = "regimen_")
#
# input_c_ii_concept2 <-
#         input_c_ii %>%
#         rubix::mutate_to_integer(regimen_concept_id) %>%
#         dplyr::select(regimen_concept_id) %>%
#         chariot::pivot_concept2(names_from = "concept_class_id") %>%
#         dplyr::select(regimen_concept_id = concept_id_1,
#                       Component) %>%
#         tidyr::separate_rows(Component, sep = "\n") %>%
#         chariot::unmerge_concepts(Component) %>%
#         dplyr::filter(!is.na(concept_id)) %>%
#         rubix::rename_at_prefix(!regimen_concept_id,
#                                 prefix = "component_") %>%
#         dplyr::select(regimen_concept_id, component_concept_id)
#
# input_c_ii2 <-
#         list(input_c_ii,
#              input_c_ii_concept2) %>%
#         purrr::map(function(x) x %>% mutate_at(vars(contains("concept_id")), as.integer))
#
# input_c_ii3 <-
#         input_c_ii2 %>%
#         purrr::reduce(left_join)
#
# input_c2 <- list(input_c_i,
#                  input_c_ii3) %>%
#         purrr::map(function(x) x %>% mutate_at(vars(contains("concept_id")), as.integer))
#
#
# input_c3 <-
#         dplyr::bind_rows(input_c2) %>%
#         dplyr::select(component_concept_id,
#                       regimen_concept_id,
#                       routine_id) %>%
#         dplyr::group_by(routine_id) %>%
#         dplyr::mutate(component_count = n()) %>%
#         dplyr::ungroup()
#
#
# # Getting all the relationships these components have with Regimens
# # Derive HemOnc Regimens from Source Components
# hemonc_regimens <-
#         chariot::query_athena("SELECT * FROM concept WHERE vocabulary_id = 'HemOnc' AND concept_class_id = 'Regimen'") %>%
#         chariot::filter_for_valid()
#
#
# hemonc_regimen_concept2 <-
#         chariot::pivot_concept2(hemonc_regimens %>%
#                                         dplyr::select(regimen_concept_id = concept_id) %>%
#                                         dplyr::mutate_all(as.integer),
#                                 names_from = "concept_class_id") %>%
#         dplyr::select(hemonc_regimen_concept_id = concept_id_1,
#                       Component,
#                       `Component Count`)
#
# hemonc_regimen_concept22 <-
#         hemonc_regimen_concept2 %>%
#         tidyr::separate_rows(Component, sep = "\n")
#
# hemonc_regimen_concept23 <-
#         hemonc_regimen_concept22 %>%
#         chariot::unmerge_concepts(concept_col = Component) %>%
#         dplyr::select(component_concept_id = concept_id,
#                       hemonc_regimen_concept_id,
#                       component_count = `Component Count`) %>%
#         # Remove NA component_counts, which are Regimens that do not have a 'Has antineoplastic' relationship
#         dplyr::filter(!is.na(component_count)) %>%
#         # Reordering columns: Components with their grouped counts from my map, all hemonc_regimen_concept_ids that each component is associated with in HemOnc
#         dplyr::select(starts_with("component"), everything()) %>%
#         rubix::mutate_to_integer(contains("concept_id"))
#
# # Join map with hemonc regimens by component_concept_id and component_count to get all regimens associated with the source components and their counts with component counts from hemonc
# output_c <-
#         dplyr::left_join(input_c3,
#                          hemonc_regimen_concept23,
#                          by = c("component_concept_id", "component_count"),
#                          suffix = c(".map", ".hemonc"))
#
# # Each routine_id represents a single source regimen and `output_c` is at the component-level, meaning it will have a 1-to-many relationship between hemonc_regimen_concept_id and component_concept_id. For multiple possible regimens identified by component_concept_id to hemonc_regimen_concept_ids with matching counts, filtering further for grouped hemonc_regimen_concept_id counts to find regimens that map to the same set of components
# output_c2_i <-
#         output_c %>%
#         dplyr::group_by(routine_id, hemonc_regimen_concept_id) %>%
#         dplyr::mutate(hemonc_regimen_count = length(hemonc_regimen_concept_id)) %>%
#         dplyr::filter(component_count == hemonc_regimen_count) %>%
#         ungroup()
#
# # Reincorporate routine_ids what have been completely filtered out because there isn't an instance of counts matching
# output_c2_ii <-
#         output_c[!(output_c$routine_id %in% output_c2_i$routine_id),] %>%
#         # All regimen and their counts are NA because there wasn't a match %>%
#         dplyr::mutate(hemonc_regimen_concept_id = NA) %>%
#         dplyr::mutate(hemonc_regimen_count = NA) %>%
#         # Deduplicating because the hemonc_regimen data has been normalized to a 1:1 relationship to the routine_id
#         dplyr::distinct() %>%
#         # Adding a "No Match" flag to output so I know that the input was run through this algorithm
#         dplyr::mutate(component_to_regimen_status = "No Match") %>%
#         # Returning concept_name to construct a NEW label
#         dplyr::mutate(component_concept_id = as.integer(component_concept_id)) %>%
#         chariot::left_join_concept(column = "component_concept_id",
#                                    include_synonyms = FALSE) %>%
#         dplyr::select(routine_id,
#                       component_to_regimen_status,
#                       component_concept_id,
#                       regimen_concept_id,
#                       component_count,
#                       hemonc_regimen_concept_id,
#                       hemonc_regimen_count,
#                       component_concept_name = concept_name
#         )
#
# # Creating NEWRegimenConcept label
# output_c2_ii2 <-
#         output_c2_ii %>%
#         dplyr::group_by(routine_id) %>%
#         # Alphabetical order by routine_id
#         dplyr::arrange(component_concept_name)
#
# # Split by component count to generate the labels
# output_c2_ii3 <- split(output_c2_ii2,
#                        output_c2_ii2$component_count)
#
# # Are there any names in the object that could not be interpreted as a component count?
# qa <- any(is.na(names(output_c2_ii3) %>% as.integer()))
# if (qa) {
#         stop("Invalid component count")
# }
#
# if ("0" %in% names(output_c2_ii3)) {
#         stop("Cannot make a NEW regimen with component of length 0")
# }
#
# output_c2_ii4 <- list()
# if ("1" %in% names(output_c2_ii3)) {
#         output_c2_ii4[[1]] <-
#                 output_c2_ii3$`1` %>%
#                 dplyr::mutate(NewRegimenConcept = paste0("NEW ", component_concept_name, " monotherapy")) %>%
#                 dplyr::select(routine_id,NewRegimenConcept, component_count) %>%
#                 dplyr::distinct()
# }
#
#
# if ("2" %in% names(output_c2_ii3)) {
#         output_c2_ii4[[2]] <-
#                 output_c2_ii3$`2` %>%
#                 dplyr::select(routine_id,
#                               component_concept_name,
#                               component_count) %>%
#                 dplyr::group_by(routine_id, component_count) %>%
#                 dplyr::arrange(component_concept_name) %>%
#                 dplyr::summarize_at(vars(component_concept_name),
#                                     function(x) paste(unique(x), collapse = " and ")) %>%
#                 dplyr::ungroup() %>%
#                 dplyr::rename(NewRegimenConcept = component_concept_name) %>%
#                 dplyr::mutate(NewRegimenConcept = paste0("NEW ", NewRegimenConcept)) %>%
#                 dplyr::select(routine_id,NewRegimenConcept, component_count)
# }
#
#
# output_c2_ii4[[3]] <-
#         output_c2_ii3[!(names(output_c2_ii3) %in% c("1","2"))] %>%
#         dplyr::bind_rows() %>%
#         dplyr::select(routine_id,
#                       component_count,
#                       component_concept_name) %>%
#         dplyr::distinct() %>%
#         dplyr::group_by(routine_id, component_count) %>%
#         dplyr::arrange(component_concept_name) %>%
#         dplyr::summarize_at(vars(component_concept_name),
#                             function(x) paste(unique(x), collapse = ", ")) %>%
#         dplyr::ungroup() %>%
#         dplyr::rename(NewRegimenConcept = component_concept_name) %>%
#         dplyr::mutate(NewRegimenConcept = paste0("NEW ", NewRegimenConcept)) %>%
#         dplyr::select(routine_id,NewRegimenConcept, component_count)
#
#
# output_c2_ii4 <- dplyr::bind_rows(output_c2_ii4)
#
# output_c2_ii5 <-
#         dplyr::left_join(output_c2_ii2,
#                          output_c2_ii4)
#
# output_c2 <- list(output_c2_i,
#                   output_c2_ii5)
#
#
# output_c3 <-
#         output_c2 %>%
#         rubix::map_names_set(function(x) x %>%
#                                      dplyr::mutate_at(vars(contains("concept_id"), contains("count")),
#                                                       as.integer))
# output_c4 <-
#         dplyr::bind_rows(output_c3)
#
#
# # Add RegimenConcept
# output_c4_concept <-
#         chariot::left_join_concept(output_c4 %>%
#                                            dplyr::select(hemonc_regimen_concept_id),
#                                    include_synonyms = FALSE) %>%
#         chariot::merge_concepts(into = "HemoncRegimenConcept") %>%
#         dplyr::select(hemonc_regimen_concept_id,
#                       HemoncRegimenConcept) %>%
#         dplyr::distinct()
#
#
# # Add concept strips to output
# output_c5 <-
#         dplyr::left_join(output_c4,
#                          output_c4_concept)
#
# # Aggregate back to routine_id level
# output_c6 <-
#         output_c5 %>%
#         rubix::group_by_unique_aggregate(routine_id, component_to_regimen_status, component_count, hemonc_regimen_count, NewRegimenConcept,
#                                          agg.col = HemoncRegimenConcept)
#
# # QA
# nrow(output_c6)
# length(unique(output_c6$routine_id))
#
# final_output_c <-
#         dplyr::left_join(input,
#                          output_c6,
#                          by = "routine_id")
#
# nrow(input_map) == nrow(final_output_c)
# length(unique(input_map$routine_id)) == length(unique(final_output_c$routine_id))
# broca::view_as_csv(final_output_c)
# final_routine_output <-
#         dplyr::left_join(final_input,
#                          output3)
#
#
# broca::simply_write_csv(final_routine_output,
#                       path_to_output_fn)
