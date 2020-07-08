##############################################
### INTRODUCTION
##############################################
# Clear env
# clean_env()

# Source function and vars
# source("startup.R")

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
target_col <- source_col

# Normalize NAs because some are "NA" and others are true NA
input2 <-
        input %>%
        normalize_na()

# Create final input object to join with final output object before export
final_input <- input2

# Select only relevant columns
input3 <-
        input2 %>%
        dplyr::select(routine_id, all_of(target_col), !!attribute_col)


# If any routine_id is NA
qa1 <-  input3$routine_id[is.na(input3$routine_id)]

if (length(qa1) > 0) {

        stop("routine_ids present in the input are missing in input3")

}

##############################################
### PROCEDURE
##############################################
# Create output
output <- list()

# Notify start
typewrite_start(type = type)



for (i in 1:nrow(input3)) {

        # Per row
        input_row <- input3 %>%
                dplyr::filter(row_number() == i)


        if (!(is.na(input_row %>%
                    dplyr::select(!!attribute_col) %>%
                    unlist() %>%
                    unname()))) {

                        input_row2 <-
                                input_row %>%
                                tidyr::separate_rows(!!attribute_col,
                                                     sep = "\n") %>%
                                dplyr::mutate_all(trimws) %>%
                                tidyr::extract(col = !!attribute_col,
                                               into = c("SearchPrefix", "Attribute"),
                                               regex = "(^.*?[:]{1}[ ]{0,1})(.*$)") %>%
                                dplyr::select(-SearchPrefix) %>%
                                chariot::unmerge_concepts(concept_col = Attribute) %>%
                                dplyr::mutate(concept_id = as.integer(concept_id)) %>%
                                dplyr::select(routine_id,
                                              !!target_col,
                                              attribute_concept_id = concept_id)

                        # Number of components in the source data
                        source_component_count <- length(unique(input_row2$attribute_concept_id))

                        # Getting all the regimens associated with the component
                        output_a <-
                        chariot::left_join_relationship(input_row2 %>%
                                                                dplyr::select(attribute_concept_id),
                                                        merge_concept2 = FALSE) %>%
                                dplyr::filter(relationship_id == "Antineoplastic of") %>%
                                dplyr::select(source_component_concept_id = concept_id_1,
                                              regimen_concept_id = concept_id_2) %>%
                                dplyr::group_by(regimen_concept_id) %>%
                                dplyr::mutate(regimen_count = length(regimen_concept_id)) %>%
                                #If the regimen isn't documented for the same number of components
                                dplyr::filter(regimen_count == source_component_count) %>%
                                dplyr::select(-regimen_count)



                        output_b <-
                        output_a %>%
                                # Getting the component count for all the regimens
                                chariot::left_join_relationship(column = "regimen_concept_id",
                                                                merge_concept2 = FALSE) %>%
                                dplyr::filter(relationship_id == "Has antineoplastic") %>%
                                rubix::summarize_grouped_n(concept_id_1) %>%
                                dplyr::select(regimen_concept_id = concept_id_1,
                                              regimen_component_count = n) %>%
                                #Finding regimens with component counts that equal the component counts of the source
                                dplyr::filter(regimen_component_count == source_component_count)


                        output[[i]] <-
                                dplyr::inner_join(output_a,
                                                  output_b) %>%
                                dplyr::select(-regimen_component_count) %>%
                                chariot::left_join_concept(column = "regimen_concept_id",
                                                           include_synonyms = FALSE) %>%
                                chariot::merge_concepts(into = !!new_col_name) %>%
                                dplyr::select(!!new_col_name)

                        names(output)[i] <- input_row$routine_id
                        typewrite_percent_progress(i = i, input = input3)

                        rm(input_row)
                        rm(input_row2)
                        gc()
                        Sys.sleep(1)
        }
}

output <- output[names(output) %>%
                         centipede::no_na()]

output2 <-
        output %>%
        purrr::keep(~nrow(.)>0) %>%
        purrr::map(function(x) x %>%
                                rubix::arrange_by_nchar(`HemOnc Component to Regimen`)) %>%
        purrr::map(function(x) x %>% dplyr::slice(1:250)) %>%
        purrr::map(function(x) dplyr::bind_rows(x)) %>%
        dplyr::bind_rows(.id = "routine_id")

output3 <-
        output2 %>%
        rubix::group_by_unique_aggregate(routine_id,
                                         agg.col = "HemOnc Component to Regimen",
                                         collapse = "\n")


# output3a <-
#         output2 %>%
#         rubix::map_names_set(function(x) x %>%
#                                                 dplyr::select(-concept_id_1) %>%
#                                                 dplyr::select_if(function(y) all(!is.na(y)))
#                 )
#
#
# output3b <-
#         output2 %>%
#         rubix::map_names_set(function(x) x %>%
#                                      dplyr::select(concept_id_1)
#                              )
#
#
# output4 <-
#         list(output3a,
#              output3b) %>%
#         transpose()
#
# output5 <-
#         output4 %>%
#         rubix::map_names_set(function(x) bind_cols(x))
#
# output6 <-
# output5 %>%
#         purrr::keep(~ncol(.) > 1) %>%
#         purrr::keep(~nrow(.)>1) %>%
#         purrr::map(~tidyr::pivot_longer(.,cols = !concept_id_1,
#                                         names_to = "regimen_concept_id")) %>%
#         dplyr::bind_rows(.id = "routine_id") %>%
#         dplyr::select(routine_id,
#                       concept_id_1,
#                       regimen_concept_id) %>%
#         dplyr::mutate(regimen_concept_id = as.integer(regimen_concept_id))
#
#
# output7 <-
#         chariot::left_join_concept(output6,
#                                    .column = "regimen_concept_id")
#
# output8 <-
#         output7 %>%
#         chariot::merge_concepts(into = "RegimenConcept") %>%
#         dplyr::select(routine_id,
#                       RegimenConcept) %>%
#         rubix::group_by_unique_aggregate(routine_id,
#                                          agg.col = RegimenConcept,
#                                          collapse = "\n")


final_routine_output <-
        dplyr::left_join(final_input,
                         output3)


broca::simply_write_csv(final_routine_output,
                      path_to_output_fn)
