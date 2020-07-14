# Derive HemOnc components from Regimens
# Current mappings are either by component or by regimen and they are being reconciled for a complete representation
input <- read_input()
path_to_output_fn <- create_path_to_output_fn()
brake_if_output_exists()

input_map <- input %>%
        dplyr::filter_at(vars(Component), any_vars(is.na(.))) %>%
        dplyr::filter_at(vars(Regimen), any_vars(!is.na(.))) %>%
        #Remove NEW without preferred label
        dplyr::filter_at(vars(Regimen), any_vars(!(. %in% "NEW"))) %>%
        dplyr::select(routine_id,
                      Regimen)

# Regimens are 1 of 3 classes: a) blank (because groups of components are mapped instead), b) NEW {Preferred Label} that is in a parseable format to derive expected HemOnc components, c) OMOP concept from which the components can be derived using HemOnc. We are focusing on b and c here.
input_a_new <-
        input_map %>%
        rubix::filter_at_grepl(Regimen,
                               grepl_phrase = "NEW ",
                               evaluates_to = TRUE,
                               ignore.case = FALSE)

# Separating NEW regimens out into components
output_a_new <-
        input_a %>%
        tidyr::extract(col = Regimen,
                       into = c("new_concept_id", "new_concept_name"),
                       regex = "(NEW )(.*$)",
                       remove = FALSE) %>%
        tidyr::separate(col = new_concept_name,
                        into = paste0("Component_", 1:(3+max(centipede::nchar_comma(input_a$Regimen)))),
                        sep = "[,]{1} | and | monotherapy") %>%
        dplyr::mutate_all(trimws) %>%
        # Normalize blanks to NA for filtering later
        rubix::normalize_all_to_na()

# Isolate the component names for an exact string match at the component level
output_a_new2 <-
        output_a_new %>%
        # Melt all components into a longer pivot for a left join
        tidyr::pivot_longer(cols = starts_with("Component_"),
                            names_to = "Component_Index",
                            values_to = "source_component",
                            values_drop_na = TRUE)


output_a2_concept_ids <-
        output_a_new2$source_component %>%
        rubix::map_names_set(function(x) chariot::query_phrase(x,
                                                               type = "exact",
                                                               where_col = "vocabulary_id",
                                                               where_col_in = "HemOnc")) %>%
        purrr::keep(~nrow(.)>0) %>%
        dplyr::bind_rows(.id = "source_component") %>%
        dplyr::distinct() %>%
        chariot::merge_concepts(into = "Component",
                                concept_class_id)


output_a_new3 <-
        output_a_new2 %>%
        dplyr::left_join(output_a2_concept_ids)

# If any of the outputs are not concept_class_id Components, need to derive the Component (such as if the match was a brand name)
qa1 <-
        output_a_new3 %>%
        dplyr::filter(!(concept_class_id %in% c(NA, "Component")))

if (nrow(qa1) > 0) {

        # Since all results are HemOnc, pivoting by relationship to get the Component
        output_a_new3_c2 <-
                chariot::pivot_concept2(qa1 %>%
                                                dplyr::select(input_concept_id = concept_id) %>%
                                                rubix::mutate_to_integer(input_concept_id),
                                        names_from = "concept_class_id",
                                        include_count = F) %>%
                rename(concept_id = concept_id_1)

        output_a_new4 <-
                dplyr::left_join(output_a_new3,
                                 output_a_new3_c2,
                                 by = "concept_id")

        output_a_new5 <-
                output_a_new4  %>%
                mutate(Component = coalesce(Component.y,
                                            Component.x)) %>%
                dplyr::select(-Component.y,
                              -Component.x)

} else {
        output_a_new5 <- output_a_new3
}

output_a4 <-
        output_a_new5 %>%
        # Filter out NA components (components that did not have a match)
        dplyr::filter(!is.na(Component))

# Correct NEW regimen preferred labels that did not contain a HemOnc component (ie called by Brand Name instead)
qa2 <-
        output_a4 %>%
        dplyr::filter(!(concept_class_id %in% c("Component")))

output_a5_part_i <- tibble()
while (nrow(qa2) > 0) {

        qa_row <-
                qa2 %>%
                rubix::filter_first_row()

        wrong_component_name <-
                qa_row %>%
                dplyr::select(source_component) %>%
                unlist()

        replacement_name <-
                chariot::unmerge_concepts(qa_row,
                                          concept_col = "Component") %>%
                        dplyr::select(concept_name) %>%
                        unlist()

        output_a5_part_i <-
                dplyr::bind_rows(output_a5_part_i,
                                qa_row %>%
                                       dplyr::mutate(NewRegimen = stringr::str_replace(Regimen,
                                                                                       pattern = wrong_component_name,
                                                                                       replacement = replacement_name)))


        qa2 <-
                qa2 %>%
                rubix::slice_off_first_row()

}

output_a5_part_i <-
        output_a5_part_i %>%
        dplyr::select(routine_id,
                      NewRegimen)

output_a5_part_ii <-
        output_a4 %>%
        dplyr::rename(NewComponent = Component) %>%
        dplyr::select(routine_id,
                      NewComponent) %>%
        rubix::group_by_aggregate(routine_id,
                                  agg.col = "NewComponent",
                                  collapse = "\n")



output_a6 <-
        list(output_a5_part_i,
             output_a5_part_ii) %>%
        purrr::reduce(full_join)


final_output_a <-
        dplyr::left_join(input,
                         output_a6)

# QA
qa3 <- nrow(input) != nrow(final_output_a)
if (qa3) {

        stop("Row counts between input and final output do not match")
}

qa4 <- length(unique(input$routine_id)) != length(unique(final_output_a$routine_id))
if (qa4) {

        stop("Final output is missing routine_ids")

}


broca::simply_write_csv(final_output_a,
                        file = path_to_output_fn)
