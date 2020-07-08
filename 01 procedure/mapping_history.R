input <-
        list.files("~/GitHub/omop_mapping/01 procedure/data/input", pattern = "COVID", full.names = TRUE) %>%
        rubix::map_names_set(broca::simply_read_csv) %>%
        dplyr::bind_rows(.id = "SOURCE_CSV")


input2 <-
        input %>%
        dplyr::group_by(CONCEPT, `MSK Concept Type`, `MSK Concept`) %>%
        dplyr::arrange(desc(SOURCE_CSV)) %>%
        rubix::filter_first_row() %>%
        dplyr::ungroup()

input3 <-
        input2 %>%
        rubix::deselect_if_all_na()

input3 %>%
        rubix::filter_at_grepl(CONCEPT,
                               grepl_phrase = "Candesartan") %>%
        dplyr::select(`MSK Concept`)


#input <- broca::read_clipboard(header = FALSE)
input2 <-
        input %>%
        dplyr::mutate(V1 = stringr::str_remove_all(V1, "MSK"))

output <-
        chariot::left_join_concept_id(input2,
                                      include_synonyms = FALSE) %>%
        chariot::merge_concepts(into = Concept, keep_other_cols = F)
