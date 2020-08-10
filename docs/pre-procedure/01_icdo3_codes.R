input <- read_workfile(routine = "execute pre-procedure.R")
input2 <-
        input %>%
        tidyr::extract(col = CONCEPT,
                        into = c("icdo3_concept_code", "icdo03_concept_name"),
                        regex = "(^C[0-9]{1,}[.]{0,1}[0-9]+?) (.*$)",
                       remove = FALSE)

output <-
        chariot::left_join_df(input2 %>%
                                      dplyr::select(icdo3_concept_code),
                              athena_table = "concept",
                              athena_column = "concept_code" #,
                              #where_athena_col = "vocabulary_id",
                             # where_athena_col_equals = "ICDO3"
                             ) %>%
        dplyr::filter(!is.na(concept_id)) %>%
        chariot::merge_concepts(into = "ICDO3 Concept", concept_code, vocabulary_id)



output2_match_counts <-
        output %>%
        tidyr::pivot_wider(id_cols = icdo3_concept_code,
                           names_from = vocabulary_id,
                           values_from = concept_id,
                           values_fn = list(concept_id = ~length(unique(.))))

# QA: all matches have an ICDO3 counterpart in Athena
qa1 <-
        output2_match_counts %>%
        dplyr::filter(is.na(ICDO3))

if (nrow(qa1) > 0) {

        stop("Not all input codes have ICD-O-3 codes in Athena")

}

output2 <-
        output %>%
        dplyr::filter(vocabulary_id == "ICDO3")


final_output <-
        dplyr::left_join(input2,
                         output2,
                         by = "icdo3_concept_code") %>%
        #dplyr::mutate("MSK Concept" = coalesce(`MSK Concept.x`, `MSK Concept.y`)) %>%
        #dplyr::select(-ends_with(".x"), -ends_with(".y")) %>%
        dplyr::distinct()

broca::copy_to_clipboard(final_output)
