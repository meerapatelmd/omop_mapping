input <- read_workfile(routine = "execute pre-procedure.R")
input2 <-
        input %>%
        tidyr::extract(col = PV_CODE,
                        into = c("MSK Prefix", "msk_concept_id"),
                        regex = "(MSK)(.*$)")

output <-
        chariot::left_join_df_to_concept(input2 %>%
                                                 dplyr::select(msk_concept_id)) %>%
        chariot::merge_concepts(into = "Fact Concept") %>%
        dplyr::select(-concept_id, -msk_concept_id)


qa1 <- nrow(output)-nrow(input2)
if (qa1 != 0) {
        stop("output does not match input")
}

final_output <-
        dplyr::bind_cols(input2,
                         output)

broca::copy_to_clipboard(final_output)
