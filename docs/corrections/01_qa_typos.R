rm(list = ls())
source("setup.R")

input <- read_file_to_correct()


# Converting to true NA
input <-
        input %>%
        dplyr::mutate_all(stringr::str_remove_all, "[\r\n\t]") %>%
        dplyr::mutate_all(stringr::str_replace_na) %>%
        dplyr::mutate_all(stringr::str_replace_all, "^NA$", NA_character_)



# Select only the original parsed redcap column names and `Concept` columns
input2 <-
        input %>%
        dplyr::select(routine_id,
                      any_of(workfile_colnames),
                      ends_with(" Concept"))

# Separate `Concept` columns based on \n
input3 <-
        input2 %>%
        tidyr::separate_rows(`MSK Concept`,
                             sep = "\n")

# QA the Concept strip so the unmerge process will be error free (ie [V] [S] {concept id} {concept name} works)
qa1 <-
        input3 %>%
        filter_at(vars(`MSK Concept`),
                  #any_vars(grepl("\\[.{1}\\] \\[.{1}\\] [^ ]* .*? \\[.*? .*?\\] \\[.*?\\] \\[.*?\\]", .) == FALSE)) %>%
                  any_vars(grepl("^\\[|NEW", .) == FALSE)) %>%
        filter_at(vars(`MSK Concept`),
                  any_vars(!is.na(.)))

if (nrow(qa1) > 0) {
        secretary::typewrite_error("There are", nrow(qa1), "observations in unparseable format. Enter to the concept_id for each of these observations so they can be normalized to the correct format.")
        secretary::press_enter()

        temp_path <- broca::write_temp_csv(qa1 %>%
                                                   dplyr::select(FAILED_QA = `MSK Concept`) %>%
                                                   rubix::mutate1(concept_id = ""))

        system(paste0("open ", temp_path))

        secretary::typewrite_italic("Continue after concept_ids are updated.")
        secretary::press_enter()

        qa1_corrected <- broca::simply_read_csv(temp_path) %>%
                                #Trimws of concept ids
                                rubix::call_mr_clean()

        qa1_corrected_concepts <-
                chariot::left_join_df_to_concept(qa1_corrected %>%
                                                         dplyr::select(qa1_concept_id = concept_id)) %>%
                dplyr::select(-qa1_concept_id) %>%
                chariot::merge_concepts(into = `MSK Concept`)

        qa1_corrected2 <-
                dplyr::left_join(qa1_corrected,
                                 qa1_corrected_concepts) %>%
                dplyr::select(-concept_id)

        input5 <-
                dplyr::left_join(input4,
                                 qa1_corrected2,
                                 by = c("MSK Concept" = "FAILED_QA"),
                                 suffix = c(".current", ".new")) %>%
                dplyr::mutate(`MSK Concept` = coalesce(`MSK Concept.new`, `MSK Concept`)) %>%
                dplyr::select(-`MSK Concept.new`)

        qa2 <-
                input5 %>%
                filter_at(vars(`MSK Concept`),
                          any_vars(grepl("\\[.{1}\\] \\[.{1}\\] [^ ]* .*? \\[.*? .*?\\] \\[.*?\\] \\[.*?\\]", .) == FALSE)) %>%
                filter_at(vars(`MSK Concept`),
                          any_vars(!is.na(.)))

        if (nrow(qa2) > 0) {
                stop("Errors still found after corrections:", qa2$routine_id)
        } else {

                input6 <-
                        input5 %>%
                        tidyr::pivot_wider(#id_cols = routine_id,
                                names_from = `MSK Concept Type`,
                                values_from = `MSK Concept`,
                                values_fn = list(`MSK Concept` = function(x) paste(x, collapse = "\n")))

                input7 <-
                        input6 %>%
                        dplyr::select(all_of(colnames(input2)))

                # Overwrite input so don't have to fix over and over again
                qa3 <- all(input4$routine_id %in% input7$routine_id)

                if (qa3 == TRUE) {
                        broca::simply_write_csv(x = input7,
                                                file = path_to_input_fn)
                }
        }

}

#Removing all objects with "input[0-9]" pattern from the global environment
cave::rm_all_end_with("[0-9]{1}")
