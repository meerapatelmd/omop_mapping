output <-
        chariot::query_phrase("fibrinogen",
                              type = "like") %>%
        dplyr::filter(vocabulary_id %in% c("LOINC"))


chariot::add_loinc_system_col(output) %>%
        chariot::filter_out_uncommon_loinc_systems(loinc_systems_col = LOINC_System) %>%
        chariot::filter_for_standard() %>%
        dplyr::filter(LOINC_System %in% c("Plasma", "Serum", "Serum and Plasma", "Blood"))


chariot::get_merged_concept_id("3037202")
