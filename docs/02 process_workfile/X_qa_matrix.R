input <- broca::simply_read_csv(path_to_input_fn)

# Unmerge
input$`Attribute Concept` <- NULL
input$`Modifier Concept` <- NULL


input2 <-
        input %>%
        dplyr::group_by(FORM, VARIABLE, TYPE, CONCEPT) %>%
        dplyr::mutate(COMPONENT_COUNT = n())

output_a_one_to_one <-
        input2 %>%
        dplyr::filter(COMPONENT_COUNT == 1)

output_a_one_to_one_ii <-
        output_a_one_to_one %>%
        chariot::unmerge_concepts(`MSK Concept`, remove = FALSE)

output_a_one_to_one_iii <-
        output_a_one_to_one_ii %>%
        dplyr::mutate(`MSK Concept Shape` = ifelse(!is.na(concept_id), paste0("MSK", concept_id), NA)) %>%
        dplyr::mutate(`MSK Concept Shape` = coalesce(`MSK Concept Shape`, `MSK Concept`))

output_b_one_to_n_i <-
        input2 %>%
        dplyr::filter(COMPONENT_COUNT != 1)

output_b_one_to_n_ii <-
        output_b_one_to_n_i %>%
        dplyr::mutate(`MSK Concept Shape` = "NEW")

output_b_one_to_n_iii <-
        output_b_one_to_n_ii %>%
        dplyr::select(VARIABLE:CONCEPT, `MSK Concept Shape`) %>%
        dplyr::distinct()


final_output <-
        dplyr::bind_rows(output_a_one_to_one_iii,
                         output_b_one_to_n_iii)

# All 1 to many concepts are accounted for


### Add uri
topbraid_uris <- broca::simply_read_csv("~/OneDrive - Memorial Sloan Kettering Cancer Center/Glioma/gliomas_topbraid_export.csv")
topbraid_uris2 <-
        topbraid_uris %>%
        tidyr::pivot_longer(cols = c(VARIABLE, PV),
                            names_to = "TYPE",
                            values_to = "CONCEPT",
                            values_drop_na = TRUE) %>%
        dplyr::mutate(TYPE = stringr::str_replace_all(TYPE, "VARIABLE", "Variable")) %>%
        dplyr::mutate(TYPE = stringr::str_replace_all(TYPE, "PV", "Permissible Value")) %>%
        dplyr::distinct()


final_output2 <-
        final_output %>%
        dplyr::left_join(topbraid_uris2) %>%
        dplyr::distinct()


# Left join on VARIABLE since the export was provided
final_output3_var <-
        final_output2 %>%
        dplyr::left_join(topbraid_uris2, by = "TYPE") %>%
        dplyr::distinct() %>%
        dplyr::filter(VARIABLE == CONCEPT.y) %>%
        dplyr::select(-starts_with("PV_URI"))


broca::view_as_csv(final_output3_var)


topbraid_uris2b <-
        topbraid_uris %>%
        dplyr::filter(!is.na(PV_URI)) %>%
        dplyr::rename(CONCEPT = PV) %>%
        dplyr::mutate(TYPE = "Permissible Value")

final_output3_pv <-
        final_output %>%
        dplyr::left_join(topbraid_uris2b) %>%
        dplyr::filter(TYPE == "Permissible Value") %>%
        dplyr::distinct()


broca::view_as_csv(final_output3_pv)


final_output5 <-
        dplyr::bind_rows(final_output3_var,
                         final_output4_pv)

broca::copy_to_clipboard(final_output5)
