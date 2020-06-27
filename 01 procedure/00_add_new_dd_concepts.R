# Variables
prior_dd_path <- "~/GitHub/KMI/standardization-projects/glioma (neuronc)/ClinicalRadiologicalAndMolecul_DataDictionary_2019-10-22.csv"
newest_dd_path <- "~/GitHub/MSK-patelm9/escritoire/02.Neuroncology/data_without_phi/"

prior_dd <-
        broca::simply_read_csv(prior_dd_path) %>%
        cartograph::standardize_dd_cols() %>%
        dplyr::select(FORM,
                      VARIABLE,
                      FIELD_TYPE,
                      FIELD_LABEL,
                      CHOICES,
                      FIELD_NOTE)

newest_dd <-
        broca::simply_read_csv(newest_dd_path) %>%
        cartograph::standardize_dd_cols() %>%
        dplyr::select(FORM,
                      VARIABLE,
                      FIELD_TYPE,
                      FIELD_LABEL,
                      CHOICES,
                      FIELD_NOTE)

newest_dd_diff <-
newest_dd %>%
        dplyr::setdiff(prior_dd) %>%
        dplyr::left_join(broca::simply_read_csv(newest_dd_path) %>%
                                 cartograph::standardize_dd_cols())


newest_dd_diff_parsed <-
        cartograph::parse_dd_df(newest_dd_diff,
                                source_file = cave::strip_fn(newest_dd_path))


input <- read_raw_input()

# Select columns that are in in the input
newest_dd_diff_parsed2 <-
        newest_dd_diff_parsed %>%
        dplyr::select(any_of(colnames(input)))

# Join to see if any existing maps are present
mapped_input <-
        input %>%
        dplyr::filter_at(vars(!!terminal_col),
                         all_vars(!is.na(.))) %>%
        dplyr::select(all_of(c("FORM", "VARIABLE", "TYPE", "CONCEPT")), `MSK Concept Type`, `MSK Concept`)


newest_dd_diff_parsed3 <-
        newest_dd_diff_parsed2 %>%
        dplyr::left_join(mapped_input,
                         by = c("FORM", "VARIABLE", "TYPE", "CONCEPT")) %>%
        # Convert unmapped new concepts to MSK Concept Type == "Fact"
        dplyr::mutate(`MSK Concept Type` = ifelse(is.na(`MSK Concept Type`), "Fact", `MSK Concept Type`))



# Binding input to the newest additions
new_input <-
        dplyr::bind_rows(input,
                         newest_dd_diff_parsed3) %>%
        # Remove routine_id and CONCEPT_COUNT columns because obsolete
        dplyr::select(-routine_id, CONCEPT_COUNT) %>%
        # Add updated routine_id and CONCEPT_COUNT
        tibble::rowid_to_column("routine_id") %>%
        # Add CONCEPT counts %>%
        dplyr::group_by(CONCEPT) %>%
        dplyr::mutate(CONCEPT_COUNT = length(CONCEPT))

secretary::typewrite_note("Need to add QA step where all of the new dd dictionary contents is found in the `new_input` object before writing.")
secretary::press_enter()

# Overwriting input
broca::simply_write_csv(new_input,
                        file = path_to_input_fn)
