input <-
        broca::read_full_excel("~/Memorial Sloan Kettering Cancer Center/cBioPortal Standardization - KMI Only - KMI Only/Mapping Files/cBioPortal_IngestionFilev12.xlsx")
input <- input$Variable

uri <-
        broca::read_full_excel("~/Memorial Sloan Kettering Cancer Center/cBioPortal Standardization - KMI Only - KMI Only/Mapping Files/cBioPortal_TopBraid_URIs.xlsx")
uri <- uri$Sheet1
colnames(uri) <-
        unlist(uri[1,]) %>%
        stringr::str_replace_na()

dd <- broca::simply_read_csv("~/Desktop/CBioPortalREDCapShellForExtrac_DataDictionary_2020-07-24.csv") %>%
        cartograph::standardize_dd_cols()

uri <-
uri %>%
        rubix::filter_first_row(invert = TRUE) %>%
        dplyr::select(-all_of("NA"))


essential_columns <-
        c("PROJECT_URI",
          "PROJECT_ALIAS",
          "FORM_NAME",
          "VARIABLE_FIELD_NAME",
          "VARIABLE_URI",
          "FIELD_LABEL",
          "VARIABLE_TYPE",
          "PHI",
          "MSK_CONCEPT_ID")

input_columns <- colnames(input)

qa1 <- essential_columns[!(essential_columns %in% input_columns)]
if (length(qa1)) {

        stop("Missing columns: ", paste(qa1, collapse = ", "), ". See qa1 object.")

}

topbraid_vars <- unique(uri$redcapFieldLabel)
ingestion_file_vars <- unique(input$FIELD_LABEL)

qa2 <- topbraid_vars[!(topbraid_vars %in% ingestion_file_vars)]
if (length(qa2)) {
        stop("TopBraid redcapFieldLabel not in Ingestion File: ", paste(qa2, collapse = ", "), ". See qa2 object.")
}

qa3 <- ingestion_file_vars[!(ingestion_file_vars %in% topbraid_vars)]
if (length(qa3)) {
        stop("Ingestion FIELD_LABEL not in TopBraid: ", paste(qa3, collapse = ", "), ". See qa3 object.")
}

qaOutput <-
        dplyr::bind_rows(
                tibble(DQ_RULE = "All TopBraid redcapFieldLabel are in Ingestion File FIELD_LABEL column",
                       DQ_RULE_STATUS = TRUE),
                tibble(DQ_RULE = "All Ingestion File FIELD_LABEL in TopBraid redcapFieldLabel column",
                       DQ_RULE_STATUS = TRUE)
        )


input <-
        input %>%
        rubix::bring_to_front(all_of(essential_columns)) %>%
        dplyr::left_join(uri %>%
                                 dplyr::select(redcapField,
                                               redcapFieldLabel) %>%
                                 dplyr::distinct(),
                         by = c("FIELD_LABEL" = "redcapFieldLabel")) %>%
        dplyr::left_join(dd,
                         by = "FIELD_LABEL")

qa_a <-
        input %>%
        dplyr::filter(FORM_NAME != FORM)

if (nrow(qa_a)) {
        qa_a <-
                qa_a %>%
                dplyr::select(FIELD_LABEL,FORM_NAME, FORM) %>%
                dplyr::distinct()

        stop('Ingestion File FORM_NAME does not match Data Dictionary FORM. See qa_a object.')
}


qa_b <-
        input %>%
        dplyr::filter(VARIABLE_FIELD_NAME != VARIABLE)

if (nrow(qa_b)) {
        qa_b <-
                qa_b %>%
                dplyr::select(FIELD_LABEL,VARIABLE_FIELD_NAME, VARIABLE) %>%
                dplyr::distinct()

        stop('Ingestion File VARIABLE_FIELD_NAME does not match Data Dictionary VARIABLE. See qa_b object.')
}

qaOutput <-
        dplyr::bind_rows(
                qaOutput,
                tibble(DQ_RULE = "All VARIABLE_FIELD_NAME in Ingestion File same as VARIABLE in Data Dictionary?",
                       DQ_RULE_STATUS = TRUE),
                tibble(DQ_RULE = "All FORM_NAME in Ingestion File same as FORM in Data Dictionary?",
                       DQ_RULE_STATUS = TRUE),
                tibble(DQ_RULE = c(mapply(paste, essential_columns, "column exists"),
                                   mapply(paste, essential_columns, "column in Position", 1:length(essential_columns))),
                       DQ_RULE_STATUS = TRUE),
                tibble(DQ_RULE = "TopBraid projectName same as PROJECT_ALIAS",
                       DQ_RULE_STATUS = unique(uri$projectName) == unique(input$PROJECT_ALIAS)),
                tibble(DQ_RULE = input$FIELD_LABEL %>%
                               purrr::map(function(x) paste(x, "FIELD_LABEL in TopBraid?")) %>%
                               unlist(),
                       DQ_RULE_STATUS = input$FIELD_LABEL %>%
                               purrr::map(function(x) x %in% uri$redcapFieldLabel) %>%
                               unlist()),
                tibble(DQ_RULE = input$FIELD_LABEL %>%
                               purrr::map(function(x) paste(x, "FIELD_LABEL has valid TopBraid URI?")) %>%
                               unlist(),
                       DQ_RULE_STATUS = input$redcapField %>%
                               purrr::map(~!is.na(.)) %>%
                               unlist()),
                tibble(DQ_RULE = input$MSK_CONCEPT_ID %>%
                               centipede::no_na() %>%
                               purrr::map(~paste(., "exists in OMOP Vocabulary?")) %>%
                               unlist(),
                       DQ_RULE_STATUS = input$MSK_CONCEPT_ID %>%
                               centipede::no_na() %>%
                               stringr::str_remove_all("MSK") %>%
                               as.integer() %>%
                               purrr::map(chariot::concept_id_exists) %>%
                               unlist())
        )

qa4 <- qaOutput %>%
        dplyr::filter(DQ_RULE_STATUS == FALSE)

if (nrow(qa4)) {
        stop('Failed data quality checkpoint. See qa4 object.')
}

broca::view_as_csv(qaOutput)

