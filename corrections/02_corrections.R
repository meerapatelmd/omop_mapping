# This is to process files that have already been transformed from workfile to pre-ingestion format and corrections in the mappings need to be made. The concept_id is in the file, but the remainder of the elements need to be added
rm(list = ls())
source("setup.R")

path_to_output_fn <- paste0("output/", input_file_stem, "_", cave::strip_fn(cave::present_script_path()), ".csv")

if (file.exists(path_to_output_fn)) {
        secretary::typewrite_warning("File already exists and will be overwritten.")
        secretary::press_enter()
}

input <- read_file_to_correct()

input2a <-
        input %>%
        dplyr::filter_at(vars(concept_name), all_vars(is.na(.)))


input2b <-
        input[!(input$routine_id %in% input2a$routine_id),]


qa1 <- nrow(input2a)+nrow(input2b)-nrow(input)

if (qa1 != 0) {

                stop("input not cleanly split into input2a and input2b.")

}

output_a_concept <-
        chariot::left_join_df_to_concept(input2a %>%
                                                 dplyr::select(correction_concept_id = concept_id) %>%
                                                 dplyr::distinct() %>%
                                                 rubix::call_mr_clean()) %>%
        dplyr::select(-correction_concept_id) %>%
        dplyr::mutate_all(as.character)


output_a <-
        dplyr::left_join(input2a %>%
                                 dplyr::select(-any_of(c("concept_name",
                                                         "domain_id",
                                                         "vocabulary_id",
                                                         "concept_class_id",
                                                         "standard_concept",
                                                         "concept_code",
                                                         "valid_start_date",
                                                         "valid_end_date",
                                                         "invalid_reason"))),
                         output_a_concept)


output_b <- input2b

qa2 <- colnames(output_a)[!(colnames(output_a) %in% colnames(output_b))]
if (length(qa2) != 0) {

                stop('There are fields missing between output_a and output_b.')

}

output <-
        dplyr::bind_rows(output_a,
                         output_b)

# Are all routine_ids in the output?
qa3 <- all(input$routine_id %in% output$routine_id)

if (qa3 == FALSE) {

                stop("routine_ids are missing in output")

}

# Do the row counts match?
qa4 <- nrow(input)-nrow(output)

if (qa4 != 0) {

                stop("Row counts different between input and output.")

}

# Are there any concept elements that are still NA?
qa5 <-
        output %>%
        dplyr::filter_at(vars(concept_id,
                              concept_name,
                              domain_id,
                              vocabulary_id),
                         any_vars(is.na(.))) %>%
        dplyr::filter(!(concept_id %in% c("NEW")))

if (nrow(qa5) > 0) {

                stop("Some concept_ids did not join with the concept table.")
}

broca::simply_write_csv(x = output,
                        file = path_to_output_fn)

