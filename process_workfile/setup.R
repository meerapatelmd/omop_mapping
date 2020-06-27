rm(list = ls())


if ("/Users/patelm9/GitHub/omop_mapping/process_workfile" != getwd()) {
        setwd("/Users/patelm9/GitHub/omop_mapping/process_workfile")
}

source('utils.R')
source('~/GitHub/omop_mapping/variables.R')

# If the input_fn does not exist in the input subdir, it is written to the input subdir to the input_fn provided above
input_fn <- paste0(input_file_stem, "_", origin_terminal_tab, ".csv")
path_to_input_fn <- paste0("input/", input_fn)
if (!file.exists(path_to_input_fn)) {
        origin_data <- broca::read_full_excel(origin_fn)
        input <- origin_data[[origin_terminal_tab]]

        if (is.null(input)) {
                stop('input is null.')
        }

        if (input_format == "long") {

                if ("routine_id" %in% colnames(input)) {

                        input <-
                                input %>%
                                dplyr::mutate_at(vars(routine_id), as.character) %>%
                                dplyr::select(-starts_with("Source "), -any_of(c("routine_id")))  %>%
                                tidyr::separate_rows(!!terminal_col,
                                                sep = "\n") %>%
                                dplyr::mutate_all(~na_if(., "")) %>%
                                tibble::rowid_to_column("routine_id")

                } else {
                        input <-
                                input %>%
                                dplyr::select(-starts_with("Source "))  %>%
                                tidyr::separate_rows(!!terminal_col,
                                                     sep = "\n") %>%
                                dplyr::mutate_all(~na_if(., "")) %>%
                                rowid_to_column(var = "routine_id")
                }


                input <-
                        input %>%
                        rubix::mutate_if_not_exist(column_name = "MSK Concept Type",
                                                   value = "Fact")

                if (!is.null(filter_for_form)) {
                        input <-
                                input %>%
                                dplyr::filter(FORM %in% filter_for_form)
                }

        # Copy Input to input folder
        broca::simply_write_csv(x = input,
                                file = path_to_input_fn,
                                log_details = paste0(origin_fn, "TAB: ", origin_terminal_tab, "written to ", input_fn))
        } else {
                secretary::typewrite("Need to write short format section.")

                input <-
                        input %>%
                        rubix::mutate_if_not_exist(column_name = "Attribute",
                                                   value = NA) %>%
                        rubix::mutate_if_not_exist(column_name = "Modifier",
                                                   value = NA) %>%
                        tidyr::pivot_longer(cols = c(Fact, Attribute, Modifier, "Observation Group"),
                                            names_to = "MSK Concept Type",
                                            values_to = "MSK Concept",
                                            values_drop_na = TRUE) %>%
                        tidyr::separate_rows("MSK Concept",
                                             sep = "\n") %>%
                        tibble::rowid_to_column("routine_id")

                if (!is.null(filter_for_form)) {
                        input <-
                                input %>%
                                dplyr::filter(FORM %in% filter_for_form)
                }

                broca::simply_write_csv(x = input,
                                        file = path_to_input_fn,
                                        log_details = paste0(origin_fn, "TAB: ", origin_terminal_tab, "written to ", input_fn))
        }

}

