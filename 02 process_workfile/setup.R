# Process a complete or semi-complete workfile for pre-ingestion

rm(list = ls())
if ("/Users/patelm9/GitHub/omop_mapping/02 process_workfile" != getwd()) {
        setwd("/Users/patelm9/GitHub/omop_mapping/02 process_workfile")
}

source('utils.R')
source('variables.R')

# If the input_fn does not exist in the input subdir, it is written to the input subdir to the input_fn provided above
input_fn <- paste0(origin_terminal_tab, ".csv")
path_to_input_fn <- paste0(path_to_input_dir, "/", input_fn)
if (!file.exists(path_to_input_fn)) {
        origin_data <- broca::read_full_excel(origin_fn)
        input <- origin_data[[origin_terminal_tab]]

        if (is.null(input)) {
                stop('input is null.')
        }

        if (input_format == "long") {

                if ("routine_id" %in% colnames(input)) {

                        input <- input %>%
                                        dplyr::select(-routine_id)

                }

                secretary::typewrite(crayon::bold("Row count before separate rows:"), nrow(input))

                        input <-
                                input %>%
                                dplyr::select(-starts_with("Source "))  %>%
                                tidyr::separate_rows(!!terminal_col,
                                                     sep = "\n|\r\n") %>%
                                dplyr::mutate_all(~na_if(., "")) %>%
                                rowid_to_column(var = "routine_id")

                secretary::typewrite(crayon::bold("Row count after separate rows:"), nrow(input))
                }


                input <-
                        input %>%
                        rubix::mutate_if_not_exist(column_name = "MSK Concept Type",
                                                   value = "Fact")


                #QA for typos
                if (any(!(input$`MSK Concept Type` %in% c("Observation Group",
                                                        "Fact",
                                                        "Lead Fact",
                                                        "Attribute",
                                                        "Modifier")))) {


                        secretary::typewrite_error("The following is not a MSK Concept Type:")

                        input$`MSK Concept Type`[!(input$`MSK Concept Type` %in% c("Observation Group",
                                                                                   "Fact",
                                                                                   "Lead Fact",
                                                                                   "Attribute",
                                                                                   "Modifier"))] %>%
                                purrr::map(~secretary::typewrite(., tabs = 1))

                        stop("MSK Concept Types not recognized.")


                }

        # Copy Input to input folder
        broca::simply_write_csv(x = input,
                                file = path_to_input_fn,
                                log_details = paste0(origin_fn, "TAB: ", origin_terminal_tab, "written to ", input_fn))
         } else {
                secretary::typewrite("Need to write short format section, such as the QA below")
                # Make sure that all the mapped concepts (Fact, Modifier and Attribute are included in the output)
                ## Pre-melt
                # qa4_fact <- mapped_count(input3$`Fact Concept`)+unmapped_count(input3$`Fact Concept`)
                # qa4_attribute <- mapped_count(input3$`Attribute Concept`)
                # qa4_modifier <- mapped_count(input3$`Modifier Concept`)
                # qa4a <- qa4_fact+qa4_attribute+qa4_modifier

                ## Getting all mapped and unmapped Facts at the time of melt
                # qa4b <-
                #         input4 %>%
                #         dplyr::filter(`MSK Concept Type` %in% c('Fact Concept')) %>%
                #         dplyr::distinct() %>%
                #         nrow() +
                #         ## Getting all mapped Attributes and Modifiers
                #         input4 %>%
                #         dplyr::filter(!(`MSK Concept Type` %in% c('Fact Concept', 'Observation Group Concept'))) %>%
                #         dplyr::filter(!is.na(`MSK Concept`)) %>%
                #         dplyr::select(`MSK Concept`) %>%
                #         nrow()

                input <-
                        input %>%
                        rubix::mutate_if_not_exist(column_name = "Attribute",
                                                   value = NA) %>%
                        rubix::mutate_if_not_exist(column_name = "Modifier",
                                                   value = NA) %>%
                        tidyr::pivot_longer(cols = c(Fact, Attribute, Modifier, "Observation Group"),
                                            names_to = "MSK Concept Type",
                                            values_to = "MSK Concept",
                                            #Don't drop NAs because they represent unmapped concepts
                                            values_drop_na = FALSE) %>%
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



