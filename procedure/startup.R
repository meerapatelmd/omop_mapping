rm(list = ls())
source('/Users/patelm9/GitHub/omop_mapping/procedure/config/funs.R')
set_this_wd()
source('/Users/patelm9/GitHub/omop_mapping/procedure/config/constants.R')
source('/Users/patelm9/GitHub/omop_mapping/procedure/config/variables.R')




# If the input_fn does not exist in the input subdir, it is written to the input subdir to the input_fn provided above
if (grepl("[.]xlsx$", origin_fn) == TRUE) {
                input_fn <- paste0(input_file_stem, origin_tab, ".csv")
                path_to_input_fn <- paste0("data/input/", input_fn)
                if (!file.exists(path_to_input_fn)) {
                        origin_data <- broca::read_full_excel(origin_fn,
                                                              log_details = paste0("Load: ", origin_tab))
                        input <- origin_data[[origin_tab]]

                        input <-
                                input %>%
                                rubix::mutate_all_rm_multibyte_chars()

                        if (is.null(input)) {
                                stop('wrong origin_tab')
                        }

                        if ("routine_id" %in% colnames(input)) {
                        # Using prior routine_id to make sure no garbage is imported
                        # input$routine_id <- suppressWarnings(as.integer(input$routine_id))
                        # input <-
                        #         input %>%
                        #         dplyr::filter(!is.na(routine_id))

                        input <-
                                input %>%
                                dplyr::mutate_at(vars(routine_id), as.character) %>%
                                dplyr::select(-(any_of(output_colnames)), -starts_with("Source ")) %>%
                                        tibble::rowid_to_column("routine_id")
                        } else {
                                input <-
                                        input %>%
                                        dplyr::select(-(any_of(output_colnames)), -starts_with("Source ")) %>%
                                        tibble::rowid_to_column("routine_id")
                        }

                        # Copy Input to input folder
                        broca::simply_write_csv(x = input,
                                                file = path_to_input_fn,
                                                log_details = paste0(origin_fn, "TAB: ", origin_tab, "written to ", input_fn))

                }
}

if (grepl("[.]csv$", origin_fn) == TRUE) {

                        input_fn <- paste0(input_file_stem, cave::strip_fn(origin_fn), ".csv")
                        path_to_input_fn <- paste0("data/input/", input_fn)
                        if (!file.exists(path_to_input_fn)) {
                                input<- broca::simply_read_csv(origin_fn)
                                #input <- origin_data[[origin_tab]]

                                if (is.null(input)) {
                                        stop('empty input')
                                }

                                if ("routine_id" %in% colnames(input)) {
                                        # Using prior routine_id to make sure no garbage is imported
                                        input$routine_id <- suppressWarnings(as.integer(input$routine_id))
                                        input <-
                                                input %>%
                                                dplyr::filter(!is.na(routine_id))

                                        input <-
                                                input %>%
                                                dplyr::mutate_at(vars(routine_id), as.character) %>%
                                                dplyr::select(-(any_of(output_colnames)), -starts_with("Source ")) %>%
                                                tibble::rowid_to_column("routine_id")
                                } else {
                                        input <-
                                                input %>%
                                                dplyr::select(-(any_of(output_colnames)), -starts_with("Source ")) %>%
                                                tibble::rowid_to_column("routine_id")
                                }

                                # Copy Input to input folder
                                broca::simply_write_csv(x = input,
                                                        file = path_to_input_fn)



        }
}
