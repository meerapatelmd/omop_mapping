read_workfile <-
        function(routine) {

                x <- broca::simply_read_csv(path_to_input_fn,
                                            log_details = routine)

                print(
                        x %>%
                                dplyr::select(!!terminal_col) %>%
                                dplyr::mutate_at(vars(!!terminal_col),
                                                 function(x)
                                                         ifelse(is.na(x),
                                                                "Unmapped",
                                                                "Mapped")) %>%
                                group_by_at(vars(!!terminal_col)) %>%
                                summarize(COUNT = n())
                )

                return(x)
                cat("\n")
        }