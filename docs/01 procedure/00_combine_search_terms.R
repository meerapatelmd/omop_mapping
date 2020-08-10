input2 <- read_workfile(routine = "00_combine_search_term")

if (length(grep("SEARCH_TERM_", colnames(input2))) == 1) {

                        input3 <-
                                input2 %>%
                                rubix::mutate_if_not_exist(column_name = "SEARCH_TERM",
                                                           value = input2 %>%
                                                                   dplyr::select(starts_with("SEARCH_TERM_")) %>%
                                                                   unlist())

                        broca::simply_write_csv(x = input3,
                                                file = path_to_input_fn)

} else {

                        input3 <- input2 %>%
                                tidyr::pivot_longer(cols = starts_with("SEARCH_TERM_"),
                                                    names_to = "SEARCH_TERM_ITERATION",
                                                    values_to = "SEARCH_TERM",
                                                    values_drop_na = TRUE)

                        input4b <-
                                input3$SEARCH_TERM %>%
                                rubix::map_names_set(cave::string_to_vector) %>%
                                purrr::set_names(input3$routine_id) %>%
                                purrr::map(function(x) rubix::vector_to_tibble(as.character(x), new_col = "SEARCH_TERM")) %>%
                                dplyr::bind_rows(.id = "routine_id") %>%
                                dplyr::distinct() %>%
                                rubix::split_deselect(column_name = "routine_id")  %>%
                                purrr::map(unlist) %>%
                                purrr::map(cave::vector_to_string) %>%
                                purrr::map(function(x) rubix::vector_to_tibble(x, new_col = "SEARCH_TERM")) %>%
                                dplyr::bind_rows(.id = "routine_id")


                        output <- dplyr::left_join(input2,
                                                   input4b) %>%
                                dplyr::select(-starts_with("SEARCH_TERM_"))


                        broca::simply_write_csv(x = output,
                                                file = path_to_input_fn)


}

