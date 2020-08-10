# Read and also select particular columns in a csv
read_select_csv <-
        function(file, col) {

                broca::simply_read_csv(file) %>%
                        dplyr::select(any_of(col))
        }

# Test factor counts
count_na <-
        function(.data, col) {
                col <- enquo(col)
                .data %>%
                        dplyr::select(!!col) %>%
                        dplyr::mutate_all(function(x) str_replace_na(x)) %>%
                        dplyr::mutate_all(function(x) ifelse(x %in% c("NA"), "Unmapped", "Mapped")) %>%
                        dplyr::group_by_all() %>%
                        dplyr::summarize(COUNT = n()) %>%
                        dplyr::ungroup() %>%
                        tidyr::pivot_wider(names_from = !!col,
                                           values_from = COUNT) %>%
                        dplyr::mutate(Total = Mapped+Unmapped)

        }



# read_select_csv of all the csvs in input_dir
read_select_input_dir <-
        function(path_to_input_dir, ...) {
                list.files(path_to_input_dir, full.names = TRUE) %>%
                        rubix::map_names_set(read_select_csv, ...) %>%
                        purrr::keep(~ncol(.) > 0)
        }
