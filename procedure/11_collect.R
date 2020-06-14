rm(list = ls())
source('setup.R')

output_b <-
list.files("output", full.names = TRUE, pattern = cave::strip_fn(input_fn)) %>%
       rubix::map_names_set(broca::simply_read_csv) %>%
        purrr::reduce(left_join) %>%
        dplyr::distinct()

output_a <-
list.files("input", full.names = TRUE, pattern = cave::strip_fn(input_fn)) %>%
        broca::simply_read_csv()

final_output <-
        dplyr::left_join(output_a,
                         output_b)

broca::copy_to_clipboard(final_output)
broca::view_as_csv(final_output)
