rm(list = ls())
source('setup.R')

output <-
list.files("output", full.names = TRUE, pattern = cave::strip_fn(input_fn)) %>%
       rubix::map_names_set(broca::simply_read_csv) %>%
        purrr::reduce(left_join) %>%
        dplyr::distinct()


broca::copy_to_clipboard(output)
