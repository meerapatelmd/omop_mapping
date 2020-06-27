input <-
        list.files("data/input", pattern = "COVID", full.names = TRUE) %>%
        rubix::map_names_set(broca::simply_read_csv) %>%
        dplyr::bind_rows(.id = "SOURCE_CSV")


input2 <-
        input %>%
        dplyr::distinct()