# Identify input dir of previous day
path_to_prior_day_dir <-
file.info(list.files("output", full.names = TRUE)) %>%
        tibble::rownames_to_column("path") %>%
        dplyr::filter(isdir == TRUE) %>%
        dplyr::select(path) %>%
        tidyr::separate(col = path,
                        into = c("input_folder", "date_dir"),
                        sep = "[/]{1}",
                        remove = FALSE) %>%
        dplyr::arrange(desc(as.Date(date_dir))) %>%
        dplyr::filter(row_number() == 2) %>%
        dplyr::select(path) %>%
        unname() %>%
        unlist()

# Paths to prior folder
prior_dd <- broca::simply_read_csv(paste0(path_to_prior_day_dir, "/", origin_input_fn)) %>%
                dplyr::select(all_of(common_fields))

prior_master <-
        paste0(path_to_prior_day_dir, "/", basename(proc_dd_fn)) %>%
        readxl::excel_sheets() %>%
        set_names() %>%
        purrr::map(readxl::read_excel, path = paste0(path_to_prior_day_dir, "/", basename(proc_dd_fn))) %>%
        purrr::map(rename_field_to_changed_field)
