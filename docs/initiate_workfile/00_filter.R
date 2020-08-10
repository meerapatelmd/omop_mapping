# Filter input ddict for only my forms
#mp_forms <- broca::read_clipboard()
mp_forms <-
mp_forms %>%
        dplyr::filter_at(vars(mp.and.ts),
                         all_vars(. %in% c("mp"))) %>%
        dplyr::select(1) %>%
        unlist() %>%
        unname()



# Isolating forms in DD
dd <- broca::simply_read_csv(path_to_input_fn)

parsed_dd <-
        cartograph::parse_dd_df2(dd) %>%
        dplyr::filter(FORM %in% mp_forms) %>%
        dplyr::select(all_of(workfile_colnames))

parsed_dd_failures <-
        cartograph::parse_dd_df2(dd, return_failures = TRUE) %>%
        dplyr::filter(FORM %in% mp_forms) %>%
        dplyr::select(all_of(workfile_colnames))



