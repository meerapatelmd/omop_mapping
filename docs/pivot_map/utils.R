read_input <-
function() {

        x <- broca::simply_read_csv(path_to_input_fn)
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
        cat("\n")

        return(x)

}
