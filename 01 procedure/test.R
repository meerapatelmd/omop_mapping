


testDf <-
        data.frame(A = 1:3,
                   B = 4:6)


addFilters <- list("A == 2",
                   "B == 5")

testDf %>%
        dplyr::filter(unlist(addFilters))



filterWithAdd <-
        function(.data, ...) {

                newFilters <- paste(unlist(rlang::list2(...)), collapse = ", ")

                print(newFilters)

                eval(
                rlang::parse_expr(
                paste0(
                ".data %>%
                        dplyr::filter(",newFilters, ")")))
        }
