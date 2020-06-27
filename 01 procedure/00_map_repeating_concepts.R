raw_input <- read_raw_input()

raw_input2 <-
        split(raw_input, raw_input$CONCEPT)



output <-
        raw_input2 %>%
        purrr::map(function(x) x %>% dplyr::filter(`MSK Concept Type` == "Fact")) %>%
        purrr::keep(~nrow(.)>1) %>%
        purrr::keep(function(x) length(unique(x$`MSK Concept`)) > 1)

names(output)
