# Parsing all the non-monotherapy regimens to the component level for mapping (for example "Regimen (component + component) into Regimen, Component, Component)
input <- read_input()
path_to_output_fn <- create_path_to_output_fn()


# Getting the pattern of Regimen labels to see the max components there are
max_part_count <-
input$Regimen %>%
        centipede::nchar_punct() %>%
        max()

output <-
        input %>%
        tidyr::separate(col = source_col,
                       into = paste0("SourcePart", 1:(1+max_part_count)),
                       sep = "[(]{1}|[+]{1}|[)]{1}|[,]{1}"
                       ) %>%
        tidyr::pivot_longer(cols = starts_with("SourcePart"),
                            names_to = "PartIndex",
                            values_to = "Part",
                            values_drop_na = TRUE) %>%
        dplyr::mutate(Part = trimws(Part)) %>%
        dplyr::filter(Part != "")

final_output <-
        dplyr::left_join(input,
                         output) %>%
        dplyr::distinct()
broca::simply_write_csv(final_output,
                        file = path_to_output_fn)


