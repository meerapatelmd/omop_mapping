path_to_input_existing_map_fn <- "input/COVID_existing_map.csv"
existing_map <- load_newest_inventory()

existing_map <-
        existing_map %>%
        dplyr::select(TYPE:last_col()) %>%
        dplyr::filter(!(`MSK Concept Type` %in% c("Observation Group"))) %>%
        dplyr::rename("Existing Concept" = "MSK Concept") %>%
        dplyr::distinct()

input <- read_workfile(routine = "execute pre-procedure.R")


output <-
        dplyr::left_join(input,
                         existing_map %>%
                                 dplyr::filter(TYPE == "Variable"),
                         by = c("Field Label" = "CONCEPT"))


broca::copy_to_clipboard(output)
