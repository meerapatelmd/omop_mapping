rm(list = ls())
source("setup.R")

path_to_output_fn <- paste0(path_to_output_dir, "/", cave::strip_fn(path_to_input_fn), "_", cave::strip_fn(cave::present_script_path()), ".csv")

if (file.exists(path_to_output_fn)) {
        secretary::typewrite_warning(path_to_output_fn, "already exists and will be overwritten.")
        secretary::press_enter()
}


input <- read_terminal_workfile()

# Converting to true NA
input <-
        input %>%
        dplyr::mutate_all(stringr::str_remove_all, "[\r\n\t]") %>%
        dplyr::mutate_all(stringr::str_replace_na) %>%
        dplyr::mutate_all(stringr::str_replace_all, "^NA$", NA_character_)

input <-
        input %>%
        dplyr::distinct()

input <-
        input %>%
        separate_rows("MSK Concept",
                      sep = "\n")

#Split by Form
input2 <-
        split(input, input$FORM)


#For each FORM, if a Observation Group mapping is missing, add it
for (i in 1:length(input2)) {

        if (!("Observation Group" %in% input2$`MSK Concept Type`)) {

                secretary::typewrite("Observation Group is missing.")
                secretary::typewrite(names(input2)[i])
                obs_gp <- readline("Enter concept_id for this observation group: ")
                obs_gp <- chariot::get_merged_concept_id(obs_gp)
                input2[[i]] <-
                        dplyr::bind_rows(input2[[i]],
                                         input2[[i]] %>%
                                        dplyr::mutate("MSK Concept Type" = "Observation Group") %>%
                                        dplyr::mutate("MSK Concept" = obs_gp))

        }
}

final_routine_output <-
        dplyr::bind_rows(input2)

broca::simply_write_csv(x = final_routine_output,
                        file = path_to_output_fn)
