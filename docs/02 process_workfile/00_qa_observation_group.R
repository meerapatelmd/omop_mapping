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

#Split by Form
input2 <-
        split(input, input$FORM)


qa1 <-
        input2 %>%
        purrr::keep(function(x) 0==nrow(x %>%
                                             dplyr::select(`MSK Concept Type`) %>%
                                             dplyr::distinct() %>%
                                             dplyr::filter_at(vars(`MSK Concept Type`), any_vars(. == "Observation Group")))) %>%
        names()

if (length(qa1) > 0) {

        stop("The following Forms are missing an Observation Group: ", paste(qa1, collapse = ", "))
}



# #For each FORM, if a Observation Group mapping is missing, add it
# input3 <- input2
# for (i in 1:length(input3)) {
#
#         if (!("Observation Group" %in% input3[[i]]$`MSK Concept Type`)) {
#
#                 secretary::typewrite("Observation Group is missing.")
#                 secretary::typewrite(names(input3)[i])
#                 obs_gp <- readline("Enter concept_id for this observation group: ")
#                 obs_gp <- chariot::get_merged_concept_id(obs_gp)
#                 input3[[i]] <-
#                         dplyr::bind_rows(input3[[i]],
#                                          input3[[i]] %>%
#                                         dplyr::mutate("MSK Concept Type" = "Observation Group") %>%
#                                         dplyr::mutate("MSK Concept" = obs_gp))
#
#         }
# }

# # QA Again
# for (i in 1:length(input3)) {
#
#         if (!("Observation Group" %in% input3[[i]]$`MSK Concept Type`)) {
#
#                 secretary::typewrite("Observation Group is missing.")
#                 secretary::typewrite(names(input3)[i])
#                 obs_gp <- readline("Enter concept_id for this observation group: ")
#                 obs_gp <- chariot::get_merged_concept_id(obs_gp)
#                 input3[[i]] <-
#                         dplyr::bind_rows(input3[[i]],
#                                          input3[[i]] %>%
#                                                  dplyr::mutate("MSK Concept Type" = "Observation Group") %>%
#                                                  dplyr::mutate("MSK Concept" = obs_gp))
#
#         }
# }


# Is there one and only Observation Group per form?
qa2 <-
        input2 %>%
        rubix::map_names_set(function(x) x %>% dplyr::filter(`MSK Concept Type` == "Observation Group") %>%
                                                dplyr::select(`MSK Concept`) %>%
                                                dplyr::distinct() %>%
                                                nrow()) %>%
        purrr::keep(~.!=1)

if (length(qa2) > 0) {

        stop("More than 1 observation group is found in one of the forms")

}

final_routine_output <-
        dplyr::bind_rows(input2)



broca::simply_write_csv(x = final_routine_output,
                        file = path_to_output_fn)
