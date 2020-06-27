rm(list = ls())
source("setup.R")

path_to_output_fn <- paste0(path_to_output_dir, "/", cave::strip_fn(path_to_input_fn), "_", cave::strip_fn(cave::present_script_path()), ".csv")

if (file.exists(path_to_output_fn)) {
        secretary::typewrite_warning(path_to_output_fn, "already exists and will be overwritten.")
        secretary::press_enter()
}



input <- read_terminal_workfile()
#input <- broca::simply_read_csv(path_to_input_fn)

# Converting to true NA
input <-
        input %>%
        dplyr::mutate_all(stringr::str_remove_all, "[\r\n\t]") %>%
        dplyr::mutate_all(stringr::str_replace_na) %>%
        dplyr::mutate_all(stringr::str_replace_all, "^NA$", NA_character_)


# Select only the original parsed redcap column names and `Concept` columns
input2 <-
        input %>%
        dplyr::select(routine_id,
                any_of(workfile_colnames),
                 `MSK Concept Type`,
                `MSK Concept`)


# Separate `Concept` columns based on \n
input3 <-
        input2 %>%
        separate_rows("MSK Concept",
                      sep = "\n")


# Unmerging `MSK Concept` to get the concept_id from the merge strip (`msk_concept_id`)
output <-
        input3 %>%
        chariot::unmerge_concepts(`MSK Concept`, remove = FALSE) %>%
        dplyr::select(msk_concept_id = concept_id,
                      msk_concept_name = concept_name,
                      routine_id:last_col())

output3 <-
        dplyr::left_join(output,
        output %>%
        dplyr::group_by(FORM, FIELD_LABEL) %>%
        dplyr::summarise(count = n()))

# Creating a fresh `Concept` strip based on msk_concept_id as a part of QA to make sure all the msk_concept_ids exist
output2_concept <-
        chariot::left_join_df_to_concept(output %>%
                                                 dplyr::select(msk_concept_id) %>%
                                                 dplyr::distinct() %>%
                                                 # Remove NAs from unmapped concepts because can cause issues with QA downstream
                                                 dplyr::filter_all(any_vars(!is.na(.)))) %>%
        chariot::merge_concepts(into = "MSK Concept") %>%
        dplyr::select(msk_concept_id = concept_id,
                      `MSK Concept`)


output2 <-
        output %>%
        dplyr::left_join(output2_concept,
                         by = "msk_concept_id",
                         suffix = c(".current", ".new"))


# Are any of the new `MSK Concept` strips NA while the current isn't?
qa1 <-
        output2 %>%
        dplyr::filter(is.na(`MSK Concept.new`) && !is.na(`MSK Concept.current`))


if (nrow(qa1) > 0) {
        stop('There are new MSK concepts that are NA while the current ones are not.')
}

output3 <- output2 %>%
        dplyr::mutate(`MSK Concept` = coalesce(`MSK Concept.new`, `MSK Concept.current`)) %>%
        dplyr::select(-starts_with("MSK Concept."))

output4_concept <-
        chariot::left_join_df_to_concept(output3 %>%
                                                 dplyr::select(msk_concept_id))


output4 <-
        dplyr::left_join(output3,
                         output4_concept) %>%
        dplyr::distinct()

#QA 2: do all the original msk_concept_id == concept_id that was just bound columnwise?
qa2 <-
        output4 %>%
        dplyr::filter(msk_concept_id != concept_id)

if (nrow(qa2) > 0) {
        stop("error in final join with concept id")
} else {
        output5 <-
                output4 %>%
                dplyr::select(-msk_concept_id)
}

# Removing NAs for empty modifier and attribute concepts (Keep for fact because it indicates whether a concept is mapped or not)
output6 <-
        dplyr::bind_rows(
                output5 %>%
                        dplyr::filter(`MSK Concept Type` %in% c("Observation Group Concept", "Fact Concept")),
                output5 %>%
                        dplyr::filter(!(`MSK Concept Type` %in% c("Observation Group Concept","Fact Concept"))) %>%
                        dplyr::filter(!is.na(`MSK Concept`))
        )

# Are all routine ids accounted for?
qa3 <- all(output6$routine_id %in% input$routine_id)
if (qa3 == FALSE) {
        stop('routine_ids are missing in the output.')
}


final_routine_output <-
                        output6

## Getting all concepts in final output
broca::simply_write_csv(x = final_routine_output,
                        file = path_to_output_fn)
