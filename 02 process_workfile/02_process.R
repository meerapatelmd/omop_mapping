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


# Select only the original parsed redcap column names and `Concept` columns
input2 <-
        input %>%
        dplyr::select(routine_id,
                any_of(workfile_colnames),
                !!terminal_col)


# Separate `Concept` columns based on \n
input3 <-
        input2 %>%
        separate_rows(!!terminal_col,
                      sep = "\n")



# Unmerging `MSK Concept` to get the concept_id from the merge strip (`msk_concept_id`)
output <-
        input3 %>%
        chariot::unmerge_concepts(!!terminal_col, remove = FALSE) %>%
        dplyr::select(msk_concept_id = concept_id,
                      msk_concept_name = concept_name,
                      routine_id:last_col())

# Creating a fresh `Concept` strip based on msk_concept_id as a part of QA to make sure all the msk_concept_ids exist
output2_concept <-
        chariot::left_join_concept(output %>%
                                                 dplyr::select(msk_concept_id) %>%
                                                 dplyr::distinct() %>%
                                                dplyr::mutate_all(as.integer) %>%
                                                 # Remove NAs from unmapped concepts because can cause issues with QA downstream
                                                 dplyr::filter_all(any_vars(!is.na(.)))) %>%
        chariot::merge_concepts(into = "MSK Concept") %>%
        dplyr::select(msk_concept_id = concept_id,
                      `MSK Concept`)


output2 <-
        output %>%
        rubix::mutate_to_integer(msk_concept_id) %>%
        dplyr::left_join(output2_concept,
                         by = "msk_concept_id",
                         suffix = c(".current", ".new"))


# Are any of the new `MSK Concept` strips NA while the current isn't?
qa1 <-
        output2 %>%
        dplyr::filter(is.na(!!terminal_col) && !is.na(`MSK Concept`))


if (nrow(qa1) > 0) {
        stop('There are new MSK concepts that are NA while the current ones are not.')
}

output3 <- output2 %>%
        dplyr::select(-!!terminal_col)

output4_concept <-
        chariot::left_join_concept(output3 %>%
                                                 dplyr::select(msk_concept_id))


output4 <-
        dplyr::left_join(output3,
                         output4_concept) %>%
        dplyr::distinct()

#QA 2: do all the original msk_concept_id == concept_id that was just bound column-wise?
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
                        dplyr::filter(`MSK Concept Type` %in% c("Observation Group", "Fact")),
                output5 %>%
                        dplyr::filter(!(`MSK Concept Type` %in% c("Observation Group","Fact"))) %>%
                        dplyr::filter(!is.na(`MSK Concept`))
        )

# Are all routine ids accounted for?
qa3 <- all(output6$routine_id %in% input$routine_id)
if (qa3 == FALSE) {
        stop('routine_ids are missing in the output.')
}

# Finally getting MSK concept id (Lead Fact)
lead_fact <-
        output6 %>%
        dplyr::filter(`MSK Concept Type` == "Fact") %>%
        dplyr::select(FORM,
                      VARIABLE,
                      FIELD_LABEL,
                      FIELD_TYPE,
                      CHOICES,
                      PV,
                      PV_CODE,
                      TYPE,
                      CONCEPT,
                      `MSK Concept Lead Fact`) %>%
        dplyr::distinct()

# QA: Is every FORM-VARIABLE-TYPE-CONCEPT combination 1:! with a MSK Concept Lead Fact?
concept_group_cols <-
        c("FORM",
          "VARIABLE",
          "FIELD_LABEL",
          "FIELD_TYPE",
          "CHOICES",
          "PV",
          "PV_CODE",
          "TYPE",
          "CONCEPT")
qa4 <-
        lead_fact %>%
        dplyr::group_by_at(vars(all_of(concept_group_cols))) %>%
        dplyr::mutate(Count = length(`MSK Concept Lead Fact`)) %>%
        dplyr::filter(Count != 1)

if (nrow(qa4) > 0) {

        stop("Not all Lead Facts are 1.")

}

lead_fact2 <-
        lead_fact %>%
        chariot::unmerge_concepts(`MSK Concept Lead Fact`) %>%
        dplyr::select(all_of(concept_group_cols),
                      msk_lead_fact_concept_id = concept_id)


lead_fact2_concept <-
        lead_fact2 %>%
        dplyr::select(msk_lead_fact_concept_id) %>%
        chariot::left_join_df_to_concept()


lead_fact3 <-
        dplyr::left_join(lead_fact2,
                         lead_fact2_concept) %>%
        dplyr::select(-msk_lead_fact_concept_id) %>%
        dplyr::rename_at(vars(concept_id:last_col()),
                         function(x) paste0("msk_lead_fact_", x)) %>%
        dplyr::mutate(MSK_CONCEPT_ID = ifelse(!is.na(msk_lead_fact_concept_id),
                                              paste0("MSK", msk_lead_fact_concept_id),
                                              NA_character_)
        ) %>%
        dplyr::select(all_of(concept_group_cols),
                      MSK_CONCEPT_ID,
                      everything())

final_lead_fact <- lead_fact3 %>%
                                dplyr::distinct()


# Getting remainder of the components
output7 <- output6

# Break it apart
output8 <- split(output7,
                 output7$`MSK Concept Type`)

# Observation group
observation_group <- output8$`Observation Group`
observation_group2 <-
        observation_group %>%
        dplyr::select(all_of(concept_group_cols),
                      `MSK Concept`) %>%
        chariot::unmerge_concepts(`MSK Concept`) %>%
        dplyr::mutate(OBSERVATION_GROUP_ID = paste0("MSK", concept_id)) %>%
        dplyr::select(OBSERVATION_GROUP_ID,
                      OBSERVATION_GROUP_NAME = concept_name,
                      all_of(concept_group_cols)) %>%
        dplyr::distinct()

# First 2 Tabs
Final <-
        dplyr::full_join(observation_group2,
                         final_lead_fact)

# Remainder of the mappings
output9 <- output8
## Make sure that all Lead Facts are accounted for in the Facts
fact <-
        split(output9$Fact,
              output9$Fact$CONCEPT)
qa7a <-
        fact %>%
        rubix::map_names_set(function(x) list(Fact = unique(x$`MSK Concept`),
                                              Lead = unique(x$`MSK Concept Lead Fact`)))


qa7b <-
        qa7a %>%
        purrr::map(function(x) all(x$Lead %in% x$Fact)) %>%
        purrr::keep(function(x) x == FALSE) %>%
        names()

if (length(qa7b)) {

        fact2 <-
        dplyr::bind_rows(fact) %>%
                dplyr::left_join(
                        tibble(CONCEPT = qa7b,
                               LEAD_FACT_MISMATCH_FLAG = TRUE)) %>%
                dplyr::mutate(LEAD_FACT_MISMATCH_FLAG = ifelse(is.na(LEAD_FACT_MISMATCH_FLAG),
                                                               FALSE,
                                                               LEAD_FACT_MISMATCH_FLAG))


} else {
        fact2 <- fact
}


output9$Fact <- NULL

output9[[1+length(output9)]] <- fact2
names(output9)[length(output9)] <- "Fact"
output10 <- output9
output10 <-
        output10 %>%
        purrr::map(function(x) x %>%
                           dplyr::select(all_of(concept_group_cols),
                                         `MSK Concept`)) %>%
        purrr::map2(names(output10),
                    function(x,y) x %>%
                                dplyr::rename(!!y := `MSK Concept`)) %>%
        purrr::reduce(left_join)



final_routine_output <-
                        output6

## Getting all concepts in final output
broca::simply_write_csv(x = final_routine_output,
                        file = path_to_output_fn)
