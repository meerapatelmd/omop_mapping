rm(list = ls())
source("setup.R")

path_to_output_fn <- paste0(path_to_output_dir, "/", cave::strip_fn(path_to_input_fn), "_", cave::strip_fn(cave::present_script_path()), ".csv")

if (file.exists(path_to_output_fn)) {
        secretary::typewrite_warning(path_to_output_fn, "already exists and will be overwritten.")
        secretary::press_enter()
}


# input <- read_terminal_workfile(routine = "02_process.R")
input <-
        list.files(path_to_output_dir, pattern = origin_terminal_tab, full.names = TRUE) %>%
        rubix::map_names_set(broca::simply_read_csv)


input2 <-
        dplyr::left_join(input$`data/GLIOMA/Glioma_Workfile/output/MAP_03_03_add_observation_group.csv`,
                         input$`data/GLIOMA/Glioma_Workfile/output/MAP_03_02_process.csv`)

# Converting to true NA
input2 <-
        input2 %>%
        dplyr::mutate_all(stringr::str_remove_all, "[\r\n\t]") %>%
        dplyr::mutate_all(stringr::str_replace_na) %>%
        dplyr::mutate_all(stringr::str_replace_all, "^NA$", NA_character_)

input2 <-
        input2 %>%
        dplyr::distinct()

input2 <-
        input2 %>%
        separate_rows("MSK Concept",
                      sep = "\n")

input2a <-
        input2 %>%
        tidyr::pivot_wider(id_cols = c(FORM, VARIABLE, FIELD_TYPE, FIELD_LABEL, CHOICES, PV, PV_CODE, TYPE, CONCEPT),
                        names_from = `MSK Concept Type`,
                           values_from = `MSK Concept`,
                        values_fn = list(`MSK Concept` = function(x) paste(unique(x), collapse = "\n")))

input2b <-
        input2 %>%
        tidyr::pivot_wider(id_cols = c(FORM, VARIABLE, FIELD_TYPE, PV, PV_CODE, TYPE, CONCEPT),
                           names_from = `MSK Concept Type`,
                           values_from = `MSK Concept`,
                           values_fn = list(`MSK Concept` = function(x) length(unique(!is.na(x)))))



#Rules
qa1 <-
        all(input2b$`Observation Group` == 1)

if (qa1 != TRUE) {
        stop("Not all concepts have exactly 1 Observation Group.")
}

qa2 <-
        all(input2b$Fact >= 1)

if (qa2 != TRUE|is.na(qa2)) {
        secretary::typewrite_warning("There are some unmapped Facts:")
        secretary::typewrite_italic(input2b$Fact[!(input2b$Fact >=1)] %>% unique(), tabs = 3)
}


# If a concept has more than 1 Fact, the number will be in "MSK_CONCEPT_ID"
input2b <-
        input2b %>%
        dplyr::mutate(MSK_CONCEPT_ID = "") %>%
        dplyr::mutate(MSK_CONCEPT_ID = ifelse(!is.na(Fact) && Fact > 1, as.character(Fact), MSK_CONCEPT_ID))


# if ("Attribute" %in% colnames(input2b)) {
#         input2b <-
#                 input2b %>%
#                 dplyr::mutate(MSK_CONCEPT_ID = ifelse(!is.na(Attribute), "NEW", MSK_CONCEPT_ID))
#
# }
#
# if ("Modifier" %in% colnames(input2b)) {
#         input2b <-
#                 input2b %>%
#                 dplyr::mutate(MSK_CONCEPT_ID = ifelse(!is.na(Modifier), "NEW", MSK_CONCEPT_ID))
#
# }

input2b <-
        input2b %>%
        dplyr::select(-any_of(c("Fact", "Observation Group", "Attribute", "Modifier")))


input3 <-
        dplyr::left_join(input2a,
                         input2b)


input4 <-
        input3 %>%
        chariot::unmerge_concepts(concept_col = Fact, remove = FALSE)

#If MSK_CONCEPT_ID is not "NEW", it takes the concept_id
output <- input4
output$MSK_CONCEPT_ID <-
output$MSK_CONCEPT_ID %>%
        purrr::map2(output$concept_id, function(x, y) ifelse(x == "" && !is.na(y),
                                                             paste0("MSK", y),
                                                             x)) %>%
        unlist()

output2 <- output
output2$MSK_CONCEPT_ID <-
        output2$MSK_CONCEPT_ID %>%
        purrr::map2(output2$Fact, function(x, y) ifelse(x == "" && y == "NEW",
                                                             "NEW",
                                                             x)) %>%
        unlist()

broca::view_as_csv(output2)


secretary::typewrite_bold("Save the file as `Preingestion` file.")

# broca::copy_to_clipboard(input3)
#
# # Select only the original parsed redcap column names and `Concept` columns
# input2 <-
#         input %>%
#         dplyr::select(routine_id,
#                 any_of(workfile_colnames),
#                       ends_with(" Concept"))
#
# # Separate `Concept` columns based on \n
# input3 <-
#         input2 %>%
#         tidyr::separate_rows(`Fact Concept`,
#                              `Modifier Concept`,
#                              `Attribute Concept`,
#                              sep = "\n")
#
# # Pivot to longer table to make QA easier.
# input4 <-
#         input3 %>%
#         tidyr::pivot_longer(cols = ends_with(" Concept"),
#                             names_to = "MSK Concept Type",
#                             values_to = "MSK Concept",
#                             values_drop_na = FALSE) %>% #Don't drop NAs because they represent unmapped concepts
#         #The following 2 step was added because in the subsequent QA step, any "NA" was being flagged as requiring correction
#         dplyr::mutate_all(stringr::str_replace_na) %>%
#         dplyr::mutate_all(stringr::str_replace_all, "^NA$", NA_character_)
#
#
#
#
# # Unmerging `MSK Concept` to get the concept_id from the merge strip (`msk_concept_id`)
# output <-
#         input4 %>%
#         chariot::unmerge_concepts(`MSK Concept`, remove = FALSE) %>%
#         dplyr::select(msk_concept_id = concept_id,
#                       msk_concept_name = concept_name,
#                       routine_id:last_col())
#
# outputb <-
#         output %>%
#         dplyr::select("Observation Group") %>%
#         chariot::unmerge_concepts("Observation Group", remove = FALSE) %>%
#         dplyr::mutate("Observation Group Label" = paste0(concept_id, " ", concept_name)) %>%
#         dplyr::select(starts_with("Observation Group")) %>%
#         dplyr::distinct()
#
#
# output2 <-
#         dplyr::left_join(output,
#                          outputb)
#
# output3 <-
#         dplyr::left_join(output2,
#         output2 %>%
#         dplyr::group_by(`Form Name`, `Field Label`) %>%
#         dplyr::summarise(count = n()))
#
# # Creating a fresh `Concept` strip based on msk_concept_id as a part of QA to make sure all the msk_concept_ids exist
# output2_concept <-
#         chariot::left_join_df_to_concept(output %>%
#                                                  dplyr::select(msk_concept_id) %>%
#                                                  dplyr::distinct() %>%
#                                                  # Remove NAs from unmapped concepts because can cause issues with QA downstream
#                                                  dplyr::filter_all(any_vars(!is.na(.)))) %>%
#         chariot::merge_concepts(into = "MSK Concept") %>%
#         dplyr::select(msk_concept_id = concept_id,
#                       `MSK Concept`)
#
#
# output2 <-
#         output %>%
#         dplyr::left_join(output2_concept,
#                          by = "msk_concept_id",
#                          suffix = c(".current", ".new"))
#
#
# # Are any of the new `MSK Concept` strips NA while the current isn't?
# qa1 <-
#         output2 %>%
#         dplyr::filter(is.na(`MSK Concept.new`) && !is.na(`MSK Concept.current`))
#
#
# if (nrow(qa1) > 0) {
#         stop('There are new MSK concepts that are NA while the current ones are not.')
# }
#
# output3 <- output2 %>%
#         dplyr::mutate(`MSK Concept` = coalesce(`MSK Concept.new`, `MSK Concept.current`)) %>%
#         dplyr::select(-starts_with("MSK Concept."))
#
# output4_concept <-
#         chariot::left_join_df_to_concept(output3 %>%
#                                                  dplyr::select(msk_concept_id))
#
#
# output4 <-
#         dplyr::left_join(output3,
#                          output4_concept) %>%
#         dplyr::distinct()
#
# #QA 2: do all the original msk_concept_id == concept_id that was just bound columnwise?
# qa2 <-
#         output4 %>%
#         dplyr::filter(msk_concept_id != concept_id)
#
# if (nrow(qa2) > 0) {
#         stop("error in final join with concept id")
# } else {
#         output5 <-
#                 output4 %>%
#                 dplyr::select(-msk_concept_id)
# }
#
# # Removing NAs for empty modifier and attribute concepts (Keep for fact because it indicates whether a concept is mapped or not)
# output6 <-
#         dplyr::bind_rows(
#                 output5 %>%
#                         dplyr::filter(`MSK Concept Type` %in% c("Observation Group Concept", "Fact Concept")),
#                 output5 %>%
#                         dplyr::filter(!(`MSK Concept Type` %in% c("Observation Group Concept","Fact Concept"))) %>%
#                         dplyr::filter(!is.na(`MSK Concept`))
#         )
#
# # Are all routine ids accounted for?
# qa3 <- all(output6$routine_id %in% input$routine_id)
# if (qa3 == FALSE) {
#         stop('routine_ids are missing in the output.')
# }
#
# # Make sure that all the mapped concepts (Fact, Modifier and Attribute are included in the output)
# ## Pre-melt
# qa4_fact <- mapped_count(input3$`Fact Concept`)+unmapped_count(input3$`Fact Concept`)
# qa4_attribute <- mapped_count(input3$`Attribute Concept`)
# qa4_modifier <- mapped_count(input3$`Modifier Concept`)
# qa4a <- qa4_fact+qa4_attribute+qa4_modifier
#
# ## Getting all mapped and unmapped Facts at the time of melt
# qa4b <-
# input4 %>%
#         dplyr::filter(`MSK Concept Type` %in% c('Fact Concept')) %>%
#         dplyr::distinct() %>%
#         nrow() +
#         ## Getting all mapped Attributes and Modifiers
#         input4 %>%
#         dplyr::filter(!(`MSK Concept Type` %in% c('Fact Concept', 'Observation Group Concept'))) %>%
#         dplyr::filter(!is.na(`MSK Concept`)) %>%
#         dplyr::select(`MSK Concept`) %>%
#         nrow()
#
# qa4 <- qa4a-qa4b
#
# if (qa4 != 0) {
#
#         stop("not all mappings were passed to the melted dataframe input4")
#
# }
#
# # Repeating for output
# qa5a <-
#         output6 %>%
#         dplyr::filter(`MSK Concept Type` == 'Fact Concept') %>%
#         nrow() +
#         ## Getting all mapped Attributes and Modifiers
#         output6 %>%
#         dplyr::filter(!(`MSK Concept Type` %in% c('Fact Concept', 'Observation Group Concept'))) %>%
#         dplyr::filter(!is.na(`MSK Concept`)) %>%
#         nrow()
#
#
# qa5 <- qa4b-qa5a
#
# if (qa5 != 0) {
#
#         stop("not all mappings were passed to output6")
#
# }
#
# final_routine_output <-
#         output6 %>%
#         dplyr::mutate(`MSK Concept Type` = stringr::str_remove(`MSK Concept Type`, " Concept$"))
#
# ## Getting all concepts in final output
# broca::simply_write_csv(x = final_routine_output,
#                         file = path_to_output_fn)
