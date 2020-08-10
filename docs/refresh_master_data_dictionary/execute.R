input_fn <- "~/Memorial Sloan Kettering Cancer Center/KM COVID - General/Mappings/COVID_Meera.xlsx"
input <- broca::read_full_excel(input_fn)
input2a_total <- input$MEERA_SUBSET
input2b_current <- input$MAP_06

# Aligning column names for input2a_total
colnames(input2a_total)[!(colnames(input2a_total) %in% colnames(input2b_current))]

input2a_total2 <-
        input2a_total %>%
        dplyr::select(-rowid, -concept_id) %>%
        dplyr::rename(`Fact Concept` = Concept)


colnames(input2a_total2)[!(colnames(input2a_total2) %in% colnames(input2b_current))]

# Isolating only the concept_ids that need to be added
input2a_total3 <-
        dplyr::filter(input2a_total2,
                      FORM %in% c("covid19_chest_imaging_admission_icu", "covid19_chest_imaging_admission_icu_piped"))



# Combining final
final_output <-
        dplyr::bind_rows(input2b_current,
                         input2a_total3)


broca::copy_to_clipboard(final_output)
