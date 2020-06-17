# Search Settings
# Global filters for all queries to the concept table
vocabularies <- c("RxNorm", "RxNorm Extension", "HemOnc", "ATC", "SNOMED", "LOINC")
concept_classes <- NULL
domains <- c("Drug")
standard_concepts <- c("S", "C", "NA", NA)
invalid_reasons <- c(NA, "NA")

# Project Setup
origin_fn <-  "~/Memorial Sloan Kettering Cancer Center/KM COVID - General/Mappings/Meera_PreIngestion_Standard_Library.xlsx"
origin_tab <- "MAP_00"

# Target Columns: column where queries are sourced from. Note that the column called "CONCEPT" has been changed to "SOURCE" in this routine since the merge of OMOP concepts is functionalized to be called `Concept`.
## Source Columns: 1:1 concepts
input_file_stem <- "COVID_SL_ROUND2_"
source_col <- "CONCEPT"
word_split <- "[ ]{1}|[(]{1}|[)]{1}|[,]{1}" #The regex to split() the SOURCE column on to retrieve words for words-based searches

## Term Columns: series of search terms and phrases to each original concept to further search for the concept. Term columns are manually inputed by an end-user.
term_col <- "SEARCH_TERM_01"

# Terminal Column: name of the column in the input that, if populated, indicates that a concept has been mapped and further search routines are ignored
terminal_col <- "MSK Concept"

# Skip parameters: parameters that signals skipping over a concept
source_skip_nchar <- 5
additional_filters <- list("CORE_VARIABLES == 'Yes',", #Additional filters fed into the dplyr filter function to apply to the input. Comment out if no filters should be applied.
"FORM %in% c('covid19_testing',
                'covid19_serology_testing',
                'covid19_noninvasive_ventilation_support',
                'covid19_chest_imaging_admission_icu',
                'covid19_anticoagulant_medications',
                'covid19_antiplatelet_medications',
                'covid19_directed_medications')")


# Creating settings object
settings <-
        list(vocabularies,
             concept_classes,
             domains,
             standard_concepts,
             invalid_reasons)

names(settings) <- c("vocabularies",
                     "concept_classes",
                     "domains",
                     "standard_concepts",
                     "invalid_reasons")

rm(list = c("vocabularies",
            "concept_classes",
            "domains",
            "standard_concepts",
            "invalid_reasons"))

# Writing settings for this input_file_stem-origin tab name combo
setting_history_fn <- paste0("data/setting_history/", input_file_stem, origin_tab, ".txt")

cat(Sys.time(), "\n", file = setting_history_fn)


settings_to_write <- settings
while (length(settings_to_write) > 0) {
        setting <- settings_to_write[[1]]
        setting_name <- names(settings_to_write)[1]

        if (!is.null(setting)) {
                cat(setting_name, ": ", paste(setting, collapse = ", "), "\n", file = setting_history_fn, append = TRUE)
        } else {
                cat(setting_name, ": NULL\n", file = setting_history_fn, append = TRUE)
        }

        settings_to_write[[1]] <- NULL

        rm(setting)
        rm(setting_name)

}
rm(settings_to_write)

cat("origin file: ", origin_fn, "\n", file = setting_history_fn, append = TRUE)
cat("origin tab: ", origin_tab, "\n", file = setting_history_fn, append = TRUE)
cat("file stem: ", input_file_stem, "\n", file = setting_history_fn, append = TRUE)
