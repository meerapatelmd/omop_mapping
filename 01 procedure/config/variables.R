# Search Settings
# Global filters for all queries to the concept table
vocabularies <- c("SNOMED")
concept_classes <- NULL
domains <- NULL
standard_concepts <- NULL
invalid_reasons <- c(NA, "NA")

# Project Setup
project_name <- "CBIOPORTAL"
origin_fn <-  "~/Memorial Sloan Kettering Cancer Center/cBioPortal Standardization - KMI Only - KMI Only/Mapping Files/cBioPortal_Workfile.xlsx"
origin_tab <- "MAP_20"


# Target Columns: column where queries are sourced from. Note that the column called "CONCEPT" has been changed to "SOURCE" in this routine since the merge of OMOP concepts is functionalized to be called `Concept`.
## Source Columns: 1:1 concepts
#input_file_stem <- "ESOPHAGUS_"
source_col <- "CONCEPT"
word_split <- "[ ]{1}|[(]{1}|[)]{1}|[,]{1}|[/]{1}|[+]{1}|[-]{1}[>]{1}" #The regex to split() the SOURCE column on to retrieve words for words-based searches

## Term Columns: series of search terms and phrases to each original concept to further search for the concept. Term columns are manually inputed by an end-user.
term_col <- NULL

# Terminal Column: name of the column in the input that, if populated, indicates that a concept has been mapped and further search routines are ignored
terminal_col <- "Fact"
attribute_col <- NULL

regimen_col <- NULL
component_col <- NULL

# Skip parameters: parameters that signals skipping over a concept
source_skip_nchar <- 3
additional_filters <- list("VARIABLE == 'Cancer Type'")

#Additional filters fed into the dplyr filter function to apply to the input. Comment out if no filters should be applied.

# Creating project directory if it does not exist
path_to_project_data <- paste0("data/", project_name)
cave::create_dir_if_not_exist(path_to_project_data)

# Creating source file subdir if it does not exist
path_to_file_subdir <- paste0(path_to_project_data, "/", cave::strip_fn(origin_fn))
cave::create_dir_if_not_exist(path_to_file_subdir)

# Creating subdirectories
path_to_input_dir <- paste0(path_to_file_subdir, "/input")
cave::create_dir_if_not_exist(path_to_input_dir)
path_to_output_dir <- paste0(path_to_file_subdir, "/output")
cave::create_dir_if_not_exist(path_to_output_dir)
path_to_settings_dir <- paste0(path_to_file_subdir, "/settings")
cave::create_dir_if_not_exist(path_to_settings_dir)

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
setting_history_fn <- paste0(path_to_settings_dir, "/", origin_tab, ".txt")

cat("\n", file = setting_history_fn)


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
cat("source col: ", source_col, "\n", file = setting_history_fn, append = TRUE)
cat("term col: ", term_col, "\n", file = setting_history_fn, append = TRUE)
cat("terminal col: ", terminal_col, "\n", file = setting_history_fn, append = TRUE)
cat("attribute col: ", attribute_col, "\n", file = setting_history_fn, append = TRUE)