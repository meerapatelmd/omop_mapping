rm(list = ls(all.names = TRUE))
source('~/GitHub/omop_mapping/01 procedure/config/funs.R')

# Search Settings
# Global filters for all queries to the concept table
vocabulary_id <- c("HemOnc", "RxNorm", "RxNorm Extension", "ATC")
concept_class_id <- NULL
domain_id <- "Drug"
standard_concept <- NULL
invalid_reason <- NA


# Project Setup
project_name <- "DRUG_CLASSIFICATION"
origin_fn <-  "~/OneDrive - Memorial Sloan Kettering Cancer Center/escritoire-data/Drug Classification/Mapping Files/antineoplastic.xlsx"
origin_tab <- "MAP_07"

# Required
# Target Columns: column where queries are sourced from. Note that the column called "CONCEPT" has been changed to "SOURCE" in this routine since the merge of OMOP concepts is functionalized to be called `Concept`.
## Source Columns: 1:1 concepts
#input_file_stem <- "ESOPHAGUS_"
source_col <- "APR_GENERIC_NAME"


# Terminal Column: name of the column in the input that, if populated, indicates that a concept has been mapped and further search routines are ignored
terminal_col <- "IS_MAPPED"

#Optional
## Term Columns: series of search terms and phrases to each original concept to further search for the concept. Term columns are manually inputed by an end-user.
search_term_col <- NULL
attribute_col <- NULL
regimen_col <- NULL
component_col <- NULL

# Skip parameters: parameters that signals skipping over a concept
source_skip_nchar <- 3
additional_filters <- NULL
word_split <- "[ ]{1}|[(]{1}|[)]{1}|[,]{1}|[/]{1}|[+]{1}|[-]{1}[>]{1}" # The regex to split() the SOURCE column on to retrieve words for words-based searches

setupProjectDirs()
saveSettings()
createSettingsObj()

filterSettings <-
        .SETTINGS$OutputSettings %>%
        purrr::keep(~!is.null(.))

filterSettings <-
        filterSettings %>%
        purrr::map2(names(filterSettings), function(x,y) as_tibble_col(x, column_name = y)) %>%
        purrr::reduce(cbind) %>%
        as_tibble() %>%
        rubix::normalize_all_to_na()
