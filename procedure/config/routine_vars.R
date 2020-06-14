# Search Settings
# Global filters for all queries to the concept table
vocabularies <- c("HemOnc", "RxNorm", "RxNorm Extension")
concept_classes <- NULL
domains <- NULL
standard_concepts <- c("S", "C", "NA", NA)
invalid_reasons <- c(NA, "NA")

# Project Setup
origin_fn <-  "~/Memorial Sloan Kettering Cancer Center/KM COVID - General/Mappings/BY FORM/Workfile.xlsx"
origin_tab <- "MAP_08"


# Target Columns: column where queries are sourced from. Note that the column called "CONCEPT" has been changed to "SOURCE" in this routine since the merge of OMOP concepts is functionalized to be called `Concept`.
## Source Columns: 1:1 concepts
input_file_stem <- "COVID_SL_"
source_col <- "CONCEPT"

## Term Columns: series of search terms and phrases to each original concept to further search for the concept. Term columns are manually inputed by an end-user.
term_col <- "SEARCH_TERM_01"

# Terminal Column: name of the column in the input that, if populated, indicates that a concept has been mapped and further search routines are ignored
terminal_col <- "MSK Concept"

# Skip parameters: parameters that signals skipping over a concept
source_skip_nchar <- 5
additional_filters <- list("STANDARD_LIBRARY == TRUE") #Additional filters fed into the dplyr filter function to apply to the input. Comment out if no filters should be applied.