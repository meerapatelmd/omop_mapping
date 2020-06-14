if ("/Users/patelm9/GitHub/KMI/termite/Map_to_OMOP/pre-procedure" != getwd()) {
        setwd("/Users/patelm9/GitHub/KMI/termite/Map_to_OMOP/pre-procedure")
}

source('utils.R')

# Search Settings
# Global filters for all queries to the concept table
vocabularies <- c("LOINC", "SNOMED")
concept_classes <- NULL
domains <- NULL
standard_concepts <- c("S", "C")
invalid_reasons <- c(NA, "NA")


# Target Columns: column where queries are sourced from. Note that the column called "CONCEPT" has been changed to "SOURCE" in this routine since the merge of OMOP concepts is functionalized to be called `Concept`.
## Source Columns: 1:1 concepts
input_file_stem <- "COVID_CORE_"
source_col <- "Field Label"
## Term Columns: series of search terms and phrases to each original concept to further search for the concept. Term columns are manually inputed by an end-user.
term_col <- "SEARCH_TERM_01"

# Terminal Column: name of the column in the input that, if populated, indicates that a concept has been mapped and further search routines are ignored
terminal_col <- "MSK Concept"

# Skip parameters: parameters that signals skipping over a concept
source_skip_nchar <- 5


# Project Setup
origin_fn <-  "~/Memorial Sloan Kettering Cancer Center/KM COVID - General/Mappings/MASTER COVID MDD - Mappings 2020-06-09.xlsx"
origin_tab <- "Core Variables Meera Subset"


# If the input_fn does not exist in the input subdir, it is written to the input subdir to the input_fn provided above
input_fn <- paste0(input_file_stem, origin_tab, ".csv")
path_to_input_fn <- paste0("input/", input_fn)
if (!file.exists(path_to_input_fn)) {
        origin_data <- broca::read_full_excel(origin_fn)
        input <- origin_data[[origin_tab]]

        if ("routine_id" %in% colnames(input)) {
                        # Using prior routine_id to make sure no garbage is imported
                        input$routine_id <- suppressWarnings(as.integer(input$routine_id))
                        input <-
                                input %>%
                                dplyr::filter(!is.na(routine_id))

                        #If maximum routine_id does not equal number of rows
                        qa1 <- nrow(input)-max(input$routine_id)

                        if (qa1 != 0) {
                                stop("filtering input for valid routine_id failed")
                        }

                        input <-
                                input %>%
                                dplyr::mutate_at(vars(routine_id), as.character) %>%
                                dplyr::select(-(any_of(c("routine_id",
                                                               "Source Exact",
                                                              "Source Like",
                                                              "Source String as Vector",
                                                              "Search Term Exact",
                                                              "Search Term Like",
                                                         "Search Term Synonym Exact",
                                                         "Search Term Synonym Like",
                                                         "Search Term Synonym Str As Vector",
                                                         "Search Term String as Vector",
                                                         "Source Synonym Like",
                                                         "Source Synonym Exact",
                                                         "Source First Word Exact",
                                                         "Source First Word Like",
                                                         "Source No Parentheses Exact",
                                                         "Source No Parentheses Like",
                                                         "Source No Parentheses Str as Vector"))))  %>%
                                        tibble::rowid_to_column("routine_id")
        } else {

                                input <-
                                        input %>%
                                        #dplyr::mutate_at(vars(routine_id), as.character) %>%
                                        dplyr::select(-(any_of(c("routine_id",
                                                                 "Source Exact",
                                                                 "Source Like",
                                                                 "Source String as Vector",
                                                                 "Search Term Exact",
                                                                 "Search Term Like",
                                                                 "Search Term Synonym Exact",
                                                                 "Search Term Synonym Like",
                                                                 "Search Term Synonym Str As Vector",
                                                                 "Search Term String as Vector",
                                                                 "Source Synonym Like",
                                                                 "Source Synonym Exact",
                                                                 "Source First Word Exact",
                                                                 "Source First Word Like",
                                                                 "Source No Parentheses Exact",
                                                                 "Source No Parentheses Like",
                                                                 "Source No Parentheses Str as Vector"))))  %>%
                                        tibble::rowid_to_column("routine_id")
        }

        # Add terminal col if it doesn't add it to the input
        if (!(terminal_col %in% colnames(input))) {
                input <- input %>%
                                rubix::mutate_if_not_exist(column_name = terminal_col,
                                                           value = NA_character_)
        }

        # Copy Input to input folder
        broca::simply_write_csv(x = input,
                                file = path_to_input_fn,
                                log_details = paste0(origin_fn, "TAB: ", origin_tab, "written to ", input_fn))

        #cave::rm_all_objects_that_start_with("origin_")
        #rm(input)

}

