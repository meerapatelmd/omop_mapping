rm(list = ls(all.names = TRUE))
setwd("/Users/patelm9/GitHub/omop_mapping/01 procedure/")
source('/Users/patelm9/GitHub/omop_mapping/01 procedure/config/funs.R')
source('/Users/patelm9/GitHub/omop_mapping/01 procedure/config/constants.R')

# Search Settings
# Global filters for all queries to the concept table
vocabulary_id <- c("HemOnc", "RxNorm", "RxNorm Extension", "ATC")
concept_class_id <- NULL
domain_id <- "Drug"
standard_concept <- NULL
invalid_reason <- NA


# Project Setup
project_name <- "DRUG_CLASSIFICATION"
origin_fn <-  "~/OneDrive - Memorial Sloan Kettering Cancer Center/escritoire-data/Drug Classification/data_without_phi/IDB/missing-drugs-invest-flag.xlsx"
origin_tab <- "MAP_04"

# Required
# Target Columns: column where queries are sourced from. Note that the column called "CONCEPT" has been changed to "SOURCE" in this routine since the merge of OMOP concepts is functionalized to be called `Concept`.
## Source Columns: 1:1 concepts
#input_file_stem <- "ESOPHAGUS_"
source_col <- "APR_GENERIC_NAME"


# Terminal Column: name of the column in the input that, if populated, indicates that a concept has been mapped and further search routines are ignored
terminal_col <- "Fact"

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
        purrr::map2(names(filterSettings), function(x,y)
                as_tibble_col(x,
                              column_name = y)) %>%
        purrr::reduce(cbind) %>%
        as_tibble() %>%
        rubix::normalize_all_to_na()

# If the input_fn does not exist in the input subdir, it is written to the input subdir to the input_fn provided above

if (broca::is_excel(.SETTINGS$Origin$origin_fn)) {
        inputFile <- paste0(.SETTINGS$Origin$origin_tab, ".csv")
        inputPath <- paste0(.PATHS$INPUT, "/", inputFile)
} else {

        inputFile <- paste0(cave::strip_fn(origin_fn), ".csv")
        inputPath <- paste0(.PATHS$INPUT, "/", inputFile)

}

if (!file.exists(inputPath)) {

        origin_data <- read_origin()

        if (is.null(origin_data)) {

                stop('`origin_data` is null')

        }

        raw_input <-
                origin_data %>%
                dplyr::mutate_all(stringr::str_replace_all,
                                  "[^ -~\n]", "") %>%
                dplyr::select(#-(any_of(output_colnames)),
                        -any_of("routine_id"),
                        -starts_with("Source "),
                        -starts_with("Terms ")) %>%
                tibble::rowid_to_column("routine_id")

        broca::simply_write_csv(x = raw_input,
                                file = inputPath)

}

INPUT <- list(File = inputFile,
              Path = inputPath)

.Settings <- .SETTINGS$OutputSettings
#startup_qa()
# if (grepl("[.]xlsx$", origin_fn) == TRUE) {
#                 input_fn <- paste0(origin_tab, ".csv")
#                 path_to_input_fn <- paste0(path_to_input_dir, "/", input_fn)
#                 if (!file.exists(path_to_input_fn)) {
#                         # origin_data <- broca::read_full_excel(origin_fn,
#                         #                                       log_details = paste0("Load: ", origin_tab))
#                         # input <- origin_data[[origin_tab]]
#
#
#                         origin <- read_origin(log_details = paste0("Load: ", origin_tab))
#
#                         input <-
#                                 origin %>%
#                                 dplyr::mutate_all(stringr::str_replace_all,
#                                                       "[^ -~\n]", "")
#
#                         if (is.null(input)) {
#                                 stop('wrong origin_tab')
#                         }
#
#                         if ("routine_id" %in% colnames(input)) {
#                                 input <-
#                                         input %>%
#                                         dplyr::select(-routine_id)
#                         }
#
#
#                         input <-
#                                 input %>%
#                                 dplyr::select(-(any_of(output_colnames)), -starts_with("Source "), -starts_with("Terms ")) %>%
#                                         tibble::rowid_to_column("routine_id")
#
#
#                         if (!("CONCEPT_COUNT" %in% colnames(input))) {
#                                 input <-
#                                         input %>%
#                                         dplyr::group_by_at(vars(!!source_col)) %>%
#                                         dplyr::mutate(CONCEPT_COUNT = length(!!source_col))
#                         }
#
#
#                         # Copy Input to input folder
#                         broca::simply_write_csv(x = input,
#                                                 file = path_to_input_fn,
#                                                 log_details = paste0(origin_fn, "TAB: ", origin_tab, "written to ", input_fn))
#
#                 }
# }
#
# if (grepl("[.]csv$", origin_fn) == TRUE) {
#
#                         input_fn <- paste0(input_file_stem, cave::strip_fn(origin_fn), ".csv")
#                         path_to_input_fn <- paste0("data/input/", input_fn)
#                         if (!file.exists(path_to_input_fn)) {
#                                 input <- read_origin(log_details = paste0("Load: ", origin_tab))
#
#                                 if (is.null(input)) {
#                                         stop('empty input')
#                                 }
#
#                                 if ("routine_id" %in% colnames(input)) {
#                                         # Using prior routine_id to make sure no garbage is imported
#                                         input$routine_id <- suppressWarnings(as.integer(input$routine_id))
#                                         input <-
#                                                 input %>%
#                                                 dplyr::filter(!is.na(routine_id))
#
#                                         input <-
#                                                 input %>%
#                                                 dplyr::mutate_at(vars(routine_id), as.character) %>%
#                                                 dplyr::select(-(any_of(output_colnames)), -starts_with("Source "), -starts_with("Terms ")) %>%
#                                                 tibble::rowid_to_column("routine_id")
#                                 } else {
#                                         input <-
#                                                 input %>%
#                                                 dplyr::select(-(any_of(output_colnames)), -starts_with("Source "), -starts_with("Terms ")) %>%
#                                                 tibble::rowid_to_column("routine_id")
#                                 }
#
#                                 # Copy Input to input folder
#                                 broca::simply_write_csv(x = input,
#                                                         file = path_to_input_fn)
#
#         }
# }



