rm(list = ls())
library(tidyverse)
setwd("/Users/patelm9/GitHub/omop_mapping/01 procedure/")
source('/Users/patelm9/GitHub/omop_mapping/01 procedure/config/funs.R')
source('/Users/patelm9/GitHub/omop_mapping/01 procedure/config/constants.R')
source('/Users/patelm9/GitHub/omop_mapping/01 procedure/config/variables.R')



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
