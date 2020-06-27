# Project Setup
project_name <- "GLIOMA"
origin_fn <-  "~/Memorial Sloan Kettering Cancer Center/Glioma REDCap Standardization - KMI Only - KMI Only/Glioma_PreIngestion.xlsx"
origin_tab <- "Sheet1"


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
# settings <-
#         list(vocabularies,
#              concept_classes,
#              domains,
#              standard_concepts,
#              invalid_reasons)
#
# names(settings) <- c("vocabularies",
#                      "concept_classes",
#                      "domains",
#                      "standard_concepts",
#                      "invalid_reasons")
#
# rm(list = c("vocabularies",
#             "concept_classes",
#             "domains",
#             "standard_concepts",
#             "invalid_reasons"))
#
# # Writing settings for this input_file_stem-origin tab name combo
# setting_history_fn <- paste0(path_to_settings_dir, "/", origin_tab, ".txt")
#
# cat("\n", file = setting_history_fn)
#
#
# settings_to_write <- settings
# while (length(settings_to_write) > 0) {
#         setting <- settings_to_write[[1]]
#         setting_name <- names(settings_to_write)[1]
#
#         if (!is.null(setting)) {
#                 cat(setting_name, ": ", paste(setting, collapse = ", "), "\n", file = setting_history_fn, append = TRUE)
#         } else {
#                 cat(setting_name, ": NULL\n", file = setting_history_fn, append = TRUE)
#         }
#
#         settings_to_write[[1]] <- NULL
#
#         rm(setting)
#         rm(setting_name)
#
# }
# rm(settings_to_write)
#
# cat("origin file: ", origin_fn, "\n", file = setting_history_fn, append = TRUE)
# cat("origin tab: ", origin_tab, "\n", file = setting_history_fn, append = TRUE)
