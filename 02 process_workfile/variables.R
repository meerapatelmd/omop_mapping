# Project Setup
project_name <- "COVID Standard Library"

# Project Setup
# Origin: Workfile
origin_fn <-  "~/Memorial Sloan Kettering Cancer Center/KM COVID - General/Mappings/Final Ingestion Files/Archive/Meera_StandardLibrary_Workfile.xlsx"
origin_terminal_tab <- "MAP_13"


input_format <- "long" #Is input long format by MSK Concept Type or is it short format where Fact, Attribute, and Modifier are cast across?
terminal_col <- "MSK Concept" #What is the terminal column?
filter_for_form <- c('covid19_concomitant_medications',
                        'covid19_oxygen_support', 'covid19_infection', 'covid19_treatment_before_baseline', 'covid19_thrombotic_events', 'covid19_bleeding_events', 'covid19_invasive_ventilation_support_initiation', 'covid19_testing', 'covid19_chest_imaging_admission_icu', 'covid19_cardiology_testing', 'cancer_status', 'covid19_invasive_ventilation_support_initiation_piped', 'covid19_oxygen_support_piped', 'covid19_noninvasive_ventilation_support', 'covid19_noninvasive_ventilation_support_piped', 'covid19_serology_testing', 'covid19_testing_piped', 'covid19_infection_piped', 'covid19_concomitant_medications_piped')


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

# Writing settings for this input_file_stem-origin tab name combo
# setting_history_fn <- paste0(path_to_settings_dir, "/", origin_terminal_tab, ".txt")
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
