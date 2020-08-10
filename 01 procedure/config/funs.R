####################
## Operational    ##
####################

setupProjectDirs <-
        function() {
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


                .PATHS <<- list(INPUT = path_to_input_dir,
                               OUTPUT = path_to_output_dir,
                               SETTINGS = path_to_settings_dir)
        }

saveSettings <-
        function() {

                # Deleting the current settings file if it exists
                settingsFile <- paste0(.PATHS$SETTINGS, "/", origin_tab, ".txt")

                if (file.exists(settingsFile)) {
                        file.remove(settingsFile)
                }

                # Getting all non-function objects
                nonFunctObjs <-
                ls(envir = globalenv(), sorted = FALSE) %>%
                        rubix::map_names_set(~class(get(.))) %>%
                        purrr::keep(~!(any(. %in% c("function")))) %>%
                        names()

                # Grouping R objects based on desired heading
                Groups <- c("Origin", "Required", "NonRequiredCols", "OutputSettings", "MiscSettings", "Other")
                Origin <- grep("origin", nonFunctObjs, ignore.case = T, value = TRUE)
                NonOrigin <- grep("origin", nonFunctObjs, ignore.case = T, value = TRUE, invert = TRUE)
                Required <- c("project_name", "source_col", "terminal_col")
                NonRequired <- NonOrigin[!(NonOrigin %in% Required)]
                NonRequiredCols <- grep("col", NonRequired, ignore.case = TRUE, value = TRUE)
                OutputSettings <- c("vocabulary_id",
                                    "concept_class_id",
                                    "domain_id",
                                    "standard_concept",
                                    "invalid_reason")
                MiscSettings <- c("source_skip_nchar", "additional_filters", "word_split")


                # Paste expression in case the settings ever need to be loaded again
                nonFunctObjsValues <-
                nonFunctObjs %>%
                        rubix::map_names_set(function(x) paste0(x, " <- '", get(x, globalenv()), "'")) %>%
                        unlist() %>%
                        purrr::map(rubix::vector_to_tibble, new_col = "Expression") %>%
                        dplyr::bind_rows(.id = "Object") %>%
                        dplyr::mutate(Group = factor(Object))

                # Adding the Groups each Object belongs to
                nonFunctObjsValues$Group <-
                        as.character(
                        forcats::fct_collapse(nonFunctObjsValues$Group,
                                              Origin = Origin,
                                              Required = Required,
                                              NonRequiredCols = NonRequiredCols,
                                              OutputSettings = OutputSettings,
                                              MiscSettings = MiscSettings,
                                              other_level = "Other"
                        ))

                # Re-leveling the groups
                nonFunctObjsValues$Group2 <-
                        factor(nonFunctObjsValues$Group,
                               levels = Groups,
                               labels = 1:length(Groups))

                nonFunctObjsValues2 <-
                        nonFunctObjsValues %>%
                        dplyr::filter(Group2 != 6) %>%
                        rubix::group_by_aggregate(Group, Group2,
                                                  agg.col = Expression,
                                                  collapse = "\n") %>%
                        dplyr::arrange(Group2) %>%
                        dplyr::transmute(Expression = paste0("# ", Group, "\n", Expression, "\n\n")) %>%
                        unlist() %>%
                        paste(collapse = "")


                cat(nonFunctObjsValues2, file = settingsFile, sep = "")

        }


createSettingsObj <-
        function() {
                # Getting all non-function objects
                nonFunctObjs <-
                        ls(envir = globalenv(), sorted = FALSE) %>%
                        rubix::map_names_set(~class(get(.))) %>%
                        purrr::keep(~!(any(. %in% c("function")))) %>%
                        names()

                # Grouping R objects based on desired heading
                Groups <- c("Origin", "Required", "NonRequiredCols", "OutputSettings", "MiscSettings", "Other")

                Origin <- grep("origin", nonFunctObjs, ignore.case = T, value = TRUE)
                NonOrigin <- grep("origin", nonFunctObjs, ignore.case = T, value = TRUE, invert = TRUE)
                Required <- c("project_name", "source_col", "terminal_col")
                NonRequired <- NonOrigin[!(NonOrigin %in% Required)]
                NonRequiredCols <- grep("col", NonRequired, ignore.case = TRUE, value = TRUE)
                OutputSettings <- c("vocabulary_id",
                                    "concept_class_id",
                                    "domain_id",
                                    "standard_concept",
                                    "invalid_reason")
                MiscSettings <- c("source_skip_nchar", "additional_filters", "word_split")

                Groups <-
                        list(
                                Origin = Origin,
                                Required = Required,
                                NonRequiredCols = NonRequiredCols,
                                OutputSettings = OutputSettings,
                                MiscSettings = MiscSettings
                        )

                .SETTINGS <<-
                Groups %>%
                        rubix::map_names_set(function(x) rubix::map_names_set(x,
                                                                              function(y) get(y, envir = globalenv())))

                Groups %>%
                        rubix::map_names_set(function(x) rubix::map_names_set(x,
                                                                              function(y) rm(list = y, envir = globalenv())))

        }

createOutputPath <-
        function() {
                path.expand(paste0(getwd(), "/", path_to_output_dir, "/", origin_tab,"_", cave::strip_fn(cave::present_script_path()), ".csv"))

        }

brake_if_output_exists <-
        function() {

                if (file.exists(path_to_output_fn)) {

                        secretary::typewrite_warning(path_to_output_fn, "already exists and will be overwritten.")
                        secretary::press_enter()
                }
        }



####################
## Apply Settings ##
####################
# Creates a settings object containing all the variables required by the filter_for_settings() function
# make_settings <-
#         function(override_vocabularies = NULL,
#                  override_concept_classes = NULL,
#                  override_domains = NULL,
#                  override_standard_concepts = NULL,
#                  override_invalid_reasons = NULL) {
#
#                 concept_setting_elements <-
#                         c(
#                                 "vocabularies",
#                                 "concept_classes",
#                                 "domains",
#                                 "standard_concepts",
#                                 "invalid_reasons"
#                         )
#
#                 if (any(!exists(concept_setting_elements, envir = globalenv()))) {
#
#                         stop("filter settings do not exist in .GlobalEnv")
#
#                 }
#
#
#                 global_settings <-
#                         concept_setting_elements %>%
#                         rubix::map_names_set(get, envir = globalenv())
#
#
#
#                 fun_settings <-
#                         list(
#                                 "vocabularies" = override_vocabularies,
#                                 "concept_classes" = override_concept_classes,
#                                 "domains" = override_domains,
#                                 "standard_concepts" = override_standard_concepts,
#                                 "standard_concepts" = override_standard_concepts
#                         ) %>%
#                         purrr::keep(~!is.null(.))
#
#
#
#                 final_settings <- c(fun_settings,
#                                     global_settings[!(names(global_settings) %in% names(fun_settings))])
#
#                 final_settings <- final_settings[concept_setting_elements]
#
#                 return(final_settings)
#
#         }


# Filters the resultset of a concept table search for the settings in variables.R
# dep_filter_for_settings <-
#         function(.data,
#                  override_vocabularies = NULL,
#                  override_concept_classes = NULL,
#                  override_domains = NULL,
#                  override_standard_concepts = NULL,
#                  override_invalid_reasons = NULL)   {
#
#
#                 settings <-
#                 make_settings(override_vocabularies = override_vocabularies,
#                               override_concept_classes = override_concept_classes,
#                                  override_domains = override_domains,
#                                  override_standard_concepts = override_standard_concepts,
#                                  override_invalid_reasons = override_invalid_reasons)
#
#                 if (!is.null(settings$vocabularies)) {
#
#                                 .data <-
#                                         .data %>%
#                                         dplyr::filter(vocabulary_id %in% settings$vocabularies)
#
#                 }
#
#
#                 if (!is.null(settings$concept_classes)) {
#
#                         .data <-
#                                 .data %>%
#                                 dplyr::filter(concept_class_id %in% settings$concept_classes)
#
#                 }
#
#                 if (!is.null(settings$domains)) {
#
#                         .data <-
#                                 .data %>%
#                                 dplyr::filter(domain_id %in% settings$domains)
#
#                 }
#
#
#                 if (!is.null(settings$standard_concepts)) {
#
#                         .data <-
#                                 .data %>%
#                                 dplyr::filter(standard_concept %in% settings$standard_concepts)
#
#                 }
#
#                 if (!is.null(settings$invalid_reasons)) {
#
#                         .data <-
#                                 .data %>%
#                                 dplyr::filter(invalid_reason %in% settings$invalid_reasons)
#
#                 }
#
#                 return(.data)
#
#         }


# filter_for_settings <-
#         function(.data)   {
#
#                 if (!is.null(settings$vocabularies)) {
#
#                         .data <-
#                                 .data %>%
#                                 dplyr::filter(vocabulary_id %in% settings$vocabularies)
#
#                 }
#
#
#                 if (!is.null(settings$concept_classes)) {
#
#                         .data <-
#                                 .data %>%
#                                 dplyr::filter(concept_class_id %in% settings$concept_classes)
#
#                 }
#
#                 if (!is.null(settings$domains)) {
#
#                         .data <-
#                                 .data %>%
#                                 dplyr::filter(domain_id %in% settings$domains)
#
#                 }
#
#
#                 if (!is.null(settings$standard_concepts)) {
#
#                         .data <-
#                                 .data %>%
#                                 dplyr::filter(standard_concept %in% settings$standard_concepts)
#
#                 }
#
#                 if (!is.null(settings$invalid_reasons)) {
#
#                         .data <-
#                                 .data %>%
#                                 dplyr::filter(invalid_reason %in% settings$invalid_reasons)
#
#                 }
#
#                 return(.data)
#
#         }

#################################
## Apply More Filters to Input ##
#################################

read_origin <-
        function(...) {

                if (broca::is_excel(origin_fn)) {

                        origin_data <- broca::read_full_excel(origin_fn,
                                                              ...)

                        origin_data[[origin_tab]]

                } else if (broca::is_csv(origin_fn)) {

                        broca::simply_read_csv(origin_fn, ...)

                } else {

                        stop("Invalid originating file.")

                }
        }

read_raw_input <-
        function(verbose = TRUE) {

                x <- broca::simply_read_csv(path_to_input_fn,
                                            log_details = "read input") %>%
                        rubix::normalize_all_to_na()


                if (verbose) {

                                secretary::typewrite(crayon::bold("Terminal Column:"), terminal_col)

                                cat("\n")

                                print(
                                        x %>%
                                                dplyr::select(!!terminal_col) %>%
                                                dplyr::mutate_at(vars(!!terminal_col),
                                                                 function(x) ifelse(is.na(x), "Unmapped", "Mapped")) %>%
                                                group_by_at(vars(!!terminal_col)) %>%
                                                summarize(COUNT = n(), .groups = "drop") %>%
                                                dplyr::ungroup()
                                )

                                cat("\n")

                }

                return(x)

        }



read_input <-
        function(all_types = FALSE) {

                raw_input <- read_raw_input()

                if (!is.null(additional_filters)) {
                        input <- raw_input %>%
                                        rubix::filterList(additional_filters)
                } else {
                        input <- raw_input
                }

                cat("\n")

                secretary::typewrite(crayon::bold("Terminal Column:"), terminal_col)
                secretary::typewrite(crayon::bold("Source Column:"), source_col)
                secretary::typewrite(crayon::bold("Term Column:"), term_col)
                secretary::typewrite(crayon::bold("Attribute Column:"), attribute_col)

                if (!is.null(additional_filters)) {

                        secretary::typewrite(crayon::bold("Filters:"))
                        additional_filters %>%
                                purrr::map(function(x) secretary::typewrite(x, tabs = 1))

                }

                cat("\n")

                print(
                        x %>%
                                dplyr::select(!!terminal_col) %>%
                                dplyr::mutate_at(vars(!!terminal_col),
                                                 function(x)
                                                         ifelse(is.na(x),
                                                                "Unmapped",
                                                                "Mapped")) %>%
                                group_by_at(vars(!!terminal_col)) %>%
                                summarize(COUNT = n(), .groups = "drop") %>%
                                dplyr::ungroup()
                )

                cat("\n")

                return(x)

        }

filter_out_null <-
        function(output) {
                output %>%
                        purrr::keep(~!is.null(.)) %>%
                        purrr::keep(~nrow(.)>0)
        }



read_raw_input <-
        function() {

                if (terminal_col == "MSK Concept") {

                        x <- broca::simply_read_csv(path_to_input_fn,
                                                    log_details = "read input") %>%
                                normalize_na() %>%
                                dplyr::mutate_all(as.character)

                } else if (terminal_col == "Fact") {

                        x <- broca::simply_read_csv(path_to_input_fn,
                                                    log_details = "read input") %>%
                                normalize_na() %>%
                                dplyr::mutate_all(as.character)

                } else {

                        x <- broca::simply_read_csv(path_to_input_fn,
                                                    log_details = "read input") %>%
                                normalize_na() %>%
                                dplyr::mutate_all(as.character)
                }

                cat("\n")

                secretary::typewrite(crayon::bold("Terminal Column:"), terminal_col)

                cat("\n")

                print(
                        x %>%
                                dplyr::select(!!terminal_col) %>%
                                dplyr::mutate_at(vars(!!terminal_col),
                                                 function(x)
                                                         ifelse(is.na(x),
                                                                "Unmapped",
                                                                "Mapped")) %>%
                                group_by_at(vars(!!terminal_col)) %>%
                                summarize(COUNT = n(), .groups = "drop") %>%
                                dplyr::ungroup()
                )

                cat("\n")

                return(x)

        }


####################
## Cleanup Data   ##
####################
# Normalize all NA values that are strings to true NAs
normalize_na <-
        function(.data) {
                .data %>%
                        dplyr::mutate_all(stringr::str_replace_all, "^NA$", "") %>%
                        dplyr::mutate_all(na_if, "")
        }

# Filter for 250 rows, the most Excel can handle in a single cell
filter_max_250 <-
        function(.data) {
                .data %>%
                        dplyr::slice(1:250)

        }

# Filter for a n number of rows, particularly helpful with combination searches where the total of all search results cannot be greater than 250
filter_max_n <-
        function(.data, n) {
                .data %>%
                        dplyr::slice(1:n)
        }


##################
## Deprecated   ##
##################
read_workfile <-
        function(routine, ...) {

                x <- broca::simply_read_csv(path_to_input_fn,
                                       log_details = routine) %>%
                        normalize_na()

                if (!missing(...)) {
                        x <-
                                x %>%
                                dplyr::filter(...)
                }

                print(
                        x %>%
                        dplyr::select(!!terminal_col) %>%
                                dplyr::mutate_at(vars(!!terminal_col),
                                                 function(x)
                                                         ifelse(is.na(x),
                                                                "Unmapped",
                                                                "Mapped")) %>%
                                group_by_at(vars(!!terminal_col)) %>%
                                summarize(COUNT = n())
                )

                return(x)

        }

create_types_output_object <-
        function(param_types = NULL) {
                if (is.null(param_types)) {
                                final_output <- list()
                                for (i in 1:length(types)) {
                                        final_output[[i]] <- tibble()
                                }
                                names(final_output) <- types

                                return(final_output)
                } else {
                        types <- param_types
                        final_output <- list()
                        for (i in 1:length(types)) {
                                final_output[[i]] <- tibble()
                        }
                        names(final_output) <- types

                        return(final_output)

                }
        }


##############
## Errors ####
##############

try_catch_as_na_df <-
        function(expr) {

                tryCatch(expr = expr,
                         error = function(e) NA %>%
                                                rubix::vector_to_tibble(!!new_col_name))

        }

#########
## QA Input ##
#########
# Make sure all columns are in the input
startup_qa <-
        function() {
                x <- broca::simply_read_csv(path_to_csv = path_to_input_fn)
                if (!(source_col %in% colnames(x))) {
                        stop("`source_col` not in input.")
                }

                if (!(terminal_col %in% colnames(x))) {
                        stop("`terminal_col` not in input.")
                }

        }

# View input columns if failed qa
view_columns <-
        function() {
                x <- broca::simply_read_csv(path_to_csv = path_to_input_fn)
                secretary::typewrite_bold("Columns in input:")
                colnames(x) %>%
                        purrr::map(function(x) secretary::typewrite(x, tabs = 1))

        }



##################
## Query        ##
##################
query_phrase_in_athena <-
        function(phrase, type, remove_regex = "[']{1}|[?]{1}$|[[:punct:]]{1}$", n = 250, synonym = FALSE) {

                        secretary::typewrite(crayon::bold("Raw:"), phrase)
                        mod_phrase <- stringr::str_remove_all(phrase, pattern = remove_regex)


                        if (synonym) {

                                if (mod_phrase != phrase) {
                                        if (type != "string") {
                                                secretary::typewrite(crayon::bold("Modified:"), mod_phrase)
                                                chariot::query_phrase_synonym(phrase = mod_phrase,
                                                                      type = type) %>%
                                                        dplyr::mutate_all(as.character) %>%
                                                        filter_for_settings() %>%
                                                        rubix::arrange_by_nchar(concept_name) %>%
                                                        filter_max_n(n = n)
                                        } else {
                                                secretary::typewrite(crayon::bold("Modified:"), mod_phrase)
                                                chariot::query_string_as_vector_synonym(mod_phrase,
                                                                                split =  " |[[:punct:]]") %>%
                                                        dplyr::mutate_all(as.character) %>%
                                                        filter_for_settings() %>%
                                                        rubix::arrange_by_nchar(concept_name) %>%
                                                        filter_max_n(n = n)
                                        }
                                } else {
                                        if (type != "string") {
                                                chariot::query_phrase_synonym(phrase = phrase,
                                                                      type = type) %>%
                                                        dplyr::mutate_all(as.character) %>%
                                                        filter_for_settings() %>%
                                                        rubix::arrange_by_nchar(concept_name) %>%
                                                        filter_max_n(n = n)
                                        } else {
                                                secretary::typewrite(crayon::bold("Modified:"), mod_phrase)
                                                chariot::query_string_as_vector_synonym(mod_phrase,
                                                                                split = " |[[:punct:]]") %>%
                                                        dplyr::mutate_all(as.character) %>%
                                                        filter_for_settings() %>%
                                                        rubix::arrange_by_nchar(concept_name) %>%
                                                        filter_max_n(n = n)
                                        }
                                }



                        } else {

                        if (mod_phrase != phrase) {
                                if (type != "string") {
                                        secretary::typewrite(crayon::bold("Modified:"), mod_phrase)

                                        chariot::query_phrase(phrase = mod_phrase,
                                                              type = type) %>%
                                                dplyr::mutate_all(as.character) %>%
                                                #filter_for_settings() %>%
                                                rubix::arrange_by_nchar(concept_name)
                                } else {
                                        secretary::typewrite(crayon::bold("Modified:"), mod_phrase)
                                        chariot::query_string_as_vector(mod_phrase,
                                                                        split =  " |[[:punct:]]") %>%
                                                dplyr::mutate_all(as.character) %>%
                                                filter_for_settings() %>%
                                                rubix::arrange_by_nchar(concept_name) %>%
                                                filter_max_n(n = n)
                                }
                        } else {
                                if (type != "string") {
                                        chariot::query_phrase(phrase = phrase,
                                                              type = type) %>%
                                                dplyr::mutate_all(as.character) %>%
                                                filter_for_settings() %>%
                                                rubix::arrange_by_nchar(concept_name) %>%
                                                filter_max_n(n = n)
                                } else {
                                        secretary::typewrite(crayon::bold("Modified:"), mod_phrase)
                                        chariot::query_string_as_vector(mod_phrase,
                                                                        split = " |[[:punct:]]") %>%
                                                dplyr::mutate_all(as.character) %>%
                                                filter_for_settings() %>%
                                                rubix::arrange_by_nchar(concept_name) %>%
                                                filter_max_n(n = n)
                                }
                        }
                        }


        }


lookup_cancer_gov_dictionary <-
        function(phrase, type) {
                if (!exists("cg_dict", envir = globalenv())) {
                        cg_dict <<- broca::simply_read_csv("/Users/patelm9/GitHub/KMI/biblio-tech/DICTIONARY/CancerGov_Drugs/DrugDictionary.csv") %>%
                                dplyr::mutate(`Code String` = stringr::str_remove_all(`Code name`, "[-]"))
                }

                if (type == "like") {

                        try_catch_as_na_df(
                        cg_dict %>%
                                dplyr::filter_all(function(x) grepl(phrase,
                                                                    x,
                                                                    ignore.case = TRUE) == TRUE))
                } else if (type == "exact") {
                        try_catch_as_na_df(
                        cg_dict %>%
                                dplyr::filter_all(function(x) x %in% phrase)
                        )
                } else if (type == "string") {
                        Args <- strsplit(phrase, " |[[:punct:]]") %>%
                                        unlist() %>%
                                        centipede::no_na() %>%
                                        centipede::no_blank() %>%
                                        unique()

                        for (i in 1:length(Args)) {
                                if (i == 1) {

                                        output <-
                                                try_catch_as_na_df(
                                                cg_dict %>%
                                                dplyr::filter_all(function(x) grepl(Args[1],
                                                                                    x,
                                                                                    ignore.case = TRUE) == TRUE)
                                        )
                                } else {
                                        output <-
                                                try_catch_as_na_df(
                                                output %>%
                                                dplyr::filter_all(function(x) grepl(Args[i],
                                                                                    x,
                                                                                    ignore.case = TRUE) == TRUE)
                                                )
                                }
                        }
                        return(output)

                                }
        }


search_cancer.gov_dictionary <-
        function(phrase) {
                if (!exists("cg_dict", envir = globalenv())) {
                        cg_dict <<- broca::simply_read_csv("/Users/patelm9/GitHub/KMI/biblio-tech/DICTIONARY/CancerGov_Drugs/DrugDictionary.csv") %>%
                                dplyr::mutate(`Code String` = stringr::str_remove_all(`Code name`, "[-]")) %>%
                                tidyr::pivot_longer(cols = c('Abbreviation',
                                                             'Chemical structure',
                                                             'Synonym',
                                                             'Foreign brand name',
                                                             'Code name',
                                                             'US brand name',
                                                             'Acronym',
                                                             'Code String'),
                                                    names_to = "Synonym Type",
                                                    values_to = "Synonym",
                                                    values_drop_na = TRUE) %>%
                                dplyr::select(-`Input File`)
                }

                cg_dict %>%
                        rubix::filter_at_grepl(Synonym,
                                               grepl_phrase = phrase)

        }

search_drug_in_uptodate <-
        function(drug, all_names = TRUE) {
                if (!exists("uptodate_drug_dict", envir = globalenv())) {
                        uptodate_drug_dict <<- broca::read_full_excel("/Users/patelm9/GitHub/KMI/biblio-tech/DICTIONARY/UpToDate_Drugs/MASTER.xlsx")
                }

                if (all_names) {
                        name_cols <-
                                c("DRUG",
                                  "Brand Names: US",
                                  "Brand Names: Canada")

                        output <- list()
                        for (i in 1:length(name_cols)) {
                                output[[i]] <-
                                        uptodate_drug_dict$MASTER %>%
                                        rubix::filter_at_grepl(!!name_cols[i],
                                                               grepl_phrase = drug)
                        }

                        output <- dplyr::bind_rows(output)

                } else {
                        output <-
                                uptodate_drug_dict$MASTER %>%
                                rubix::filter_at_grepl(DRUG,
                                                       grepl_phrase = drug)
                }

                return(output)
        }

search_uptodate <-
        function(drug) {
                if (!exists("uptodate_drug_dict", envir = globalenv())) {
                        uptodate_drug_dict <<- broca::read_full_excel("/Users/patelm9/GitHub/KMI/biblio-tech/DICTIONARY/UpToDate_Drugs/MASTER.xlsx")
                }

                if (!exists("uptodate_lookup", envir = globalenv())) {
                        uptodate_lookup <<-
                                uptodate_drug_dict$MASTER %>%
                                tidyr::pivot_longer(cols = !DRUG,
                                                    names_to = "Variable",
                                                    values_to = "Value",
                                                    values_drop_na = TRUE)

                }

                output_01 <-
                uptodate_lookup %>%
                        rubix::filter_at_grepl(Value,
                                               grepl_phrase = drug)


                if (nrow(output_01) > 0) {

                output_01 %>%
                        tidyr::separate_rows(Value,
                                             sep = "[\r\n]") %>%
                        rubix::filter_at_grepl(Value,
                                               grepl_phrase = drug) %>%
                                rubix::call_mr_clean()
                } else {
                        output_01
                }


        }

synonyms_from_chemidplus <-
        function(phrase,
                 type = "contains") {

                require(httr)
                require(rvest)

                #Remove all spaces
                phrase <- stringr::str_remove_all(phrase, "\\s")

                url <- paste0("https://chem.nlm.nih.gov/chemidplus/name/", type, "/",  phrase)
                url = police::try_catch_error_as_null(url(url, "rb"))

                #print("714")
                if (!is.null(url)) {

                        #print("718")
                        resp <- police::try_catch_error_as_null(read_html(url))
                        Sys.sleep(1)
                        close(url)

                        if (!is.null(resp)) {

                                #print("723")

                                # If there aren't any #names headers, it is likely that the query resulted in multiple search results and needs to be tied to an RN number
                                qa <-
                                resp %>%
                                        html_nodes("#names")

                                # If there are 0 html_names #names, checking to see if it landed on a multiple results page
                                if (length(qa) == 0) {
                                        #print("733")

                                        multiple_results <-
                                                resp %>%
                                                html_nodes("div") %>%
                                                html_text()

                                        if (any(grepl("^1$", multiple_results))) {
                                                # Example: Tamoxifen [INN:BAN]10540-29-1
                                                first_drug_name <- multiple_results[grep("^1$", multiple_results)+1][1]
                                                print(first_drug_name)

                                                # Getting the numeric rn to re-query ie 10540-29-1 to get https://chem.nlm.nih.gov/chemidplus/rn/10540-29-1
                                                first_drug_rn <- stringr::str_replace_all(first_drug_name, "(^.*?[^[0-9]]{1})([0-9]{2,}[-]{1}[0-9]{2,}[-]{1}[0-9]{1,}$)", "\\2")
                                                print(first_drug_rn)
                                                #print("746")

                                                url <- paste0("https://chem.nlm.nih.gov/chemidplus/rn/", first_drug_rn)
                                                url <- police::try_catch_error_as_null(url(url, "rb"))

                                                resp <- police::try_catch_error_as_null(read_html(url))

                                                Sys.sleep(1)

                                                if (!is.null(url)) {close(url)}

                                        }
                                }
                        }
                }

                if (!is.null(resp)) {

                        resp <-
                                resp %>%
                                html_nodes("#names") %>%
                                html_text() %>%
                                strsplit(split = "\n") %>%
                                unlist() %>%
                                stringr::str_remove_all("Systematic Name|Names and Synonyms|Results Name|Name of Substance|MeSH Heading|Synonyms|[^ -~]") %>%
                                trimws("both") %>%
                                centipede::no_blank() %>%
                                unique()

                        return(resp)

                }
                Sys.sleep(3)
                closeAllConnections()
}

##################
## Typewrite    ##
##################
typewrite_percent_progress <-
        function(i, input) {

                # if (interactive()) {

                                if (is.data.frame(input)) {

                                                .data <- input

                                                if (i != nrow(.data)) {
                                                        if (!exists("percent_progress", envir = globalenv())) {
                                                                percent_progress <<- signif((i/nrow(.data))*100, digits = 2)
                                                        }

                                                        percent_progress <- get("percent_progress", envir = globalenv())

                                                        current_percent <- signif((i/nrow(.data))*100, digits = 2)

                                                        if (current_percent != percent_progress) {

                                                                percent_progress <<- current_percent
                                                                Sys.sleep(.1)

                                                                if ((percent_progress %% 5) == 0) {
                                                                        secretary::typewrite(Sys.time())
                                                                        secretary::typewrite(current_percent, "percent complete.")
                                                                        secretary::typewrite(i, "out of", nrow(.data))
                                                                }
                                                        }

                                                } else {
                                                        secretary::typewrite("100% complete.")
                                                        rm(percent_progress, envir = globalenv())
                                                }


                                } else {
                                        if (i != length(input)) {
                                                if (!exists("percent_progress", envir = globalenv())) {
                                                        percent_progress <<- signif((i/length(input))*100, digits = 2)
                                                }

                                                percent_progress <- get("percent_progress", envir = globalenv())

                                                current_percent <- signif((i/length(input))*100, digits = 2)

                                                if (current_percent != percent_progress) {

                                                        percent_progress <<- current_percent
                                                        Sys.sleep(.1)

                                                        if ((percent_progress %% 5) == 0) {
                                                                secretary::typewrite(Sys.time())
                                                                secretary::typewrite(percent_progress, "percent complete.")
                                                                secretary::typewrite(i, "out of", length(input))
                                                        }
                                                }

                                        } else {
                                                secretary::typewrite("COMPLETE.")
                                                rm(percent_progress, envir = globalenv())
                                        }
                                }

                # } else {
                #
                #         if (is.data.frame(input)) {
                #                 case_count <- nrow(input)
                #         } else {
                #                 case_count <- length(input)
                #         }
                #
                #         progress <- signif((i/case_count)*100, digits = 2)
                #         if ((progress %% 5) == 0) {
                #                 secretary::typewrite(progress, "percent complete.")
                #         }
                #
                # }

        }


typewrite_progress <-
        function(i, input) {

                if (is.data.frame(input)) {

                        .data <- input

                        total_iterations <- nrow(.data)

                        if (i != total_iterations) {
                                if (!exists("percent_progress", envir = globalenv())) {
                                        percent_progress <<- signif((i/total_iterations)*100, digits = 2)
                                }

                                percent_progress <- get("percent_progress", envir = globalenv())

                                current_percent <- signif((i/total_iterations)*100, digits = 2)

                                if (current_percent != percent_progress) {

                                        percent_progress <<- current_percent
                                        Sys.sleep(.1)

                                        if ((percent_progress %% 5) == 0) {
                                                cat("\n")
                                                secretary::typewrite(crayon::red(Sys.time()))
                                                secretary::typewrite(crayon::red(current_percent, "percent complete."))
                                                secretary::typewrite(crayon::red(i, "out of", total_iterations))
                                                cat("\n")
                                        }
                                }

                        } else {
                                cat("\n")
                                secretary::typewrite(crayon::red("100% complete."))
                                cat("\n")
                                rm(percent_progress, envir = globalenv())
                        }


                } else {
                        total_iterations <- length(input)

                        if (i != total_iterations) {
                                if (!exists("percent_progress", envir = globalenv())) {
                                        percent_progress <<- signif((i/total_iterations)*100, digits = 2)
                                }

                                percent_progress <- get("percent_progress", envir = globalenv())

                                current_percent <- signif((i/total_iterations)*100, digits = 2)

                                if (current_percent != percent_progress) {

                                        percent_progress <<- current_percent
                                        Sys.sleep(.1)

                                        if ((percent_progress %% 5) == 0) {
                                                cat("\n")
                                                secretary::typewrite(crayon::red(Sys.time()))
                                                secretary::typewrite(crayon::red(percent_progress, "percent complete."))
                                                secretary::typewrite(crayon::red(i, "out of", total_iterations))
                                                cat("\n")

                                                if (!interactive()) {

                                                        joblog <- tibble(Timestmap = Sys.time(),
                                                                         Routine = cave::present_script_path(),
                                                                         Percent = percent_progress,
                                                                         Iteration = i,
                                                                         Total_Iterations = total_iterations)

                                                        if (!file.exists("joblog.csv")) {

                                                                broca::simply_write_csv(x = joblog,
                                                                                        file = "joblog.csv")

                                                        } else {

                                                                readr::write_csv(x = joblog,
                                                                                 path = "joblog.csv",
                                                                                 append = TRUE)
                                                        }
                                                }
                                        }
                                }

                        } else {
                                cat("\n")
                                secretary::typewrite(crayon::red("100% complete."))
                                cat("\n")
                                rm(percent_progress, envir = globalenv())
                        }
                }


        }


typewrite_source_progress <-
        function(input_concept) {

                if (is.data.frame(input)) {

                        if (!exists("net_input", envir = globalenv())) {
                                net_input <<-
                                        input %>%
                                        dplyr::filter_at(vars(!!terminal_col), all_vars(is.na(.))) %>%
                                        dplyr::select(!!source_col) %>%
                                        dplyr::distinct() %>%
                                        unlist() %>%
                                        centipede::no_na()
                        }

                        if (input_concept %in% net_input) {

                        net_df <-
                                tibble(net_input) %>%
                                rowid_to_column("source_progress") %>%
                                # No duplicate concepts to prevent backtracking of the percentage progress
                                group_by(net_input) %>%
                                mutate(count = n()) %>%
                                dplyr::filter(count == 1) %>%
                                ungroup()

                        total_max <- max(net_df$source_progress)

                        current_i <-
                        net_df %>%
                                dplyr::filter(net_input == input_concept) %>%
                                dplyr::select(source_progress) %>%
                                unlist() %>%
                                unname()


                        fraction_completed <- current_i/total_max*100
                        fraction_completed <- signif(fraction_completed, 2)


                                cat("\n")
                                secretary::typewrite(crayon::yellow(Sys.time()))
                                secretary::typewrite(crayon::yellow(fraction_completed, "percent of filtered input complete."))
                                secretary::typewrite(crayon::yellow(current_i, "out of", total_max))
                                cat("\n")
                        }

                }
        }



typewrite_complete <-
        function() {
                secretary::typewrite_bold("Routine complete.")
        }

typewrite_start <-
        function(type) {
                secretary::typewrite("Starting", type, "search.")
                Sys.sleep(1)
                cat("\n")
        }

typewrite_file_exists <-
        function() {
                secretary::typewrite("File exists.")
        }
