####################
## Operational    ##
####################
set_this_wd <-
        function() {
                if ("/Users/patelm9/GitHub/omop_mapping/procedure" != getwd()) {
                        setwd("/Users/patelm9/GitHub/omop_mapping/procedure")
                }
        }

clean_env <-
        function() {
                rm(list = ls(envir = globalenv()), envir = globalenv())
        }


create_path_to_output_fn <-
        function() {
                paste0(stringr::str_replace(path_to_input_fn, "(^.*[/]{1})(.*?)([.]{1}csv$)", "data/output/\\2_"), cave::strip_fn(cave::present_script_path()), ".csv")
        }

brake_if_output_exists <-
        function() {
                path_to_output_fn <<- create_path_to_output_fn()

                if (file.exists(path_to_output_fn)) {

                        secretary::typewrite_warning(path_to_output_fn, "already exists and will be overwritten.")
                        secretary::press_enter()
                }
        }



####################
## Apply Settings ##
####################
# Creates a settings object containing all the variables required by the filter_for_settings() function
make_settings <-
        function(override_vocabularies = NULL,
                 override_concept_classes = NULL,
                 override_domains = NULL,
                 override_standard_concepts = NULL,
                 override_invalid_reasons = NULL) {

                concept_setting_elements <-
                        c(
                                "vocabularies",
                                "concept_classes",
                                "domains",
                                "standard_concepts",
                                "invalid_reasons"
                        )

                if (any(!exists(concept_setting_elements, envir = globalenv()))) {

                        stop("filter settings do not exist in .GlobalEnv")

                }


                global_settings <-
                        concept_setting_elements %>%
                        rubix::map_names_set(get, envir = globalenv())



                fun_settings <-
                        list(
                                "vocabularies" = override_vocabularies,
                                "concept_classes" = override_concept_classes,
                                "domains" = override_domains,
                                "standard_concepts" = override_standard_concepts,
                                "standard_concepts" = override_standard_concepts
                        ) %>%
                        purrr::keep(~!is.null(.))



                final_settings <- c(fun_settings,
                                    global_settings[!(names(global_settings) %in% names(fun_settings))])

                final_settings <- final_settings[concept_setting_elements]

                return(final_settings)

        }


# Filters the resultset of a concept table search for the settings in variables.R
dep_filter_for_settings <-
        function(.data,
                 override_vocabularies = NULL,
                 override_concept_classes = NULL,
                 override_domains = NULL,
                 override_standard_concepts = NULL,
                 override_invalid_reasons = NULL)   {


                settings <-
                make_settings(override_vocabularies = override_vocabularies,
                              override_concept_classes = override_concept_classes,
                                 override_domains = override_domains,
                                 override_standard_concepts = override_standard_concepts,
                                 override_invalid_reasons = override_invalid_reasons)

                if (!is.null(settings$vocabularies)) {

                                .data <-
                                        .data %>%
                                        dplyr::filter(vocabulary_id %in% settings$vocabularies)

                }


                if (!is.null(settings$concept_classes)) {

                        .data <-
                                .data %>%
                                dplyr::filter(concept_class_id %in% settings$concept_classes)

                }

                if (!is.null(settings$domains)) {

                        .data <-
                                .data %>%
                                dplyr::filter(domain_id %in% settings$domains)

                }


                if (!is.null(settings$standard_concepts)) {

                        .data <-
                                .data %>%
                                dplyr::filter(standard_concept %in% settings$standard_concepts)

                }

                if (!is.null(settings$invalid_reasons)) {

                        .data <-
                                .data %>%
                                dplyr::filter(invalid_reason %in% settings$invalid_reasons)

                }

                return(.data)

        }


filter_for_settings <-
        function(.data)   {

                if (!is.null(settings$vocabularies)) {

                        .data <-
                                .data %>%
                                dplyr::filter(vocabulary_id %in% settings$vocabularies)

                }


                if (!is.null(settings$concept_classes)) {

                        .data <-
                                .data %>%
                                dplyr::filter(concept_class_id %in% settings$concept_classes)

                }

                if (!is.null(settings$domains)) {

                        .data <-
                                .data %>%
                                dplyr::filter(domain_id %in% settings$domains)

                }


                if (!is.null(settings$standard_concepts)) {

                        .data <-
                                .data %>%
                                dplyr::filter(standard_concept %in% settings$standard_concepts)

                }

                if (!is.null(settings$invalid_reasons)) {

                        .data <-
                                .data %>%
                                dplyr::filter(invalid_reason %in% settings$invalid_reasons)

                }

                return(.data)

        }

#################################
## Apply More Filters to Input ##
#################################
read_input <-
        function() {

                apply_input_filters <-
                        function(.data) {
                                x <- .data
                                if (exists("additional_filters", envir = globalenv())) {
                                        eval(rlang::parse_expr(paste0("x %>% ", paste(paste0("dplyr::filter(", additional_filters, ")"), collapse = " %>% "))))
                                } else {
                                        x
                                }
                        }



                x <- broca::simply_read_csv(path_to_input_fn,
                                            log_details = "read input") %>%
                        normalize_na() %>%
                        dplyr::mutate_all(as.character)

                x <- apply_input_filters(x)

                cat("\n")

                secretary::typewrite(crayon::bold("Terminal Column:"), terminal_col)

                if (exists("additional_filters", envir = globalenv())) {

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
                        cg_dict <<- broca::simply_read_csv("/Users/patelm9/GitHub/KMI/biblio-tech/DICTIONARY/CancerGov_Drugs/DrugDictionary.csv")
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

lookup_uptodate_dictionary <-
        function(phrase) {
                dictionary <- broca::read_full_excel("/Users/patelm9/GitHub/KMI/biblio-tech/DICTIONARY/UpToDate_Drugs/MASTER.xlsx")

                dictionary$MASTER %>%
                        dplyr::filter_all(function(x) grepl(phrase,
                                                            x,
                                                            ignore.case = TRUE) == TRUE)
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


typewrite_complete <-
        function() {
                secretary::typewrite_bold("Routine complete.")
        }

