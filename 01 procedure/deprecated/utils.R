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


filter_for_settings <-
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

normalize_na <-
        function(.data) {
                .data %>%
                        dplyr::mutate_all(stringr::str_replace_all, "^NA$", "") %>%
                        dplyr::mutate_all(na_if, "")
        }

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


apply_input_filters <-
        function(.data) {
                x <- .data
                if (exists("additional_filters", envir = globalenv())) {
                        eval(rlang::parse_expr(paste0("x %>% ", paste(paste0("dplyr::filter(", additional_filters, ")"), collapse = " %>% "))))
                } else {
                        x
                }
        }

read_input <-
        function() {

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


filter_max_250 <-
        function(.data) {
                .data %>%
                        dplyr::slice(1:250)

        }

filter_max_n <-
        function(.data, n) {
                .data %>%
                        dplyr::slice(1:n)
        }



clean_env <-
        function() {
                rm(list = ls(envir = globalenv()), envir = globalenv())
        }


create_path_to_output_fn <-
        function() {
                paste0(stringr::str_replace(path_to_input_fn, "(^.*?[/]{1})(.*?)([.]{1}csv$)", "output/\\2_"), cave::strip_fn(cave::present_script_path()), ".csv")
        }



brake_if_output_exists <-
        function() {
                path_to_output_fn <<- create_path_to_output_fn()

                if (file.exists(path_to_output_fn)) {

                        secretary::typewrite_warning(path_to_output_fn, "already exists and will be overwritten.")
                        secretary::press_enter()
                }
        }


create_types_output_object <-
        function() {
                final_output <- list()
                for (i in 1:length(types)) {
                        final_output[[i]] <- tibble()
                }
                names(final_output) <- types

                return(final_output)
        }



query_phrase_in_athena <-
        function(phrase, type, remove_regex = "[']{1}|[?]{1}$", n = 250) {

                        secretary::typewrite(crayon::bold("Raw:"), phrase)

                        phrase <- stringr::str_remove_all(phrase, pattern = remove_regex)

                        secretary::typewrite(crayon::bold("Modified:"), phrase)

                        chariot::query_phrase(phrase = phrase,
                                              type = type) %>%
                        filter_for_settings() %>%
                        rubix::arrange_by_nchar(concept_name) %>%
                        filter_max_n(n = n)

        }


typewrite_percent_progress <-
        function(i, input) {

                if (interactive()) {

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
                                                                secretary::typewrite(current_percent, "percent complete.")
                                                                secretary::typewrite(i, "out of", length(input))
                                                        }
                                                }

                                        } else {
                                                secretary::typewrite("100% complete.")
                                                rm(percent_progress, envir = globalenv())
                                        }
                                }

                } else {

                        if (is.data.frame(input)) {
                                case_count <- nrow(input)
                        } else {
                                case_count <- length(input)
                        }

                        progress <- signif((i/case_count)*100, digits = 2)
                        if ((progress %% 5) == 0) {
                                secretary::typewrite(progress, "percent complete.")
                        }

                }

        }


typewrite_complete <-
        function() {
                secretary::typewrite_bold("Routine complete.")
        }

