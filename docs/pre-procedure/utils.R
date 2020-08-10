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


read_workfile <-
        function(routine) {

                x <- broca::simply_read_csv(path_to_input_fn,
                                       log_details = routine)

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


read_existing_mappings <-
        function(routine) {

                x <- broca::simply_read_csv(path_to_input_existing_map_fn,
                                            log_details = routine)

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


load_newest_inventory <-
        function() {
                broca::simply_read_csv(path_to_input_existing_map_fn,
                                       log_details = "load for script execution")
        }
