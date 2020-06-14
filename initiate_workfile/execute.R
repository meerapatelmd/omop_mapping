# Joining existing mappings to input
existing_map <- load_newest_inventory()


output <-
        dplyr::left_join(parsed_dd,
                         existing_map,
                         by = c("FORM",
                                "VARIABLE",
                                "PV",
                                "PV_CODE",
                                "TYPE",
                                "CONCEPT"),
                         suffix = c(".inventory", ".exists")) %>%
        dplyr::mutate(BRANCHING_LOGIC = coalesce(BRANCHING_LOGIC.inventory,
                                                 BRANCHING_LOGIC.exists),
                      CHOICES = coalesce(CHOICES.inventory,
                                         CHOICES.exists),
                      CUSTOM = coalesce(CUSTOM.inventory,
                                        CUSTOM.exists),
                      FIELD_ANNOTATION = coalesce(FIELD_ANNOTATION.inventory,
                                                  FIELD_ANNOTATION.exists),
                      FIELD_LABEL = coalesce(FIELD_LABEL.inventory,
                                             FIELD_LABEL.exists),
                      FIELD_NOTE = coalesce(FIELD_NOTE.inventory,
                                            FIELD_NOTE.exists),
                      FIELD_TYPE = coalesce(FIELD_TYPE.inventory,
                                            FIELD_TYPE.exists),
                      IDENTIFIER = coalesce(IDENTIFIER.inventory,
                                            IDENTIFIER.exists),
                      MATRIX_GROUP = coalesce(MATRIX_GROUP.inventory,
                                              MATRIX_GROUP.exists),
                      MATRIX_RANK = coalesce(MATRIX_RANK.inventory,
                                             MATRIX_RANK.exists),
                      PV_LITERAL = coalesce(PV_LITERAL.inventory,
                                            PV_LITERAL.exists),
                      QUESTION = coalesce(QUESTION.inventory,
                                          QUESTION.exists),
                      REQUIRED = coalesce(REQUIRED.inventory,
                                          REQUIRED.exists),
                      SECTION = coalesce(SECTION.inventory,
                                         SECTION.exists),
                      TEXT_VALIDATION_MAX = coalesce(TEXT_VALIDATION_MAX.inventory,
                                                     TEXT_VALIDATION_MAX.exists),
                      TEXT_VALIDATION_MIN = coalesce(TEXT_VALIDATION_MIN.inventory,
                                                     TEXT_VALIDATION_MIN.exists),
                      TEXT_VALIDATION = coalesce(TEXT_VALIDATION.inventory,
                                                 TEXT_VALIDATION.exists)) %>%
        #dplyr::mutate(`MSK Concept Type` = coalesce(`MSK Concept Type.exists`, `MSK Concept Type.inventory`),
        #              `MSK Concept` = coalesce(`MSK Concept.exists`, `MSK Concept.inventory`)) %>%
        dplyr::select(-ends_with(".inventory"), -ends_with(".exists")) %>%
        dplyr::distinct() %>%
        #dplyr::select(-routine_id,
        #              -rowid) %>%
        dplyr::distinct() %>%
        rowid_to_column("routine_id") %>%
        dplyr::select(all_of(workfile_colnames), everything()) %>%
        dplyr::mutate_all(stringr::str_remove_all, "\"")

broca::copy_to_clipboard(output) #Copied to a MAP_00 Tab
