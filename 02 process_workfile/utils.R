workfile_colnames <-
c('routine_id',
'FORM',
'VARIABLE',
'SECTION',
'FIELD_TYPE',
'FIELD_LABEL',
'CHOICES',
'FIELD_NOTE',
'TEXT_VALIDATION',
'TEXT_VALIDATION_MIN',
'TEXT_VALIDATION_MAX',
'IDENTIFIER',
'BRANCHING_LOGIC',
'REQUIRED',
'CUSTOM',
'QUESTION',
'MATRIX_GROUP',
'MATRIX_RANK',
'FIELD_ANNOTATION',
'PV_CODE',
'PV_LITERAL',
'PV',
'TYPE',
'CONCEPT')

read_terminal_workfile <-
        function(terminal_col) {

                terminal_col <- enquo(terminal_col)

                x <- broca::simply_read_csv(path_to_input_fn)

                if (!is.null(filter_for_form)) {
                        x <- x %>%
                                rubix::filter_for_vector(filter_col = FORM,
                                                         inclusion_vector = filter_for_form)
                }

                if (nrow(x) == 0) {
                        stop("no data")
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

read_file_to_correct <-
        function() {

                broca::simply_read_csv(path_to_input_fn)

        }


unmapped_count <-
        function(vector) {

                length(vector[is.na(vector)])

        }


mapped_count <-
        function(vector) {

                length(vector[!is.na(vector)])

        }
