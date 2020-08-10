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
