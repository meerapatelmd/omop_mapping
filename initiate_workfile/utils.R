workfile_colnames <-
        c(
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


load_newest_inventory <-
        function() {
                broca::simply_read_csv(path_to_input_existing_map_fn,
                                       log_details = "load for script execution")
        }