common_fields <-
        c(
        'FORM',
        'VARIABLE',
        'FIELD_TYPE',
        'FIELD_LABEL',
        'CHOICES',
        'FIELD_NOTE')



rename_field_to_changed_field <-
        function(.data) {

                if ("Field" %in% colnames(.data)) {
                        .data %>%
                                dplyr::rename(`Changed Field` = Field)
                } else {
                        .data
                }
        }