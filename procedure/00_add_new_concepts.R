origin <- broca::read_full_excel(origin_fn)
origin <- origin[[origin_tab]]


new_concepts <- broca::read_full_excel("~/Memorial Sloan Kettering Cancer Center/KM COVID - General/Mappings/COVID_Parsed_2020-05-27.xlsx")
new_concepts <- new_concepts$PARSED_DD

new_concepts <-
        new_concepts %>%
        dplyr::select(FORM, VARIABLE, FIELD_TYPE, FIELD_LABEL, CHOICES, PV, PV_CODE, CORE_VARIABLES, TYPE, CONCEPT)


new_concepts2 <-
        new_concepts %>%
        dplyr::left_join(origin %>%
                                 dplyr::select(-STANDARD_LIBRARY,
                                               -routine_id,
                                               -total,
                                               -mapped)) %>%
        rowid_to_column("routine_id")

broca::view_as_csv(new_concepts2)

origin_concepts <-
        origin %>%
        dplyr::select(all_of(colnames(new_concepts)))


all_concepts <-
        dplyr::bind_rows(origin_concepts,
                         new_concepts) %>%
        dplyr::distinct()


all_concepts2 <-
        dplyr::left_join(all_concepts,
                         origin) %>%
        dplyr::select(-routine_id, -FIELD_NOTE, -total, -mapped) %>%
        dplyr::distinct() %>%
        tibble::rowid_to_column("routine_id")


broca::view_as_csv(all_concepts2)
