# Mapping components and using the same components found in Regimens
regimen_components <- broca::read_full_excel("~/Memorial Sloan Kettering Cancer Center/ewewge - General/Athena_Mapping/Source of Truth Mapping.xlsx")
regimen_components <- regimen_components$FINAL_REGIMENS_MAP


colnames(regimen_components) <-
        regimen_components %>%
        rubix::filter_first_row() %>%
        unlist()

regimen_components2 <-
        regimen_components %>%
        rubix::filter_first_row(invert = TRUE)

regimen_components3 <-
        regimen_components2 %>%
        dplyr::select(Component) %>%
        tidyr::separate_rows(Component,
                             sep = "\n") %>%
        mutate_all(trimws) %>%
        unique() %>%
        tidyr::extract(Component,
                       into = c("component_id", "component_name"),
                       regex = "(^.*?) (.*$)",
                       remove = FALSE) %>%
        arrange(component_name)

broca::view_as_csv(regimen_components3)
