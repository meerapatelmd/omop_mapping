input <- broca::read_full_excel("~/Memorial Sloan Kettering Cancer Center/ewewge - General/Athena_Mapping/Source of Truth Mapping.xlsx")
input <- input$FINAL_REGIMENS_MAP

output_regimens_a_new <-
        input %>%
        dplyr::select(SourceRegimenId,
                      Regimen) %>%
        rubix::filter_at_grepl(Regimen,
                               grepl_phrase = "NEW",
                               ignore.case = F) %>%
        tidyr::extract(Regimen,
                        into = c("concept_id", "concept_name"),
                        regex = "(NEW) (.*)") %>%
        chariot::merge_concepts(into = "Regimen",
                                label = T)

output_regimens_b_not_new <-
        input %>%
        dplyr::select(SourceRegimenId,
                      Regimen) %>%
        rubix::filter_at_grepl(Regimen,
                               grepl_phrase = "NEW",
                               evaluates_to = F,
                               ignore.case = F) %>%
        chariot::unmerge_concepts(Regimen) %>%
        chariot::merge_concepts(into = "Regimen",
                                label = T)

output_regimens <-
        dplyr::bind_rows(output_regimens_a_new,
                         output_regimens_b_not_new) %>%
        rubix::bring_to_front(SourceRegimenId)

broca::view_as_csv(output_regimens)

# Reload to repeat with components
input <- broca::read_full_excel("~/Memorial Sloan Kettering Cancer Center/ewewge - General/Athena_Mapping/Source of Truth Mapping.xlsx")
input <- input$FINAL_REGIMENS_MAP

output_comp_a_new <-
        input %>%
        dplyr::select(SourceRegimenId,
                      Component) %>%
        tidyr::separate_rows(Component,
                             sep = "\n") %>%
        rubix::filter_at_grepl(Component,
                               grepl_phrase = "NEW",
                               ignore.case = F) %>%
        tidyr::extract(Component,
                       into = c("concept_id", "concept_name"),
                       regex = "(NEW) (.*)") %>%
        chariot::merge_concepts(into = "Component",
                                label = T)

output_comp_b_not_new <-
        input %>%
        dplyr::select(SourceRegimenId,
                      Component) %>%
        tidyr::separate_rows(Component,
                             sep = "\n") %>%
        rubix::filter_at_grepl(Component,
                               grepl_phrase = "NEW",
                               evaluates_to = F,
                               ignore.case = F) %>%
        chariot::unmerge_concepts(Component) %>%
        chariot::merge_concepts(into = "Component",
                                label = T)


output_comp2 <-
        dplyr::bind_rows(output_comp_a_new,
                         output_comp_b_not_new) %>%
        group_by(SourceRegimenId) %>%
        summarize_at(vars(concept_id, Component), function(x) paste(x, collapse = "\n")) %>%
        ungroup()

broca::view_as_csv(output_comp2)
