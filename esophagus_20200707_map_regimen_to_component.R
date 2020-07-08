# Current mappings are either by component or by regimen and they are being reconciled for a complete representation
input <- broca::read_full_excel("~/Memorial Sloan Kettering Cancer Center/Esophagogastric REDCap Standardization - KMI Only - KMI Only/2020-07-10 Meeting/Esophagus_Analysis.xlsx")
input <- input$MAP_17
input_map <- input %>%
        dplyr::select(routine_id,
                      Regimen)

# Regimens are 1 of 3 classes: a) blank (because groups of components are mapped instead), b) NEW {Preferred Label} that is in a parseable format to derive expected HemOnc components, c) OMOP concept from which the components can be derived using HemOnc. We are focusing on b and c here.
input_a <-
        input_map %>%
        rubix::filter_at_grepl(Regimen,
                               grepl_phrase = "NEW ",
                               evaluates_to = TRUE,
                               ignore.case = FALSE)


output_a <-
        input_a %>%
        tidyr::extract(col = Regimen,
                       into = c("new_concept_id", "new_concept_name"),
                       regex = "(NEW )(.*$)") %>%
        tidyr::separate(col = new_concept_name,
                        into = paste0("Component_", 1:(3+max(centipede::nchar_comma(input_a$Regimen)))),
                        sep = "[,]{1} | and | monotherapy")

# Isolate the component names for an exact string match at the component level
output_a2 <-
        output_a %>%
        # Melt all components into a longer pivot for a left join
        tidyr::pivot_longer(cols = starts_with("Component_"),
                            names_to = "Component_Index",
                            values_to = "Component",
                            values_drop_na = TRUE) %>%
        dplyr::mutate_at(vars(starts_with("Component")), trimws) %>%
        dplyr::mutate_at(vars(starts_with("Component")), na_if, "") %>%
        dplyr::mutate_at(vars(starts_with("Component")), stringr::str_replace_na)


output_a2_concept_ids <-
        output_a2$Component %>%
        rubix::map_names_set(function(x) chariot::query_phrase(x,
                                                               type = "exact",
                                                               where_col = "vocabulary_id",
                                                               where_col_in = "HemOnc") %>%
                                     chariot::merge_concepts(into = "ComponentConcept")) %>%
        purrr::keep(~nrow(.)>0) %>%
        dplyr::bind_rows(.id = "Component") %>%
        dplyr::distinct() %>%
        dplyr::rename(component_concept_id = concept_id)

output_a3 <-
        dplyr::left_join(output_a2,
                         output_a2_concept_ids)

output_a4 <-
        output_a3 %>%
        rubix::group_by_aggregate(routine_id,
                                  agg.col = "ComponentConcept",
                                  collapse = "\n")

final_output_a <-
        dplyr::left_join(input,
                         output_a4)

# QA
nrow(input_map) == nrow(final_output_a)
length(unique(input_map$routine_id)) == == length(unique(final_output_a$routine_id))
broca::view_as_csv(final_output_a)



# Update newest map to do part B
input <- broca::read_full_excel("~/Memorial Sloan Kettering Cancer Center/Esophagogastric REDCap Standardization - KMI Only - KMI Only/2020-07-10 Meeting/Esophagus_Analysis.xlsx")
input <- input$MAP_18
input_map <- input %>%
        dplyr::select(routine_id,
                      Regimen)



input_b <-
        input_map %>%
        rubix::filter_at_grepl(Regimen,
                               grepl_phrase = "NEW ",
                               evaluates_to = FALSE,
                               ignore.case = FALSE) %>%
        filter_at(vars(Regimen), ~!is.na(.))



input_b_regimen_concept_ids <-
        input_b %>%
        chariot::unmerge_concepts(Regimen, remove = FALSE)

output_b <-
        chariot::pivot_relationship_id(input_b_regimen_concept_ids  %>%
                                                dplyr::select(regimen_concept_id = concept_id) %>%
                                                dplyr::mutate_all(as.integer)) %>%
        dplyr::select(
                      concept_id_1,
                      starts_with("Has antineoplastic"))

output_b_concept <-
        output_b %>%
        dplyr::select(regimen_concept_id = concept_id_1) %>%
        dplyr::mutate_all(as.integer) %>%
        chariot::left_join_concept(include_synonyms = FALSE) %>%
        chariot::merge_concepts(into = "Regimen") %>%
        dplyr::select(-concept_id) %>%
        dplyr::mutate_all(as.character)


output_b2 <-
        dplyr::left_join(output_b_concept,
                         output_b %>%
                                 dplyr::mutate_all(as.character),
                         by = c("regimen_concept_id" = "concept_id_1")) %>%
        dplyr::distinct()

output_b3 <-
        input_b_regimen_concept_ids %>%
        dplyr::select(routine_id,
                      regimen_concept_id = concept_id) %>%
        dplyr::left_join(output_b2)


final_output_b <-
        dplyr::left_join(input,
                         output_b3,
                         by = "routine_id")

# QA
nrow(input_map) == nrow(final_output_b)
length(unique(input_map$routine_id)) == length(unique(final_output_b$routine_id))
broca::view_as_csv(final_output_b)
