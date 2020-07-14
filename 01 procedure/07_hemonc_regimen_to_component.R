# Derive HemOnc components from Regimens
# Current mappings are either by component or by regimen and they are being reconciled for a complete representation
input <- read_input()
path_to_output_fn <- create_path_to_output_fn()
brake_if_output_exists()

input_map <- input %>%
        dplyr::filter_at(vars(Component), any_vars(is.na(.))) %>%
        dplyr::filter_at(vars(Regimen), any_vars(!is.na(.))) %>%
        #Remove NEW without preferred label
        dplyr::filter_at(vars(Regimen), any_vars(!(. %in% "NEW"))) %>%
        dplyr::select(routine_id,
                      Regimen)

# Regimens are 1 of 3 classes: a) blank (because groups of components are mapped instead), b) NEW {Preferred Label} that is in a parseable format to derive expected HemOnc components, c) OMOP concept from which the components can be derived using HemOnc. We are focusing on b and c here.

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
                         by = "routine_id") %>%
        rename(Regimen = Regimen.x,
               NewRegimen2 = Regimen.y,
               NewRegimen2HasAntineoplastic = `Has antineoplastic`,
               NewRegimen2HasAntineoplasticCount = `Has antineoplastic Count`)

# QA
qa3 <- nrow(input) != nrow(final_output_b)
if (qa3) {

        stop("Row counts between input and final output do not match")
}

qa4 <- length(unique(input$routine_id)) != length(unique(final_output_b$routine_id))
if (qa4) {

        stop("Final output is missing routine_ids")

}


broca::simply_write_csv(final_output_b,
                        file = path_to_output_fn)

