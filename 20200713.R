vasopressors <-
c("Norepinephrine",
  "Phenylephrine",
  "Epinephrine",
  "Ephedrine",
  "Dopamine",
  "Dobutamine",
  "Isoproterenol",
  "Vasopressin (usp)",
  "Milrinone",
  "Angiotensin II")

vasopressor_concept_ids <-
        vasopressors %>%
        rubix::map_names_set(function(x) chariot::query_phrase(x,
                                                               type = "exact",
                                                               where_col = "vocabulary_id",
                                                               where_col_in = c("RxNorm", "RxNorm Extension")))

qa <-
        vasopressor_concept_ids %>%
        purrr::keep(~nrow(.) != 1)

vasopressor_concepts <-
        vasopressor_concept_ids %>%
        dplyr::bind_rows(.id = "Vasopressor") %>%
        chariot::merge_concepts(into = "IngredientConcept") %>%
        dplyr::rename(ingredient_concept_id = concept_id)

vasopressor_concepts_relatives <-
        chariot::left_join_relatives(vasopressor_concepts %>%
                                             dplyr::select(ingredient_concept_id))

vasopressor_concepts_relatives2 <-
vasopressor_concepts_relatives %>%
        rubix::filter_at_grepl(relative_vocabulary_id,
                               grepl_phrase = "rxnorm")

test <-
fantasia::left_join_df_to_omop(vasopressor_concepts_relatives2 %>%
                                       dplyr::select(relative_concept_id),
                               omop_table = "omop_cdm_2.drug_exposure",
                               omop_column = "drug_concept_id")


pressor_drug_exposures <-
        test %>%
        rubix::normalize_all_to_na() %>%
        dplyr::filter(!is.na(drug_exposure_id)) %>%
        dplyr::select(drug_concept_id) %>%
        dplyr::distinct()

pressor_concept <-
        chariot::left_join_concept(pressor_drug_exposures %>%
                                           dplyr::mutate_all(as.integer)) %>%
        chariot::merge_concepts(into = "DrugExposure") %>%
        dplyr::select(drug_exposure_concept_id = concept_id,
                      DrugExposure)


pressor_drug_exposures_relationship <-
        chariot::pivot_relationship_id(pressor_concept,
                                        "drug_exposure_concept_id") %>%
        rubix::filter_at_grepl(`RxNorm has dose form`,
                               grepl_phrase = "rectal|nasal|oral|Ophthalmic|Topical|Pack",
                               evaluates_to = FALSE)  %>%
        rubix::filter_at_grepl(`Constitutes`,
                               grepl_phrase = "rectal|nasal|oral|Ophthalmic|Topical",

pressor_drug_exposures2 <-
        dplyr::inner_join(pressor_concept,
                         pressor_drug_exposures_relationship,
                         by = c("drug_exposure_concept_id" = "concept_id_1"))

####
input2 <-
        chariot::left_join_concept(input) %>%
        dplyr::select(concept_id, concept_name)

input2$concept_id %>%
        purrr::map2(input2$concept_name,
                    function(x,y) paste0(x, ", --", y, "\n")) %>%
        unlist() %>%
        cat()

test <- fantasia::query_omop(read_file("~/GitHub/KMI/redcap-queries/ccc19-sepsis-pressors.sql"))
