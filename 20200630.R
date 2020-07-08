input <- broca::read_clipboard()
input2 <-
        input %>%
        dplyr::select(choice_concept_id,
                      relative_drug_concept_id) %>%
        rubix::group_by_unique_aggregate(choice_concept_id,
                                         agg.col = relative_drug_concept_id)

#input <- broca::read_clipboard()
input2 <-
        input %>%
        tidyr::separate_rows(Ingredient,
                             sep = "\n") %>%
        chariot::unmerge_concepts(concept_col = Ingredient) %>%
        dplyr::select(choice_concept_id,
                      ingredient_concept_id = concept_id) %>%
        rubix::call_mr_clean()



device_exposure_sample <-
        fantasia::query_omop("SELECT * FROM omop_cdm_2.device_exposure WHERE device_concept_id = 4052536 LIMIT 1")

fantasia::query_omop("SELECT * FROM omop_cdm_2.observation WHERE value_as_concept_id = 4052536 LIMIT 1")

conn <- fantasia::connect_to_omop(schema = "omop_cdm_2")
cdm_tables <-
DatabaseConnector::dbListTables(conn = conn,
                                schema = "omop_cdm_2")
cdm_tables2 <- cdm_tables
output <- tibble()
while (length(cdm_tables2) > 0) {

                cdm_table <- cdm_tables2[1]

                cdm_table_fields <-
                DatabaseConnector::dbListFields(conn = conn,
                                                name = cdm_table)


                cdm_table_cid_fields <-
                        grep("concept_id", cdm_table_fields, ignore.case = TRUE,
                             value = TRUE)


                while (length(cdm_table_cid_fields) > 0) {


                                cdm_table_cid_field <- cdm_table_cid_fields[1]


                                sql <- paste0("SELECT * FROM ", cdm_table, " WHERE ", cdm_table_cid_field, " = ", concept_id, " LIMIT 1")

                                resultset <-
                                        fantasia::query_omop(sql_statement = sql,
                                                             schema = "omop_cdm_2")


                                if (nrow(resultset) > 0) {

                                                output <-
                                                        dplyr::bind_rows(output,
                                                                         tibble(Table = cdm_table,
                                                                                Field = cdm_table_cid_field))

                                }


                                cdm_table_cid_fields <- cdm_table_cid_fields[-1]


                }


                #secretary::press_enter()



                cdm_tables2 <- cdm_tables2[-1]


}
output %>%
        dplyr::distinct()

output <-
        input2 %>%
        chariot::left_join_all_relatives(id_column = "ingredient_concept_id")


drug_concept_ids <-
        fantasia::query_omop("SELECT DISTINCT drug_concept_id FROM omop_cdm_2.drug_exposure") %>%
        mutate_all(as.character)


output2 <-
        dplyr::inner_join(output,
                          drug_concept_ids,
                          by = c("relative_concept_id" = "drug_concept_id"))


final_output <-
        output2 %>%
        rubix::group_by_unique_aggregate(ingredient_concept_id,
                                         agg.col = relative_concept_id,
                                         collapse = "|")

output2_a <-
        output %>%
        dplyr::select(ingredient_concept_id,
                      relative_concept_id)

final_output <-
fantasia::left_join_df_to_omop(output2_a,
                               dataframe_column = "relative_concept_id",
                               omop_table = "omop_cdm_2.drug_exposure",
                               omop_column = "drug_concept_id")


input3 <-
        chariot::left_join_all_relatives(input2) %>%


output <-
        input3 %>%
        rubix::rename_all_remove("^relative_") %>%
        chariot::merge_concepts(into = "RelativeConcept",
                                concept_class_id) %>%
        select(choice_concept_id, concept_class_id, RelativeConcept) %>%
        tidyr::pivot_wider(id_cols = choice_concept_id,
                           names_from = concept_class_id,
                           values_from = RelativeConcept,
                           values_fn = list(RelativeConcept = function(x) paste(unique(x)[1:250] %>% centipede::no_na(), collapse = "\n"))) %>%
        mutate_all(substring, 1, 20000)

broca::view_as_csv(
chariot::left_join_relationship(input2)
)
broca::view_as_csv(
        chariot::pivot_wider_by_relationship_concept_class(input2)
)


output <-
        chariot::left_join_df_to_concept(input2 %>%
                                                 dplyr::select(index),
                                         concept_column = "concept_code",
                                         include_synonyms = F) %>%
        dplyr::distinct()

broca::view_as_csv(output)
