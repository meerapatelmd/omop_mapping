
condition_occurrence <-
        fantasia::query_omop("SELECT DISTINCT condition_concept_id,condition_source_value,condition_source_concept_id,source_vocabulary_id FROM omop_cdm_2.condition_occurrence\n;")

device_exposures <-
        fantasia::query_omop("SELECT DISTINCT device_concept_id,device_type_concept_id,device_source_value,device_source_concept_id,source_vocabulary_id FROM omop_cdm_2.device_exposure\n;")

measurements <-
        fantasia::query_omop("SELECT DISTINCT measurement_concept_id,measurement_type_concept_id,measurement_source_value,measurement_source_concept_id FROM omop_cdm_2.measurement\n;")


episode <-
        fantasia::query_omop("SELECT DISTINCT episode_concept_id,episode_object_concept_id,episode_type_concept_id,episode_source_value,episode_source_concept_id FROM omop_cdm_2.episode\n;")

observation <-
fantasia::query_omop("SELECT DISTINCT observation_concept_id,observation_type_concept_id,value_as_concept_id,qualifier_concept_id,unit_concept_id,observation_source_value,observation_source_concept_id,unit_source_value,qualifier_source_value,source_vocabulary_id FROM omop_cdm_2.observation\n;")

procedure_occurrence <-
fantasia::query_omop("SELECT DISTINCT procedure_concept_id,procedure_type_concept_id,procedure_source_value,procedure_source_concept_id,source_vocabulary_id FROM omop_cdm_2.procedure_occurrence\n;")

visit_occurrence <-
fantasia::query_omop("SELECT DISTINCT visit_concept_id,visit_type_concept_id,visit_source_value,visit_source_concept_id FROM omop_cdm_2.visit_occurrence\n;")

specimen <-
fantasia::query_omop("SELECT specimen_concept_id,specimen_type_concept_id,unit_concept_id,anatomic_site_concept_id,disease_status_concept_id,specimen_source_id,specimen_source_value,unit_source_value,anatomic_site_source_value,disease_status_source_value,source_vocabulary_id FROM omop_cdm_2.specimen\n;")

