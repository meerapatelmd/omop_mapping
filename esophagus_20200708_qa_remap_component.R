# Current mappings are either by component or by regimen and they are being reconciled for a complete representation
input <- broca::read_full_excel("~/Memorial Sloan Kettering Cancer Center/Esophagogastric REDCap Standardization - KMI Only - KMI Only/2020-07-10 Meeting/Esophagus_Analysis.xlsx")
input <- input$MAP_24
input_map <- input %>%
        dplyr::select(routine_id,
                      Component)


#Melt Component
output <-
        input_map %>%
        tidyr::separate_rows(Component,
                             sep = "\n") %>%
        # Removing all trailing "\r"
        mutate_all(trimws) %>%
        # Replace blanks created by the trim with NA to filter out
        mutate_at(vars(Component),
                  stringr::str_replace_all, "^$|^NA$", NA_character_) %>%
        # Remove NA components
        dplyr::filter(!is.na(Component))

# Getting concept_id to join to concept table to retrieve concept_class for qa
output2 <-
        output %>%
        chariot::unmerge_concepts(Component, remove = FALSE)

# QA: have all Components successfully unmerge?
qa <-
output2 %>%
        dplyr::select(concept_id, Component) %>%
        dplyr::filter(is.na(concept_id), !is.na(Component)) %>%
        rubix::filter_at_grepl(Component,
                               grepl_phrase = "NEW ",
                               evaluates_to = FALSE)

if (nrow(qa)) {

        stop('Component column failed to fully unmerge')

}


output3 <-
        output2 %>%
        #Remove all NEW concepts because will not join with concept column and cause NA results downstream
        rubix::filter_at_grepl(Component,
                               grepl_phrase = "NEW ",
                               evaluates_to = FALSE) %>%
        dplyr::select(component_concept_id = concept_id,
                      routine_id)

output3_concept <-
        chariot::left_join_concept(output3 %>%
                                           dplyr::select(component_concept_id) %>%
                                           dplyr::mutate_all(as.integer),
                                   include_synonyms = FALSE) %>%
        # selecting concept_id and concept_class_id for join
        dplyr::select(component_concept_id = concept_id,
                      component_concept_class_id = concept_class_id) %>%
        dplyr::distinct()

output4 <- list(output3,
                output3_concept)

output5 <-
        output4 %>%
        purrr::map(function(x) x %>%
                                dplyr::mutate_at(vars(contains("concept_id")),
                                                 as.integer))

output6 <-
        output5 %>%
        purrr::reduce(left_join) %>%
        dplyr::distinct()

#Getting source components that are not actually of either the Component or Regimen concept class
output7 <-
output6 %>%
        dplyr::filter(!(component_concept_class_id %in% c("Component", "Regimen")))

#Getting the related Component for these mismatched classes (ie Brand Name class)
output7_relationships <-
        chariot::pivot_concept2(output7 %>%
                                               dplyr::select(component_concept_id),
                                names_from = "concept_class_id")

# QA: are there Component classes associated with the input?
qa2 <- !("Component" %in% colnames(output7_relationships))

if (qa2) {

        stop("No relationships with Component class found")
}

# Does each input Component have exactly 1 Component it is related to?
qa3 <-
        output8_relationships %>%
        dplyr::filter(`Component Count` != 1)

if (nrow(qa3)) {

        stop("Component relationships not exactly one-to-one")

}

# Joining the Component relationship
output8 <-
        dplyr::left_join(output7,
                         output7_relationships,
                         by = c("component_concept_id" = "concept_id_1")) %>%
        dplyr::distinct()


# Selecting add-on columns
output9 <-
        output8 %>%
        dplyr::select(routine_id,
                      CorrectComponent = Component) %>%
        dplyr::distinct()

# Aggregating to "\n"-separated column For routine_ids that have >1 CorrectComponents
output10 <-
        output9 %>%
        rubix::group_by_unique_aggregate(routine_id,
                                         agg.col = CorrectComponent,
                                         collapse = "\n")
final_output <-
        dplyr::left_join(input,
                         output10)

nrow(final_output) == nrow(input)
all(unique(final_output$routine_id) %in% unique(input$routine_id))

broca::view_as_csv(final_output)
