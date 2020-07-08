rm(list = ls())
source("setup.R")


input <- read_terminal_workfile()


# Converting to true NA
input <-
        input %>%
        dplyr::mutate_all(stringr::str_remove_all, "[\r\n\t]") %>%
        dplyr::mutate_all(stringr::str_replace_na) %>%
        dplyr::mutate_all(stringr::str_replace_all, "^NA$", NA_character_) %>%
        dplyr::mutate_all(stringr::str_replace_all, "^$", NA_character_)


output_a <-
        split(input,
              input$`MSK Concept Type`)

# Do all Facts have a single Observation Group?
facts_missing_a_observation_group <-
setdiff(
output_a$Fact %>%
        dplyr::select(FORM, VARIABLE, TYPE, CONCEPT) %>%
        dplyr::distinct(),
output_a$`Observation Group` %>%
        dplyr::select(FORM, VARIABLE, TYPE, CONCEPT) %>%
        dplyr::distinct())

facts_missing_a_observation_group2 <-
        output_a$Fact %>%
        dplyr::right_join(facts_missing_a_observation_group) %>%
        dplyr::distinct()

# If there are Observation Groups missing for a fact, copy to clipboard and make a new MAP
if (nrow(facts_missing_a_observation_group2)) {
        stop("There are observation groups missing.")
        broca::copy_to_clipboard(facts_missing_a_observation_group2)
}


# Do all Observation Group rows have a corresponding Fact?
observation_group_missing_a_fact <-
        setdiff(
                output_a$`Observation Group` %>%
                        dplyr::select(FORM, VARIABLE, TYPE, CONCEPT) %>%
                        dplyr::distinct(),
                output_a$Fact %>%
                        dplyr::select(FORM, VARIABLE, TYPE, CONCEPT) %>%
                        dplyr::distinct())

observation_group_missing_a_fact2 <-
        output_a$`Observation Group` %>%
        dplyr::right_join(observation_group_missing_a_fact) %>%
        dplyr::distinct()

# If there are Observation Groups missing for a fact, copy to clipboard and make a new MAP
if (nrow(observation_group_missing_a_fact2)) {
        stop("There are Facts missing.")
        broca::view_as_csv(observation_group_missing_a_fact2)

}

# Getting counts of facts per concept
facts <- output_a$Fact
facts2 <-
        facts %>%
        group_by(FORM,
                 VARIABLE,
                 TYPE,
                 CONCEPT) %>%
        dplyr::mutate(distinct_fact_count = length(unique(`MSK Concept`)),
                         na_fact_count = length(`MSK Concept`[is.na(`MSK Concept`)]),
                         new_fact_count = length(grep("^NEW",`MSK Concept`, value = TRUE))) %>%
        dplyr::ungroup()


broca::view_as_csv(dplyr::bind_rows(facts2,
                                    output_a$`Observation Group`,
                                    output_a$Attribute,
                                    output_a$Modifier)
)


