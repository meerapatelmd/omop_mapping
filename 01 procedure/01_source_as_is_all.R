library(tidyverse)
# Source function and vars
if (interactive()) {
                source("startup.R")
}

# Set search parameters
type <- "all"

# Create output variables
outputPath <- createOutputPath()


##############################################
### INTRODUCTION
##############################################

# Temporary stop if the output file exists
if (interactive()) {

        brake_if_output_exists()

}

##############################################
### PRE-PROCEDURE
##############################################
# Read input
input <- read_input()
target_col <- source_col


# Create final input object to join with final output object before export
final_input <- input

# Select only relevant columns
input2 <-
        input %>%
        dplyr::select(routine_id, !!target_col, !!terminal_col) %>%
        dplyr::filter_at(vars(!!target_col), any_vars(!is.na(.))) %>%
        dplyr::filter_at(vars(!!terminal_col), any_vars(is.na(.)))


# If any routine_id is NA
qa1 <-  input2$routine_id[is.na(input2$routine_id)]

if (length(qa1) > 0) {

        stop("routine_ids present in the input are missing in input3")

}

##############################################
### PROCEDURE
##############################################
# Create output
output <- list()

# Notify start
typewrite_start(type = type)

input3 <- input2


pb <- progress::progress_bar$new(total = nrow(input3),
                                 format = "[:bar] :elapsedfull :current/:total (:percent)",
                                 clear = FALSE)

pb$tick(0)
Sys.sleep(0.2)

# For each concept
for (i in 1:nrow(input3)) {

                        # Isolate concept
                        input_row <- input3 %>%
                                        dplyr::filter(row_number() == i)


                        pb$tick()
                        Sys.sleep(0.2)


                        # Convert concept info into objects
                        rubix::release_df(input_row)

                        input_concept <- get(target_col)
                        output_concept <- get(terminal_col)
                        input_routine_id <- routine_id


                        # Search if input concept is above the character number threshold
                        if (nchar(input_concept) > source_skip_nchar) {


                                output[[i]] <-
                                        chariot::queryPhrase(schema = "public",
                                                             phrase = input_concept,
                                                             split = " |[[:punct:]]") %>%
                                        purrr::map(chariot::filterSettings)

                                names(output)[[i]] <- input_routine_id

                        }
                        rm(list = colnames(input_row))
                        rm(input_row)
}

output2 <-
output %>%
        purrr::map(function(x) lapply(x, nrow)) %>%
        purrr::map(function(x) lapply(x, rubix::vector_to_tibble, new_col = "SearchTypeRowCount")) %>%
        purrr::map(function(x) dplyr::bind_rows(x, .id = "SearchType")) %>%
        purrr::map(function(x) x %>%
                                tidyr::unite(col = SearchResults,
                                             dplyr::everything(),
                                             sep = " = ")) %>%
        dplyr::bind_rows(.id = "routine_id") %>%
        rubix::group_by_aggregate(routine_id,
                                  agg.col = SearchResults,
                                  collapse = "\n") %>%
        dplyr::mutate(SearchResultsSettings = paste(unlist(.Settings),
                                                    collapse = "\n"))

output2b <-
        output %>%
        purrr::transpose() %>%
        purrr::map(dplyr::bind_rows, .id = "routine_id") %>%
        purrr::map(function(x) chariot::mergeStrip(x, into = "Concept")) %>%
        purrr::keep(~nrow(.) >0) %>%
        dplyr::bind_rows(.id = "SearchType") %>%
        dplyr::rename(concept_synonym_id = Concept_id) %>%
        chariot::leftJoinConcept(column = "concept_synonym_id") %>%
        dplyr::select(-concept_synonym_id, -concept_name) %>%
        dplyr::mutate(concept_name = concept_synonym_name) %>%
        chariot::mergeStrip(into = "concept_synonym") %>%
        dplyr::mutate(concept_synonym = ifelse(is.na(concept_synonym_name), NA, concept_synonym)) %>%
        dplyr::mutate(concept = coalesce(concept_synonym, concept)) %>%
        dplyr::select(routine_id,
                      searchtype,
                      concept) %>%
        dplyr::group_by(routine_id) %>%
        dplyr::slice(1:250) %>%
        dplyr::ungroup() %>%
        tidyr::pivot_wider(id_cols = routine_id,
                           names_from = searchtype,
                           values_from = concept,
                           values_fn = list(concept = function(x) paste(x, collapse = "\n"))) %>%
        dplyr::rename_at(vars(!routine_id),
                         function(x) paste0("SearchResults_", x))


final_output2 <-
        dplyr::left_join(output2,
                         output2b)



final_routine_output <-
        dplyr::left_join(final_input,
                         final_output2)

##############################################
### QA
##############################################
qa2 <- all(final_routine_output$routine_id %in% final_input$routine_id)
if (qa2 == FALSE) {
        stop("all routine_ids from final_input not in final_routine_output")
}

qa3 <- nrow(final_routine_output) - nrow(final_input)
if (qa3 != 0) {
        stop("row counts between final_input and final_routine_output don't match")
}

##############################################
### OUTRO
##############################################
broca::simply_write_csv(x = final_routine_output,
                        file = outputPath)

typewrite_complete()