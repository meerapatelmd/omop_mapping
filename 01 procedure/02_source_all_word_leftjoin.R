library(tidyverse)
library(rvest)

releaseSettings()
target_col <- source_col

# Routine Variables
new_col_name <- paste0(target_col, " All Word Left Join")
new_col_name_status <- paste0(new_col_name, " Status")


# Routine Variables
outputPath <- createOutputPath()
# Temporary stop if the output file exists
brake_if_output_exists()


# Read input
input <- read_input()

# QA steps
# Stop if there aren't any rows because there is no data to feed into the algorithm
if (nrow(input) == 0) {
        stop("input has 0 rows")
}

# Select core columns
input2 <-
        input %>%
        dplyr::select(routine_id, !!target_col, !!terminal_col)

# Filter for NA terminal_col
input3 <-
        input2 %>%
                dplyr::filter_at(vars(!!terminal_col), all_vars(is.na(.)))


# QA steps
# Stop if there aren't any rows because there is no data to feed into the algorithm
if (nrow(input3) == 0) {
        stop("input3 has 0 rows")
}

if (!is.null(source_skip_nchar)) {

        input4 <-
                input3 %>%
                dplyr::filter_at(vars(!!target_col),
                                function(x) nchar(x) > as.integer(source_skip_nchar))

} else {

        input4 <-
                input3

}

# QA steps
# Stop if there aren't any rows because there is no data to feed into the algorithm
if (nrow(input4) == 0) {
        stop("input4 has 0 rows")
}

input5 <-
        input4 %>%
        tidyr::separate_rows(!!target_col,
                             sep = word_split) %>%
        rename(Word = !!target_col) %>%
        mutate(Word = trimws(Word)) %>%
        rowid_to_column("word_id")

input6 <-
        list(input5,
             input5 %>%
                     dplyr::mutate(Word = tolower(Word)),
             input5 %>%
                     dplyr::mutate(Word = toupper(Word)),
             input5 %>%
                     dplyr::mutate(Word = stringr::str_to_title(Word))) %>%
        bind_rows() %>%
        distinct()

url <- "https://en.wikipedia.org/wiki/Most_common_words_in_English"
commonWordsWiki <- read_html(url)
commonWords <-
        commonWordsWiki %>%
        html_node("table") %>%
        html_table() %>%
        dplyr::select(Word) %>%
        dplyr::distinct() %>%
        unlist()

input7 <-
        input6 %>%
        dplyr::mutate(filterWord = tolower(Word)) %>%
        rubix::filter_for(filter_col = filterWord,
                          inclusion_vector = commonWords,
                          invert = TRUE) %>%
        dplyr::select(-filterWord)

# 2. Stop If any routine_id is NA
qa1 <-  input4$routine_id[is.na(input4$routine_id)]
if (length(qa1) > 0) {
        stop("NA routine_id in input4")
}


# output <-
# chariot::leftJoin(input7 %>%
#                           dplyr::select(routine_id, Word, word_id),
#                   column = "Word",
#                   athena_schema = "public",
#                   athena_table = "concept_synonym",
#                   athena_column = "concept_synonym_name") %>%
#         dplyr::select(
#                 routine_id,
#                 word_id,
#                 word,
#                 word_concept_id = concept_id
#                )

input8 <- input7 %>%
                tibble::rowid_to_column()

output_a <- input8

output_b <-
        input8$Word %>%
        purrr::map(chariot::queryPhraseLikeSynonym,
                   schema = "public",
                   caseInsensitive = TRUE) %>%
        purrr::set_names(input8$rowid)

output_b2 <-
        output_b %>%
        dplyr::bind_rows(.id = "rowid") %>%
        dplyr::select(-concept_synonym_name) %>%
        dplyr::distinct()


output2 <-
        output_a %>%
        dplyr::select(-!!terminal_col) %>%
        dplyr::left_join(output_b2 %>%
                                mutate(rowid = as.integer(rowid))) %>%
        dplyr::select(-rowid) %>%
        dplyr::distinct()

output3 <-
        output2 %>%
        dplyr::inner_join(filterSettings)


output4 <-
        split(output3, output3$routine_id) %>%
        purrr::map(function(x) split(x, x$word_id)) %>%
        purrr::map(function(x) x %>%
                                purrr::map(function(y) y %>%
                                                        dplyr::select(word_id,
                                                                      routine_id,
                                                                      Word,
                                                                      concept_id))) %>%
        purrr::map(function(x) x %>%
                                purrr::reduce(inner_join,
                                              by = "concept_id") %>%
                                   dplyr::select(concept_id,
                                                 any_of("Word"),
                                                 starts_with("Word.")) %>%
                                   dplyr::distinct()) %>%
        dplyr::bind_rows(.id = "routine_id") %>%
        tidyr::unite(col = Word2,
                     starts_with("Word."),
                     sep = " && ",
                     na.rm = TRUE) %>%
        dplyr::transmute(routine_id,
                         concept_id,
                         Words = coalesce(Word, Word2))


output5 <-
        output4 %>%
        dplyr::rename(input_concept_id = concept_id) %>%
        chariot::leftJoinConcept(column = "input_concept_id") %>%
        chariot::mergeStrip(into = "Concept") %>%
        dplyr::group_by(routine_id, Concept) %>%
        rubix::filter_first_row() %>%
        dplyr::ungroup() %>%
        dplyr::transmute(routine_id,
                         !!new_col_name_status := "Complete",
                         !!new_col_name := paste0(words, ": ", Concept))


output6 <-
        output5 %>%
        rubix::group_by_unique_aggregate(routine_id,
                                         agg.col = !!new_col_name,
                                         collapse = "\n") %>%
        dplyr::left_join(output5 %>%
                                 rubix::group_by_unique_aggregate(routine_id,
                                                                  agg.col = !!new_col_name_status,
                                                                  collapse = "\n"))

# Excel Limits
# If character count > 20,000 add ellipsis
output7 <-
        output6 %>%
        dplyr::mutate(char_count = nchar(!!as.symbol(new_col_name))) %>%
        dplyr::mutate(!!new_col_name := ifelse(char_count > 19996,
                                               paste0(substr(!!as.symbol(new_col_name),
                                                             1,
                                                             19996),
                                                      "..."),
                                               !!as.symbol(new_col_name))) %>%
        dplyr::select(-char_count)

# Reorder
output8 <-
        output7 %>%
        dplyr::select(routine_id,
                      !!new_col_name_status,
                      !!new_col_name)

# Join with final_input object
final_routine_output <-
        dplyr::left_join(input,
                         output8)


#QA
qa2 <- all(final_routine_output$routine_id %in% input$routine_id)
if (qa2 == FALSE) {
        stop("all routine_ids from final_input not in final_routine_output")
}

qa3 <- nrow(final_routine_output) - nrow(input)
if (qa3 != 0) {
        stop("row counts between final_input and final_routine_output don't match")
}

broca::simply_write_csv(x = final_routine_output,
                        file = outputPath)
