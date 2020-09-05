if (!interactive()) {
        library(tidyverse)
}

releaseSettings()
target_col <- source_col

# Routine Variables
new_col_name <- paste0(target_col, " Longest Word Left Join")
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
        dplyr::mutate(Word = trimws(Word)) %>%
        dplyr::mutate(ncharWord = nchar(Word)) %>%
        dplyr::group_by(routine_id) %>%
        dplyr::arrange(desc(ncharWord), .by_group = TRUE) %>%
        rubix::filter_first_row() %>%
        dplyr::ungroup() %>%
        dplyr::select(-ncharWord)


if (nrow(input5) != nrow(input4)) {
        stop('missing routine_ids in input5')
}

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


# 2. Stop If any routine_id is NA
qa1 <-  input4$routine_id[is.na(input4$routine_id)]
if (length(qa1) > 0) {
        stop("NA routine_id in input2")
}


output <-
chariot::leftJoin(input6 %>%
                          dplyr::select(routine_id, Word),
                  column = "Word",
                  athena_schema = "public",
                  athena_table = "concept_synonym",
                  athena_column = "concept_synonym_name") %>%
        dplyr::select(
                routine_id,
                word,
                word_concept_id = concept_id
               )


output2 <-
        chariot::leftJoin(output,
                          column = "word_concept_id",
                          athena_schema = "public",
                          athena_table = "concept",
                          athena_column = "concept_id")


output3 <-
        output2 %>%
        dplyr::inner_join(filterSettings) %>%
        chariot::mergeStrip(into = "Concept") %>%
        dplyr::transmute(routine_id,
                         !!new_col_name_status := "Complete",
                         !!new_col_name := paste0(word, ": ", Concept))


output4 <-
        output3 %>%
        rubix::group_by_unique_aggregate(routine_id,
                                         agg.col = !!new_col_name,
                                         collapse = "\n") %>%
        dplyr::left_join(output3 %>%
                                 rubix::group_by_unique_aggregate(routine_id,
                                                                  agg.col = !!new_col_name_status,
                                                                  collapse = "\n"))

# Excel Limits
# If character count > 20,000 add ellipsis
output5 <-
        output4 %>%
        dplyr::mutate(char_count = nchar(!!as.symbol(new_col_name))) %>%
        dplyr::mutate(!!new_col_name := ifelse(char_count > 19996,
                                               paste0(substr(!!as.symbol(new_col_name),
                                                             1,
                                                             19996),
                                                      "..."),
                                               !!as.symbol(new_col_name))) %>%
        dplyr::select(-char_count)

# Reorder
output6 <-
        output5 %>%
        dplyr::select(routine_id,
                      !!new_col_name_status,
                      !!new_col_name)

# Join with final_input object
final_routine_output <-
        dplyr::left_join(input,
                         output6)


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
