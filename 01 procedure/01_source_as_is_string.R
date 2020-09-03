if (!interactive()) {
        library(tidyverse)
}

releaseSettings()
target_col <- source_col

# Routine Variables
new_col_name <- "Source As-Is String"
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

# 2. Stop If any routine_id is NA
qa1 <-  input4$routine_id[is.na(input4$routine_id)]
if (length(qa1) > 0) {
        stop("NA routine_id in input2")
}


for (i in 1:nrow(input4)) {

        input_row <- input4 %>%
                dplyr::filter(row_number() == i)

        rubix::release(input_row)
        input_concept <- get(target_col)

        if (is.logical(input_concept) && !(input_concept %in% c(NA, "NA"))) {

                output[[i]] <-
                        tibble(!!new_col_name_status := "Error: NA input concept",
                               !!new_col_name := NA)

        } else {

                output[[i]] <-
                        chariot::queryPhraseStringSynonym(schema = "public",
                                                   phrase = input_concept,
                                                   split = " |[[:punct:]]") %>%
                        dplyr::select(-concept_synonym_name) %>%
                        dplyr::inner_join(filterSettings) %>%
                        chariot::mergeStrip(into = "Concept") %>%
                        dplyr::transmute(!!new_col_name_status := "Complete",
                                         !!new_col_name)
        }
        names(output)[i] <- routine_id

}

# Excel Limits
# Filter first 250 rows
output2 <-
        output %>%
        purrr::map(filter_max_250)

# Flatten list to data frame
output3 <-
        output2 %>%
        # Convert logical NA to character NA to bind rows
        purrr::map(mutate_all, as.character) %>%
        dplyr::bind_rows(.id = "routine_id")

# Flatten rows to 1 row
output4 <-
        output3 %>%
        rubix::group_by_unique_aggregate(routine_id,
                                         agg.col = !!new_col_name,
                                         collapse = "\n") %>%
        dplyr::full_join(output3 %>%
                                 dplyr::select(routine_id, !!new_col_name_status) %>%
                                 dplyr::distinct()) %>%
        dplyr::distinct()

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


