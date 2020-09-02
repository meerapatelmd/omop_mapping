if (!interactive()) {
        library(tidyverse)
}

releaseSettings()
target_col <- source_col

# Routine Variables
new_col_name <- "Source Each Word Space After"
new_col_name_status <- paste0(new_col_name, " Status")

space_before <- FALSE
space_after <- TRUE
between_space <- FALSE
exact_match <- FALSE
cacheOnly <- FALSE

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

output <- list()
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

                # Each word that is split on the word_split object and
                # filtered for 1) not blank,
                # 2) unique,
                # 3) above the same character count threshold minus 1
                Words <- tibble(all_words = strsplit(input_concept, split = word_split) %>%
                                        unlist() %>%
                                        centipede::no_blank() %>%
                                        unique()) %>%
                                dplyr::mutate(nchar = centipede::nchar_letter(all_words)) %>%
                                dplyr::filter(nchar >= (as.integer(source_skip_nchar)-1)) %>%
                                dplyr::arrange(desc(nchar)) %>%
                                dplyr::select(all_words) %>%
                                unlist() %>%
                                unique()


                if (length(Words) > 1) {

                        #secretary::press_enter()
                        secretary::typewrite(crayon::bold("Full:"), input_concept)
                        secretary::typewrite(crayon::bold("Words:"))
                        Words %>%
                                purrr::map(secretary::typewrite, tabs = 1)

                        for (j in 1:length(Words)) {

                                Word <- Words[j]

                                if (j == 1) {
                                        wordSqlList <- list()
                                }

                                if (space_after) {
                                        input_word <- paste0(Word, " %")
                                        wordSqlList[[1+length(wordSqlList)]] <-
                                                pg13::buildQuery(schema = "public",
                                                                 tableName = "concept",
                                                                 whereInField = "concept_name",
                                                                 whereInVector = input_word,
                                                                 caseInsensitive = TRUE)

                                        names(wordSqlList)[length(wordSqlList)] <- "space_after"
                                }

                                if (space_before) {
                                        input_word <- paste0("% ", Word)
                                        wordSqlList[[1+length(wordSqlList)]] <-
                                                pg13::buildQuery(schema = "public",
                                                                 tableName = "concept",
                                                                 whereInField = "concept_name",
                                                                 whereInVector = input_word,
                                                                 caseInsensitive = TRUE)

                                        names(wordSqlList)[length(wordSqlList)] <- "space_before"

                                }

                                if (between_space) {
                                        input_word <- paste0("% ", Word, " %")
                                        wordSqlList[[1+length(wordSqlList)]] <-
                                                pg13::buildQuery(schema = "public",
                                                                 tableName = "concept",
                                                                 whereInField = "concept_name",
                                                                 whereInVector = input_word,
                                                                 caseInsensitive = TRUE)

                                        names(wordSqlList)[length(wordSqlList)] <- "between_space"
                                }

                                if (exact_match) {
                                        input_word <- Word
                                        wordSqlList[[1+length(wordSqlList)]] <-
                                                pg13::buildQuery(schema = "public",
                                                                 tableName = "concept",
                                                                 whereInField = "concept_name",
                                                                 whereInVector = input_word,
                                                                 caseInsensitive = TRUE)

                                        names(wordSqlList)[length(wordSqlList)] <- "exact_match"
                                }

                                names(wordSqlList)[j] <- Word

                        }
                        output[[i]] <- wordSqlList

                } else {

                        output[[i]] <-
                                tibble(!!new_col_name_status := "Not performed: Only 1 word after split",
                                       !!new_col_name := NA)

                }
        }
        names(output)[i] <- routine_id
}

output_a <-
        output %>%
        purrr::keep(is.data.frame)

output_b <-
        output %>%
        purrr::keep(~!is.data.frame(.))


output2_b <- list()
if (cacheOnly) {
        for (i in 1:length(output_b)) {
                sqlList <- output_b[[i]]
                routine_id <- names(output_b)[i]


                #secretary::press_enter()
                output_row <-
                        sqlList %>%
                        purrr::map(pg13::loadCachedQuery, db = "athena") %>%
                        purrr::set_names(names(sqlList)) %>%
                        purrr::keep(~!is.null(.)) %>%
                        dplyr::bind_rows(.id = "Word") %>%
                        as_tibble()

                if (nrow(output_row)) {
                        output_row <-
                                output_row %>%
                                dplyr::inner_join(filterSettings) %>%
                                chariot::mergeStrip(into = "Concept") %>%
                                dplyr::transmute(!!new_col_name_status := "Complete",
                                                 !!new_col_name := paste0(Word, ": ", Concept))
                } else {
                        output_row <-
                                tibble(!!new_col_name_status := "No cached results",
                                       !!new_col_name := NA)
                }
                output2_b[[i]] <- output_row
                names(output2_b)[i] <- routine_id

        }

} else {

        for (i in 1:length(output_b)) {
                sqlList <- output_b[[i]]
                routine_id <- names(output_b)[i]

                output_row <-
                        sqlList %>%
                        purrr::map(chariot::queryAthena) %>%
                        purrr::set_names(names(sqlList)) %>%
                        dplyr::bind_rows(.id = "Word") %>%
                        as_tibble() %>%
                        dplyr::inner_join(filterSettings) %>%
                        chariot::mergeStrip(into = "Concept") %>%
                        dplyr::transmute(!!new_col_name_status := "Complete",
                                         !!new_col_name := paste0(Word, ": ", Concept))


                output2_b[[i]] <- output_row
                names(output2_b)[i] <- routine_id


        }
}

output3 <- list(output_a %>%
                        purrr::map(filter_max_250),
                output2_b %>%
                        purrr::map(filter_max_250)) %>%
        purrr::map(function(x) lapply(x, mutate_all, as.character)) %>%
        purrr::map(bind_rows, .id = "routine_id") %>%
        dplyr::bind_rows()


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
