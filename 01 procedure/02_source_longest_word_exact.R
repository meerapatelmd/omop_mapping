space_before <- FALSE
space_after <- FALSE
between_space <- FALSE
exact_match <- TRUE
cacheOnly <- FALSE

releaseSettings()
target_col <- source_col

# Routine Variables
new_col_name <- "Source Longest Word Exact"
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

output <- list()
for (i in 1:nrow(input4)) {

        input_row <- input4 %>%
                        dplyr::filter(row_number() == i)

        rubix::release(input_row)
        input_concept <- get(target_col)

        if (is.logical(input_concept) && !(input_concept %in% c(NA, "NA"))) {
                output_row <-
                        tibble(!!new_col_name_status := "Error",
                               !!new_col_name := NA)
        } else {
                LongestWord <- tibble(all_words = strsplit(input_concept, split = word_split) %>%
                                              unlist() %>%
                                              centipede::no_blank() %>%
                                              unique()) %>%
                                        dplyr::mutate(nchar = nchar(all_words)) %>%
                                        dplyr::filter(nchar >= source_skip_nchar) %>%
                                        dplyr::arrange(desc(nchar)) %>%
                                        dplyr::select(-nchar) %>%
                                        rubix::filter_first_row() %>%
                                        unlist() %>%
                                        centipede::no_na() %>%
                                        centipede::no_blank()

                secretary::typewrite(crayon::bold("Full:"), input_concept)
                secretary::typewrite(crayon::bold("Longest Word:"), LongestWord)

                if (length(LongestWord)) {

                        # Write SQL Statements
                        sqlList <- list()

                        if (space_after) {
                                input_word <- paste0(LongestWord, " %")
                                sqlList[[1+length(sqlList)]] <-
                                        pg13::buildQuery(schema = "public",
                                                         tableName = "concept",
                                                         whereInField = "concept_name",
                                                         whereInVector = input_word,
                                                         caseInsensitive = TRUE)

                                names(sqlList)[length(sqlList)] <- "space_after"
                        }

                        if (space_before) {
                                input_word <- paste0("% ", LongestWord)
                                sqlList[[1+length(sqlList)]] <-
                                        pg13::buildQuery(schema = "public",
                                                         tableName = "concept",
                                                         whereInField = "concept_name",
                                                         whereInVector = input_word,
                                                         caseInsensitive = TRUE)

                                names(sqlList)[length(sqlList)] <- "space_before"

                        }

                        if (between_space) {
                                input_word <- paste0("% ", LongestWord, " %")
                                sqlList[[1+length(sqlList)]] <-
                                        pg13::buildQuery(schema = "public",
                                                         tableName = "concept",
                                                         whereInField = "concept_name",
                                                         whereInVector = input_word,
                                                         caseInsensitive = TRUE)

                                names(sqlList)[length(sqlList)] <- "between_space"
                        }

                        if (exact_match) {
                                input_word <- LongestWord
                                sqlList[[1+length(sqlList)]] <-
                                        pg13::buildQuery(schema = "public",
                                                         tableName = "concept",
                                                         whereInField = "concept_name",
                                                         whereInVector = input_word,
                                                         caseInsensitive = TRUE)

                                names(sqlList)[length(sqlList)] <- "exact_match"
                        }

                output[[i]] <-
                        dplyr::bind_rows(space_after_results,
                                         space_before_results,
                                         no_space_results,
                                         between_results) %>%
                        dplyr::distinct() %>%
                        #chariot::filterSettings() %>%
                        rubix::arrange_by_nchar(concept_name) %>%
                        filter_max_250()


                names(output)[i] <- input_routine_id
        }




                        if (space_after) {

                                #Add space after
                                input_word <- paste0(LongestWord, " %")
                                space_after_results <-
                                        chariot::queryPhraseExact(schema = "public",
                                                                  phrase = input_word,
                                                                  caseInsensitive = TRUE)

                        } else {
                                space_after_results <- NULL
                        }


                        if (space_before) {


                                #Add space before
                                input_word <- paste0("% ", LongestWord)
                                space_before_results <-
                                        chariot::queryPhraseExact(schema = "public",
                                                                  phrase = input_word,
                                                                  caseInsensitive = TRUE)
                        } else {
                                space_before_results <- NULL
                        }

                        if (between_space) {

                                #No space
                                input_word <- paste0("% ", LongestWord, " %")
                                between_results <-
                                        chariot::queryPhraseExact(schema = "public",
                                                                  phrase = input_word,
                                                                  caseInsensitive = TRUE)
                        } else {
                                between_results <- NULL
                        }

                        #No space
                        input_word <- LongestWord
                        if (cacheOnly) {
                                sql_statement <-
                                        pg13::buildQuery(schema = "public",
                                                         tableName = "concept",
                                                         whereInField = "concept_name",
                                                         whereInVector = input_word,
                                                         caseInsensitive = TRUE)

                                no_space_results <-
                                        pg13::loadCachedQuery(sqlQuery = sql_statement,
                                                              db = "athena")
                        } else {
                                no_space_results <-
                                        chariot::queryPhraseExact(schema = "public",
                                                                  phrase = input_word,
                                                                  caseInsensitive = TRUE)
                        }


                        output[[i]] <-
                                dplyr::bind_rows(space_after_results,
                                                 space_before_results,
                                                 no_space_results,
                                                 between_results) %>%
                                dplyr::distinct() %>%
                                #chariot::filterSettings() %>%
                                rubix::arrange_by_nchar(concept_name) %>%
                                filter_max_250()


                        names(output)[i] <- input_routine_id
        }
}

input_concepts <-
if (!is.logical(input_concept) && !(input_concept %in% c(NA, "NA")))



                output <- list()
                for (i in 1:nrow(input3)) {

                                        input_row <- input3 %>%
                                                        dplyr::filter(row_number() == i)


                                        rubix::release_df(input_row)


                                        input_concept <- get(target_col)
                                        output_concept <- get(terminal_col)
                                        input_routine_id <- routine_id


                        if (!is.logical(input_concept) && !(input_concept %in% c(NA, "NA"))) {

                                        if (is.na(output_concept)) {

                                                if (nchar(input_concept) > source_skip_nchar) {

                                                        secretary::typewrite(crayon::bold("Full:"), input_concept)

                                                        LongestWord <- tibble(all_words = strsplit(input_concept, split = word_split) %>%
                                                                                                unlist() %>%
                                                                                                centipede::no_blank() %>%
                                                                                                unique()) %>%
                                                                dplyr::mutate(nchar = nchar(all_words)) %>%
                                                                dplyr::filter(nchar >= source_skip_nchar) %>%
                                                                dplyr::arrange(desc(nchar)) %>%
                                                                dplyr::select(-nchar) %>%
                                                                rubix::filter_first_row() %>%
                                                                unlist() %>%
                                                                centipede::no_na() %>%
                                                                centipede::no_blank()


                                                        secretary::typewrite(crayon::bold("Longest Word:"), LongestWord)



                                                        if (length(LongestWord)) {

                                                                if (space_after) {

                                                                        #Add space after
                                                                        input_word <- paste0(LongestWord, " %")
                                                                        space_after_results <-
                                                                                chariot::queryPhraseExact(schema = "public",
                                                                                                          phrase = input_word,
                                                                                                          caseInsensitive = TRUE)

                                                                } else {
                                                                        space_after_results <- NULL
                                                                }


                                                                if (space_before) {


                                                                        #Add space before
                                                                        input_word <- paste0("% ", LongestWord)
                                                                        space_before_results <-
                                                                                chariot::queryPhraseExact(schema = "public",
                                                                                                          phrase = input_word,
                                                                                                          caseInsensitive = TRUE)
                                                                } else {
                                                                        space_before_results <- NULL
                                                                }

                                                                if (between_space) {

                                                                        #No space
                                                                        input_word <- paste0("% ", LongestWord, " %")
                                                                        between_results <-
                                                                                chariot::queryPhraseExact(schema = "public",
                                                                                                          phrase = input_word,
                                                                                                          caseInsensitive = TRUE)
                                                                } else {
                                                                        between_results <- NULL
                                                                }

                                                                        #No space
                                                                        input_word <- LongestWord
                                                                        if (cacheOnly) {
                                                                        sql_statement <-
                                                                        pg13::buildQuery(schema = "public",
                                                                                         tableName = "concept",
                                                                                         whereInField = "concept_name",
                                                                                         whereInVector = input_word,
                                                                                         caseInsensitive = TRUE)

                                                                        no_space_results <-
                                                                        pg13::loadCachedQuery(sqlQuery = sql_statement,
                                                                                              db = "athena")
                                                                        } else {
                                                                        no_space_results <-
                                                                                chariot::queryPhraseExact(schema = "public",
                                                                                                          phrase = input_word,
                                                                                                          caseInsensitive = TRUE)
                                                                        }


                                                                        output[[i]] <-
                                                                                dplyr::bind_rows(space_after_results,
                                                                                                 space_before_results,
                                                                                                 no_space_results,
                                                                                                 between_results) %>%
                                                                                dplyr::distinct() %>%
                                                                                #chariot::filterSettings() %>%
                                                                                rubix::arrange_by_nchar(concept_name) %>%
                                                                                filter_max_250()


                                                                names(output)[i] <- input_routine_id





                                                        output[[i]] <-
                                                                output[[i]] %>%
                                                                #dplyr::bind_rows() %>%
                                                                chariot::merge_concepts(into = `Concept`) %>%
                                                                dplyr::mutate(Concept = paste0(LongestWord, ": ", Concept)) %>%
                                                                dplyr::select(!!new_col_name := `Concept`)

                                                } else {
                                                        output[[i]] <- NA
                                                        output[[i]] <-
                                                                output[[i]] %>%
                                                                rubix::vector_to_tibble(!!new_col_name)
                                                }



                                        } else {
                                                output[[i]] <- NA
                                                output[[i]] <-
                                                        output[[i]] %>%
                                                        rubix::vector_to_tibble(!!new_col_name)
                                        }

                        } else {
                                output[[i]] <- NA
                                output[[i]] <-
                                        output[[i]] %>%
                                        rubix::vector_to_tibble(!!new_col_name)
                        }

                        names(output)[i] <- input_routine_id

                        typewrite_progress(i = i, input3)
                        rm(list = colnames(input_row))
                        rm(input_row)
        }

        final_output <- output %>%
                                dplyr::bind_rows(.id = "routine_id")

                }

        # Aggregating the search result columns to the original routine_id
        final_output2 <-
                final_output %>%
                rubix::group_by_unique_aggregate(routine_id,
                                                 agg.col = all_of(new_col_name),
                                                 collapse = "\n") %>%
                dplyr::mutate_at(vars(!routine_id), substr, 1, 25000)

        # # If the search type is both exact and like, would need to reduce the list with left_join so each routine_id will have both searches associated with it in the dataframe
                final_output3 <-
                        final_output2

        # Join with final_input object
        final_routine_output <-
                dplyr::left_join(final_input,
                                 final_output3)


        #QA
        qa2 <- all(final_routine_output$routine_id %in% final_input$routine_id)
        if (qa2 == FALSE) {
                stop("all routine_ids from final_input not in final_routine_output")
        }

        qa3 <- nrow(final_routine_output) - nrow(final_input)
        if (qa3 != 0) {
                stop("row counts between final_input and final_routine_output don't match")
        }


        broca::simply_write_csv(x = final_routine_output,
                                file = path_to_output_fn)
