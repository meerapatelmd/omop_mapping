if (interactive()) {

                ##############################################
                ### INTRODUCTION
                ##############################################
                # Clear env
                clean_env()

                # Source function and vars
                source("startup.R")

                # Set search parameters
                type <- "like"

                # Create output variables
                new_col_name <- "Source Each Word HemOnc Monotherapy"
                path_to_output_fn <- create_path_to_output_fn()

                # Temporary stop if the output file exists
                brake_if_output_exists()

                ##############################################
                ### PRE-PROCEDURE
                ##############################################
                # Read input
                input <- read_input()
                target_col <- source_col

                # Normalize NAs because some are "NA" and others are true NA
                input2 <-
                        input %>%
                        normalize_na()

                # Create final input object to join with final output object before export
                final_input <- input2

                # Select only relevant columns
                input3 <-
                        input2 %>%
                        dplyr::select(routine_id, all_of(target_col), !!terminal_col)


                # If any routine_id is NA
                qa1 <-  input3$routine_id[is.na(input3$routine_id)]

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



                for (i in 1:nrow(input3)) {

                                        # Per row
                                        input_row <- input3 %>%
                                                        dplyr::filter(row_number() == i)


                                        # Release single row df as r objects
                                        rubix::release_df(input_row)


                                        input_concept <- get(target_col)
                                        output_concept <- get(terminal_col)
                                        input_routine_id <- routine_id


                                        if (!is.logical(input_concept) && !(input_concept %in% c(NA, "NA"))) {

                                                        if (is.na(output_concept)) {

                                                                # Entire input concept must be above character count threshold
                                                                if (nchar(input_concept) > source_skip_nchar) {

                                                                        # Each word that is split on the word_split object and
                                                                        # filtered for 1) not blank,
                                                                                     # 2) unique,
                                                                                     # 3) above the same character count threshold minus 1
                                                                        Words <- tibble(all_words = strsplit(input_concept, split = word_split) %>%
                                                                                                                unlist() %>%
                                                                                                                centipede::no_blank() %>%
                                                                                                                unique()) %>%
                                                                                dplyr::mutate(nchar = centipede::nchar_letter(all_words)) %>%
                                                                                dplyr::filter(nchar >= (source_skip_nchar-1)) %>%
                                                                                dplyr::arrange(desc(nchar)) %>%
                                                                                dplyr::select(all_words) %>%
                                                                                unlist() %>%
                                                                                unique()

                                                                        if (length(Words)) {

                                                                                output[[i]] <- list()

                                                                                for (j in 1:length(Words)) {


                                                                                        secretary::typewrite(crayon::bold("Raw:"), Words[j])
                                                                                        word_resultset <-
                                                                                                chariot::hemonc_monotherapy(component = Words[j])

                                                                                        output[[i]][[j]] <-
                                                                                                word_resultset %>%
                                                                                                dplyr::distinct() %>%
                                                                                                rubix::arrange_by_nchar(concept_name)

                                                                                }

                                                                                names(output)[i] <- input_routine_id
                                                                                names(output[[i]]) <- Words

                                                                                # Some words return 0 rows and these should be removed to be able to split the 250 line limit across the ones that have results
                                                                                # Removing 0 row list entries
                                                                                output[[i]] <-
                                                                                        output[[i]] %>%
                                                                                        purrr::keep(~nrow(.)>0)

                                                                                # Slice each remaining resultsets based on total length of words that returned results
                                                                                n_words <- length(output[[i]])

                                                                                #If all Words returned 0 rows, the n_words would be 0 and these results are NA while the remaining are sent further down the pipeline
                                                                                if (n_words > 0) {

                                                                                        output[[i]] <-
                                                                                                output[[i]] %>%
                                                                                                purrr::map(function(x) filter_max_n(x,
                                                                                                                                    n = (250/n_words)))


                                                                                        # List is bound into a single dataframe, preserving the word in the concept strip
                                                                                        output[[i]] <-
                                                                                                output[[i]] %>%
                                                                                                dplyr::bind_rows(.id = "Word") %>%
                                                                                                chariot::merge_concepts(into = `Concept`) %>%
                                                                                                dplyr::mutate(Concept = paste0(Word, ": ", Concept)) %>%
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



        # Join with final_input object
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
                                file = path_to_output_fn)

        typewrite_complete()

}