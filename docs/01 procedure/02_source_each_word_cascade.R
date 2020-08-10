
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
                new_col_name <- "Source Each Word Cascade"
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


                                                                                        current_resultset <-
                                                                                                chariot::query_phrase_synonym(phrase = Words[1],
                                                                                                                       type = type)

                                                                                        Words <- Words[-1]

                                                                                        while (length(Words) > 0) {

                                                                                                Word <- Words[1]

                                                                                                prior_resultset <- current_resultset
                                                                                                current_resultset <-
                                                                                                        current_resultset %>%
                                                                                                        rubix::filter_at_grepl(concept_name,
                                                                                                                               grepl_phrase = Word)

                                                                                                if (nrow(current_resultset)==0) {

                                                                                                        output[[i]] <-
                                                                                                                prior_resultset
                                                                                                        names(output)[i] <- input_routine_id
                                                                                                        Words <- NULL

                                                                                                } else {

                                                                                                        Words <- Words[-1]
                                                                                                }


                                                                                        }


                                                                                }


                                        typewrite_progress(i = i, input3)
                                        rm(list = colnames(input_row))
                                        rm(input_row)
                                                                }
                                                        }
                                        }
                }


output2 <-
        output %>%
        purrr::keep(~!is.null(.)) %>%
        purrr::keep(~nrow(.)>0)

final_output <-
        dplyr::bind_rows(output2,
                         .id = "routine_id") %>%
        chariot::merge_concepts(into = !!new_col_name) %>%
        dplyr::select(routine_id, !!new_col_name)


# Aggregating the search result columns to the original routine_id
final_output2 <-
        final_output %>%
        rubix::group_by_unique_aggregate(routine_id,
                                         agg.col = !!new_col_name,
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

