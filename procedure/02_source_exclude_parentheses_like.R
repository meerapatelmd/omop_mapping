if (interactive()) {

clean_env()

source('startup.R')
# Routine Variables

# Temporary stop if the output file exists
brake_if_output_exists()

final_output <- list()
# Read input
input <- read_input()
target_col <- source_col

# Routine Variables
type <- c("like")
new_col_name <- paste0("Source Exclude Parentheses ", centipede::in_title_format(type))


# Normalize NAs because some are "NA" and others are true NA
input2 <-
        input %>%
        normalize_na()

                # Creating final input object to join with final output object
                final_input <- input2

                # Parse the vectors that are strings
                input3 <-
                        input2 %>%
                        dplyr::select(routine_id, all_of(target_col), !!terminal_col)


                # If any routine_id is NA
                qa1 <-  input3$routine_id[is.na(input3$routine_id)]

                if (length(qa1) > 0) {

                        stop("routine_ids present in input are missing in input3")

                }


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


                                                                        if (grepl("[(]{1}", input_concept)) {

                                                                                        FirstWord <-
                                                                                                stringr::str_replace_all(input_concept,
                                                                                                                         pattern = "(^.*?)([(]{1}.*$)",
                                                                                                                         replacement = "\\1") %>%
                                                                                                trimws(which = "both") %>%
                                                                                                centipede::no_na() %>%
                                                                                                centipede::no_blank()

                                                                                        secretary::typewrite(crayon::bold("Concept without Parentheses:"), FirstWord)

                                                                                        output[[i]] <-
                                                                                                query_phrase_in_athena(phrase = FirstWord,
                                                                                                                       type = type) %>%
                                                                                                chariot::merge_concepts(into = `Concept`) %>%
                                                                                                dplyr::mutate(Concept = paste0(FirstWord, ": ", Concept)) %>%
                                                                                                dplyr::select(!!new_col_name := `Concept`)


                                                                                names(output)[i] <- input_routine_id

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
                                                 agg.col = contains("Source Exclude Parentheses "),
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



} else {


        source('/Users/patelm9/GitHub/omop_mapping/procedure/startup.R')


        final_output <- list()
        # Read input
        input <- read_input()
        target_col <- source_col


        path_to_output_fn <- create_path_to_output_fn()

        # Routine Variables
        type <- c("like")
        new_col_name <- paste0("Source Exclude Parentheses ", centipede::in_title_format(type))


        # Normalize NAs because some are "NA" and others are true NA
        input2 <-
                input %>%
                normalize_na()

        # Creating final input object to join with final output object
        final_input <- input2

        # Parse the vectors that are strings
        input3 <-
                input2 %>%
                dplyr::select(routine_id, all_of(target_col), !!terminal_col)


        # If any routine_id is NA
        qa1 <-  input3$routine_id[is.na(input3$routine_id)]

        if (length(qa1) > 0) {

                stop("routine_ids present in input are missing in input3")

        }


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


                                        if (grepl("[(]{1}", input_concept)) {

                                                FirstWord <-
                                                        stringr::str_replace_all(input_concept,
                                                                                 pattern = "(^.*?)([(]{1}.*$)",
                                                                                 replacement = "\\1") %>%
                                                        trimws(which = "both") %>%
                                                        centipede::no_na() %>%
                                                        centipede::no_blank()

                                                secretary::typewrite(crayon::bold("Concept without Parentheses:"), FirstWord)

                                                output[[i]] <-
                                                        query_phrase_in_athena(phrase = FirstWord,
                                                                               type = type) %>%
                                                        chariot::merge_concepts(into = `Concept`) %>%
                                                        dplyr::mutate(Concept = paste0(FirstWord, ": ", Concept)) %>%
                                                        dplyr::select(!!new_col_name := `Concept`)


                                                names(output)[i] <- input_routine_id

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

        }

        final_output <- output %>%
                dplyr::bind_rows(.id = "routine_id")
        # Aggregating the search result columns to the original routine_id
        final_output2 <-
                final_output %>%
                rubix::group_by_unique_aggregate(routine_id,
                                                 agg.col = contains("Source Exclude Parentheses "),
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


}
