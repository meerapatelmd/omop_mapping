# Clear env
clean_env()

if (interactive()) {
                ##############################################
                ### INTRODUCTION
                ##############################################
                # Source function and vars
                source("startup.R")

                umls_cui_col <- "MSK_CONCEPT_ID_UMLS"

                # Set search parameters
                type <- "UMLS API Synonyms"

                # Create output variables
                new_col_name <- "UMLS API Synonyms"
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
                        dplyr::select(routine_id, all_of(target_col), !!umls_cui_col, !!terminal_col)


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


                # For each concept
                for (i in 1:nrow(input3)) {

                                        # Isolate concept
                                        input_row <- input3 %>%
                                                        dplyr::filter(row_number() == i)


                                        # Convert concept info into objects
                                        rubix::release_df(input_row)

                                        input_concept <- get(source_col)
                                        input_cui <- get(umls_cui_col)
                                        output_concept <- get(terminal_col)
                                        input_routine_id <- routine_id



                                        # Search if input is not NA
                                        if (!is.logical(input_cui) && !(input_cui %in% c(NA, "NA"))) {

                                                # Search if output concept is NA, meaning unmapped
                                                 if (is.na(output_concept)) {

                                                         cat("\n")
                                                         secretary::typewrite(crayon::bold("Concept: "), input_concept)
                                                         secretary::typewrite(crayon::bold("CUI: "), input_cui)
                                                         cat("\n")

                                                         secretary::press_enter()

                                                         output[[i]] <-
                                                                 rubix::vector_to_tibble(
                                                                         metaorite::umls_api_get_cui(input_cui) %>%
                                                                                 dplyr::select(STR) %>%
                                                                                 rubix::mutate_all_rm_multibyte_chars() %>%
                                                                                 dplyr::mutate_at(vars(STR), tolower) %>%
                                                                                 dplyr::distinct() %>%
                                                                                 dplyr::mutate(nchar_str = centipede::nchar_lower_letter(STR)) %>%
                                                                                 dplyr::filter(nchar_str >= source_skip_nchar) %>%
                                                                                 rubix::arrange_by_nchar(STR, desc = FALSE) %>%
                                                                                 #dplyr::slice(1:5) %>%
                                                                                 unlist() %>%
                                                                                 tolower() %>%
                                                                                 unique() %>%
                                                                                 cave::vector_to_string(),
                                                                         !!new_col_name)

                                                         #secretary::press_enter()

                                                                # Search if input concept is above the character number threshold
                                                                # if (nchar(input_concept) > source_skip_nchar) {
                                                                #
                                                                #
                                                                #         output[[i]] <-
                                                                #                 query_phrase_in_athena(phrase = input_concept, type = type) %>%
                                                                #                 chariot::merge_concepts(into = `Concept`) %>%
                                                                #                 dplyr::select(!!new_col_name := `Concept`)
                                                                #
                                                                # } else {
                                                                #         output[[i]] <- NA
                                                                #         output[[i]] <-
                                                                #                 output[[i]] %>%
                                                                #                 rubix::vector_to_tibble(!!new_col_name)
                                                                # }



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


        # Aggregating the search result columns to the routine_id
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

} else {
        path_to_output_fn <- create_path_to_output_fn()
        if (!file.exists(path_to_output_fn)) {


        } else {

                        typewrite_file_exists()

        }

}