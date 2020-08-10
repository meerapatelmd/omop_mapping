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
                type <- "UMLS Code Crosswalk"

                # Create output variables
                new_col_name <- "UMLS Code Crosswalk"
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


                                                         output_a <-
                                                                  metaorite::query_cui(input_cui) %>%
                                                                                 dplyr::select(SAB, CODE) %>%
                                                                                dplyr::filter(SAB %in% c("SNOMEDCT_US",
                                                                                                         "LNC",
                                                                                                         "RXNORM")) %>%
                                                                                dplyr::mutate(SAB = ifelse(SAB == "SNOMEDCT_US",
                                                                                                           "SNOMED",
                                                                                                           SAB)) %>%
                                                                                dplyr::mutate(SAB = ifelse(SAB == "RXNORM",
                                                                                                           "RxNorm",
                                                                                                           SAB)) %>%
                                                                                dplyr::mutate(SAB = ifelse(SAB == "LNC",
                                                                                                           "LOINC",
                                                                                                           SAB)) %>%
                                                                                 dplyr::distinct() %>%
                                                                                 rubix::mutate_all_rm_multibyte_chars() %>%
                                                                                dplyr::select(umls_vocabulary_id = SAB,
                                                                                              umls_concept_code = CODE)

                                                         if (nrow(output_a)) {

                                                         table_name <- paste0("v", stampede::stamp_this(without_punct = TRUE))
                                                         seagull::create_table_via_temp_file(dataframe = output_a,
                                                                                             table_name = table_name,
                                                                                             dbname = "athena")

                                                         output_b <- chariot::query_athena(paste0("SELECT * FROM ", table_name,
                                                                                       " LEFT JOIN concept c ON c.concept_code = umls_concept_code AND c.vocabulary_id = umls_vocabulary_id"))

                                                         seagull::drop_table(table_name = table_name, dbname = "athena")

                                                         # Sometimes a crosswalk is unsuccessfull (ie the LOINC concept isn't in Athena though it is in UMLS, so filtering out the NA results)
                                                         output_b <-
                                                                 output_b %>%
                                                                 dplyr::filter(!is.na(concept_id))

                                                                if (nrow(output_b)) {

                                                                        output[[i]] <-
                                                                                output_b %>%
                                                                                chariot::merge_concepts(into = !!new_col_name) %>%
                                                                                dplyr::select(!!new_col_name)


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