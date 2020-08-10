# if (interactive()) {

                ##############################################
                ### INTRODUCTION
                ##############################################
                # Clear env
                clean_env()

                # Source function and vars
                source("startup.R")

                # Set search parameters
                type <- "Terms ChemiDPlus Synonyms"

                # Create output variables
                new_col_name <- "Terms ChemiDPlus Synonyms"
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
                        dplyr::select(routine_id, all_of(target_col), all_of(term_col), !!terminal_col)


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
                                        input_terms <- get(term_col)
                                        output_concept <- get(terminal_col)
                                        input_routine_id <- routine_id


                                        if (!is.logical(input_concept) && !(input_concept %in% c(NA, "NA"))) {

                                                        if (is.na(output_concept)) {

                                                                        if (!is.na(input_terms)) {
                                                                                if (grepl("^c[(]{1}.*[)]{1}$", input_terms)) {
                                                                                        AllTerms <- cave::string_to_vector(input_terms)
                                                                                } else {
                                                                                        AllTerms <- input_terms
                                                                                }


                                                                                secretary::typewrite(crayon::bold("Concept:"), input_concept)
                                                                                secretary::typewrite(crayon::bold("Search Terms:"))

                                                                                AllTerms %>%
                                                                                        purrr::map(secretary::typewrite, tabs = 1)

                                                                                output[[i]] <-
                                                                                        AllTerms %>%
                                                                                        rubix::map_names_set(function(x) synonyms_from_chemidplus(phrase = stringr::str_replace_all(x, " ", "%20"))) %>%
                                                                                        purrr::keep(~length(.)>0)

                                                                                names(output)[i] <- input_routine_id


                                                                                typewrite_progress(i = i, input3)
                                                                                rm(list = colnames(input_row))
                                                                                rm(input_row)

                                                                        }
                                                        }

                                        }
                }

# Remove all NULLs in output
output2 <-
        output %>%
        purrr::keep(~!is.null(.)) %>%
        purrr::keep(~length(.)>0)

# Convert all vectors to strings
output3 <-
        output2 %>%
        rubix::map_names_set(function(x) lapply(x, function(y) cave::vector_to_string(y))) %>%
        # Then convert to dataframe
        purrr::map(function(x) lapply(x, function(y) rubix::vector_to_tibble(y, new_col = "Synonyms"))) %>%
        purrr::map(bind_rows, .id = "Term") %>%
        dplyr::bind_rows(.id = "routine_id") %>%
        dplyr::mutate(!!new_col_name := paste0(`Term`, ": ", `Synonyms`)) %>%
        dplyr::select(routine_id,
                      !!new_col_name) %>%
        rubix::group_by_aggregate(routine_id, agg.col = !!new_col_name, collapse = "\n")

# Subsetting for Excel constraints
output4 <-
        output3 %>%
        dplyr::mutate_at(vars(!routine_id), substr, 1, 25000)





        # Join with final_input object
        final_routine_output <-
                dplyr::left_join(final_input,
                                 output4)


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

# } else {
#         path_to_output_fn <- create_path_to_output_fn()
#         if (!file.exists(path_to_output_fn)) {
#
#         } else {
#
#                 typewrite_file_exists()
#
#         }
#
# }
