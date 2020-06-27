if (interactive()) {

                clean_env()

                source("startup.R")

                # Temporary stop if the output file exists
                brake_if_output_exists()


                # Read input
                input <- read_input()
                target_col <- source_col

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



                type <- "exact"
                new_col_name <- "CancerGov Drug Definition Exact"



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

                                                                        input_words <-
                                                                                strsplit(input_concept, split = word_split) %>%
                                                                                unlist() %>%
                                                                                unique()


                                                                        output[[i]] <-
                                                                                input_words %>%
                                                                                rubix::map_names_set(function(x) lookup_cancer_gov_dictionary(phrase = x, type = type)) %>%
                                                                                dplyr::bind_rows() %>%
                                                                                dplyr::distinct()

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


type <- "like"
new_col_name <- "CancerGov Drug Definition Like"



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

                                input_words <-
                                        strsplit(input_concept, split = word_split) %>%
                                        unlist() %>%
                                        unique()


                                output[[i]] <-
                                        input_words %>%
                                        rubix::map_names_set(function(x) lookup_cancer_gov_dictionary(phrase = x, type = type)) %>%
                                        dplyr::bind_rows() %>%
                                        dplyr::distinct()

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

output2 <-
   output %>%
        dplyr::bind_rows(.id = "routine_id") %>%
                dplyr::distinct()

final_output <-
        dplyr::left_join(input,
                         output2)

broca::view_as_csv(final_output)


