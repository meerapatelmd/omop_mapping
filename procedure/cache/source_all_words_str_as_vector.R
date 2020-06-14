if (interactive()) {
                clean_env()
                source('startup.R')
                # Read input
                input <- read_input()
                target_col <- source_col

                # Normalize NAs because some are "NA" and others are true NA
                input2 <-
                        input %>%
                        normalize_na()


                # Parse the vectors that are strings
                input3 <-
                        input2 %>%
                        dplyr::select(all_of(target_col)) %>%
                        unlist() %>%
                        unname() %>%
                        unique()

                # Getting words
                input4 <-
                        strsplit(input3, split = "[(]{1}|[)]{1}|[,]{1}") %>%
                        unlist() %>%
                        trimws("both") %>%
                        centipede::no_blank() %>%
                        stringr::str_remove_all("[']{1}|[?]{1}") %>%
                        unique()

                # Filtering for only words that meeting the character limit
                input5 <- input4[nchar(input4) >= source_skip_nchar]



                        cat("\n")


                        for (i in 1:length(input5)) {

                                input_word <- input5[i]

                                if (any(grepl(pattern = " ", input_word) == TRUE)) {

                                        secretary::typewrite(crayon::bold("Phrase:"), input_word)

                                        x <- chariot::query_string_as_vector(input_word)

                                }

                                typewrite_percent_progress(i = i,
                                                           input5)
                        }
} else {

        secretary::typewrite("Starting non-interactive session.")

        source('/Users/patelm9/GitHub/omop_mapping/procedure/startup.R')
        # Read input
        input <- read_input()
        target_col <- source_col

        # Normalize NAs because some are "NA" and others are true NA
        input2 <-
                input %>%
                normalize_na()


        # Parse the vectors that are strings
        input3 <-
                input2 %>%
                dplyr::select(all_of(target_col)) %>%
                unlist() %>%
                unname() %>%
                unique()

        # Getting words
        input4 <-
                strsplit(input3, split = "[(]{1}|[)]{1}|[,]{1}") %>%
                unlist() %>%
                trimws("both") %>%
                centipede::no_blank() %>%
                stringr::str_remove_all("[']{1}|[?]{1}") %>%
                unique()

        # Filtering for only words that meeting the character limit
        input5 <- input4[nchar(input4) >= source_skip_nchar]



        cat("\n")


        for (i in 1:length(input5)) {

                input_word <- input5[i]

                # Unlike the other caching routines, this one only runs if there is a space, justifying a str_as_vector search
                if (any(grepl(pattern = " ", input_word) == TRUE)) {

                        secretary::typewrite(crayon::bold("Phrase:"), input_word)

                        x <- chariot::query_string_as_vector(input_word)

                }

                typewrite_percent_progress(i = i,
                                           input5)
        }
}

