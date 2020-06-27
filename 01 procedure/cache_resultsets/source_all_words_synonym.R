if (interactive()) {
        clean_env()
        source('setup.R')
                # Read input
                input <- read_input()
                target_col <- source_col

                # Search Settings
                types <- c("exact", "like") # Can be either or both of c("exact", "like"), but "like" is not advised because the return can be a massive number of rows


                # Normalize NAs because some are "NA" and others are true NA
                input2 <-
                        input %>%
                        normalize_na()

                # Creating final input object to join with final output object
                final_input <- input2

                # Parse the vectors that are strings
                input3 <-
                        input2 %>%
                        dplyr::select(all_of(target_col)) %>%
                        unlist() %>%
                        unname() %>%
                        unique()

                # Getting words
                input4 <-
                strsplit(input3, split = " |[(]{1}|[)]{1}|[,]{1}") %>%
                        unlist() %>%
                        trimws("both") %>%
                        centipede::no_blank() %>%
                        stringr::str_remove_all("[']{1}|[?]{1}") %>%
                        unique()

                # Filtering for only words that meeting the character limit
                input5 <- input4[nchar(input4) >= source_skip_nchar]


                while (length(types) > 0) {
                        type <- types[1]

                        secretary::typewrite("Starting", type, "search.")
                        Sys.sleep(1)
                        cat("\n")


                        for (i in 1:length(input5)) {

                                input_word <- input5[i]

                                secretary::typewrite(crayon::bold("Phrase:"), input_word)

                                x <- chariot::query_phrase_synonym(phrase = input_word,
                                                                   type = type)

                                typewrite_percent_progress(i = i,
                                                           input5)
                        }

                        types <- types[-1]
                }

} else {
        secretary::typewrite("Starting non-interactive session.")
        source('/Users/patelm9/GitHub/omop_mapping/procedure/startup.R')

        # Read input
        input <- read_input()
        target_col <- source_col

        # Search Settings
        types <- c("exact", "like") # Can be either or both of c("exact", "like"), but "like" is not advised because the return can be a massive number of rows


        # Normalize NAs because some are "NA" and others are true NA
        input2 <-
                input %>%
                normalize_na()

        # Creating final input object to join with final output object
        final_input <- input2

        # Parse the vectors that are strings
        input3 <-
                input2 %>%
                dplyr::select(all_of(target_col)) %>%
                unlist() %>%
                unname() %>%
                unique()

        # Getting words
        input4 <-
                strsplit(input3, split = " |[(]{1}|[)]{1}|[,]{1}") %>%
                unlist() %>%
                trimws("both") %>%
                centipede::no_blank() %>%
                stringr::str_remove_all("[']{1}|[?]{1}") %>%
                unique()

        # Filtering for only words that meeting the character limit
        input5 <- input4[nchar(input4) >= source_skip_nchar]


        while (length(types) > 0) {
                type <- types[1]

                secretary::typewrite("Starting", type, "search.")
                Sys.sleep(1)
                cat("\n")


                for (i in 1:length(input5)) {

                        input_word <- input5[i]

                        secretary::typewrite(crayon::bold("Phrase:"), input_word)

                        x <- chariot::query_phrase_synonym(phrase = input_word,
                                                           type = type)

                        typewrite_percent_progress(i = i,
                                                   input5)
                }

                types <- types[-1]
        }
}