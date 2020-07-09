#clean_env()
#source('startup.R')

type <- "exact"

# Read input
input <- read_raw_input()
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
        dplyr::select(all_of(target_col)) %>%
        unlist() %>%
        unname() %>%
        unique()

# Getting words
input4 <-
        input3 %>%
        strsplit(split = word_split) %>%
        unlist() %>%
        trimws() %>%
        centipede::no_blank() %>%
        stringr::str_remove_all("[']{1}|[?]{1}") %>%
        unique()

# Filtering for only words that meeting the character limit
input5 <- input4[nchar(input4) >= source_skip_nchar]

# Scope current cache
cached_scope <-
input5 %>%
        rubix::map_names_set(chariot::is_phrase_query_cached, type = type)

cached_count <-
        cached_scope %>%
        purrr::keep(~.==TRUE)

input6 <-
        cached_scope %>%
        purrr::keep(~.!=TRUE) %>%
        names()

secretary::typewrite("Starting", type, "search.")
secretary::typewrite(crayon::bold("Total:", length(cached_scope)))
secretary::typewrite(crayon::bold("Cached:", length(cached_count)), tabs = 1)
secretary::typewrite(crayon::bold("To Do:", length(input6)), tabs = 1)
Sys.sleep(1)
cat("\n")


for (i in length(input6):1) {

        input_word <- input6[i]

        secretary::typewrite(crayon::bold("Phrase:"), input_word)

        x <- query_phrase_in_athena(phrase = input_word,
                                    type = type)

        typewrite_percent_progress(i = i,
                                   input6)

}

