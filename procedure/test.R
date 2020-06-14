rm(list = ls())
source('setup.R')

path_to_output_fn <- paste0(stringr::str_replace(path_to_input_fn, "(^.*?[/]{1})(.*?)([.]{1}csv$)", "output/\\2_"), cave::strip_fn(cave::present_script_path()), ".csv")

if (file.exists(path_to_output_fn)) {
        secretary::typewrite_warning(path_to_output_fn, "already exists and will be overwritten.")
        secretary::press_enter()
}


input2 <- read_workfile(routine = "01_search_source",
                        STANDARD_LIBRARY == "TRUE")
target_col <- source_col

# Search Settings
types <- c("exact", "like") # Can be either or both of c("exact", "like"), but "like" is not advised because the return can be a massive number of rows

# Add rowid
input3 <-
        input2 %>%
        #tibble::rowid_to_column("routine_id") %>%
        dplyr::mutate_all(as.character)

# Normalize NAs because some are "NA" and others are true NA
input4 <-
        input3 %>%
        dplyr::mutate_all(stringr::str_replace_all, "^NA$", "") %>%
        dplyr::mutate_all(na_if, "")

# Creating final input object to join with final output object
final_input <- input4

# Parse the vectors that are strings
input_vector <-
        input4 %>%
        dplyr::select(all_of(target_col)) %>%
        unlist() %>%
        #purrr::map(cave::string_to_vector) %>%
        purrr::set_names(input4$routine_id)

input_fact_concept <-
        input4 %>%
        dplyr::select(!!terminal_col) %>%
        unlist()

input_routine_id <- input4$routine_id

qa1 <- length(input_fact_concept)-length(input_vector)

if (qa1 != 0) {
        stop("x and y are not equal in length.")
}


# Creating final_output object: named list by search type
final_output <- list()
for (i in 1:length(types)) {
        final_output[[i]] <- tibble()
}
names(final_output) <- types


while (length(types) > 0) {
        type <- types[1]
        output <- list()
        for (i in 1:length(input_vector)) {

                if (!is.logical(input_vector[[i]]) && !(input_vector[[i]] %in% c(NA, "NA"))) {

                        if (is.na(input_fact_concept[i])) {

                                if (grepl("[(]{1}", input_vector[i])) {



                                        first_word <- stringr::str_replace_all(input_vector[i],
                                                                               pattern = "(^.*?)([(]{1})(.*?)([)]{1}.*$)",
                                                                               replacement = "\\3") %>%
                                                trimws(which = "both")


                                        if (grepl("[,]", first_word)) {

                                                print(first_word)
                                                #secretary::press_enter()

                                                Words <- strsplit(first_word, split = ",") %>%
                                                        unlist() %>%
                                                        trimws("both")

                                                output[[i]] <- list()

                                                for (j in 1:length(Words)) {
                                                        Word <- Words[j]

                                                        if (nchar(Word) > source_skip_nchar) {
                                                                print(Word)
                                                                output[[i]][[j]] <-
                                                                        chariot::query_phrase(Word, type = type) %>%
                                                                        filter_for_settings() %>%
                                                                        # dplyr::mutate_at(vars(concept_name), function(x) str_replace_all(x, "[,]{1} ", " ")) %>%
                                                                        rubix::arrange_by_nchar(concept_name) %>%
                                                                        #filtering for only 250 lines (max in Excel)
                                                                        dplyr::slice(1:250) %>%
                                                                        rubix::mutate_all_rm_multibyte_chars()
                                                                names(output[[i]])[j] <- Word
                                                        }

                                                }

                                        }
                                }
                        }
                }
        }
}

