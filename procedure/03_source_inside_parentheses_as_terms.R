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
type <- c("exact") # Can be either or both of c("exact", "like"), but "like" is not advised because the return can be a massive number of rows

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

                                        } else {

                                                output[[i]] <- NA

                                        }

                                } else {

                                        output[[i]] <- NA

                                }
                        } else {

                                output[[i]] <- NA

                        }
                } else {

                        output[[i]] <- NA

                }
                names(output)[i] <- input_routine_id[i]
        }



# Binding all search results with the search term preserved
for (i in 1:length(output)) {

        if (is.list(output[[i]])) {
                if (length(output[[i]]) == 0) {

                        output[[i]] <- NA

                }
        }

        if (!is.logical(output[[i]]) && (length(output[[i]]) != 0)) {

                new_col_name <- "Source Terms Inside Parentheses Terms"

                #secretary::press_enter()

                output[[i]] <- dplyr::bind_rows(output[[i]],
                                                .id = "Source Term") %>%
                                        chariot::merge_concepts(into = `Concept`) %>%
                                dplyr::select(!!new_col_name := `Concept`)

        }
}


# For concepts that are already mapped or do not have an entry in the target column, creating a blank entry for the new columns
for (i in 1:length(output)) {

        if (is.logical(output[[i]])) {

                        output[[i]] <-
                                        rubix::vector_to_tibble(output[[i]],
                                                                !!new_col_name)

        }
}

# Binding back to routine_id
output <-
        output %>%
        dplyr::bind_rows(.id = "routine_id")

# Adding to final output
final_output <- output %>%
                                dplyr::distinct()



# Aggregating the search result columns to the original routine_id
final_output2 <-
        final_output %>%
        rubix::group_by_unique_aggregate(routine_id,
                                         agg.col = contains("Source"),
                                         collapse = "\n") %>%
        dplyr::mutate_at(vars(!routine_id), substr, 1, 25000)

# If the search type is both exact and like, would need to reduce the list with left_join so each routine_id will have both searches associated with it in the dataframe
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
