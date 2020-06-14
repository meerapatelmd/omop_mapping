
clean_env()

path_to_output_fn <- paste0(stringr::str_replace(path_to_input_fn, "(^.*?[/]{1})(.*?)([.]{1}csv$)", "output/\\2_"), cave::strip_fn(cave::present_script_path()), ".csv")

if (file.exists(path_to_output_fn)) {
        secretary::typewrite_warning(path_to_output_fn, "already exists and will be overwritten.")
        secretary::press_enter()
}


input1 <- read_workfile(routine = "01_search_source")
target_col <- source_col

# Search Types
types <- c("exact", "like")

input2 <-
        input1 %>%
        dplyr::mutate_all(as.character)

# Normalize NAs because some are "NA" and others are true NA
# input3 <-
#         input2 %>%
#         dplyr::mutate_all(stringr::str_replace_all, "^NA$", "") %>%
#         dplyr::mutate_all(na_if, "")


# Creating final input object to join with final output object
final_input <- input2

# Parse the vectors that are strings
input_vector_df <-
        final_input %>%
        dplyr::select(all_of(target_col), !!terminal_col, routine_id)



# Creating final_output object: named list by search type
final_output <- list()
for (i in 1:length(types)) {
        final_output[[i]] <- tibble()
}
names(final_output) <- types


while (length(types) > 0) {
        type <- types[1]
        output <- list()
        for (i in 1:nrow(input_vector_df)) {
                input_vector <-
                input_vector_df %>%
                        dplyr::filter(row_number() == i)

                rubix::release_df(input_vector)

                input_phrase <- get(target_col)
                terminal_value <- get(terminal_col)

                if (!is.logical(input_phrase) && !(input_phrase %in% c(NA, "NA"))) {

                                if (is.na(terminal_value)) {

                                        # Character count threshold
                                        if (nchar(input_phrase) > source_skip_nchar) {

                                                print(i)

                                        output[[i]] <-
                                                chariot::query_phrase(input_phrase, type = type) %>%
                                                filter_for_settings() %>%
                                                rubix::arrange_by_nchar(concept_name) %>%
                                                filter_max_250()

                                        #names(output[[i]]) <- input_vector[[i]]
                                        } else {
                                                output[[i]] <- NA_character_
                                        }



                                } else {
                                        output[[i]] <- NA_character_
                                }

                } else {
                        output[[i]] <- NA_character_
                }

                names(output)[i] <- routine_id

        }


        # Binding all search results with the search term preserved
        for (i in 1:length(output)) {

                if (!is.logical(output[[i]])) {

                        if (type %in% c("exact")) {
                                new_col_name <- "Source Exact"
                        } else {
                                new_col_name <- "Source Like"
                        }

                        output[[i]] <- dplyr::bind_rows(output[[i]],
                                                        .id = "Source") %>%
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
        final_output[[type]] <- output %>%
                                        dplyr::distinct()

        # Removing first type
        types <- types[-1]
}

# Aggregating the search result columns to the original routine_id
final_output2 <-
        final_output %>%
        rubix::map_names_set(function(x) x %>%
                                                rubix::group_by_unique_aggregate(routine_id,
                                                                                 agg.col = contains("Source"),
                                                                                 collapse = "\n")) %>%
        purrr::map(function(x) x %>%
                                                        dplyr::mutate_at(vars(!routine_id), substr, 1, 25000))

# If the search type is both exact and like, would need to reduce the list with left_join so each routine_id will have both searches associated with it in the dataframe
final_output3 <-
        final_output2  %>%
        purrr::reduce(left_join)


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