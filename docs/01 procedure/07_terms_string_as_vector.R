rm(list = ls())
source('setup.R')

path_to_output_fn <- paste0(stringr::str_replace(path_to_input_fn, "(^.*?[/]{1})(.*?)([.]{1}csv$)", "output/\\2_"), cave::strip_fn(cave::present_script_path()), ".csv")

if (file.exists(path_to_output_fn)) {
        secretary::typewrite_warning(path_to_output_fn, "already exists and will be overwritten.")
        secretary::press_enter()
}

# Search Settings
##Automate covid dd permissible values
new_col_name <- "Search Term String as Vector"
target_col <- term_col
input2 <- read_workfile(routine = "04_search_terms")

# Add rowid
input3 <-
        input2 %>%
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
        purrr::map(cave::string_to_vector) %>%
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
        output[[i]] <- list()
        if (!is.logical(input_vector[[i]])) {

                        if (is.na(input_fact_concept[i])) {

                                for (j in 1:length(input_vector[[i]])) {

                                        output[[i]][[j]] <-
                                                chariot::query_string_as_vector(string = input_vector[[i]][j]) %>%
                                                filter_for_settings() %>%
                                                rubix::arrange_by_nchar(concept_name) %>%
                                                #filtering for only 250 lines (max in Excel)
                                                dplyr::slice(1:250) %>%
                                                dplyr::mutate_all(as.character)

                                }

                                names(output[[i]]) <- input_vector[[i]]

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

        if (!is.logical(output[[i]])) {



                output[[i]] <- dplyr::bind_rows(output[[i]],
                                                .id = "Search Term") %>%
                                        chariot::merge_concepts(into = `Concept`) %>%
                                        rubix::arrange_by_nchar(nchar_col = `Concept`) %>%
                                dplyr::transmute(!!new_col_name := paste0(`Search Term`, ": ", `Concept`))

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
        rubix::group_by_aggregate(routine_id,
                                         agg.col = `Search Term String as Vector`,
                                         collapse = "\n") %>%
        dplyr::mutate_at(vars(!routine_id), substr, 1, 25000)




# Join with final_input object
final_routine_output <-
        dplyr::left_join(final_input,
                         final_output2)


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
