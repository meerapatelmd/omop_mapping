rm(list = ls())
source('setup.R')

path_to_output_fn <- paste0(stringr::str_replace(path_to_input_fn, "(^.*?[/]{1})(.*?)([.]{1}csv$)", "output/\\2_"), cave::strip_fn(cave::present_script_path()), ".csv")

if (file.exists(path_to_output_fn)) {
        secretary::typewrite_warning(path_to_output_fn, "already exists and will be overwritten.")
        secretary::press_enter()
}


input2 <- read_workfile(routine = "02_search_source_str_as_vector")
target_col <- source_col

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

                                if (nchar(input_vector[[i]]) > source_skip_nchar) {

                                #secretary::press_enter()
                                        print(i)
                                output[[i]] <- chariot::query_string_as_vector(string = input_vector[i]) %>%
                                        filter_for_settings() %>%
                                        #Delete commas to prevent issues with delimiting in Excel
                                        #dplyr::mutate_at(vars(concept_name), function(x) str_replace_all(x, "[,]{1} ", " ")) %>%
                                        #Convention in RxNorm Extension that causes similar problems in delimiting
                                        #dplyr::mutate_at(vars(concept_name), function(x) str_remove_all(x, "\"|\'")) %>%
                                        rubix::arrange_by_nchar(concept_name) %>%
                                        #filtering for only 250 lines (max in Excel)
                                        dplyr::slice(1:250)
                                #names(output)[i] <- names(input_vector)[i]

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

        if (!is.logical(output[[i]])) {

                new_col_name <- "Source String as Vector"

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

        } else if (nrow(output[[i]]) == 0) {

                        output[[i]] <-
                                rubix::vector_to_tibble(NA,
                                                        !!new_col_name)
        }
}


# Adding to final output
final_output <- output %>%
                        dplyr::bind_rows(.id = "routine_id")


# Aggregating the search result columns to the original routine_id
final_output2 <-
        final_output %>%
        rubix::group_by_unique_aggregate(routine_id,
                                                                                 agg.col = contains("Source"),
                                                                                 collapse = "\n")


final_output2$`Source String as Vector` <-
        final_output2$`Source String as Vector` %>%
        purrr::map(substr, 1, 25000) %>%
        unlist()


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

