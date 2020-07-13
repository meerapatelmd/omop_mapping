# Source function and vars
if (interactive()) {
                source("startup.R")
}

# Set search parameters
type <- "string"

# Create output variables
new_col_name <- "Source String"
path_to_output_fn <- create_path_to_output_fn()


##############################################
### INTRODUCTION
##############################################

# Temporary stop if the output file exists
brake_if_output_exists()

##############################################
### PRE-PROCEDURE
##############################################
# Read input
input <- read_input()
target_col <- source_col


# Create final input object to join with final output object before export
final_input <- input

# Select only relevant columns
input2 <-
        input %>%
        dplyr::select(routine_id, !!target_col, !!terminal_col) %>%
        dplyr::filter_at(vars(!!target_col), any_vars(!is.na(.))) %>%
        dplyr::filter_at(vars(!!terminal_col), any_vars(is.na(.)))


# If any routine_id is NA
qa1 <-  input2$routine_id[is.na(input2$routine_id)]

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

input3 <- input2
# For each concept
for (i in 1:nrow(input3)) {

                        # Isolate concept
                        input_row <- input3 %>%
                                        dplyr::filter(row_number() == i)


                        # Convert concept info into objects
                        rubix::release_df(input_row)

                        input_concept <- get(target_col)
                        output_concept <- get(terminal_col)
                        input_routine_id <- routine_id


                        # Search if input concept is above the character number threshold
                        if (nchar(input_concept) > source_skip_nchar) {


                                output[[i]] <-
                                        query_phrase_in_athena(phrase = input_concept, type = type) %>%
                                        chariot::merge_concepts(into = `Concept`) %>%
                                        dplyr::select(!!new_col_name := `Concept`)

                                names(output)[[i]] <- input_routine_id

                                typewrite_progress(i = i, input3)


                        }
                        rm(list = colnames(input_row))
                        rm(input_row)
}

final_output <-
        output %>%
        filter_out_null() %>%
        dplyr::bind_rows(.id = "routine_id")


if (nrow(final_output)) {
        # Aggregating the search result columns to the routine_id
        final_output2 <-
                final_output %>%
                rubix::group_by_unique_aggregate(routine_id,
                                                 agg.col = !!new_col_name,
                                                 collapse = "\n") %>%
                dplyr::mutate_at(vars(!routine_id), substr, 1, 25000)

        # Join with final_input object
        final_routine_output <-
                dplyr::left_join(final_input,
                                 final_output2)


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


} else {
        broca::simply_write_csv(x = final_output,
                                file = path_to_output_fn)
}

typewrite_complete()