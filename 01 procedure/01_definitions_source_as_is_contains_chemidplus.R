##############################################
### INTRODUCTION
##############################################
# Source function and vars
source("startup.R")

# Set search parameters
type <- "contains"
sleep_secs <- 20 # Number of seconds between API calls to prevent 503 messages
# Create output variables
new_col_name <- "ChemiDPlus Contains"
path_to_output_fn <- create_path_to_output_fn()

# Temporary stop if the output file exists
brake_if_output_exists()

##############################################
### PRE-PROCEDURE
##############################################
# Read input
input <- read_input()
target_col <- source_col

# Normalize NAs because some are "NA" and others are true NA
input2 <-
        input %>%
        normalize_na()

# Create final input object to join with final output object before export
final_input <- input2

# Select only relevant columns
input3 <-
        input2 %>%
        dplyr::select(routine_id, all_of(target_col), !!terminal_col) %>%
        dplyr::filter_at(vars(!!terminal_col), any_vars(is.na(.)))


# If any routine_id is NA
qa1 <-  input3$routine_id[is.na(input3$routine_id)]

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
secretary::typewrite(crayon::bold("Estimated time to completion:"), (sleep_secs*nrow(input3))/60, "minutes")


for (i in 1:nrow(input3)) {

        # Per row
        input_row <- input3 %>%
                        dplyr::filter(row_number() == i)


        # Release single row df as r objects
        rubix::release_df(input_row)


        input_concept <- get(target_col)
        output_concept <- get(terminal_col)
        input_routine_id <- routine_id

        secretary::typewrite_bold("Starting", i)

        resultset <-
                testthat::capture_warning(synonyms_from_chemidplus(phrase = input_concept, type = type))

        if (!is.null(resultset)) {
                output[[i]] <- resultset
                names(output)[i] <- input_routine_id
        }

        # Sleeping to prevent'503 Service Unavailable'
        secretary::typewrite_bold(i, "done")
        secretary::typewrite_bold("Sleeping...")
        Sys.sleep(sleep_secs)

        typewrite_progress(i = i, input3)
        rm(list = colnames(input_row))
        rm(input_row)
}

# Remove NULLS
output2 <- output %>%
                purrr::keep(~!is.null(.)) %>%
                #Convert warning message of class warning to character
                rubix::map_names_set(as.character) %>%
                # Combine 1:many results into a vector string to preserve the 1:1 relationship with routine_id
                rubix::map_names_set(function(x) ifelse(length(x)>1, cave::vector_to_string(x), x))


final_output <- output %>%
                        dplyr::bind_rows(.id = "routine_id")





# Aggregating the search result columns to the original routine_id
final_output2 <-
final_output %>%
rubix::group_by_unique_aggregate(routine_id,
                                 agg.col = all_of(new_col_name),
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

typewrite_complete()
