##############################################
### INTRODUCTION
##############################################
# Clear env
clean_env()

# Source function and vars
source("startup.R")

# Set search parameters
type <- "qa"

# Create output variables
new_col_name <- "QA Source Each Word Count"
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
        dplyr::select(routine_id, all_of(target_col), !!terminal_col, !!attribute_col) %>%
        tidyr::separate_rows(!!attribute_col,
                             sep = "\n") %>%
        mutate_all(trimws) %>%
        rubix::normalize_all_to_na() %>%
        filter_at(vars(!!attribute_col), all_vars(!is.na(.)))


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

output <-
input3 %>%
        dplyr::select(!!target_col) %>%
        unlist() %>%
        purrr::map(~strsplit(., split = word_split)) %>%
        purrr::map(unlist) %>%
        purrr::map(centipede::no_blank) %>%
        purrr::map(centipede::no_na) %>%
        purrr::map(length) %>%
        purrr::set_names(input3$routine_id)

output2 <-
        tibble(routine_id = names(output),
               !!new_col_name := unlist(output)) %>%
        dplyr::distinct()

# Get component count
output_b <-
        input3 %>%
        dplyr::group_by(routine_id) %>%
        dplyr::summarise(`Component Count` = length(unique(Component))) %>%
        ungroup() %>%
        distinct()

# Combining
final_output <-
        dplyr::full_join(output2,
                         output_b)


# Join with final_input object
final_routine_output <-
dplyr::left_join(final_input,
                 final_output)


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


