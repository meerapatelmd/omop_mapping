
source("startup.R")
type <- "like"
new_col_name <- "CancerGov Like"
path_to_output_fn <- create_path_to_output_fn()
# Temporary stop if the output file exists
brake_if_output_exists()


# Read input
input <- read_input()
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
        dplyr::select(routine_id, all_of(target_col), !!terminal_col) %>%
        dplyr::filter_at(vars(!!terminal_col), any_vars(is.na(.)))

if (nrow(input3) == 0) {

        stop("no rows in input3")
}


# If any routine_id is NA
qa1 <-  input3$routine_id[is.na(input3$routine_id)]

if (length(qa1) > 0) {

        stop("routine_ids present in input are missing in input3")

}



output <- list()
for (i in 1:nrow(input3)) {

                input_row <- input3 %>%
                                dplyr::filter(row_number() == i)


                rubix::release_df(input_row)


                input_concept <- get(target_col)
                output_concept <- get(terminal_col)
                input_routine_id <- routine_id

                input_words <-
                        strsplit(input_concept, split = word_split) %>%
                        unlist() %>%
                        unique()


                # Searching
                output[[i]] <-
                        input_words %>%
                        rubix::map_names_set(function(x) chariot::query_athena(paste0("SELECT * FROM cancergov_drug_name WHERE drug LIKE '%", x, "%' OR name LIKE '%", x, "%'"))) %>%
                        dplyr::bind_rows() %>%
                        dplyr::distinct()

                # If the search returns results, creating a TERM style string, otherwise, return a "NOT Found" status
                if (nrow(output[[i]]) > 0) {
                        print(length(unlist(output[[i]])))
                        secretary::press_enter()
                        output[[i]] <-
                        unlist(output[[i]]) %>%
                                cave::vector_to_string() %>%
                                rubix::vector_to_tibble(new_col = !!new_col_name)
                } else {

                        output[[i]] <-
                                tibble(!!new_col_name := "Not Found")

                }

                names(output)[i] <- input_routine_id
                typewrite_progress(i = i, input3)
                rm(list = colnames(input_row))
                rm(input_row)
}


final_output <- output %>%
                        dplyr::bind_rows(.id = "routine_id")


# Doing final bind for final_routine_output
final_routine_output <-
        dplyr::left_join(final_input,
                         final_output)

broca::simply_write_csv(final_routine_output,
                        path_to_output_fn)
