##############################################
### INTRODUCTION
##############################################
# Clear env
clean_env()

# Source function and vars
source("startup.R")

# Set search parameters
type <- "Pivot"

# Create output variables
new_col_name <- "Attribute Relationship"
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
        dplyr::select(routine_id, all_of(target_col), !!attribute_col)


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



for (i in 1:nrow(input3)) {

        # Per row
        input_row <- input3 %>%
                dplyr::filter(row_number() == i)


        if (!(is.na(input_row %>%
                    dplyr::select(!!attribute_col) %>%
                    unlist() %>%
                    unname()))) {

                        input_row2 <-
                                input_row %>%
                                tidyr::separate_rows(!!attribute_col,
                                                     sep = "\n") %>%
                                dplyr::mutate_all(trimws) %>%
                                tidyr::extract(col = !!attribute_col,
                                               into = c("SearchPrefix", "Attribute"),
                                               regex = "(^.*?[:]{1}[ ]{0,1})(.*$)") %>%
                                dplyr::select(-SearchPrefix) %>%
                                chariot::unmerge_concepts(concept_col = Attribute) %>%
                                dplyr::select(routine_id,
                                              !!target_col,
                                              concept_name)

                        output[[i]] <-
                                chariot::hemonc_monotherapy(input_row2$concept_name)

                        names(output)[i] <- input_row2$routine_id
                        typewrite_percent_progress(i = i, input = input3)

                        rm(input_row)
                        rm(input_row2)
                        gc()
                        Sys.sleep(1)
        }
}

output <- output[names(output) %>% centipede::no_na()]

output2 <-
        output %>%
        purrr::keep(~nrow(.)>0)

output3 <-
        output2 %>%
        dplyr::bind_rows(.id = "routine_id")

output4 <-
        output3 %>%
        chariot::merge_concepts(into = "MonotherapyConcept")

output5 <-
        output4 %>%
        rubix::group_by_unique_aggregate(routine_id,
                                         agg.col = MonotherapyConcept,
                                         collapse = "\n")

output4 <-
        list(output3a,
             output3b) %>%
        transpose()

output5 <-
        output4 %>%
        rubix::map_names_set(function(x) bind_cols(x))


final_routine_output <-
        dplyr::left_join(final_input,
                         output5)


                             # %>%
                             #                                                    dplyr::select(-concept_id_1) %>%
                             #                                                    dplyr::select_if(function(x) all(!is.na(x))))
                             #
