rm(list = ls())
source('startup.R')

# It is important that the read_input function isn't used to get the true input because read_input may have applied additional_filters so the entire input is not represented
origin <-
        broca::simply_read_csv(inputPath)
origin_colnames <- colnames(origin)[!(colnames(origin) %in% "routine_id")]

# Get all the outputs from this run based on the input_fn pattern
all_outputs <-
list.files(.PATHS$OUTPUT, full.names = TRUE, pattern = cave::strip_fn(inputFile)) %>%
       rubix::map_names_set(broca::simply_read_csv) %>%
        purrr::keep(~nrow(.)>0) %>%
        purrr::map(~select(., -any_of(origin_colnames)))


# QA on output
## 1. Are all the new column names produced by each subprocess unique so that do not accidentally join with each other?
qa1 <-
all_outputs %>%
        # Isolate column names not found in the origin file
        purrr::map(function(x) colnames(x)[!(colnames(x) %in% origin_colnames)]) %>%
        # Convert to dataframe to bind into a dataframe and perform unique counts for each new column name
        purrr::map(rubix::vector_to_tibble, new_col = New_Colname) %>%
        dplyr::bind_rows(.id = "Source File") %>%
        # Getting counts for each New_Colname
        rubix::summarize_grouped_n(New_Colname) %>%

        # Filter for any counts greater than 1
        dplyr::filter(n > 1)


if (nrow(qa1) > 0) {
        qa1_a <-
                all_outputs %>%
                # Isolate column names not found in the origin file
                purrr::map(function(x) colnames(x)[colnames(x) %in% c(qa1$New_Colname)])

        stop("Duplicate new columns found in some output csvs. Please see qa1_a object.")
}


# Since all New_Columns are unique, all outputs are left joined on the origin column names
all_outputs2 <-
        all_outputs %>%
        purrr::reduce(left_join, by = "routine_id")

# The completely left-joined outputs are then joined with the origin to include all the original concepts, which is especially important in cases with the additional_filters object was created in this run.
finalCollection <-
        dplyr::left_join(origin,
                         all_outputs2,
                         by = "routine_id")



## 2. Are all routine_ids in the origin represented in the output?
qa2 <- all(origin$routine_id %in% finalCollection$routine_id)
if (qa2 == FALSE) {
        stop("all origin routine_ids are not in finalCollection.")
}

## 3. Are there any duplicates introduced?
qa3 <- nrow(origin)-nrow(finalCollection)
if (qa3 != 0) {
        stop("The row count between origin and finalCollection do not match.")
}


#broca::copy_to_clipboard(final_output)
broca::view_as_csv(finalCollection)
