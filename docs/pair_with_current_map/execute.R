input <- read_workfile(routine = "execute.R")

# Getting mapped concepts
input2a_mapped <-
        input %>%
        dplyr::select(routine_id,
                        !!source_col,
                      `MSK Concept Type`,
                      !!terminal_col) %>%
        dplyr::mutate_all(as.character) %>%
        dplyr::mutate_all(str_replace_na, NA_character_) %>%
        dplyr::filter_all(all_vars(!is.na(.))) %>%
        dplyr::distinct()

# Create a second version of input2a_mapped that does not select out columns in order to bind for the final output later
final_input2a_mapped <-
        input %>%
        dplyr::mutate_all(as.character) %>%
        dplyr::mutate_all(str_replace_na, NA_character_) %>%
        dplyr::filter_at(vars(routine_id,
                              !!source_col,
                              `MSK Concept Type`,
                              !!terminal_col), all_vars(!is.na(.))) %>%
        dplyr::distinct()

qa1 <- nrow(final_input2a_mapped) - nrow(input2a_mapped)

if (qa1 != 0) {
        stop("final_input2a_mapped and input2a_mapped do not have the same row count.")
}

# Getting the difference
input2b_unmapped <-
        input[!(input$routine_id %in% input2a_mapped$routine_id),]


# QA
qa1 <- all(c(input2a_mapped$routine_id, input2b_unmapped$routine_id) %in% input$routine_id)


# If a CONCEPT joins with an unmapped CONCEPT it is included in an appended "Mapped" column in the output
input2b_unmapped2 <-
        input2b_unmapped %>%
        # Full join to make sure all of the mapped Concept Types (such as Observation Group are also included in the output)
        dplyr::full_join(input2a_mapped %>%
                                 # Remove routine_id to avoid collisions with unmapped routine_id (used earlier to reconcile that all rows are accounted for)
                                 dplyr::select(-routine_id),
                         by = c(source_col,
                                "MSK Concept Type"),
                         suffix = c("", " Mapped")) %>%
        # Due to full join, there are some NAs introduced and they need to be filtered out
        dplyr::filter(!is.na(routine_id)) %>%
        dplyr::distinct()

#Are all the routine_ids in input2b_unmapped2 in input2b_unmapped?
qa2 <- all(input2b_unmapped2$routine_id %in% input2b_unmapped$routine_id)

if (qa2 == FALSE) {

        stop("Not all routine_ids made it to the input2b_unmapped2.")

}

# Combine unmapped and mapped to reform output
output <- dplyr::bind_rows(final_input2a_mapped,
                           input2b_unmapped2)

# QA: cannot do a simple row count because the full join to the unmapped concepts was 1:many due to Concept Type also being included so output is based on routine_id alone
qa3 <- all(output$routine_id %in% input$routine_id)

if (qa3 == FALSE) {

        stop("Not all routine_ids in the input made it to the output.")

}

final_routine_output <- output

broca::copy_to_clipboard(final_routine_output)
