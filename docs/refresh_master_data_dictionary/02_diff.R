# Assessing the difference between the originating data dictionary from today and the previous one based on the input subdir dates

# Identifying the column where the difference is
## Full join between the 2 data dictionaries to get all removals, additions, and changes between the 2
output_02 <-
        dplyr::full_join(current_dd,
                         prior_dd,
                         by = c("FORM", "VARIABLE"),
                         suffix = c(".CURRENT", ".PRIOR"))

## Pivoting joined data to isolate differences by column
output_02_a_changed <-

        output_02 %>%

        # Add identifier for pivot
        tibble::rowid_to_column() %>%

        # Casting dataframe by Field
        tidyr::pivot_longer(cols = FIELD_TYPE.CURRENT:last_col()) %>%

        # Separate data elements that are current from the prior record to be able to filter for differences between the two
        tidyr::separate(col = name,
                        into = c("Changed Field", "Version"),
                        sep = "[.]{1}") %>%
        tidyr::pivot_wider(names_from = Version,
                           values_from = value) %>%

        # Filtering for the difference between the current and prior
        dplyr::filter(CURRENT != PRIOR) %>%

        # Renaming fields to clearer labels
        dplyr::rename("Changed From" = PRIOR,
                      "Changed To" = CURRENT) %>%

        # Rejoin to the current Raw Data Dictionary from prior master for the raw_id
        dplyr::left_join(prior_master$`Current Raw Data Dictionary` %>%
                                 dplyr::select(-starts_with("Changed "))) %>%

        # Removing onyl old information from the current join
        dplyr::select(-(FIELD_TYPE:last_col()), -rowid) %>%

        # Joining again with the current dd to have the most updated version of the variable
        dplyr::left_join(current_dd) %>%
        dplyr::distinct() %>%
        dplyr::mutate(changed_date = Sys.Date()) %>%
        dplyr::select(-any_of(c("added_date", "removed_date")))

qa1 <- output_02_a_changed %>%
                dplyr::filter(is.na(raw_id))

if (nrow(qa1) != 0) {
        stop("raw_ids are missing in the changed concepts")
}

# New concepts: concepts that are in the current version but not in the previous version
output_02_b_new <-
        output_02 %>%
        tibble::rowid_to_column() %>%
        tidyr::pivot_longer(cols = FIELD_TYPE.CURRENT:last_col()) %>%
        tidyr::separate(col = name,
                        into = c("Changed Field", "Version"),
                        sep = "[.]{1}") %>%
        tidyr::pivot_wider(names_from = Version,
                           values_from = value) %>%
        dplyr::filter(is.na(PRIOR), !is.na(CURRENT)) %>%
        tidyr::pivot_wider(names_from = `Changed Field`,
                           values_from = CURRENT) %>%
        dplyr::select(-PRIOR, -rowid) %>%
        dplyr::mutate(added_date = Sys.Date()) %>%
        dplyr::select(-any_of(c("changed_date", "removed_date")))

# Current concepts: concepts that are in the prior version but not in the current version
output_02_c_deleted <-
        output_02 %>%
        tibble::rowid_to_column() %>%
        tidyr::pivot_longer(cols = FIELD_TYPE.CURRENT:last_col()) %>%
        tidyr::separate(col = name,
                        into = c("Changed Field", "Version"),
                        sep = "[.]{1}") %>%
        tidyr::pivot_wider(names_from = Version,
                           values_from = value) %>%
        dplyr::filter(!is.na(PRIOR), is.na(CURRENT)) %>%
        tidyr::pivot_wider(names_from = `Changed Field`,
                           values_from = PRIOR) %>%
        dplyr::filter(!is.na(FIELD_LABEL)) %>%
        dplyr::select(-CURRENT, -rowid) %>%
        # Add raw_id
        dplyr::left_join(prior_master$`Current Raw Data Dictionary` %>%
                                 dplyr::select(raw_id, FORM, VARIABLE)) %>%
        dplyr::mutate(removed_date = Sys.Date()) %>%
        dplyr::select(-any_of(c("added_date", "changed_date")))

qa3 <-
        output_02_c_deleted %>%
        dplyr::filter(is.na(raw_id))

if (nrow(qa3) != 0) {
        stop("raw_ids are missing in the deleted concepts")
}

# Creating final output first with the current raw data dictionary in the master with the concepts that already have raw_ids (changed or deleted)
final_output_02 <-
        dplyr::bind_rows(
                         output_02_a_changed,
                         output_02_c_deleted) %>%
        dplyr::mutate(raw_id = as.integer(raw_id))

# The remaining identified concepts are new and require a new raw_id
## Get maximum raw_id
max_raw_id <- max(as.integer(prior_master$`Current Raw Data Dictionary`$raw_id))


if (is.na(max_raw_id)) {
        stop("max raw_id is NA")
}


final_output_02_2 <-
        dplyr::bind_rows(final_output_02,
                         output_02_b_new %>%
                                 dplyr::mutate(raw_id = (1+max_raw_id):(max_raw_id+nrow(output_02_b_new))))


final_diff <- final_output_02_2

# Getting new current dd
current_dd %>%
        dplyr::left_join(prior_master$`Current Raw Data Dictionary` %>%
                                 dplyr::select(raw_id, FORM, VARIABLE)) %>%
        tail()



final_output_02_3 <-
        final_output_02_2 %>%
        tidyr::pivot_longer(cols = c(added_date, removed_date, changed_date),
                            names_to = c("Status", "Type"),
                            names_sep = "[_]{1}",
                            values_to = "Date",
                            values_drop_na = TRUE) %>%
        mutate_all(str_remove_all, "\"")


# Making new Current version
final_output_02_4 <-
        dplyr::bind_rows(prior_master$`Current Raw Data Dictionary` %>%
                                 dplyr::mutate_all(as.character),
                final_output_02_3) %>%
        dplyr::group_by(raw_id) %>%
        dplyr::arrange(desc(as.Date(Date))) %>%
        dplyr::filter(Date == max(Date))

length(unique(final_output_02_4$raw_id))


# Since the changes can have a 1 to many relationship, aggregating for a 1:1 relationship
final_output_02_5 <-
final_output_02_4 %>%
        dplyr::mutate(Changed = ifelse(!(`Changed Field` %in% c("")),
                              paste0(`Changed Field`, " ", `Changed To`, " ", `Changed From`),
                              "")) %>%
        rubix::group_by_unique_aggregate(raw_id,
                                  agg.col = Changed,
                                  collapse = "\n") %>%
        dplyr::right_join(final_output_02_4 %>%
                                  dplyr::select(-starts_with("Changed ")) %>%
                                  dplyr::distinct())


max(as.integer(final_output_02_5$raw_id))
nrow(final_output_02_5)

final_output_02_4 %>%
        group_by(raw_id) %>%
        summarize(count = n()) %>%
        arrange(desc(count))


final_output_02_4 %>%
                      dplyr::filter(raw_id == "1167")

#
#
#
# broca::copy_to_clipboard(final_output_02_3)
#
#
# broca::copy_to_clipboard(
# final_output_02_3 %>%
#         dplyr::mutate(Date = as.Date(Date)) %>%
#         dplyr::group_by(raw_id) %>%
#         dplyr::arrange(desc(Date)) %>%
#         rubix::filter_first_row() %>%
#         dplyr::filter(Status != "removed"))
#
