# 1. Web Protocols are saved as XLSX without any modifications
# 2. Raw excel is read
raw_export <- 
        broca::read_full_excel("~/Memorial Sloan Kettering Cancer Center/Glioma REDCap Standardization - KMI Only - KMI Only/2020-07-17/TherapeuticProtocols_BrainCancer.xlsx")
raw_export <- raw_export[[1]]

# 3. Raw headers are removed to get a tidy dataframe
colnames(raw_export) <-
        raw_export %>%
        rubix::filter_row_n(n = 2) %>%
        unlist() %>%
        unname()

raw_export <- 
        raw_export %>%
        rubix::filter_row_n(n = 2, invert = TRUE) %>%
        filter_all(all_vars(!is.na(.)))

# Getting Phase
output <- raw_export
output$Phase <-
        output$Title %>%
        purrr::map(function(x) centipede::strsplit(x,
                                                   split = "Phase",
                                                   type = "before") %>%
                                unlist()) %>%
        purrr::map(function(x) grep("^Phase", x, value = TRUE)) %>%
        purrr::map(function(x) stringr::str_replace_all(x,
                                                        pattern = "(Phase) (.*?) (.*$)",
                                                        replacement = "\\1 \\2") %>%
                           stringr::str_remove_all("[[:punct:]]{1,}$")) %>%
        purrr::map(function(x) ifelse(length(x) == 0, "Not Found", x)) %>%
        purrr::map(function(x) paste(x, collapse = "|")) %>%
        unlist()


hemonc <- 
        chariot::query_athena("SELECT * FROM concept WHERE vocabulary_id = 'HemOnc'", override_cache = TRUE) %>%
        chariot::filter_for_valid() %>%
        dplyr::filter(concept_class_id %in% c("Component",
                                              "Brand Name",
                                              "Regimen")) %>%
        dplyr::select(hemonc_concept_id = concept_id) %>%
        chariot::left_join_df(athena_table = "concept_synonym",
                              athena_column = "concept_id") %>%
        dplyr::select(concept_id,
                      concept_name = concept_synonym_name) %>%
        # Filter out concept_name == "A" because matches any mention of "A" such as in "A trial"
        dplyr::filter(concept_name != "A") %>%
        # Mutate all to lower for the join
        dplyr::mutate(concept_name = tolower(concept_name)) %>%
        dplyr::distinct()


output$HemOnc_Matches <-
output$Title %>%
        purrr::map(function(x) centipede::strsplit(x,
                                                   split = " ",
                                                   type = "remove") %>%
                           unlist() %>%
                           stringr::str_remove_all("[[:punct:]]{1,}$") %>%
                           # converting all to lowercase to join with hemonc concept names that are also lowercase
                           tolower() %>%
                           # convert to table for left join 
                           rubix::vector_to_tibble(new_col = "source_word") %>%
                           # add identifier for aggregation for 1:many join 
                           tibble::rowid_to_column() %>%
                           dplyr::inner_join(hemonc,
                                            by = c("source_word" = "concept_name")) %>%
                           dplyr::mutate(concept_name = source_word) %>%
                           dplyr::mutate(concept_label = paste0(concept_id, " ", concept_name)) %>%
                           dplyr::select(-concept_id, -concept_name) %>%
                           rubix::group_by_unique_aggregate(rowid, source_word, agg.col = concept_label, collapse = "\n") %>%
                           tidyr::unite(col = "matches",
                                        everything(),
                                        sep = " ") %>%
                           dplyr::select(matches) %>%
                           unlist() %>%
                           paste(collapse = "\n")) %>%
        unlist()

output$InvestigationalDrug_Matches <-
output$Title %>%
        purrr::map(function(x) centipede::strsplit(x,
                                                   split = " ",
                                                   type = "remove") %>%
                           unlist() %>%
                           stringr::str_remove_all("[[:punct:]]{1,}$") %>%
                           # convert to table for left join 
                           rubix::vector_to_tibble(new_col = "source_word") %>%
                           # add identifier for aggregation for 1:many join 
                           tibble::rowid_to_column() %>%
                           rubix::filter_at_grepl(source_word,
                                                  grepl_phrase = "[A-Za-z]{1,}[-]{0,1}[0-9]{1,}$") %>%
                           tidyr::unite(col = "matches",
                                        everything(),
                                        sep = " ") %>%
                           dplyr::select(matches) %>%
                           unlist() %>%
                           paste(collapse = "\n")) %>%
        unlist()

output$Word_Index <-
        output$Title %>%
        purrr::map(function(x) centipede::strsplit(x,
                                                   split = " ",
                                                   type = "remove") %>%
                           unlist() %>%
                           stringr::str_remove_all("[[:punct:]]{1,}$") %>%
                           # convert to table for left join 
                           rubix::vector_to_tibble(new_col = "source_word") %>%
                           # add identifier for aggregation for 1:many join 
                           tibble::rowid_to_column() %>%
                           tidyr::unite(col = "matrix",
                                        everything(),
                                        sep = " ") %>%
                           dplyr::select(matrix) %>%
                           unlist() %>%
                           paste(collapse = "\n")) %>%
        unlist()

final_output <- 
        output %>%
        dplyr::select(all_of(colnames(raw_export)),
                      Word_Index,
                      everything())

broca::view_as_xlsx(final_output)

