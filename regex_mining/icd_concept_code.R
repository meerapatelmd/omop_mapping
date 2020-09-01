all_concept_codes <-
        chariot::query_athena("SELECT DISTINCT concept_code,vocabulary_id FROM concept;")


all_concept_codes2 <-
        all_concept_codes %>%
        mutate_at(vars(concept_code), function(x) stringr::str_replace_all(x, "[0-9]", "0")) %>%
        mutate_at(vars(concept_code), function(x) stringr::str_replace_all(x, "[A-Za-z]", "Z")) %>%
        dplyr::distinct()

target_vocabularies <-
c('UCUM',
'ICD10PCS',
'ICD9Proc',
'ICD9ProcCN',
'ICD9CM',
'HemOnc',
'LOINC',
'NAACCR',
'RxNorm',
'Nebraska Lexicon',
'SNOMED',
'ICD10',
'ICDO3',
'ATC',
'EphMRA ATC',
'ICD10CM',
'ICD10CN',
'RxNorm Extension')

all_concept_codes3 <-
        all_concept_codes2 %>%
        rubix::filter_for_vector(filter_col = vocabulary_id,
                                 inclusion_vector = target_vocabularies) %>%
        dplyr::arrange(vocabulary_id)


all_concept_codes4 <-
        all_concept_codes3 %>%
        tidyr::pivot_wider(names_from = concept_code,
                           values_from = concept_code,
                           values_fn = list(concept_code = length))


output <- vector()
for (i in 2:ncol(all_concept_codes4)) {
        output <-
                c(output,
                  all_concept_codes4[,i] %>%
                          unlist() %>%
                          sum(na.rm = TRUE))

}

names(output) <-
        all_concept_codes4 %>%
        dplyr::select_at(vars(2:last_col())) %>%
        colnames()


output2 <-
        output[output == 1]


unique_codes <- names(output2)

all_concept_codes5 <-
        all_concept_codes4 %>%
        dplyr::select(vocabulary_id, all_of(unique_codes))


all_concept_codes6 <-
        all_concept_codes5 %>%
        tidyr::pivot_longer(cols = !vocabulary_id,
                                names_to = "concept_code",
                            values_to = "count",
                            values_drop_na = TRUE)

all_concept_codes7 <-
       rubix::split_deselect(all_concept_codes6,
                             column_name = "vocabulary_id")

replace_Z_with_regex <-
        function(string) {
                stringr::str_replace_all(string = string,
                                         pattern = "^Z$",
                                         replacement = "[A-Za-z]{1}")
        }

replace_num_with_regex <-
        function(string) {
                stringr::str_replace_all(string = string,
                                         pattern = "^0$",
                                         replacement = "[0-9]{1}")
        }

replace_slash_with_regex <-
        function(string) {
                stringr::str_replace_all(string = string,
                                         pattern = "^/$",
                                         replacement = "[/]{1}")
        }

replace_dec_with_regex <-
        function(string) {
                stringr::str_replace_all(string = string,
                                         pattern = "^.$",
                                         replacement = "[.]{1}")
        }

replace_dash_with_regex <-
        function(string) {
                stringr::str_replace_all(string = string,
                                         pattern = "^-$",
                                         replacement = "[-]{1}")
        }

perform_all_replacements <-
        function(string) {
                string <-
                        replace_Z_with_regex(string)

                string <-
                        replace_num_with_regex(string)

                string <-
                        replace_slash_with_regex(string)

                string <-
                        replace_dec_with_regex(string)

                string <-
                        replace_dash_with_regex(string)

                return(string)
        }

all_concept_codes8 <-
        all_concept_codes7 %>%
        rubix::map_names_set(function(x) x %>%
                                                dplyr::select(concept_code) %>%
                                                unlist() %>%
                                     centipede::strsplit(split = "[Z]|[0]|[[:punct:]]", type = "before") %>%
                                     purrr::map(trimws)
                             )

all_concept_codes8$UCUM <- NULL
all_concept_codes8$NAACCR <- NULL

all_concept_codes9 <-
        all_concept_codes8 %>%
        rubix::map_names_set(function(x) x %>% purrr::map(function(y) perform_all_replacements(y)))

all_concept_codes10 <-
        all_concept_codes9 %>%
        rubix::map_names_set(function(x) x %>% purrr::map(function(y) paste(y, collapse = "")))

all_concept_codes11_pattern <-
        all_concept_codes10 %>%
        rubix::map_names_set(function(x) paste(unlist(x), collapse = "|"))

all_concept_codes11 <-
        all_concept_codes10 %>%
        rubix::map_names_set(function(x) paste(paste0("^",unlist(x), "$"), collapse = "|"))


%>%
                                                            purrr::map(paste, collapse = "")))


all_concept_codes9 <-
        all_concept_codes8 %>%
        rubix::map_names_set(function(x) perform_all_replacements(x)) %>%
        purrr::map(paste, collapse = "") %>%
        purrr::map(function(x) paste0("^", x, "$"))


all_concept_codes10 <-
        paste(all_concept_codes9 %>%
                      unlist(),
              collapse = "|")

grepl(all_concept_codes10,
      x = "9000/3")
