# Files are split by form in the following path:
path_to_files <- paste0(path_to_output_dir, "/02_file_qa")

# Creating dir for each file
routine_path <- paste0(path_to_output_dir, "/", cave::strip_fn(cave::present_script_path()))
cave::create_dir_if_not_exist(routine_path)

# Has the observation group been simplified?
file_paths <-
        list.files(path_to_files, full.names = TRUE, pattern = ".xlsx$")


while (length(file_paths) > 0) {
        file_path <- file_paths[1]

        input <- broca::read_full_excel(file_path)
        input <- input$`Sheet 1`
        input <- input$`Observation Group`

        if (any(grepl("^[0-9]{1,}[ ]{1}.*$", input) == FALSE)) {
                secretary::typewrite(file_path)
                secretary::press_enter()
        }


        file_paths <- file_paths[-1]
}

# Fact, Modifier and Attribute Columns removed
# file_paths <-
#         list.files(path_to_files, full.names = TRUE, pattern = ".xlsx$")
#
#
# while (length(file_paths) > 0) {
#         file_path <- file_paths[1]
#
#         input <- broca::read_full_excel(file_path)
#         input <- input$`Sheet 1`
#
#
#         if (any(c("Fact", "Attribute", "Modifier") %in% colnames(input))) {
#                 secretary::typewrite(file_path)
#                 secretary::press_enter()
#         }
#
#
#         file_paths <- file_paths[-1]
# }


# If there are any "NEW" concepts in the file, to make sure there is a PREFERRED_LABEL and PRECOORDINATED_FACTS column with data
file_paths <-
        list.files(path_to_files, full.names = TRUE, pattern = ".xlsx$")



while (length(file_paths) > 0) {
        file_path <- file_paths[1]

        input <- broca::read_full_excel(file_path)
        input <- input$`Sheet 1`
        input_b <- input$MSK_CONCEPT_ID


        if (any(c("NEW") %in% input_b)) {

                if (any(!(c("PRECOORDINATED_FACTS", "PREFERRED_LABEL") %in% colnames(input)))) {
                        secretary::typewrite(file_path)
                        secretary::press_enter()

                }

        }


        file_paths <- file_paths[-1]
}

# If there are any "NEW" concepts in the file, to make sure there is a PREFERRED_LABEL and PRECOORDINATED_FACTS column contains data
file_paths <-
        list.files(path_to_files, full.names = TRUE, pattern = ".xlsx$")



while (length(file_paths) > 0) {
        file_path <- file_paths[1]

        input <- broca::read_full_excel(file_path)
        input <- input$Sheet1
        input_b <- input$MSK_CONCEPT_ID



        if (any(c("NEW") %in% input_b)) {

                input <- input %>%
                        dplyr::select(all_of(c("MSK_CONCEPT_ID",
                                               "PRECOORDINATED_FACTS",
                                               "PREFERRED_LABEL"))) %>%
                        dplyr::filter(MSK_CONCEPT_ID == "NEW")

                if (any((c("NA", NA, "") %in% input$PRECOORDINATED_FACTS))|
                    any((c("NA", NA, "") %in% input$PREFERRED_LABEL))) {
                        secretary::typewrite(file_path)
                        secretary::press_enter()

                }

        }


        file_paths <- file_paths[-1]
}

# Are all the same CONCEPT in a FORM mapped to the same concept id?
file_paths <-
        list.files(path_to_files, full.names = TRUE, pattern = ".xlsx$")



while (length(file_paths) > 0) {
        file_path <- file_paths[1]

        input <- broca::read_full_excel(file_path)
        input <- input$`Sheet 1`
        input <- split(input, input$CONCEPT)
        input <-
                input %>%
                purrr::keep(~nrow(.)>1)

        input <-
                input %>%
                purrr::keep(function(x) 1 != (x %>%
                                                      dplyr::select(MSK_CONCEPT_ID) %>%
                                                      dplyr::distinct() %>%
                                                      unlist() %>%
                                                      length()))




        if (length(input)) {
                secretary::typewrite(file_path)
                secretary::press_enter()

        }


        file_paths <- file_paths[-1]
}


# Conversely, is each concept_id mapped to more than 1 unique CONCEPT?
file_paths <-
        list.files(path_to_files, full.names = TRUE, pattern = ".xlsx$")



while (length(file_paths) > 0) {
        file_path <- file_paths[1]

        input <- broca::read_full_excel(file_path)
        input <- input$`Sheet 1`
        input <- split(input, input$MSK_CONCEPT_ID)
        input <-
                input %>%
                purrr::keep(~nrow(.)>1)

        input <-
                input %>%
                purrr::keep(function(x) 1 != (x %>%
                                                      dplyr::select(CONCEPT) %>%
                                                      dplyr::distinct() %>%
                                                      unlist() %>%
                                                      length()))


        if ("NEW" %in% names(input)) {
                input$NEW <- NULL
        }


        if (length(input)) {
                secretary::typewrite(file_path)
                secretary::press_enter()

        }


        file_paths <- file_paths[-1]
}

# Are there duplicate entries at the concept level?
file_paths <-
        list.files(path_to_files, full.names = TRUE, pattern = ".xlsx$")



while (length(file_paths) > 0) {
        file_path <- file_paths[1]

        input <- broca::read_full_excel(file_path)
        input <- input[[1]]
        input_b <-
                input %>%
                dplyr::group_by(FORM, VARIABLE, TYPE, CONCEPT) %>%
                dplyr::summarize(ROW_COUNT = length(unique(MSK_CONCEPT_ID)))


        if (any(input_b$ROW_COUNT != 1)) {
                secretary::typewrite(file_path)
                secretary::press_enter()
        }


        file_paths <- file_paths[-1]
}

###################
# OUTRO
final_routine_output <-
        list.files(path_to_files, full.names = TRUE, pattern = "xlsx$") %>%
        purrr::map(function(x) broca::read_full_excel(x)[[1]]) %>%
        dplyr::bind_rows()

broca::write_full_excel(x = final_routine_output,
                        file = paste0(routine_path, "/", origin_tab, ".xlsx"))
