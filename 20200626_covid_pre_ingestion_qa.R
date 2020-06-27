# Files are split by form in the following path:
path_to_files <- "~/Memorial Sloan Kettering Cancer Center/KM COVID - General/Mappings/To Ingestion/02 Pre-QA"
# QA the files themselves
file_paths <-
        list.files(path_to_files, full.names = TRUE, pattern = ".xlsx$")

# Stop if the file has > 1 tab
while (length(file_paths) > 0) {
                file_path <- file_paths[1]

                input <- broca::read_full_excel(file_path)

                if (length(input) != 1) {
                        secretary::typewrite(file_path)
                        secretary::press_enter()
                }

                file_paths <- file_paths[-1]
}

# Stop if that single tab is not named "Sheet1"
file_paths <-
        list.files(path_to_files, full.names = TRUE, pattern = ".xlsx$")

while (length(file_paths) > 0) {
        file_path <- file_paths[1]

        input <- broca::read_full_excel(file_path)

        if (names(input) != "Sheet1") {
                                secretary::typewrite(file_path)
                                secretary::press_enter()
        }


        file_paths <- file_paths[-1]
}


# Remove all concept table elements if present
concept_table_fields <- c("concept_id",
                          "concept_name",
                          "domain_id",
                          "vocabulary_id",
                          "concept_class_id",
                          "standard_concept",
                          "concept_code",
                          "valid_start_date",
                          "valid_end_date")

file_paths <-
        list.files(path_to_files, full.names = TRUE, pattern = ".xlsx$")


while (length(file_paths) > 0) {
        file_path <- file_paths[1]

        input <- broca::read_full_excel(file_path)

        if (any(concept_table_fields %in% colnames(input$Sheet1))) {
                secretary::typewrite(file_path)
                secretary::press_enter()
        }


        file_paths <- file_paths[-1]
}

# Has the observation group been simplified?
file_paths <-
        list.files(path_to_files, full.names = TRUE, pattern = ".xlsx$")


while (length(file_paths) > 0) {
        file_path <- file_paths[1]

        input <- broca::read_full_excel(file_path)
        input <- input$Sheet1
        input <- input$`Observation Group`

        if (any(grepl("^[0-9]{1,}[ ]{1}.*$", input) == FALSE)) {
                secretary::typewrite(file_path)
                secretary::press_enter()
        }


        file_paths <- file_paths[-1]
}

# Fact, Modifier and Attribute Columns removed
file_paths <-
        list.files(path_to_files, full.names = TRUE, pattern = ".xlsx$")


while (length(file_paths) > 0) {
        file_path <- file_paths[1]

        input <- broca::read_full_excel(file_path)
        input <- input$Sheet1


        if (any(c("Fact", "Attribute", "Modifier") %in% colnames(input))) {
                secretary::typewrite(file_path)
                secretary::press_enter()
        }


        file_paths <- file_paths[-1]
}


# If there are any "NEW" concepts in the file, to make sure there is a PREFERRED_LABEL and PRECOORDINATED_FACTS column with data
file_paths <-
        list.files(path_to_files, full.names = TRUE, pattern = ".xlsx$")



while (length(file_paths) > 0) {
        file_path <- file_paths[1]

        input <- broca::read_full_excel(file_path)
        input <- input$Sheet1
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
        input <- input$Sheet1
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
        input <- input$Sheet1
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
        input <- input$Sheet1
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
output_01 <-
        list.files(path_to_files, full.names = TRUE, pattern = "xlsx$") %>%
        purrr::map(function(x) broca::read_full_excel(x)[[1]]) %>%
        dplyr::bind_rows()

broca::simply_write_csv(x = output_01,
                        file = "~/Memorial Sloan Kettering Cancer Center/KM COVID - General/Mappings/To Ingestion/03 Post Form QA/02_output.csv")

###
# Repeat QA at the combined level
input <- broca::read_full_excel("~/Memorial Sloan Kettering Cancer Center/KM COVID - General/Mappings/Final Ingestion Files/Meera_COVID_StandardLibrary.xlsx")
input <- input$Meera
output_02 <- list()
# NEW Concept accounting
output_02[[1]] <-
input %>%
        dplyr::filter(MSK_CONCEPT_ID == "NEW") %>%
        dplyr::group_by(PREFERRED_LABEL) %>%
        dplyr::mutate(COUNT = length(PREFERRED_LABEL))

input <-
input %>%
        dplyr::filter(MSK_CONCEPT_ID != "NEW") %>%
        dplyr::group_by(PREFERRED_LABEL) %>%
        dplyr::mutate(COUNT = length(PREFERRED_LABEL)) %>%
        dplyr::ungroup()


# Are all the same CONCEPT across all FORMs mapped to the same concept id?
input2 <- split(input, input$CONCEPT)
input2 <-
                input2 %>%
                purrr::keep(~nrow(.)>1)

input2 <-
                input2 %>%
                purrr::keep(function(x) 1 != (x %>%
                                                      dplyr::select(MSK_CONCEPT_ID) %>%
                                                      dplyr::distinct() %>%
                                                      unlist() %>%
                                                      length()))

        if (length(input2)) {

                secretary::press_enter()

        }


# Are all the same concept_id across all FORMs mapped to radically difference concepts?
input2 <- split(input, input$MSK_CONCEPT_ID)
input2 <-
        input2 %>%
        purrr::keep(~nrow(.)>1)

input2 <-
        input2 %>%
        purrr::keep(function(x) 1 != (x %>%
                                              dplyr::select(CONCEPT) %>%
                                              dplyr::distinct() %>%
                                              unlist() %>%
                                              length()))

while (length(input2) > 0) {
        print(names(input2)[1])
        print(input2[[1]]$CONCEPT %>% as.list())
        secretary::press_enter()

        input2[[1]] <- NULL
}

if (length(input2)) {

        secretary::press_enter()

}

#QA By Form
# 1. All the same Observation Group
# 2. Count (sequential) by Variable and Permissible Value
# 3. MSK Concept ID Exists?
#         4. Duplicate MSK Concept ID in the FORM should be for only synonymous concepts

# Are there any blank, NA, etc.. values for MSK_CONCEPT_ID
input2 <- input %>%
                rubix::filter_at_grepl(MSK_CONCEPT_ID,
                                       grepl_phrase = "NEW|MSK",
                                       evaluates_to = FALSE,
                                       ignore.case = FALSE)


# Are there any blank, NA, etc.. values for CONCEPT
input2 <- input %>%
        dplyr::filter(CONCEPT %in% c(NA, "NA", ""))

#Are all MSK_CONCEPT_IDS valid?
input2 <- input

while (nrow(input2) > 0) {
        input3 <-
                input2 %>%
                rubix::filter_first_row() %>%
                dplyr::select(MSK_CONCEPT_ID) %>%
                unlist() %>%
                stringr::str_remove_all("MSK") %>%
                trimws("both")

        output <-
                chariot::query_concept_id(input3)

        if (nrow(output) == 0) {
                secretary::press_enter()
        }

        input2 <-
                input2 %>%
                rubix::slice_off_first_row()
}


