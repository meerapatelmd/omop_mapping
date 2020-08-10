# Files are split by form in the following path:
path_to_files <- paste0(path_to_output_dir, "/01_split_files_by_form")

# Creating dir for each file
routine_path <- paste0(path_to_output_dir, "/", cave::strip_fn(cave::present_script_path()))
cave::create_dir_if_not_exist(routine_path)


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

        if (names(input) != "Sheet 1") {
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

        output <-
                input$`Sheet 1` %>%
                dplyr::select(-any_of(concept_table_fields))

        output_fn <- paste0(routine_path, "/", basename(file_path))

        broca::write_full_excel(output,
                                output_fn)

        file_paths <- file_paths[-1]
}