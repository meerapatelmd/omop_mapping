# Files are split by form in the following path:
path_to_files <- paste0(path_to_output_dir, "/03_content_qa")

# Creating dir for each file
routine_path <- paste0(path_to_output_dir, "/", cave::strip_fn(cave::present_script_path()))
cave::create_dir_if_not_exist(routine_path)


input <- broca::read_full_excel(list.files(path_to_files, full.names = TRUE))
input <- input$`Sheet 1`

output_02 <- list()
# # NEW Concept accounting
# output_02[[1]] <-
#         input %>%
#         dplyr::filter(MSK_CONCEPT_ID == "NEW") %>%
#         dplyr::group_by(PREFERRED_LABEL) %>%
#         dplyr::mutate(COUNT = length(PREFERRED_LABEL))
#
# input <-
#         input %>%
#         dplyr::filter(MSK_CONCEPT_ID != "NEW") %>%
#         dplyr::group_by(PREFERRED_LABEL) %>%
#         dplyr::mutate(COUNT = length(PREFERRED_LABEL)) %>%
#         dplyr::ungroup()


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


        if (input3 != "NEW") {

        output <-
                chariot::query_concept_id(input3)

        if (nrow(output) == 0) {
                print(input3)
                secretary::press_enter()
        }
        }

        input2 <-
                input2 %>%
                rubix::slice_off_first_row()
}


