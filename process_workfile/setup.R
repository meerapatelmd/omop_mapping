if ("/Users/patelm9/GitHub/KMI/termite/Map_to_OMOP/process_workfile" != getwd()) {
        setwd("/Users/patelm9/GitHub/KMI/termite/Map_to_OMOP/process_workfile")
}

source('utils.R')


# Project Setup
origin_fn <-  "~/Memorial Sloan Kettering Cancer Center/KM COVID - General/Mappings/BY FORM/Workfile.xlsx"
origin_terminal_tab <- "MSK Concept"
input_file_stem <- "COVID_StandardLib_"


# If the input_fn does not exist in the input subdir, it is written to the input subdir to the input_fn provided above
input_fn <- paste0(input_file_stem, "_", origin_terminal_tab, ".csv")
path_to_input_fn <- paste0("input/", input_fn)
if (!file.exists(path_to_input_fn)) {
        origin_data <- broca::read_full_excel(origin_fn)
        input <- origin_data[[origin_terminal_tab]]

        if (is.null(input)) {
                stop('input is null.')
        }

        if ("routine_id" %in% colnames(input)) {
                        # Using prior routine_id to make sure no garbage is imported
                        input$routine_id <- suppressWarnings(as.integer(input$routine_id))
                        input <-
                                input %>%
                                dplyr::filter(!is.na(routine_id))

                        #If maximum routine_id does not equal number of rows
                        # qa1 <- nrow(input)-max(input$routine_id)
                        #
                        # if (qa1 != 0) {
                        #         warning("filtering input for valid routine_id failed")
                        # }

                        input <-
                                input %>%
                                dplyr::mutate_at(vars(routine_id), as.character) %>%
                                dplyr::select(-(any_of(c("routine_id",
                                                         "Source Exact",
                                                         "Source Like",
                                                         "Source String as Vector",
                                                         "Search Term Exact",
                                                         "Search Term Like",
                                                         "Search Term Synonym Exact",
                                                         "Search Term Synonym Like",
                                                         "Search Term Synonym Str As Vector",
                                                         "Search Term String as Vector",
                                                         "Source Synonym Like",
                                                         "Source Synonym Exact",
                                                         "Source First Word Exact",
                                                         "Source First Word Like",
                                                         "Source No Parentheses Exact",
                                                         "Source No Parentheses Like",
                                                         "Source No Parentheses Str as Vector",
                                                         "Source Remove ICDO3 Code Like",
                                                         "Source Remove ICDO3 Code Exact",
                                                         "Source ICDO3 Code",
                                                         "Source Remove ICDO3 Str as Vector",
                                                         "Source Remove ICDO3 Synonym Exact",
                                                         "Source Remove ICDO3 Synonym Like",
                                                         "Source Numbers Exact",
                                                         "Source Numbers Like",
                                                         "Source Each Word Exact",
                                                         "Source Numbers as Words Exact",
                                                         "Source Numbers as Words Like",
                                                         "Source After Colon Exact",
                                                         "Source After Colon Like",
                                                         "Source Longest Word Exact",
                                                         "Source Longest Word Like"))))  %>%
                                tibble::rowid_to_column("routine_id")
        } else {

                input <-
                        input %>%
                        dplyr::select(-(any_of(c("routine_id",
                                                 "Source Exact",
                                                 "Source Like",
                                                 "Source String as Vector",
                                                 "Search Term Exact",
                                                 "Search Term Like",
                                                 "Search Term Synonym Exact",
                                                 "Search Term Synonym Like",
                                                 "Search Term Synonym Str As Vector",
                                                 "Search Term String as Vector",
                                                 "Source Synonym Like",
                                                 "Source Synonym Exact",
                                                 "Source First Word Exact",
                                                 "Source First Word Like",
                                                 "Source No Parentheses Exact",
                                                 "Source No Parentheses Like",
                                                 "Source No Parentheses Str as Vector",
                                                 "Source Remove ICDO3 Code Like",
                                                 "Source Remove ICDO3 Code Exact",
                                                 "Source ICDO3 Code",
                                                 "Source Remove ICDO3 Str as Vector",
                                                 "Source Remove ICDO3 Synonym Exact",
                                                 "Source Remove ICDO3 Synonym Like",
                                                 "Source Numbers Exact",
                                                 "Source Numbers Like",
                                                 "Source Each Word Exact",
                                                 "Source Numbers as Words Exact",
                                                 "Source Numbers as Words Like",
                                                 "Source After Colon Exact",
                                                 "Source After Colon Like",
                                                 "Source Longest Word Exact",
                                                 "Source Longest Word Like"))))  %>%
                        tibble::rowid_to_column("routine_id")

        }

        input <-
        input %>%
                rubix::mutate_if_not_exist(column_name = "Attribute Concept",
                                           value = NA) %>%
                rubix::mutate_if_not_exist(column_name = "Modifier Concept",
                                           value = NA)

        # Copy Input to input folder
        broca::simply_write_csv(x = input,
                                file = path_to_input_fn,
                                log_details = paste0(origin_fn, "TAB: ", origin_terminal_tab, "written to ", input_fn))

        cave::rm_all_objects_that_start_with("origin_")
        rm(input)

}

