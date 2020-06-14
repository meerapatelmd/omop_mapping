if ("/Users/patelm9/GitHub/KMI/termite/Map_to_OMOP/initiate_workfile" != getwd()) {
        setwd("/Users/patelm9/GitHub/KMI/termite/Map_to_OMOP/initiate_workfile")
}

source('utils.R')

# Project Setup
origin_fn <- "~/Memorial Sloan Kettering Cancer Center/KM COVID - General/COVIDScienceDatabase_DataDictionary - KM Master Dictionary.xlsx"
origin_tab <- "DDict Master Metadata"
input_file_stem <- "COVID_"


paths_to_existing_map <-
c("~/Memorial Sloan Kettering Cancer Center/KM COVID - General/Mappings/COMPLETE_Batch_1.xlsx",
  "~/Memorial Sloan Kettering Cancer Center/KM COVID - General/Mappings/COMPLETE_Batch_2.xlsx")


existing_map <-
        paths_to_existing_map %>%
        rubix::map_names_set(broca::read_full_excel) %>%
        purrr::map(function(x) x[!(names(x) %in% c("Pivot"))][[1]]) %>%
        dplyr::bind_rows() %>%
        dplyr::select(any_of(workfile_colnames),
                      contains(" Concept")) %>%
        dplyr::distinct()

if ("routine_id" %in% colnames(existing_map)) {
        existing_map <-
                existing_map %>%
                dplyr::select(-routine_id)
}


# If the input_fn does not exist in the input subdir, it is written to the input subdir to the input_fn provided above
input_fn <- paste0(input_file_stem, origin_tab, ".csv")
path_to_input_fn <- paste0("input/", input_fn)
if (!file.exists(path_to_input_fn)) {
        origin_data <- broca::read_full_excel(origin_fn)
        input <- origin_data[[origin_tab]]

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
                                         "Source Synonym Exact"))))  %>%
                        tibble::rowid_to_column("routine_id")

        # Copy Input to input folder
        broca::simply_write_csv(x = input,
                                file = path_to_input_fn,
                                log_details = paste0(origin_fn, "TAB: ", origin_tab, "written to ", input_fn))

        #cave::rm_all_objects_that_start_with("origin_")
        #rm(input)

}


# If the input_fn does not exist in the input subdir, it is written to the input subdir to the input_fn provided above
input_existing_map_fn <- paste0(input_file_stem, "existing_map", ".csv")
path_to_input_existing_map_fn <- paste0("input/", input_existing_map_fn)
if (!file.exists(path_to_input_existing_map_fn)) {


        # Copy Input to input folder
        broca::simply_write_csv(x = existing_map,
                                file = path_to_input_existing_map_fn)

        #cave::rm_all_objects_that_start_with("origin_")
        #rm(input)

}
