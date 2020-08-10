if (getwd() != "/Users/patelm9/GitHub/KMI/termite/Map_to_OMOP/refresh_master_data_dictionary") {

        setwd("/Users/patelm9/GitHub/KMI/termite/Map_to_OMOP/refresh_master_data_dictionary")

}

source('utils.R')

# Variables
origin_fn <- "~/Memorial Sloan Kettering Cancer Center/KM COVID - General/COVIDScienceDatabase_DataDictionary - KM Master Dictionary.xlsx"
origin_tab <- "DDict Master Metadata"
proc_dd_fn <- "~/Memorial Sloan Kettering Cancer Center/KM COVID - General/Mappings/MASTER Parsed Data Dictionary.xlsx"

input_file_prefix <- "COVID_"

# Constant Variables
proc_dd_ledger_tab <- "Ledger"
proc_dd_current_raw_tab <- "Current Raw Data Dictionary"
proc_dd_record_raw_tab <- "Raw Data Dictionary Record"
proc_dd_parsed_tab <- "Parsed Data Dictionary"


# Creating structure
date_input_folder <- paste0("input/", Sys.Date())
date_output_folder <- paste0("output/", Sys.Date())
cave::create_dir_if_not_exist(date_input_folder)
cave::create_dir_if_not_exist(date_output_folder)


# dd_main <- dd[[dd_parsed_tab]]
# dd_main$added_date <- as.Date(dd_main$added_date)
# dd_main$removed_date <- as.Date(dd_main$removed_date)
# dd_ledger <- dd[[dd_ledger_tab]]
# dd_ledger$Date <- as.Date(dd_ledger$Date)
# raw_dd <- dd[[dd_record_raw_tab]]


# Copying origin data dictionary to day dir in the format of "{input_file_prefix}_{origin_tab}.csv"
origin_input_fn <- paste0(input_file_prefix, origin_tab, ".csv")
path_to_origin_input <- paste0(date_input_folder, "/", origin_input_fn)

if (!file.exists(path_to_origin_input)) {

                input_01 <- broca::read_full_excel(origin_fn)
                input_01 <- input_01[[origin_tab]]

                if (is.null(input_01)) {

                        stop("input_01 is null.")

                }

                output_01 <-
                        input_01 %>%
                        cartograph::standardize_dd_cols() %>%
                        dplyr::select(all_of(common_fields))

                broca::simply_write_csv(x = output_01,
                                        file = path_to_origin_input)
}

current_dd <- broca::simply_read_csv(path_to_origin_input)

# Copying working data dictionary to day dir in original format
path_to_workfile <- paste0(date_input_folder, "/", basename(proc_dd_fn))

if (!file.exists(path_to_workfile)) {

        # Copying file
                file.copy(from = proc_dd_fn,
                          to = path_to_workfile)

}

# Reading and parsing dd dates
proc_dd <-
        path_to_workfile %>%
        readxl::excel_sheets() %>%
        set_names() %>%
        purrr::map(readxl::read_excel, path = path_to_workfile)
