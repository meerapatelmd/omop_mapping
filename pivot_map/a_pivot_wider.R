# Converting 'MSK Concept' and 'MSK Concept Type' into a 'Observation Group Concept', 'Fact Concept', 'Attribute Concept', 'Modifier Concept'
rm(list = ls())
source("setup.R")

#Creating output filename
path_to_output_fn <- stringr::str_replace_all(path_to_input_fn,
                                              "(^.*?)(/.*?)(.csv)",
                                              paste0("output\\2_", cave::strip_fn(cave::present_script_path()), "\\3"))


# Brake if the file already exists
if (file.exists(path_to_output_fn)) {

        secretary::typewrite_warning(path_to_output_fn, "already exists and will be overwritten.")
        secretary::press_enter()

}


# Reading input file
input <- read_input()


# pivoting wider
output <-
        dplyr::left_join(input %>%
                                 dplyr::select(-starts_with("MSK Concept")),
        input %>%
        tidyr::pivot_wider(
                        names_from = `MSK Concept Type`,
                           values_from = `MSK Concept`,
                           values_fn = list(`MSK Concept` = function(x) paste(x, collapse = "\n")))) %>%
        dplyr::distinct()


# QA: making sure that the FORM-TYPE-CONCEPT counts match
qa1 <-
nrow(input %>%
             dplyr::select(FORM, TYPE, CONCEPT) %>%
             dplyr::distinct()) -
nrow(output %>%
             dplyr::select(FORM, TYPE, CONCEPT) %>%
             dplyr::distinct())

if (qa1 != 0) {
        stop("FORM-TYPE-CONCEPT combinations missing in output.")
}

broca::simply_write_csv(x = output,
                        file = path_to_output_fn)

