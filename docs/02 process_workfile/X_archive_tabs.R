# Save each tab of the origin file in the input folder if it does not already exist
input_xlsx <- broca::read_full_excel(origin_fn)

while (length(input_xlsx) > 0) {

                input <- input_xlsx[[1]]
                input_name <- names(input_xlsx)[1]

                if (all(c("TYPE", "CONCEPT") %in% colnames(input))) {

                                path_to_output_fn <- paste0("input/", cave::strip_fn(origin_fn), "_", input_name, ".csv")

                                if (!file.exists(path_to_output_fn)) {

                                        broca::simply_write_csv(input,
                                                                path_to_output_fn)

                                }

                }

                input_xlsx[[1]] <- NULL
}

