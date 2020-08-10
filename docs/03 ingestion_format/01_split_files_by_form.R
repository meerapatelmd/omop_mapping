input <- read_preingest_file()

# Creating dir for each file
routine_path <- paste0(path_to_output_dir, "/", cave::strip_fn(cave::present_script_path()))
cave::create_dir_if_not_exist(routine_path)

output <-
        split(input,
              input$FORM)

output_paths <- paste0(routine_path, "/", names(output), ".xlsx") %>% as.list()


output %>%
        purrr::map2(output_paths, function(x,y) broca::write_full_excel(x,
                                                                      y))
