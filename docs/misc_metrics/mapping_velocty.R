
output <-
        read_select_input_dir(path_to_input_dir = "~/GitHub/KMI/termite/Map_to_OMOP/procedure/output/Archive",
                              "Fact Concept")

output2 <-
        output %>%
        purrr::map(function(x) count_na(x, col = "Fact Concept"))


output3 <-
        dplyr::bind_rows(output2,
                         .id = "INPUT")


output4 <-
        output3 %>%
        dplyr::mutate(INPUT_DT = file.info(INPUT)$mtime) %>%
        dplyr::select(-INPUT) %>%
        dplyr::mutate(INPUT_DT = lubridate::floor_date(INPUT_DT, unit = "minute")) %>%
        dplyr::distinct() %>%
        dplyr::group_by(Mapped, Unmapped) %>%
        dplyr::arrange(INPUT_DT) %>%
        rubix::filter_first_row() %>%
        dplyr::filter(Total < 2712)


output4$INPUT_END_DT <- c(output4$INPUT_DT[2:length(output4$INPUT_DT)], Sys.time())
output4$Mapped2 <- c(output4$Mapped[2:length(output4$Mapped)], output4$Total[length(output4$Total)])

output5 <-
        output4 %>%
        dplyr::mutate(Mapped_Count = Mapped2 - Mapped) %>%
        dplyr::mutate(Time_Interval = difftime(INPUT_END_DT, INPUT_DT, units = "hours")) %>%
        dplyr::mutate(Date = as.Date(INPUT_DT)) %>%
        dplyr::group_by(Date) %>%
        dplyr::summarize(Day_Count = sum(Mapped_Count))

