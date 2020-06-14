all_mappings <-
c("~/Memorial Sloan Kettering Cancer Center/KM COVID - General/Mappings/M_COVID_DDict_05272020_Meera_Variables.xlsx",
  "~/Memorial Sloan Kettering Cancer Center/KM COVID - General/Mappings/M_COVID_DDict_05272020_Meera_PV.xlsx") %>%
        rubix::map_names_set(broca::read_full_excel)

all_mappings2 <-
        all_mappings %>%
        rubix::map_names_set(function(x) x[[1]])
names(all_mappings2) <- c("Variable", "Permissible Value")


all_mappings3 <-
        all_mappings2 %>%
        rubix::map_names_set(function(x) x %>%
                                          dplyr::select(any_of(c("FORM", "VARIABLE", "Fact Concept", "PV"))))

all_mappings4 <- all_mappings3
all_mappings4$Variable <-
          all_mappings4$Variable %>%
          dplyr::mutate(CONCEPT = VARIABLE)
all_mappings4$`Permissible Value` <-
        all_mappings4$`Permissible Value` %>%
        dplyr::mutate(CONCEPT = PV)

all_mappings5 <-
        dplyr::bind_rows(all_mappings4,
                         .id = "TYPE")

all_mappings6 <-
        all_mappings5 %>%
        dplyr::mutate_all(str_replace_na)

# Metrics
output <-
  all_mappings6 %>%
  dplyr::mutate(Status = ifelse(`Fact Concept` %in% c("NA"), "Not Mapped", "Mapped")) %>%
  dplyr::group_by(FORM, TYPE, Status) %>%
  dplyr::summarise(COUNT = n()) %>%
  tidyr::pivot_wider(id_cols = c(FORM, TYPE),
                     names_from = Status,
                     values_from = COUNT) %>%
  dplyr::mutate_at(vars(c(Mapped, `Not Mapped`)),
                   stringr::str_replace_na,
                   0)
