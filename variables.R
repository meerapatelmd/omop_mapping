# Project Setup
origin_fn <-  "~/Memorial Sloan Kettering Cancer Center/KM COVID - General/Mappings/To Ingestion/PreIngestion_Standard_Library.xlsx"
origin_terminal_tab <- "MAP_06"


input_file_stem <- "COVID_SL_Odysseus_"
input_format <- "long" #Is input long format by MSK Concept Type or is it short format where Fact, Attribute, and Modifier are cast across?
terminal_col <- "MSK Concept" #What is the terminal column?
filter_for_form <- c('cancer_directed_medications','covid19_labs_piped','cancer_diagnosis','cancer_radiation_therapy') #NULL if all contents included