## Notes  
* In "Terms" style searches, the aggregated output includes a "{term}: " pasted to the front of the Concept strip to be able to identify the term that generated that result  
* All "Source" style searches occur in a 1:1 ratio where each raw concept results in a single search outcome while the "Terms" style can take a vector of inputs that can result in a 1:many result.  
* The scripts are organized to in the order of computational intensity  
* There are filters for both a maximum line count of 250 in a single cell and no more than 25000 characters in a single Excel cell
* No files are written to the originating file. Instead each run is written as a csv to the output subdir based on the user-provided file stem and the origin_tab appended with the script used to write the output. The final step aggregates all the files associated with the run based on the file stem pattern to clipboard, which can then be transferred to the originating file by the user. 
* routine "Source_ICDO3_Code"" is the one script where a filter on the settings is not applied. For example, a filter for RxNorm only with an ICDO3 code run in this step will include all possible vocabularies that match to the icdo3_code_pattern_regex and there also not a filter for the ICDO03 vocabulary in place.

# Logic
# Function
## At startup  
-If Excel, rubix::mutate_all_rm_multibyte_chars()
-All columns starting with "Source " are deselected.
-New routine_id column is created with rowid_to_column()

## Procedure Functions
-normalize_na()

query_athena: all settings are filtered for. String-style searched as split on spaces and punctuation


## Process
Steps 01 and 02 only requires a CONCEPT column from the parsed data dictionary or another column that contains all the concepts to search  
Steps 03 and 04 requires the end-user to introduce columns to the output that includes one or more additional terms that can be searched in a format that can be parsed by the cave::string_to_vector() function. 
The tab in the provided file is first copied to the input/ folder. This is what all subsequent scripts execute on as input, along with the designated columns to derive queries from (ie concepts or search terms) as well as the terminal column, which is the column that indicates that a concept has been mapped if it is not NA.
If a revision is made to the originating tab and it is expected to be reflected in the output, the file in input needs to be deleted to write a new copy to operate on.

