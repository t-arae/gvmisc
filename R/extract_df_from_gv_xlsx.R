
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

#' Extract data.frame from Genvestigator .xlsx file
#' @param file_path .xlsx file path
#' @importFrom readxl read_xlsx
#' @importFrom magrittr %>%
#' @export
extract_df_from_gv_xlsx <-
  function(file_path){
    skip_num <-
      read_xlsx(file_path, col_names = F) %>%
      .[[1]] %>%
      {length(.[is.na(.)])}

    inf <- read_xlsx(file_path, skip = skip_num + 1, col_names = F)
  }
