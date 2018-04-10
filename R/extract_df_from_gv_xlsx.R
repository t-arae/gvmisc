
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
    # 1st column: Path from root
    # 2nd column: Perturbations
    # 3rd column: Experiment
    # 4th column: # samples experiment
    # 5th column: # samples control
    # 6+(2n)th columns: Log2-ratio
    # 7+(2n)th columns: p-value

    temp_col <- read_xlsx(file_path, n_max = skip_num + 1, col_names = F)
    extract_col_info <-
      function(x){
        temp_col[x,] %>%
          as.character %>%
          .[6:length(.)] %>%
          replace_na("NA")
      }
    colnames(inf) <-
      c(
        c("Path_from_root", "Perturbations", "Experiment",
          "num_samples_experiment", "num_samples_control"),
        str_c(
          extract_col_info(skip_num - 1),
          extract_col_info(skip_num),
          extract_col_info(skip_num + 1),
          sep = "_"
        )
      )
    inf
  }
