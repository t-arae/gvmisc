
#' Extract data.frame from Genvestigator .xlsx file
#' @param file_path .xlsx file path
#' @export
extract_df_from_gv_xlsx <-
  function(file_path){
    inf <- readxl::read_xlsx(file_path, col_names = F)

    hoge <- extract_data_df(inf)
    dplyr::left_join(
      extract_meta_df(inf),
      hoge,
      by = colnames(hoge)[3]
    )
  }
