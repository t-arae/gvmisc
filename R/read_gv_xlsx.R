
#' read xlsx exported from GeneVestigator
#' @param fpath file path
#' @export
read_gv_xlsx <-
  function(fpath){
    readxl::read_excel(path = fpath, col_names = F)
  }
