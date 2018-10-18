
#' extract row index
#' @inheritParams param_def
#' @export
extract_na_row_idx <-
  function(df){
    df[[1]] %>%
      is.na %>%
      which
  }

#' extract column index
#' @inheritParams param_def
#' @export
extract_na_col_idx <-
  function(df){
    df[extract_na_row_idx(df),] %>%
      apply(2, is.na) %>%
      apply(2, all) %>%
      which
  }
