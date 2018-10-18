
#' meta2nona_col
#' @inheritParams param_def
#' @export
meta2nona_col <-
  function(meta_df){
    . <- NULL
    no_na_c_i <-
      meta_df %>%
      apply(2, function(x) all(!is.na(x))) %>%
      which %>%
      .[1]

    list(
      col_name = colnames(meta_df)[no_na_c_i],
      col_data = meta_df[[no_na_c_i]]
    )
  }


