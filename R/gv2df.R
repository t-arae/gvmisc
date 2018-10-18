
#' extract meta informations of genes (e.g. Gene Symbol, Mearsure (probe ID))
#' @inheritParams param_def
#' @export
gv2meta_df <-
  function(df){
    na_r_i <- extract_na_row_idx(df)
    na_c_i <- extract_na_col_idx(df)
    nc <- ncol(df)

    meta_name <-
      df[na_r_i, max(na_c_i) + 1] %>%
      dplyr::pull()

    meta_df <-
      df[na_r_i, (max(na_c_i) + 2):nc] %>%
      t.data.frame() %>%
      dplyr::as_data_frame()
    colnames(meta_df) <- meta_name

    meta_df
  }

#' extract meta informations of experiments
#' @inheritParams param_def
#' @export
gv2info_df <-
  function(df){
    id <- NULL
    na_r_i <- extract_na_row_idx(df)
    na_c_i <- extract_na_col_idx(df)
    nr <- nrow(df)

    info_col <-
      df[(max(na_r_i) + 1), 1:(max(na_c_i) + 1)] %>%
      as.character()

    info_df <-
      df[(max(na_r_i) + 2):nr, 1:(max(na_c_i) + 1)]
    colnames(info_df) <- info_col

    info_df %>%
      dplyr::mutate(id = dplyr::row_number()) %>%
      dplyr::select(id, dplyr::everything())
  }

# function(meta_df){
#     na_r_i <- extract_na_row_idx(df)
#     na_c_i <- extract_na_col_idx(df)
#     nr <- nrow(df)
#     nc <- ncol(df)
#
#     no_na_c_i <-
#       meta_df %>%
#       apply(2, function(x) all(!is.na(x))) %>%
#       which %>%
#       .[1]
#
#     col_info <-
#       df[max(na_r_i) + 1, (max(na_c_i) + 2):nc] %>%
#       as.character() %>%
#       {paste(meta_df[[no_na_c_i]], ., sep = "_")}
# }

#' extract data informations of experiments
#' @inheritParams param_def
#' @export
gv2data_df <-
  function(df){
    . <- key <- value <- id <- NULL

    na_r_i <- extract_na_row_idx(df)
    na_c_i <- extract_na_col_idx(df)
    nr <- nrow(df)
    nc <- ncol(df)

    meta_df <- gv2meta_df(df)

    col_info <- meta2nona_col(meta_df)

    unique_col_name <-
      df[max(na_r_i) + 1, (max(na_c_i) + 2):nc] %>%
      as.character() %>%
      {paste(col_info$col_data, ., sep = "_")}

    data_df <-
      df[(max(na_r_i) + 2):nr, (max(na_c_i) + 2):nc]

    colnames(data_df) <- unique_col_name
    data_df %>%
      dplyr::mutate(id = dplyr::row_number()) %>%
      dplyr::select(id, dplyr::everything()) %>%
      dplyr::mutate_if(is.character, as.double)
  }


#' extract data informations of experiments
#' @inheritParams param_def
#' @export
gv2tidy_data_df <-
  function(df){
    . <- key <- value <- id <- NULL

    meta_df <- gv2meta_df(df)
    info_df <- gv2info_df(df)
    data_df <- gv2data_df(df)

    col_info <- meta2nona_col(meta_df)

    regex_pattern <-
      paste0(
        "(",
        paste0(col_info$col_data, collapse = "|"),
        ")_"
      )

    data_df <-
      data_df %>%
      tidyr::gather(key, value, -id) %>%
      dplyr::mutate(
        ID =
          stringr::str_extract(key, regex_pattern) %>%
          stringr::str_sub(end = -2)
      ) %>%
      dplyr::mutate(
        key =
          stringr::str_remove(key, regex_pattern)
      ) %>%
      dplyr::mutate(value = as.double(value))

    colnames(data_df)[4] <- col_info$col_name
    data_df
  }
