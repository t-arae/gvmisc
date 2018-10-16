

#' extract meta informations of genes
#' @param df df
#' @export
extract_meta_df <-
  function(df){
    na_row_idx <- df[[1]] %>% is.na %>% which
    na_col_idx <- df[na_row_idx,] %>% apply(2,is.na) %>% apply(2,all) %>% which
    meta_name <-
      df[na_row_idx, max(na_col_idx) + 1] %>%
      dplyr::pull()

    meta_df <-
      df[na_row_idx, (max(na_col_idx) + 2):ncol(df)] %>%
      t.data.frame() %>%
      dplyr::as_data_frame()
    colnames(meta_df) <- meta_name

    meta_df
  }

#' extract meta informations of experiments
#' @inheritParams extract_meta_df
#' @export
extract_info_df <-
  function(df){
    na_row_idx <- df[[1]] %>% is.na %>% which
    na_col_idx <- df[na_row_idx,] %>% apply(2,is.na) %>% apply(2,all) %>% which
    info_col <-
      df[(max(na_row_idx) + 1), 1:(max(na_col_idx) + 1)] %>%
      as.character()

    info_df <-
      df[
        (max(na_row_idx) + 2):nrow(df),
        1:(max(na_col_idx) + 1)
        ]
    colnames(info_df) <- info_col

    info_df
  }

#' extract meta informations of experiments
#' @inheritParams extract_meta_df
#' @export
extract_data_df <-
  function(df){
    . <- key <- NULL
    na_row_idx <- df[[1]] %>% is.na %>% which
    na_col_idx <- df[na_row_idx,] %>% apply(2,is.na) %>% apply(2,all) %>% which
    meta_df <- extract_meta_df(df)

    col_info <-
      df[
        max(na_row_idx) + 1,
        (max(na_col_idx) + 2):ncol(df)
        ] %>%
      as.character()

    no_NA_col_idx <-
      meta_df %>%
      apply(2, function(x) all(!is.na(x))) %>%
      which %>%
      .[1]

    col_info <-
      paste(meta_df[[no_NA_col_idx]], col_info, sep = "_")

    data_df <-
      df[
        (max(na_row_idx) + 2):nrow(df),
        (max(na_col_idx) + 2):ncol(df)
        ]
    colnames(data_df) <- col_info

    regex_pattern <-
      paste0(
        "(",
        paste0(meta_df[[no_NA_col_idx]], collapse = "|"),
        ")_"
      )

    hoge <-
      data_df %>%
      tidyr::gather() %>%
      dplyr::mutate(ID = stringr::str_extract(key, regex_pattern) %>% stringr::str_sub(end = -2)) %>%
      dplyr::mutate(key = stringr::str_remove(key, regex_pattern))

    colnames(hoge)[3] <- colnames(meta_df)[no_NA_col_idx]
    hoge
  }
