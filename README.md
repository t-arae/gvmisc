
<!-- README.md is generated from README.Rmd. Please edit that file -->
gvmisc
======

The goal of gvmisc is to ...

Installation
------------

You can install the released version of gvmisc from [CRAN](https://CRAN.R-project.org) with:

``` r
devtools::install_github("t-arae/gvmisc")
```

Example
-------

### Package loading

``` r
library(gvmisc)
```

### Reading xlsx file exported from GeneVestigator

``` r
test_file <- 
  system.file(
    "extdata", "gv_test.xlsx", 
    package = "gvmisc"
  )

test_df <- read_gv_xlsx(test_file)
dplyr::glimpse(test_df)
#> Observations: 3,295
#> Variables: 14
#> $ X__1  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Path fr...
#> $ X__2  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Perturb...
#> $ X__3  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Experim...
#> $ X__4  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Experim...
#> $ X__5  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "# sampl...
#> $ X__6  <chr> "Ensembl Gene", "Ensembl Protein", "Ensembl Transcript",...
#> $ X__7  <chr> "AT1G01010", "AT1G01010.1", "AT1G01010.1", "839580", "NA...
#> $ X__8  <chr> "AT1G01010", "AT1G01010.1", "AT1G01010.1", "839580", "NA...
#> $ X__9  <chr> "AT1G01010", "AT1G01010.1", "AT1G01010.1", "839580", "NA...
#> $ X__10 <chr> "AT1G01010", "AT1G01010.1", "AT1G01010.1", "839580", "NA...
#> $ X__11 <chr> "AT1G01030", "AT1G01030.1", "AT1G01030.1", "839321", "NG...
#> $ X__12 <chr> "AT1G01030", "AT1G01030.1", "AT1G01030.1", "839321", "NG...
#> $ X__13 <chr> "AT1G01030", "AT1G01030.1", "AT1G01030.1", "839321", "NG...
#> $ X__14 <chr> "AT1G01030", "AT1G01030.1", "AT1G01030.1", "839321", "NG...
```

### Gene meta information

``` r
meta_df <- test_df %>% gv2meta_df()
dplyr::glimpse(meta_df)
#> Observations: 8
#> Variables: 12
#> $ `Ensembl Gene`         <chr> "AT1G01010", "AT1G01010", "AT1G01010", ...
#> $ `Ensembl Protein`      <chr> "AT1G01010.1", "AT1G01010.1", "AT1G0101...
#> $ `Ensembl Transcript`   <chr> "AT1G01010.1", "AT1G01010.1", "AT1G0101...
#> $ Entrez                 <chr> "839580", "839580", "839580", "839580",...
#> $ `Gene Symbol`          <chr> "NAC001", "NAC001", "NAC001", "NAC001",...
#> $ RefSeq                 <chr> "NM_099983,NP_171609", "NM_099983,NP_17...
#> $ UniGene                <chr> "At.26626", "At.26626", "At.26626", "At...
#> $ UniProt                <chr> "Q0WV96", "Q0WV96", "Q0WV96", "Q0WV96",...
#> $ Gene                   <chr> "AT1G01010", "AT1G01010", "AT1G01010", ...
#> $ Measure                <chr> "261585_at", "261585_at", "261585_at", ...
#> $ `Signal Background`    <chr> "8", "8", "8", "8", "6", "6", "6", "6"
#> $ `Expression Potential` <chr> "13", "13", "13", "13", "11", "11", "11...
```

### Data meta information

``` r
info_df <- test_df %>% gv2info_df()
dplyr::glimpse(info_df)
#> Observations: 3,282
#> Variables: 7
#> $ id                     <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, ...
#> $ `Path from root`       <chr> "paclobutrazole study 3 > Chemical", "P...
#> $ Perturbations          <chr> "paclobutrazole study 3 / untreated lea...
#> $ Experiment             <chr> "AT-00001", "AT-00002", "AT-00003", "AT...
#> $ `Experiment Title`     <chr> "GSE00000: hoge1", "GSE00000: hoge2", "...
#> $ `# samples experiment` <chr> "5", "5", "5", "5", "5", "5", "5", "5",...
#> $ `# samples control`    <chr> "5", "5", "5", "5", "5", "5", "5", "5",...
```

### Data

``` r
data_df <- test_df %>% gv2data_df()
dplyr::glimpse(data_df)
#> Observations: 3,282
#> Variables: 9
#> $ id                      <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,...
#> $ `AT1G01010_Log2-ratio`  <dbl> 0.24797498, -1.37058095, 1.74238074, -...
#> $ `AT1G01010_Fold-Change` <dbl> -0.23896333, -0.06573771, 0.41212760, ...
#> $ `AT1G01010_p-value`     <dbl> 1.11054316, 1.22812257, 0.61050836, 0....
#> $ `AT1G01010_Pi-score`    <dbl> 1.17203461, 0.24485102, 0.69644089, 1....
#> $ `AT1G01030_Log2-ratio`  <dbl> 1.09556195, -1.46577124, 1.40453264, -...
#> $ `AT1G01030_Fold-Change` <dbl> -0.55168603, 0.06460815, -0.01489009, ...
#> $ `AT1G01030_p-value`     <dbl> -0.19726274, -1.66327915, 0.21829042, ...
#> $ `AT1G01030_Pi-score`    <dbl> -1.34270379, 1.66061595, 1.29707552, -...
```

### Getting tidy data.frame

``` r
tidy_data <- test_df %>% gv2tidy_data_df()
tidy_data
#> # A tibble: 26,256 x 4
#>       id key         value `Ensembl Gene`
#>    <int> <chr>       <dbl> <chr>         
#>  1     1 Log2-ratio  0.248 AT1G01010     
#>  2     2 Log2-ratio -1.37  AT1G01010     
#>  3     3 Log2-ratio  1.74  AT1G01010     
#>  4     4 Log2-ratio -0.820 AT1G01010     
#>  5     5 Log2-ratio  1.99  AT1G01010     
#>  6     6 Log2-ratio -1.01  AT1G01010     
#>  7     7 Log2-ratio  1.42  AT1G01010     
#>  8     8 Log2-ratio  0.214 AT1G01010     
#>  9     9 Log2-ratio -0.513 AT1G01010     
#> 10    10 Log2-ratio -0.596 AT1G01010     
#> # ... with 26,246 more rows
```

### Tidy data with data meta info

``` r
tidy_data2 <-
  dplyr::left_join(
    info_df,
    tidy_data,
    by = "id"
  )
dplyr::glimpse(tidy_data2)
#> Observations: 26,256
#> Variables: 10
#> $ id                     <int> 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, ...
#> $ `Path from root`       <chr> "paclobutrazole study 3 > Chemical", "p...
#> $ Perturbations          <chr> "paclobutrazole study 3 / untreated lea...
#> $ Experiment             <chr> "AT-00001", "AT-00001", "AT-00001", "AT...
#> $ `Experiment Title`     <chr> "GSE00000: hoge1", "GSE00000: hoge1", "...
#> $ `# samples experiment` <chr> "5", "5", "5", "5", "5", "5", "5", "5",...
#> $ `# samples control`    <chr> "5", "5", "5", "5", "5", "5", "5", "5",...
#> $ key                    <chr> "Log2-ratio", "Fold-Change", "p-value",...
#> $ value                  <dbl> 0.24797498, -0.23896333, 1.11054316, 1....
#> $ `Ensembl Gene`         <chr> "AT1G01010", "AT1G01010", "AT1G01010", ...
```

### Tidy data with data meta info

``` r
col_info <- meta2nona_col(meta_df)
col_info
#> $col_name
#> [1] "Ensembl Gene"
#> 
#> $col_data
#> [1] "AT1G01010" "AT1G01010" "AT1G01010" "AT1G01010" "AT1G01030" "AT1G01030"
#> [7] "AT1G01030" "AT1G01030"

dplyr::left_join(
  meta_df %>% dplyr::distinct(),
  tidy_data2,
  by = col_info$col_name
) %>% 
  dplyr::glimpse()
#> Observations: 26,256
#> Variables: 21
#> $ `Ensembl Gene`         <chr> "AT1G01010", "AT1G01010", "AT1G01010", ...
#> $ `Ensembl Protein`      <chr> "AT1G01010.1", "AT1G01010.1", "AT1G0101...
#> $ `Ensembl Transcript`   <chr> "AT1G01010.1", "AT1G01010.1", "AT1G0101...
#> $ Entrez                 <chr> "839580", "839580", "839580", "839580",...
#> $ `Gene Symbol`          <chr> "NAC001", "NAC001", "NAC001", "NAC001",...
#> $ RefSeq                 <chr> "NM_099983,NP_171609", "NM_099983,NP_17...
#> $ UniGene                <chr> "At.26626", "At.26626", "At.26626", "At...
#> $ UniProt                <chr> "Q0WV96", "Q0WV96", "Q0WV96", "Q0WV96",...
#> $ Gene                   <chr> "AT1G01010", "AT1G01010", "AT1G01010", ...
#> $ Measure                <chr> "261585_at", "261585_at", "261585_at", ...
#> $ `Signal Background`    <chr> "8", "8", "8", "8", "8", "8", "8", "8",...
#> $ `Expression Potential` <chr> "13", "13", "13", "13", "13", "13", "13...
#> $ id                     <int> 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, ...
#> $ `Path from root`       <chr> "paclobutrazole study 3 > Chemical", "p...
#> $ Perturbations          <chr> "paclobutrazole study 3 / untreated lea...
#> $ Experiment             <chr> "AT-00001", "AT-00001", "AT-00001", "AT...
#> $ `Experiment Title`     <chr> "GSE00000: hoge1", "GSE00000: hoge1", "...
#> $ `# samples experiment` <chr> "5", "5", "5", "5", "5", "5", "5", "5",...
#> $ `# samples control`    <chr> "5", "5", "5", "5", "5", "5", "5", "5",...
#> $ key                    <chr> "Log2-ratio", "Fold-Change", "p-value",...
#> $ value                  <dbl> 0.24797498, -0.23896333, 1.11054316, 1....
```
