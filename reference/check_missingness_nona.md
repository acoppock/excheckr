# Check covariate missingness and imputed-version coverage

For each base covariate, reports the number and percentage of missing
values, whether an imputed companion column exists (identified by
`nona_suffix`), and whether that companion itself contains any missing
values.

## Usage

``` r
check_missingness_nona(
  data,
  study_id = NULL,
  covariates = NULL,
  prefix = "X_",
  nona_suffix = "_nona",
  exclude = NULL
)
```

## Arguments

- data:

  A data frame or tibble.

- study_id:

  Optional character scalar. If provided, a `study_id` column is
  appended to the returned tibble.

- covariates:

  Columns to check. Supply unquoted names or tidyselect helpers (e.g.,
  `starts_with("cov_")`), or a character vector of column names. If
  omitted, all columns starting with `prefix` are used (excluding
  `nona_suffix` and `"_missing"` suffixes).

- prefix:

  Character string. Prefix used to auto-select covariate columns when
  `covariates` is omitted (default: `"X_"`).

- nona_suffix:

  Character string. Suffix that identifies imputed companion columns
  (default: `"_nona"`). Used both to exclude companion columns from the
  base selection and to look them up when checking `nona_has_na`.

- exclude:

  Additional columns to drop from the selection. Supply unquoted names
  or tidyselect helpers (e.g., `ends_with("_old")`), or a character
  vector of exact column names. Applied after `covariates` is resolved.
  `NULL` (the default) means no additional exclusions.

## Value

A tibble with columns `variable`, `n_missing`, `pct_missing`,
`has_nona_version`, `nona_has_na`, and optionally `study_id`.
`nona_has_na` is `NA` when no companion column exists.

## Details

When `covariates` is omitted, all columns whose names start with
`prefix` are selected, excluding any that end with `nona_suffix` or
`"_missing"`. Supply `covariates` to override this default, or `exclude`
to drop additional columns from whatever was selected.

## See also

Other per-study checks:
[`check_attrition()`](https://alexandercoppock.com/excheckr/reference/check_attrition.md),
[`check_balance()`](https://alexandercoppock.com/excheckr/reference/check_balance.md),
[`check_covariate_missingness()`](https://alexandercoppock.com/excheckr/reference/check_covariate_missingness.md),
[`check_smd()`](https://alexandercoppock.com/excheckr/reference/check_smd.md),
[`check_y_bounds()`](https://alexandercoppock.com/excheckr/reference/check_y_bounds.md)

## Examples

``` r
dat <- data.frame(
  X_age      = c(25, NA, 30),
  X_age_nona = c(25, 27, 30),
  X_income   = c(NA, 50, 60)
)
check_missingness_nona(dat)
#> # A tibble: 2 × 5
#>   variable n_missing pct_missing has_nona_version nona_has_na
#>   <chr>        <int>       <dbl> <lgl>            <lgl>      
#> 1 X_age            1       0.333 TRUE             FALSE      
#> 2 X_income         1       0.333 FALSE            NA         
check_missingness_nona(dat, study_id = "my_study")
#> # A tibble: 2 × 6
#>   variable n_missing pct_missing has_nona_version nona_has_na study_id
#>   <chr>        <int>       <dbl> <lgl>            <lgl>       <chr>   
#> 1 X_age            1       0.333 TRUE             FALSE       my_study
#> 2 X_income         1       0.333 FALSE            NA          my_study
check_missingness_nona(dat, covariates = "X_age")
#> # A tibble: 1 × 5
#>   variable n_missing pct_missing has_nona_version nona_has_na
#>   <chr>        <int>       <dbl> <lgl>            <lgl>      
#> 1 X_age            1       0.333 TRUE             FALSE      
check_missingness_nona(dat, nona_suffix = "_imputed")
#> # A tibble: 3 × 5
#>   variable   n_missing pct_missing has_nona_version nona_has_na
#>   <chr>          <int>       <dbl> <lgl>            <lgl>      
#> 1 X_age              1       0.333 FALSE            NA         
#> 2 X_age_nona         0       0     FALSE            NA         
#> 3 X_income           1       0.333 FALSE            NA         
```
