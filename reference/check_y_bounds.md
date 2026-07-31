# Check that outcome variables are within \[0, 1\]

Summarises the min and max of each outcome variable and flags any that
fall outside \[0, 1\].

## Usage

``` r
check_y_bounds(
  data,
  study_id = NULL,
  outcomes = NULL,
  prefix = "Y_",
  exclude = NULL
)
```

## Arguments

- data:

  A data frame or tibble.

- study_id:

  Optional character scalar. If provided, a `study_id` column is
  appended to the returned tibble.

- outcomes:

  Columns to check. Supply unquoted names or tidyselect helpers (e.g.,
  `starts_with("outcome_")`), or a character vector of column names. If
  omitted, all columns starting with `prefix` are used (excluding
  `"_missing"` and `"_s"` suffixes).

- prefix:

  Character string. Prefix used to auto-select outcome columns when
  `outcomes` is omitted (default: `"Y_"`).

- exclude:

  Additional columns to drop from the selection. Supply unquoted names
  or tidyselect helpers (e.g., `ends_with("_raw")`), or a character
  vector of exact column names. Applied after `outcomes` is resolved.
  `NULL` (the default) means no additional exclusions.

## Value

A tibble with columns `variable`, `min`, `max`, `in_bounds`, and
optionally `study_id`. A column with no numeric values at all gets `NA`
for all three, so that an absent outcome is not reported as an
out-of-bounds one.

## Details

When `outcomes` is omitted, all columns whose names start with `prefix`
are selected, excluding any that end with `"_missing"` or `"_s"`
(standardised versions). Supply `outcomes` to override this default, or
`exclude` to drop additional columns from whatever was selected.

## See also

Other per-study checks:
[`check_attrition()`](https://alexandercoppock.com/excheckr/reference/check_attrition.md),
[`check_balance()`](https://alexandercoppock.com/excheckr/reference/check_balance.md),
[`check_covariate_missingness()`](https://alexandercoppock.com/excheckr/reference/check_covariate_missingness.md),
[`check_missingness_nona()`](https://alexandercoppock.com/excheckr/reference/check_missingness_nona.md),
[`check_smd()`](https://alexandercoppock.com/excheckr/reference/check_smd.md)

## Examples

``` r
dat <- data.frame(Y_support = c(0, 0.5, 1), Y_oppose = c(0, 1.2, 0.8))
check_y_bounds(dat)
#> # A tibble: 2 × 4
#>   variable    min   max in_bounds
#>   <chr>     <dbl> <dbl> <lgl>    
#> 1 Y_support     0   1   TRUE     
#> 2 Y_oppose      0   1.2 FALSE    
check_y_bounds(dat, study_id = "my_study")
#> # A tibble: 2 × 5
#>   variable    min   max in_bounds study_id
#>   <chr>     <dbl> <dbl> <lgl>     <chr>   
#> 1 Y_support     0   1   TRUE      my_study
#> 2 Y_oppose      0   1.2 FALSE     my_study
check_y_bounds(dat, outcomes = "Y_support")
#> # A tibble: 1 × 4
#>   variable    min   max in_bounds
#>   <chr>     <dbl> <dbl> <lgl>    
#> 1 Y_support     0     1 TRUE     
```
