# Standardized mean differences between treatment arms

Reports, for each covariate and each non-reference treatment arm, the
difference in means against the reference arm divided by the reference
arm's standard deviation. Standardizing by the reference (control) SD
rather than the pooled SD keeps the denominator fixed across arms, so
the values are comparable to each other and are not moved by
treatment-induced changes in variance.

## Usage

``` r
check_smd(
  data,
  treatment,
  covariates = NULL,
  reference = NULL,
  weights = NULL,
  study_id = NULL,
  threshold = 0.1,
  .by = NULL
)
```

## Arguments

- data:

  A data frame or tibble.

- treatment:

  Unquoted name of the treatment variable.

- covariates:

  Character vector of covariate names, or unquoted column names using
  tidyselect helpers. If omitted, all `"X_"` columns are used.

- reference:

  Value of `treatment` to use as the reference arm. Defaults to the
  first factor level, or the smallest value for numeric and character
  treatments. That default is a guess based on ordering, not on meaning:
  if the control arm is not the first level, every difference is
  computed against a treatment arm instead and every sign flips. The arm
  actually used is reported in the `reference` column of the result, so
  check it rather than assume it, and pass `reference` explicitly when
  the control arm is not first.

- weights:

  Optional character scalar naming a column of survey weights. When
  supplied, the arm means and the reference SD are computed on that
  weighted basis. Supply it whenever the balance tests beside this call
  are weighted, since an unweighted SMD and a weighted p-value describe
  two different samples and reading them together will mislead.

- study_id:

  Optional character scalar. If provided, a `study_id` column holding
  this value is appended to the result.

- threshold:

  Absolute SMD above which `flag` is `TRUE` (default `0.1`, a common
  rule of thumb).

- .by:

  Optional tidyselect expression naming columns to run the check
  separately within, e.g. `.by = c(X_pid_3, topic)`. Treatment assigned
  within strata makes a whole-sample check answer the wrong question, so
  this splits the data, runs the check on each stratum, and stacks the
  results with the grouping columns prepended. The return shape is
  unchanged, so it composes with every other argument. Strata are
  returned in order of first appearance and `NA` forms its own stratum,
  matching `tidyr::nest(.by = )`.

  Without it the caller has to write the `nest` / `map` / `unnest`
  plumbing by hand and then attach `study_id` afterwards, because the
  argument cannot survive the `map`. With it, `study_id` works.

## Value

A tibble with one row per covariate-level-by-arm contrast and columns
`covariate`, `level`, `arm`, `reference`, `n_arm`, `n_reference`,
`mean_arm`, `mean_reference`, `sd_reference`, `smd`, and `flag`.

## Details

Standardized mean differences complement the p-values from
[`check_balance`](https://alexandercoppock.com/excheckr/reference/check_balance.md):
a p-value answers "is this difference bigger than chance", an SMD
answers "is it big enough to matter". In a large sample a trivial
imbalance can be significant, and in a small one a substantial imbalance
can fail to reach significance.

Factor and character covariates are expanded to one indicator per
non-reference level and each indicator is reported separately.

## See also

Other per-study checks:
[`check_attrition()`](https://alexandercoppock.com/excheckr/reference/check_attrition.md),
[`check_balance()`](https://alexandercoppock.com/excheckr/reference/check_balance.md),
[`check_covariate_missingness()`](https://alexandercoppock.com/excheckr/reference/check_covariate_missingness.md),
[`check_missingness_nona()`](https://alexandercoppock.com/excheckr/reference/check_missingness_nona.md),
[`check_y_bounds()`](https://alexandercoppock.com/excheckr/reference/check_y_bounds.md)

## Examples

``` r
set.seed(42)
dat <- data.frame(
  Z = rep(c(0L, 1L), 100),
  X_age = rnorm(200, 50, 10),
  X_party = factor(sample(c("D", "R", "I"), 200, replace = TRUE))
)
dat$X_income <- 50000 + 3000 * dat$Z + rnorm(200, 0, 10000)
check_smd(dat, Z)
#> # A tibble: 4 × 11
#>   covariate level arm   reference n_arm n_reference mean_arm mean_reference
#>   <chr>     <chr> <chr> <chr>     <int>       <int>    <dbl>          <dbl>
#> 1 X_age     NA    1     0           100         100    50.0           49.4 
#> 2 X_party   I     1     0           100         100     0.24           0.4 
#> 3 X_party   R     1     0           100         100     0.41           0.33
#> 4 X_income  NA    1     0           100         100 51323.         51737.  
#> # ℹ 3 more variables: sd_reference <dbl>, smd <dbl>, flag <lgl>
```
