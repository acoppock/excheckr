# Check that `_s` (Glass's delta) variables are correctly standardized

For each `_s`-suffixed outcome variable, reports the control-group SD
(which should equal 1.0 by construction), the treatment-group SD, and
the ratio of treatment SD to control SD. A treatment-to-control SD ratio
far from 1 indicates that the treatment changed the outcome variance,
which is exactly the situation where Glass's delta is preferable to
Cohen's d.

## Usage

``` r
check_s_scaling(
  data,
  treatment,
  control_value = 0,
  study_id = NULL,
  outcomes = NULL,
  prefixes = c("D_", "Y_")
)
```

## Arguments

- data:

  A data frame, after
  [`scale_by_control()`](https://alexandercoppock.com/excheckr/reference/scale_by_control.md)
  has been applied.

- treatment:

  Character scalar. Name of the treatment column.

- control_value:

  Scalar. Value of `treatment` identifying the control group (default:
  `0`). An error is raised when no row matches, rather than returning a
  table of `NA`. All arms other than this one are pooled into
  `treatment_sd`, so with three or more arms that column is a pooled
  figure and not one arm's.

- study_id:

  Optional character scalar. If provided, a `study_id` column is
  appended to the returned tibble.

- outcomes:

  Character vector of `_s` column names to check, or `NULL` (default) to
  auto-select by `prefixes` and `"_s"` suffix.

- prefixes:

  Character vector of column-name prefixes used for auto-selection when
  `outcomes` is `NULL` (default: `c("D_", "Y_")`).

## Value

A tibble with columns `variable`, `control_sd`, `treatment_sd`,
`sd_ratio`, `control_sd_ok`, and optionally `study_id`. `control_sd_ok`
is `TRUE` when `control_sd` rounds to 1.000 (within floating-point
tolerance), confirming the standardization is correct.

## Details

Auto-selects columns whose names start with `D_` or `Y_` and end with
`_s`. Supply `outcomes` to override this selection.

## See also

[`scale_by_control`](https://alexandercoppock.com/excheckr/reference/scale_by_control.md)

Other outcome scaling:
[`scale_by_control()`](https://alexandercoppock.com/excheckr/reference/scale_by_control.md)

## Examples

``` r
dat <- data.frame(
  Z = c(0L, 0L, 0L, 1L, 1L, 1L),
  D_belief_01 = c(0.2, 0.4, 0.3, 0.6, 0.8, 0.7),
  Y_attitude_01 = c(0.3, 0.5, 0.4, 0.4, 0.6, 0.5)
)
dat <- scale_by_control(dat, treatment = "Z")
check_s_scaling(dat, treatment = "Z")
#> # A tibble: 2 × 5
#>   variable     control_sd treatment_sd sd_ratio control_sd_ok
#>   <chr>             <dbl>        <dbl>    <dbl> <lgl>        
#> 1 D_belief_s            1            1    1     TRUE         
#> 2 Y_attitude_s          1            1    1.000 TRUE         
```
