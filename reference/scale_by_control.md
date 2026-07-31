# Scale outcome variables by control-group standard deviation (Glass's delta)

For each outcome variable, divides by its standard deviation in the
control group to produce a standardized version. The standardized
variable is added to the data frame with a `_s` suffix.

## Usage

``` r
scale_by_control(
  data,
  treatment,
  control_value = 0,
  outcomes = NULL,
  prefixes = c("D_", "Y_"),
  strip_suffix = "_01"
)
```

## Arguments

- data:

  A data frame.

- treatment:

  Character scalar. Name of the treatment column.

- control_value:

  Scalar. Value of `treatment` identifying the control group (default:
  `0`).

- outcomes:

  Character vector of column names to standardize, or `NULL` (default)
  to auto-select by `prefixes`.

- prefixes:

  Character vector of column-name prefixes used for auto-selection when
  `outcomes` is `NULL` (default: `c("D_", "Y_")`).

- strip_suffix:

  Suffix removed from a column's name before `"_s"` is appended, or
  `NULL` to append to the name unchanged. The default `"_01"` suits a
  convention where a rescaled-to-unit-interval outcome carries that
  marker, so `D_belief_01` becomes `D_belief_s` rather than
  `D_belief_01_s`: the value is no longer on the unit interval once
  divided by an SD, so keeping the marker would be a lie. Pass `NULL` if
  your names carry no such marker.

## Value

The input data frame with the standardized columns appended. Each is
named for its source with `strip_suffix` removed and `"_s"` added, so
`Y_turnout` yields `Y_turnout_s` and `Y_turnout_01` yields
`Y_turnout_s`.

## Details

Auto-selects columns whose names start with any prefix in `prefixes`
(default: `c("D_", "Y_")`), excluding columns ending with `"_missing"`
or `"_s"`. Supply `outcomes` to override this selection.

The resulting `_s` variables are on Glass's delta scale: a one-unit
difference equals one control-group SD. Using the control-group SD as
the standardizer is Glass's delta (Glass, 1976), which is preferred when
the treatment may change the variance of the outcome. Dividing by the
pooled SD (Cohen's d) would conflate effect-size estimation with
variance changes induced by the treatment. The `_s` variables are
intentionally excluded from
[`check_y_bounds()`](https://alexandercoppock.com/excheckr/reference/check_y_bounds.md)
(which skips `"_s"` columns), since standardized values are not bounded
to \[0, 1\]. Use
[`check_s_scaling()`](https://alexandercoppock.com/excheckr/reference/check_s_scaling.md)
to verify the standardization and inspect treatment-arm variance.

## References

Glass, G. V. (1976). Primary, secondary, and meta-analysis of research.
*Educational Researcher*, *5*(10), 11–17.
[doi:10.3102/0013189X005010003](https://doi.org/10.3102/0013189X005010003)

## See also

Other outcome scaling:
[`check_s_scaling()`](https://alexandercoppock.com/excheckr/reference/check_s_scaling.md)

## Examples

``` r
dat <- data.frame(
  Z = c(0L, 0L, 0L, 1L, 1L, 1L),
  D_belief_01 = c(0.2, 0.4, 0.3, 0.6, 0.8, 0.7),
  Y_attitude_01 = c(0.3, 0.5, 0.4, 0.4, 0.6, 0.5)
)

# _01 is stripped, so the new columns are D_belief_s and Y_attitude_s
names(scale_by_control(dat, treatment = "Z"))
#> [1] "Z"             "D_belief_01"   "Y_attitude_01" "D_belief_s"   
#> [5] "Y_attitude_s" 

# Keep the full source name instead
names(scale_by_control(dat, treatment = "Z", strip_suffix = NULL))
#> [1] "Z"               "D_belief_01"     "Y_attitude_01"   "D_belief_01_s"  
#> [5] "Y_attitude_01_s"
```
