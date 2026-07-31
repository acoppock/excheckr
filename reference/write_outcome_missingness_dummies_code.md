# Write outcome missingness code

Generates tidyverse-style \`mutate()\` code to generate missingness
dummy variables (with suffix \`\_missing\`) for selected variables.

## Usage

``` r
write_outcome_missingness_dummies_code(data, ...)
```

## Arguments

- data:

  A data frame or tibble.

- ...:

  Columns to generate missingness dummy variables for. You can specify
  them unquoted (e.g., \`age\`, \`income\`) or using selection helpers
  such as \[dplyr::all_of()\] or \[tidyselect::starts_with()\]. If left
  empty, all \`"Y\_"\` columns are used.

## Value

Invisibly returns the generated code as a single string.

## Details

This function prints mutate code to the console that you can copy-paste
into your cleaning script. It does not create the variables itself.

## See also

Other code generators:
[`write_attrition_check_code()`](https://alexandercoppock.com/excheckr/reference/write_attrition_check_code.md),
[`write_balance_check_code()`](https://alexandercoppock.com/excheckr/reference/write_balance_check_code.md),
[`write_covariate_imputation_code()`](https://alexandercoppock.com/excheckr/reference/write_covariate_imputation_code.md)

## Examples

``` r
# Example data with missingness
dat <- data.frame(
   Y_attitude = rep(c(1, 2, 3, 4, 5, NA), c(10, 20, 30, 40, 50, 50)),
   Y_behavior = rep(c(0, 1, NA), c(100, 50, 50))
)

# Generate missingness dummy code
write_outcome_missingness_dummies_code(dat, Y_attitude, Y_behavior)
#> dat <-
#>   dat |>
#>   mutate(
#>     Y_attitude_missing = if_else(is.na(Y_attitude), 1, 0),
#>     Y_behavior_missing = if_else(is.na(Y_behavior), 1, 0)
#>   ) 

# Or default to all "Y_" columns
write_outcome_missingness_dummies_code(dat)
#> dat <-
#>   dat |>
#>   mutate(
#>     Y_attitude_missing = if_else(is.na(Y_attitude), 1, 0),
#>     Y_behavior_missing = if_else(is.na(Y_behavior), 1, 0)
#>   ) 

# Or use tidyselect helpers
vars <- c("Y_attitude", "Y_behavior")
write_outcome_missingness_dummies_code(dat, dplyr::all_of(vars))
#> dat <-
#>   dat |>
#>   mutate(
#>     Y_attitude_missing = if_else(is.na(Y_attitude), 1, 0),
#>     Y_behavior_missing = if_else(is.na(Y_behavior), 1, 0)
#>   ) 
```
