# Write covariate imputation code

Generates tidyverse-style \`mutate()\` code to impute missing values for
selected variables. Numeric variables are imputed with the median, while
factor or character variables are imputed with the mode (user-defined).
In addition to the imputed variable (with suffix \`\_nona\`), a
missingness dummy variable (with suffix \`\_missing\`) is created for
each input.

## Usage

``` r
write_covariate_imputation_code(data, ..., include_missingness_dummies = TRUE)
```

## Arguments

- data:

  A data frame or tibble.

- ...:

  Columns to generate imputation code for. You can specify them unquoted
  (e.g., \`age\`, \`income\`) or using selection helpers such as
  \[dplyr::all_of()\] or \[tidyselect::starts_with()\]. If left empty,
  all \`"X\_"\` columns are used.

- include_missingness_dummies:

  Logical. Should missingness dummy variables be included in the
  generated code? Defaults to TRUE.

## Value

Invisibly returns the generated code as a single string.

## Details

This function prints imputation code to the console that you can
copy-paste into your analysis script. It does not perform the imputation
itself.

## See also

Other code generators:
[`write_attrition_check_code()`](https://alexandercoppock.com/excheckr/reference/write_attrition_check_code.md),
[`write_balance_check_code()`](https://alexandercoppock.com/excheckr/reference/write_balance_check_code.md),
[`write_outcome_missingness_dummies_code()`](https://alexandercoppock.com/excheckr/reference/write_outcome_missingness_dummies_code.md)

## Examples

``` r
# Example data with missingness
dat <- data.frame(
  X_factor_variable = factor(rep(c("A", "B", NA), c(4, 5, 1))),
  X_numeric_variable = c(100, 200, NA, 400, 500, NA, 700, 800, 900, NA)
)

# Generate imputation code
write_covariate_imputation_code(dat, X_factor_variable, X_numeric_variable)
#> dat <-
#>   dat |>
#>   mutate(
#> X_factor_variable_nona = replace_na(X_factor_variable, stat_mode(X_factor_variable)),
#> X_factor_variable_missing = if_else(is.na(X_factor_variable), 1, 0),
#> X_numeric_variable_nona = replace_na(X_numeric_variable, median(X_numeric_variable, na.rm = TRUE)),
#> X_numeric_variable_missing = if_else(is.na(X_numeric_variable), 1, 0)
#>   ) 

# Or default to all "X_" columns
write_covariate_imputation_code(dat)
#> dat <-
#>   dat |>
#>   mutate(
#> X_factor_variable_nona = replace_na(X_factor_variable, stat_mode(X_factor_variable)),
#> X_factor_variable_missing = if_else(is.na(X_factor_variable), 1, 0),
#> X_numeric_variable_nona = replace_na(X_numeric_variable, median(X_numeric_variable, na.rm = TRUE)),
#> X_numeric_variable_missing = if_else(is.na(X_numeric_variable), 1, 0)
#>   ) 

# Or use tidyselect helpers
vars <- c("X_factor_variable", "X_numeric_variable")
write_covariate_imputation_code(dat, dplyr::all_of(vars))
#> dat <-
#>   dat |>
#>   mutate(
#> X_factor_variable_nona = replace_na(X_factor_variable, stat_mode(X_factor_variable)),
#> X_factor_variable_missing = if_else(is.na(X_factor_variable), 1, 0),
#> X_numeric_variable_nona = replace_na(X_numeric_variable, median(X_numeric_variable, na.rm = TRUE)),
#> X_numeric_variable_missing = if_else(is.na(X_numeric_variable), 1, 0)
#>   ) 
```
