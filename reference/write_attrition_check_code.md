# Write attrition check code

Generates code to perform attrition checks by regressing outcome
missingness indicators on treatment assignment.

## Usage

``` r
write_attrition_check_code(
  data,
  treatment,
  outcomes = NULL,
  .method = estimatr::lm_robust,
  ...
)
```

## Arguments

- data:

  A data frame or tibble.

- treatment:

  Unquoted name of the treatment variable.

- outcomes:

  Character vector of outcome variable names, or unquoted column names
  using tidyselect helpers. If left empty, all \`"Y\_"\` columns are
  used.

- .method:

  Regression function to use (default: \`estimatr::lm_robust\`).

- ...:

  Additional arguments passed to \`.method\` (e.g., \`clusters\`,
  \`se_type\`).

## Value

Invisibly returns the generated code as a single string.

## Details

This function prints R code to the console that you can copy-paste into
your analysis script. It does not perform the attrition check itself.

## See also

Other code generators:
[`write_balance_check_code()`](https://alexandercoppock.com/excheckr/reference/write_balance_check_code.md),
[`write_covariate_imputation_code()`](https://alexandercoppock.com/excheckr/reference/write_covariate_imputation_code.md),
[`write_outcome_missingness_dummies_code()`](https://alexandercoppock.com/excheckr/reference/write_outcome_missingness_dummies_code.md)

## Examples

``` r
set.seed(42)
n <- 200
dat <- data.frame(Z = rep(c(0L, 1L), n / 2))
dat$Y_attitude <- rnorm(n)
dat$Y_attitude[which(rbinom(n, 1, ifelse(dat$Z == 1, 0.35, 0.15)) == 1)] <- NA
dat$Y_behavior <- rnorm(n)
dat$Y_behavior[which(rbinom(n, 1, 0.15) == 1)] <- NA

write_attrition_check_code(dat, Z)
#> # Attrition check for Y_attitude
#> dat$Y_attitude_missing <- as.integer(is.na(dat$Y_attitude))
#> lm_robust(Y_attitude_missing ~ Z, data = dat)
#> 
#> # Attrition check for Y_behavior
#> dat$Y_behavior_missing <- as.integer(is.na(dat$Y_behavior))
#> lm_robust(Y_behavior_missing ~ Z, data = dat) 

# \donttest{
# Cluster-randomized experiment (requires randomizr)
if (requireNamespace("randomizr", quietly = TRUE)) {
  dat_cl <- data.frame(cluster_id = rep(1:20, each = 10))
  dat_cl$Z <- randomizr::cluster_ra(clusters = dat_cl$cluster_id)
  dat_cl$Y_outcome <- 0.5 * dat_cl$Z + rnorm(200)
  write_attrition_check_code(dat_cl, Z, clusters = cluster_id)
}
#> # Attrition check for Y_outcome
#> dat_cl$Y_outcome_missing <- as.integer(is.na(dat_cl$Y_outcome))
#> lm_robust(Y_outcome_missing ~ Z, data = dat_cl, clusters = cluster_id) 
# }
```
