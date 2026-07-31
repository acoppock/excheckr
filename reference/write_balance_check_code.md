# Write balance check code

Generates code to perform balance checks by regressing each covariate on
treatment assignment.

## Usage

``` r
write_balance_check_code(
  data,
  treatment,
  covariates = NULL,
  .method = estimatr::lm_robust,
  ...
)
```

## Arguments

- data:

  A data frame or tibble.

- treatment:

  Unquoted name of the treatment variable.

- covariates:

  Character vector of covariate names, or unquoted column names using
  tidyselect helpers. If left empty, all \`"X\_"\` columns are used.

- .method:

  Regression function to use (default: \`estimatr::lm_robust\`).

- ...:

  Additional arguments passed to \`.method\` (e.g., \`clusters\`,
  \`se_type\`).

## Value

Invisibly returns the generated code as a single string.

## Details

This function prints R code to the console that you can copy-paste into
your analysis script. It does not perform the balance check itself. The
joint balance test is generated as a call to
[`check_balance()`](https://alexandercoppock.com/excheckr/reference/check_balance.md),
since the cross-equation Wald test is too complex to emit as copy-paste
code.

## See also

Other code generators:
[`write_attrition_check_code()`](https://alexandercoppock.com/excheckr/reference/write_attrition_check_code.md),
[`write_covariate_imputation_code()`](https://alexandercoppock.com/excheckr/reference/write_covariate_imputation_code.md),
[`write_outcome_missingness_dummies_code()`](https://alexandercoppock.com/excheckr/reference/write_outcome_missingness_dummies_code.md)

## Examples

``` r
set.seed(42)
dat <- data.frame(
  Z = rep(c(0L, 1L), 100),
  X_age = rnorm(200, 50, 10),
  X_gender = sample(c("M", "F"), 200, replace = TRUE)
)
dat$X_income <- 50000 + 3000 * dat$Z + rnorm(200, 0, 10000)

write_balance_check_code(dat, Z)
#> # Balance check for X_age
#> glance(lm_robust(X_age ~ Z, data = dat))
#> 
#> # Balance check for X_gender (level: M)
#> dat$X_gender_M <- as.integer(dat$X_gender == 'M')
#> glance(lm_robust(X_gender_M ~ Z, data = dat))
#> 
#> # Balance check for X_income
#> glance(lm_robust(X_income ~ Z, data = dat))
#> 
#> # Joint balance test (all covariates)
#> dat$.Z_numeric <- as.numeric(as.factor(dat$Z)) - 1
#> glance(lm_robust(.Z_numeric ~ X_age + X_gender_M + X_income, data = dat)) 

# \donttest{
# Cluster-randomized experiment (requires randomizr)
if (requireNamespace("randomizr", quietly = TRUE)) {
  dat_cl <- data.frame(cluster_id = rep(1:20, each = 10))
  dat_cl$Z <- randomizr::cluster_ra(clusters = dat_cl$cluster_id)
  dat_cl$X_age <- rnorm(200, 50, 10)
  dat_cl$X_income <- 50000 + rnorm(200, 0, 10000)
  write_balance_check_code(dat_cl, Z, clusters = cluster_id)
}
#> # Balance check for X_age
#> glance(lm_robust(X_age ~ Z, data = dat_cl, clusters = cluster_id))
#> 
#> # Balance check for X_income
#> glance(lm_robust(X_income ~ Z, data = dat_cl, clusters = cluster_id))
#> 
#> # Joint balance test (all covariates)
#> dat_cl$.Z_numeric <- as.numeric(as.factor(dat_cl$Z)) - 1
#> glance(lm_robust(.Z_numeric ~ X_age + X_income, data = dat_cl, clusters = cluster_id)) 
# }
```
