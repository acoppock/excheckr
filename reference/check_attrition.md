# Check differential attrition across treatment conditions

Performs attrition checks by regressing outcome missingness indicators
on treatment assignment to test for differential attrition.

## Usage

``` r
check_attrition(
  data,
  treatment,
  outcomes = NULL,
  covariates = NULL,
  .method = estimatr::lm_robust,
  study_id = NULL,
  quiet = TRUE,
  .by = NULL,
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

- covariates:

  Character vector of covariate names, or unquoted column names using
  tidyselect helpers. If provided, fits a Lin (2013) model interacting
  treatment with demeaned covariates and performs an F-test comparing
  the full model (missingness ~ treatment \* covariates) to the
  restricted model (missingness ~ covariates).

- .method:

  Regression function to use (default: \`estimatr::lm_robust\`). Must
  accept formula and data arguments.

- study_id:

  Optional character scalar. If provided, a `study_id` column holding
  this value is appended to every returned tibble.

- quiet:

  Logical. The default `TRUE` returns the result, which auto-prints at
  the console. `FALSE` prints a labelled report instead and returns the
  same object invisibly, so nothing is printed twice.

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

- ...:

  Additional arguments passed to \`.method\` (e.g., \`clusters\`,
  \`se_type\`).

## Value

When `covariates` is `NULL`, a tibble with one row per outcome and
columns `outcome`, `F_stat`, `df1`, `df2`, `p_value`, `nobs`, and
`estimable`. The test is the omnibus F-test from regressing the
missingness indicator on treatment, so it covers multi-armed treatments
as well as binary ones; it reports no coefficient, because with three or
more arms there is no single number to report. Use `covariates` (below)
if you want the coefficients themselves.

When `covariates` is provided, a list with three elements:

- simple:

  The covariate-free test above, computed on the same data, so that
  asking for the interacted test does not cost you the simpler one.

- coefficients:

  A tibble of all coefficient estimates from the Lin model (treatment,
  demeaned covariates, and their interactions).

- f_test:

  A tibble with one row per outcome containing the Wald F-test of joint
  significance of treatment and treatment-by-covariate interactions,
  plus an `estimable` column.

## Details

For each outcome variable, creates a missingness indicator (1 if
missing, 0 otherwise) and regresses it on treatment assignment.
Significant coefficients indicate differential attrition across
treatment conditions.

If missingness indicator variables already exist (with \`\_missing\`
suffix), those are used. Otherwise, they are created on the fly.

When covariates are provided, the function follows the Lin (2013)
estimator approach used in
[`estimatr::lm_lin`](https://declaredesign.org/r/estimatr/reference/lm_lin.html):
covariates are demeaned by subtracting the full-sample mean, then the
full model `missingness ~ treatment * (demeaned covariates)` is fit. A
Wald F-test compares this to the restricted model
`missingness ~ demeaned covariates`, testing whether treatment and its
interactions with covariates jointly predict attrition.

## Read the two tests together, not one instead of the other

The covariate-free test asks whether dropout rates differ across arms.
The interacted test asks the more demanding question of whether they
differ *anywhere* in covariate space, and it spends a degree of freedom
per covariate per arm to ask it. Neither subsumes the other, and the
covariate-free one is usually the criterion to act on:

- It is sharper for the arm-level question. On a simulated study with
  planted differential attrition and two covariates, the covariate-free
  test returns `p = 0.0006` on 1 degree of freedom where the interacted
  test returns `0.0066` on 3.

- It is well calibrated under cluster randomization, at 4.3 percent
  against a nominal 5, where the interacted test rejects about 12
  percent of the time and no correction repairs it.

- It cannot run out of degrees of freedom. The interacted test can,
  which is what
  [`estimatrTools::check_attrition_lasso`](https://alexandercoppock.com/estimatrTools/reference/check_attrition_lasso.html)
  exists to address.

Earlier versions returned only the interacted test when `covariates` was
supplied, so projects that wanted both called the function twice. Both
now come back from one call, and the second fit is no longer wasted.

## When there is no p-value, and what that means

`status` records why a row has no p-value, because the reasons mean
opposite things and collapsing them distorts any rate computed from the
result. It takes four values, and `estimable` is simply
`status == "tested"`:

- `"tested"`:

  A p-value was computed. Only these belong in a uniform-reference
  diagnostic.

- `"no_attrition"`:

  Nobody was missing this outcome. **This is a pass, not a missing
  test**: with no attrition there can be no differential attrition, so
  the design question is answered in the affirmative.

- `"all_missing"`:

  Everybody was missing it, so nothing can be learned. Uninformative,
  and usually a sign the outcome was not asked of this subgroup.

- `"not_estimable"`:

  The indicator varies but no statistic came back, from rank deficiency
  or a degenerate robust covariance matrix at very low missingness.
  Uninformative.

## Two different rates, and which one you want

Because `"no_attrition"` is a pass rather than a gap, there are two
defensible rates and they answer different questions. On one real corpus
of 1320 study-arm rows, 31 flagged at `alpha = 0.05`, 849 had no
attrition, 51 were uninformative, and 389 were tested and passed:

- **How much differential attrition is in this corpus?** Count the
  no-attrition rows as passes: `31 / 1269 = 2.4%`. Exclude only the
  genuinely uninformative rows. This is the number for a sentence about
  how much attrition trouble a corpus has.

- **Are the computed p-values uniform, as a valid design implies?** Use
  only tested rows: `31 / 420 = 7.4%`. A row with no attrition produces
  no draw from Uniform(0, 1), so it cannot enter this comparison at all.

Quoting the second while describing the first overstates the failure
rate roughly threefold, since it silently narrows the denominator from
every row where the question applies to only those rows with some
attrition. Quoting the first while testing uniformity is the mirror
error, and is what reporting `p_value = 1` for the no-attrition rows
used to produce: it put a spike of 849 ones at the top of the
distribution, which made
[`summarize_check_pvalues`](https://alexandercoppock.com/excheckr/reference/summarize_check_pvalues.md)
report a badly non-uniform collection when nothing was wrong, while
moving `pct_below` in the reassuring direction.

`n_missing` is returned so either rate can be computed without going
back to the data.

## Clustered designs

Passing `clusters` and `se_type = "CR2"` is enough for the
covariate-free test, which is well calibrated (4.3 percent rejection at
a nominal 5 percent in a 30-cluster simulation). It is not enough for
the covariate-adjusted Wald test, which rejects about 12 percent of the
time in the same design. The cause is not the denominator degrees of
freedom, so no correction fixes it: substituting the number of clusters
for the residual degrees of freedom moves the rejection rate from 11.4
to 11.2 percent. The cluster-robust variance estimator is itself biased
downward when treatment is constant within cluster. A warning is emitted
when `clusters` is supplied with `covariates`; treat that p-value as
descriptive.

## See also

Other per-study checks:
[`check_balance()`](https://alexandercoppock.com/excheckr/reference/check_balance.md),
[`check_covariate_missingness()`](https://alexandercoppock.com/excheckr/reference/check_covariate_missingness.md),
[`check_missingness_nona()`](https://alexandercoppock.com/excheckr/reference/check_missingness_nona.md),
[`check_smd()`](https://alexandercoppock.com/excheckr/reference/check_smd.md),
[`check_y_bounds()`](https://alexandercoppock.com/excheckr/reference/check_y_bounds.md)

## Examples

``` r
set.seed(42)
n <- 200
dat <- data.frame(
  Z = rep(c(0L, 1L), n / 2),
  X_age = rnorm(n, 50, 10),
  X_income = rnorm(n, 50000, 10000)
)
dat$Y_attitude <- rnorm(n)
dat$Y_attitude[which(rbinom(n, 1, ifelse(dat$Z == 1, 0.35, 0.15)) == 1)] <- NA
dat$Y_behavior <- rnorm(n)
dat$Y_behavior[which(rbinom(n, 1, 0.15) == 1)] <- NA

# Simple attrition check (no covariates)
check_attrition(dat, Z)
#> # A tibble: 2 × 9
#>   outcome    F_stat   df1   df2   p_value  nobs n_missing status estimable
#>   <chr>       <dbl> <int> <int>     <dbl> <int>     <int> <chr>  <lgl>    
#> 1 Y_attitude 17.5       1   198 0.0000427   200        46 tested TRUE     
#> 2 Y_behavior  0.791     1   198 0.375       200        39 tested TRUE     

# With covariates: Lin model + F-test for differential attrition
check_attrition(dat, Z, covariates = c("X_age", "X_income"))
#> $simple
#> # A tibble: 2 × 9
#>   outcome    F_stat   df1   df2   p_value  nobs n_missing status estimable
#>   <chr>       <dbl> <int> <int>     <dbl> <int>     <int> <chr>  <lgl>    
#> 1 Y_attitude 17.5       1   198 0.0000427   200        46 tested TRUE     
#> 2 Y_behavior  0.791     1   198 0.375       200        39 tested TRUE     
#> 
#> $coefficients
#>       outcome         term      estimate    std.error   statistic      p.value
#> 1  Y_attitude  (Intercept)  1.119372e-01 3.105714e-02  3.60423546 3.979314e-04
#> 2  Y_attitude            Z  2.378656e-01 5.747820e-02  4.13836160 5.210626e-05
#> 3  Y_attitude      X_age_c  4.972012e-03 3.058514e-03  1.62563004 1.056518e-01
#> 4  Y_attitude   X_income_c -7.077492e-06 3.413156e-06 -2.07359156 3.943805e-02
#> 5  Y_attitude    Z:X_age_c -3.796488e-03 5.788502e-03 -0.65586713 5.126867e-01
#> 6  Y_attitude Z:X_income_c  1.036711e-05 6.120398e-06  1.69386167 9.189643e-02
#> 7  Y_behavior  (Intercept)  2.201731e-01 4.206262e-02  5.23441314 4.279834e-07
#> 8  Y_behavior            Z -5.053748e-02 5.669494e-02 -0.89139298 3.738226e-01
#> 9  Y_behavior      X_age_c  7.491882e-04 4.659747e-03  0.16077872 8.724350e-01
#> 10 Y_behavior   X_income_c  1.193539e-06 4.080845e-06  0.29247341 7.702373e-01
#> 11 Y_behavior    Z:X_age_c  6.928421e-04 6.142124e-03  0.11280171 9.103044e-01
#> 12 Y_behavior Z:X_income_c  5.122980e-07 5.496637e-06  0.09320208 9.258392e-01
#>         conf.low     conf.high  df
#> 1   5.068425e-02  1.731902e-01 194
#> 2   1.245032e-01  3.512279e-01 194
#> 3  -1.060196e-03  1.100422e-02 194
#> 4  -1.380915e-05 -3.458347e-07 194
#> 5  -1.521296e-02  7.619987e-03 194
#> 6  -1.703955e-06  2.243817e-05 194
#> 7   1.372144e-01  3.031319e-01 194
#> 8  -1.623551e-01  6.128012e-02 194
#> 9  -8.441080e-03  9.939457e-03 194
#> 10 -6.854980e-06  9.242057e-06 194
#> 11 -1.142107e-02  1.280675e-02 194
#> 12 -1.032854e-05  1.135314e-05 194
#> 
#> $f_test
#> # A tibble: 2 × 7
#>   outcome    F_stat   df1   df2   p_value status estimable
#>   <chr>       <dbl> <int> <int>     <dbl> <chr>  <lgl>    
#> 1 Y_attitude  8.08      3   194 0.0000426 tested TRUE     
#> 2 Y_behavior  0.279     3   194 0.841     tested TRUE     
#> 

# \donttest{
# Cluster-randomized experiment (requires randomizr)
if (requireNamespace("randomizr", quietly = TRUE)) {
  dat_cl <- data.frame(cluster_id = rep(1:20, each = 10))
  dat_cl$Z <- randomizr::cluster_ra(clusters = dat_cl$cluster_id)
  dat_cl$Y_outcome <- 0.5 * dat_cl$Z + rnorm(200)
  dat_cl$Y_outcome[which(rbinom(200, 1, ifelse(dat_cl$Z == 1, 0.30, 0.10)) == 1)] <- NA
  check_attrition(dat_cl, Z, clusters = cluster_id)
}
#> # A tibble: 1 × 9
#>   outcome   F_stat   df1   df2 p_value  nobs n_missing status estimable
#>   <chr>      <dbl> <int> <int>   <dbl> <int>     <int> <chr>  <lgl>    
#> 1 Y_outcome   12.1     1     9 0.00254   200        39 tested TRUE     
# }
```
