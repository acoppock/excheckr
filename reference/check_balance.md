# Check covariate balance across treatment conditions

Performs balance checks by regressing each covariate on treatment
assignment (covariate-by-covariate F-tests) and a joint test of all
covariates together. Supports both binary and multi-armed treatments.

## Usage

``` r
check_balance(
  data,
  treatment,
  covariates = NULL,
  .method = estimatr::lm_robust,
  declaration = NULL,
  sims = 1000,
  study_id = NULL,
  flatten = FALSE,
  quiet = TRUE,
  min_obs_per_vparam = 10,
  .by = NULL,
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

  Regression function to use (default: \`estimatr::lm_robust\`). Must
  accept formula and data arguments.

- declaration:

  Optional. A `randomizr` declaration object (e.g., from
  [`randomizr::declare_ra`](https://declaredesign.org/r/randomizr/reference/declare_ra.html)),
  or the string `"complete"`. When provided, the joint test uses
  randomization inference via
  [`ri2::conduct_ri`](https://alexandercoppock.com/ri2/reference/conduct_ri.html)
  instead of the parametric test. This is recommended for clustered
  designs or any design where exact inference is desired, and for
  multi-arm designs with many covariates, where the multinomial
  likelihood-ratio test's asymptotic reference distribution is
  unreliable. `"complete"` is shorthand for complete random assignment
  holding the observed arm sizes fixed; supply a real declaration
  whenever the design was blocked or clustered.

- sims:

  Integer. Number of simulations for randomization inference (default:
  1000). Only used when `declaration` is provided.

- study_id:

  Optional character scalar. If provided, a `study_id` column holding
  this value is appended to both returned tibbles.

- flatten:

  Logical. If `TRUE`, returns a single tibble with the covariate tests
  and the joint test stacked and distinguished by a `test` column,
  rather than a two-element list (default `FALSE`).

- quiet:

  Logical. The default `TRUE` returns the result, which auto-prints at
  the console. `FALSE` prints a labelled report instead and returns the
  same object invisibly, so nothing is printed twice.

- min_obs_per_vparam:

  Warn when the joint test has fewer than this many observations per
  variance parameter (default `10`). A robust Wald test estimates
  \\q(q+1)/2\\ covariance entries and then inverts them, so this ratio,
  not the coefficient count, is what governs whether its p-value can be
  trusted. The warning says to narrow the covariate battery; nothing is
  altered, because which covariates to test is your decision. Only the
  binary, no-`declaration` path uses this.

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

When `flatten = FALSE` (the default), a list with two elements:

- covariate_tests:

  A tibble with one row per covariate (or covariate level for factors),
  containing the test from regressing each covariate on treatment.
  Columns: covariate, level, F_stat, statistic, df1, df2, p_value, nobs.

- joint_test:

  A tibble with a single row containing the joint test of all covariates
  predicting treatment. Same columns, without covariate and level.

When `flatten = TRUE`, a single tibble stacking both, with a `test`
column taking values `"covariate"` and `"joint"`.

## Details

For numeric covariates, regresses the covariate on treatment directly
and extracts the overall model F-test (which jointly tests all treatment
dummies for multi-armed designs). For factor/character covariates,
creates dummy variables for each level (excluding the first level as
reference) and regresses each dummy on treatment.

The joint test strategy depends on the number of treatment arms and
whether a `declaration` is provided:

- Binary treatment, no declaration: F-test from regressing numeric
  treatment on all covariates via `.method`.

- Multi-armed treatment, no declaration: multinomial likelihood-ratio
  test via
  [`nnet::multinom`](https://rdrr.io/pkg/nnet/man/multinom.html).

- Any treatment with declaration: randomization inference using the
  multinomial LR statistic as the test function, via
  [`ri2::conduct_ri`](https://alexandercoppock.com/ri2/reference/conduct_ri.html).

## What `F_stat` contains

The `F_stat` column does not always hold an F statistic, because the
joint test is not always an F test. The `statistic` column records which
quantity it is, so that results stacked across studies stay
interpretable:

- `"F"`: a genuine F statistic. All covariate-by-covariate tests, and
  the joint test for a binary treatment with no `declaration`.

- `"LR/df"`: a multinomial likelihood-ratio statistic divided by its
  degrees of freedom, which is F-like but is not an F. The joint test
  for a multi-armed treatment with no `declaration`.

- `"LR"`: the raw multinomial likelihood-ratio statistic, undivided. The
  joint test on the randomization inference path, where the reference
  distribution is the permutation distribution rather than a parametric
  one, so there is nothing to divide by.

Stacking a mix of binary and multi-armed studies therefore puts
different quantities in one `F_stat` column. The `p_value` column is
comparable across all three; `F_stat` is not. Group by `statistic`
before comparing or plotting the statistics themselves.

## Clustered designs

The parametric joint test over-rejects badly under cluster
randomization, at about 13 percent against a nominal 5 percent in a
30-cluster simulation, even when `clusters` and `se_type = "CR2"` are
supplied. The denominator degrees of freedom are not the cause and no
correction repairs it: substituting the cluster count for the residual
degrees of freedom moves the rate from 11.4 to 11.2 percent. The
cluster-robust variance estimator is itself biased downward because
treatment is constant within cluster, so the effective sample size is
the number of clusters. Supplying `declaration` brings the same design
to 4.5 percent, and a warning is emitted when `clusters` is passed
without one.

## Why the diagnostics are columns and not warnings

Every reason a joint test is untrustworthy or absent is returned in the
data: `status` says why there is no p-value, `obs_per_param` says
whether the reference distribution can be believed, and
`p_value_classical` gives an inversion-free second opinion. The warnings
are still raised, but they are the backup, not the record.

That is a lesson learned the hard way rather than a style preference.
Callers run these checks inside `nest()` / `map()` /
[`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html), and
`dplyr` collapses warnings raised inside `map()` to a bare count with no
text. Nothing survives `write_rds()` at all, so a stacked corpus read
weeks later has only columns. In one real run a correct warning fired on
401 of 651 fits and appeared in the console as "There were 50 or more
warnings", which is how a 22.7 percent rejection rate went unnoticed.

`status` takes `"tested"`, `"arm_lost_to_missingness"`, `"single_arm"`,
`"constant_covariate"`, `"no_convergence"` or `"not_estimable"`, and
`estimable` is `status == "tested"`. `obs_per_param` is observations per
estimated parameter, which means variance parameters \\q(q+1)/2\\ on the
Wald path and coefficients \\q\\ on the likelihood-ratio path, because
that is what each reference distribution is asymptotic in. It is `NA` on
the randomization-inference path, which is exact and asymptotic in
nothing. Group by `statistic` before comparing it across studies, for
the same reason `F_stat` needs that treatment.

## Aliased covariates

warned about, never dropped for you: A covariate that is a linear
function of the others carries no separate information, and the joint
test cannot estimate a coefficient for it. When that happens
`check_balance` warns, names each redundant covariate and the covariates
that determine it where the relationship is exact, and prints the
`covariates =` line that would fix the call. It does not prune. Choosing
which of two redundant covariates to keep is an analysis decision that
belongs in the script where a reader can see it, not inside a check.

The warning is the only signal you get, which is why it exists.
`lm_robust` does not refuse a rank-deficient design: it drops the
aliased column and reports an F on the remainder, so without the warning
the caller receives a p-value for a covariate set they did not specify
and nothing in the returned object records the substitution. (An
unobserved factor level is the one case that can instead return `NA`
outright.) That is how balance p-values have been computed on redundant
batteries without anyone noticing.

The redundancies seen in practice are mundane and worth recognising: the
same variable under two names, such as a female indicator beside a woman
indicator; a coarsening beside the thing it coarsens, such as a
Republican indicator beside a three-category party factor, or a college
indicator beside a three-level education factor; and a continuous
measure beside a binned version of itself. Resolving them also shrinks
the covariate count, which the section below explains is worth doing for
a second, independent reason.

## When the joint test cannot be trusted

The joint test is a Wald test on a robust covariance matrix, and in real
corpora that combination has produced p-values that are not merely
anti-conservative but impossible: `5.8e-108` and `1.5e-248` while no
covariate-by-covariate p-value fell below 0.09, and `3.6e-12` against a
classical 0.07.

The cause is the *robust* variance estimator, not collinearity. On the
third case, the same fit gives:

|                                           |           |         |
|-------------------------------------------|-----------|---------|
| **statistic**                             | **value** | **p**   |
| Wald with robust (HC2) V                  | 15.92     | 3.6e-12 |
| Wald with classical V                     | 2.06      | 0.074   |
| Classical F from residual sums of squares | 2.06      | 0.074   |

The coefficients are identical, both being OLS. The Wald *form* is fine:
fed the classical variance matrix it reproduces the classical F exactly.
What differs is which variance matrix is inverted.

The classical F needs one variance parameter, \\\sigma^2\\, estimated
from \\n-k\\ residuals. A robust Wald test needs the whole \\q \times
q\\ matrix, which is \\q(q+1)/2\\ free parameters estimated from squared
residuals and leverage, and then inverted. Inverting a noisy estimate is
not the same as estimating the inverse, and the error does not average
out: it inflates the statistic. Observations per variance parameter puts
every case in one place:

|                                 |       |       |                    |
|---------------------------------|-------|-------|--------------------|
| **case**                        | **n** | **q** | **n / (q(q+1)/2)** |
| 1578 respondents, 40 covariates | 1578  | 40    | 1.9                |
| 2937 respondents, 40 covariates | 2937  | 40    | 3.6                |
| 134 respondents, 5 covariates   | 134   | 5     | 8.9                |

**This is a fact about the covariate set, so it is reported rather than
patched.** `check_balance` warns when the ratio falls below
`min_obs_per_vparam`, and returns the inversion-free classical F
alongside as `p_value_classical` so a large gap between the two is
visible in the result and survives into a stacked corpus. The p-value
you asked for is still reported: narrowing the battery changes the
hypothesis being tested, which is your decision and not something a
check should make silently.

What to do, in order: narrow the battery to the covariates you would
actually adjust for rather than everything the study recorded; resolve
redundant parameterizations of one measurement, such as a continuous age
beside binned age, which the aliasing warning above names for you; and
for a wide battery pass `declaration`, since randomization inference
compares a statistic to its permutation distribution and estimates no
variance matrix at all. The same \\n\\ relative to parameter count
governs the calibration results below, so a narrower battery buys
accuracy twice.

A note on what *not* to use as a diagnostic. The reciprocal condition
number of the covariance matrix is scale-dependent and the Wald
statistic is not: multiplying an age covariate by 100 leaves the
statistic at 15.92 and moves the condition number from `1.4e-05` to
`2.5e-03`. A condition number partly measures the spread of covariate
scales, and simulated designs with condition numbers as bad as `4e-06`
return perfectly sane p-values.

## How many observations per coefficient

Calibration is governed by \\N/q\\, where \\q\\ is the number of
coefficients the test estimates. Above roughly 30 observations per
coefficient the tests sit at nominal; below about 10 they are
anti-conservative enough to mislead. For the joint test \\q = (K-1)p\\
with \\p\\ the number of model-matrix columns, so arms and covariates
both inflate it. The covariate-by-covariate tests escape this only when
treatment is binary, which makes each a single-degree-of-freedom test;
with \\K\\ arms each becomes a \\K-1\\ degree-of-freedom test and is
subject to the same problem. See
[`vignette("balance_testing")`](https://alexandercoppock.com/excheckr/articles/balance_testing.md).

## See also

[`check_smd`](https://alexandercoppock.com/excheckr/reference/check_smd.md)
for the magnitude of each imbalance, which these p-values do not convey.

Other per-study checks:
[`check_attrition()`](https://alexandercoppock.com/excheckr/reference/check_attrition.md),
[`check_covariate_missingness()`](https://alexandercoppock.com/excheckr/reference/check_covariate_missingness.md),
[`check_missingness_nona()`](https://alexandercoppock.com/excheckr/reference/check_missingness_nona.md),
[`check_smd()`](https://alexandercoppock.com/excheckr/reference/check_smd.md),
[`check_y_bounds()`](https://alexandercoppock.com/excheckr/reference/check_y_bounds.md)

## Examples

``` r
set.seed(42)
dat <- data.frame(
  Z = rep(c(0L, 1L), 100),
  X_age = rnorm(200, 50, 10),
  X_gender = sample(c("M", "F"), 200, replace = TRUE),
  X_party = factor(sample(c("D", "R", "I"), 200, replace = TRUE))
)
dat$X_income <- 50000 + 3000 * dat$Z + rnorm(200, 0, 10000)

# Default: all X_ covariates with lm_robust
check_balance(dat, Z)
#> $covariate_tests
#> # A tibble: 5 × 8
#>   covariate level   F_stat statistic   df1   df2 p_value  nobs
#>   <chr>     <chr>    <dbl> <chr>     <int> <int>   <dbl> <int>
#> 1 X_age     NA    2.08e- 1 F             1   198 0.649     200
#> 2 X_gender  M     7.92e- 2 F             1   198 0.779     200
#> 3 X_party   I     2.12e-31 F             1   198 1         200
#> 4 X_party   R     8.96e- 2 F             1   198 0.765     200
#> 5 X_income  NA    9.39e+ 0 F             1   198 0.00249   200
#> 
#> $joint_test
#> # A tibble: 1 × 10
#>   F_stat statistic   df1   df2 p_value p_value_classical  nobs obs_per_param
#>    <dbl> <chr>     <int> <int>   <dbl>             <dbl> <int>         <dbl>
#> 1   2.17 F             5   194  0.0593            0.0818   200          13.3
#> # ℹ 2 more variables: status <chr>, estimable <lgl>
#> 

# Specific covariates
check_balance(dat, Z, c("X_age", "X_income"))
#> $covariate_tests
#> # A tibble: 2 × 8
#>   covariate level F_stat statistic   df1   df2 p_value  nobs
#>   <chr>     <chr>  <dbl> <chr>     <int> <int>   <dbl> <int>
#> 1 X_age     NA     0.208 F             1   198 0.649     200
#> 2 X_income  NA     9.39  F             1   198 0.00249   200
#> 
#> $joint_test
#> # A tibble: 1 × 10
#>   F_stat statistic   df1   df2 p_value p_value_classical  nobs obs_per_param
#>    <dbl> <chr>     <int> <int>   <dbl>             <dbl> <int>         <dbl>
#> 1   5.48 F             2   197 0.00485           0.00747   200          66.7
#> # ℹ 2 more variables: status <chr>, estimable <lgl>
#> 

# Label the results and return one tibble, ready to stack across studies
check_balance(dat, Z, study_id = "smith_2024_study_1", flatten = TRUE)
#>        test covariate level       F_stat statistic df1 df2     p_value nobs
#> 1 covariate     X_age  <NA> 2.078685e-01         F   1 198 0.648942825  200
#> 2 covariate  X_gender     M 7.923169e-02         F   1 198 0.778635264  200
#> 3 covariate   X_party     I 2.118523e-31         F   1 198 1.000000000  200
#> 4 covariate   X_party     R 8.959276e-02         F   1 198 0.765009470  200
#> 5 covariate  X_income  <NA> 9.387147e+00         F   1 198 0.002489389  200
#> 6     joint      <NA>  <NA> 2.167489e+00         F   5 194 0.059309758  200
#>             study_id p_value_classical obs_per_param status estimable
#> 1 smith_2024_study_1                NA            NA   <NA>        NA
#> 2 smith_2024_study_1                NA            NA   <NA>        NA
#> 3 smith_2024_study_1                NA            NA   <NA>        NA
#> 4 smith_2024_study_1                NA            NA   <NA>        NA
#> 5 smith_2024_study_1                NA            NA   <NA>        NA
#> 6 smith_2024_study_1        0.08181371      13.33333 tested      TRUE

# Multi-armed treatment (uses multinomial LR test)
set.seed(1)
dat2 <- data.frame(
  Z = factor(rep(c("C", "T1", "T2"), length.out = 201)),
  X_age = rnorm(201, 50, 10)
)
check_balance(dat2, Z)
#> $covariate_tests
#> # A tibble: 1 × 8
#>   covariate level F_stat statistic   df1   df2 p_value  nobs
#>   <chr>     <chr>  <dbl> <chr>     <int> <int>   <dbl> <int>
#> 1 X_age     NA      1.57 F             2   198   0.211   201
#> 
#> $joint_test
#> # A tibble: 1 × 10
#>   F_stat statistic   df1   df2 p_value p_value_classical  nobs obs_per_param
#>    <dbl> <chr>     <int> <int>   <dbl>             <dbl> <int>         <dbl>
#> 1   1.49 LR/df         2    NA   0.225                NA   201          100.
#> # ℹ 2 more variables: status <chr>, estimable <lgl>
#> 

# \donttest{
# Randomization inference with a declaration (requires randomizr and ri2)
if (requireNamespace("randomizr", quietly = TRUE) &&
    requireNamespace("ri2", quietly = TRUE)) {
  decl <- randomizr::declare_ra(N = 201, conditions = c("C", "T1", "T2"))
  check_balance(dat2, Z, declaration = decl, sims = 200)
}
#> $covariate_tests
#> # A tibble: 1 × 8
#>   covariate level F_stat statistic   df1   df2 p_value  nobs
#>   <chr>     <chr>  <dbl> <chr>     <int> <int>   <dbl> <int>
#> 1 X_age     NA      1.57 F             2   198   0.211   201
#> 
#> $joint_test
#> # A tibble: 1 × 10
#>   F_stat statistic   df1   df2 p_value p_value_classical  nobs obs_per_param
#>    <dbl> <chr>     <int> <int>   <dbl>             <dbl> <int>         <dbl>
#> 1   2.99 LR           NA    NA   0.245                NA   201            NA
#> # ℹ 2 more variables: status <chr>, estimable <lgl>
#> 
# }
```
