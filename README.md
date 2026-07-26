# excheckr

<!-- badges: start -->
[![R-CMD-check](https://img.shields.io/badge/R--CMD--check-passing-brightgreen)](https://github.com/acoppock/excheckr)
<!-- badges: end -->

Diagnostics for experimental data: covariate balance, differential attrition,
missingness, outcome bounds, and data-shape assertions. The checks are built to
be run once per study and then stacked across many studies, which is what a
meta-analysis or a multi-study project actually needs.

## Installation

```r
# install.packages("remotes")
remotes::install_github("acoppock/excheckr")
```

## Checking one study

Four checks answer the questions worth asking of a single cleaned experimental
dataset. Each takes a `study_id` and appends it to the output, which is what
makes the results stackable later.

```r
library(excheckr)

check_y_bounds(dat, study_id = "smith_2024_study_1")
check_missingness_nona(dat, study_id = "smith_2024_study_1")
check_balance(dat, Z, covariates = nona_covariates,
              study_id = "smith_2024_study_1", flatten = TRUE)
check_attrition(dat, Z, study_id = "smith_2024_study_1")
```

`check_balance()` runs a test per covariate plus a joint test of all of them,
choosing an F-test for binary treatments and a multinomial likelihood-ratio test
for multi-armed ones. Pass `declaration =` to get an exact randomization
inference p-value instead, which is worth doing for clustered designs and for
multi-arm designs with many covariates, where the asymptotic reference
distribution is unreliable.

`check_smd()` answers the question a p-value cannot: whether an imbalance is
large enough to matter. In a large sample a trivial difference will be
significant, so the two belong together.

## Checking many studies

Run the checks once per study, write the results to disk, then collapse them.

```r
all_checks <- stack_checks("checks")

report_checks(all_checks)
#> excheckr triage report (alpha = 0.05)
#>       1  outcomes outside [0, 1]
#>       0  covariates missing with no imputed companion
#>       1  joint balance tests flagged
#>       1  covariate balance tests flagged
#>       1  attrition tests flagged
#> Inspect an element with report$<name>.
```

`report_checks()` returns only the rows that need a human. For the balance
tests, though, a count of flags is the wrong thing to look at.

## A caution worth reading before acting on the output

Under a valid design, balance and attrition p-values are distributed
Uniform(0, 1). Roughly `alpha` of them fall below `alpha` **by construction**, so
in a collection of several hundred tests a handful of flags is what success
looks like. Treating each flagged test as evidence against the study that
produced it, and dropping those studies, is a reliable way to introduce exactly
the bias the checks were meant to detect.

The useful question is whether the distribution looks uniform, not which
individual tests failed:

```r
summarize_check_pvalues(all_checks$balance_covariate)
#> # A tibble: 1 × 7
#>   n_tests n_below pct_below expected_below n_below_fdr pct_below_fdr  ks_p
#>     <int>   <int>     <dbl>          <dbl>       <int>         <dbl> <dbl>
#> 1      24       1      4.17              5           1          4.17 0.327

plot_check_pvalues(all_checks$balance_covariate, group = "study_id", fdr = TRUE)
```

`pct_below` against `expected_below` is the headline comparison; `ks_p` tests the
whole distribution against the uniform. The `group` argument controls what the
false-discovery-rate correction is computed over, because adjusting within study
and adjusting across all tests answer different questions.

## Vignettes

- `vignette("checking_many_studies")`: the end-to-end workflow above, from
  per-study scripts through stacking to triage.
- `vignette("checking_experiments")`: the per-study checks in detail, on
  simulated data with known problems.
- `vignette("balance_testing")`: calibration of the balance tests under the
  null, including how the joint test behaves as the number of arms grows and
  when randomization inference earns its cost.

## Other tools

`check_schema()` and its `assert_*` companions check that a cleaned dataset has
the shape a pipeline expects, as opposed to checking that the experiment behind
it was sound. The `write_*_code()` functions emit copy-pasteable cleaning and
checking code. `stat_mode()` computes a modal value for mode imputation, and
works directly inside `tidyr::replace_na()` on factors, characters, and
`haven_labelled` columns. `scale_by_control()` puts outcomes on a
control-group-SD scale, with `check_s_scaling()` to verify the result.

## Related packages

[metaprep](https://github.com/acoppock/metaprep) for carrying estimates and
their covariance through a meta-analysis, and
[estimatrTools](https://github.com/acoppock/estimatrTools) for regression
adjustment with data-driven covariate selection.

## License

MIT
