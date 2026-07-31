# excheckr: Tools for Exploring and Checking Experimental Data

Functions for diagnosing, summarizing, and visualizing covariate
balance, differential attrition, missingness, and data integrity in
experimental datasets. Includes per-study checks that can be labelled
and stacked across many studies, plus triage and uniform-reference
diagnostics for the resulting collection of tests.

## Checking one study

Four checks answer the questions worth asking of a single cleaned
experimental dataset, and each accepts a `study_id` so its output can be
stacked later:

- [`check_y_bounds`](https://alexandercoppock.com/excheckr/reference/check_y_bounds.md):
  are the outcomes on the scale you think they are on?

- [`check_missingness_nona`](https://alexandercoppock.com/excheckr/reference/check_missingness_nona.md):
  which covariates have missing values, and do they have an imputed
  companion column?

- [`check_balance`](https://alexandercoppock.com/excheckr/reference/check_balance.md):
  does treatment predict the covariates, covariate by covariate and
  jointly?
  [`check_smd`](https://alexandercoppock.com/excheckr/reference/check_smd.md)
  answers the companion question of whether an imbalance is large enough
  to matter.

- [`check_attrition`](https://alexandercoppock.com/excheckr/reference/check_attrition.md):
  does treatment predict outcome missingness, on its own and allowing
  the pattern to differ across covariates?

For designs where the fully interacted attrition test runs out of
degrees of freedom,
[`estimatrTools::check_attrition_lasso`](https://alexandercoppock.com/estimatrTools/reference/check_attrition_lasso.html)
selects a parsimonious covariate set first. It lives there rather than
here because it fits an estimator of its own, and this package only ever
calls estimators that other packages own.

## Checking many studies

A meta-analysis or multi-study project runs those checks once per study
and then has to make sense of hundreds of tests at once.
[`stack_checks`](https://alexandercoppock.com/excheckr/reference/stack_checks.md)
reads the per-study files and binds them,
[`report_checks`](https://alexandercoppock.com/excheckr/reference/report_checks.md)
returns only the rows that need a human, and
[`summarize_check_pvalues`](https://alexandercoppock.com/excheckr/reference/summarize_check_pvalues.md)
and
[`plot_check_pvalues`](https://alexandercoppock.com/excheckr/reference/plot_check_pvalues.md)
compare the resulting p-values against the Uniform(0, 1) distribution
they should follow when nothing is wrong. See
[`vignette("checking_many_studies", package = "excheckr")`](https://alexandercoppock.com/excheckr/articles/checking_many_studies.md).

## A caution about reading these checks

Balance and attrition tests are diagnostics, not decisions. Under a
valid design their p-values are uniform, so roughly `alpha` of them will
be below `alpha` by construction: a handful of flags in a large
collection is what success looks like, not evidence of a problem. That
is why
[`summarize_check_pvalues`](https://alexandercoppock.com/excheckr/reference/summarize_check_pvalues.md)
reports the whole distribution rather than a count, and why dropping
studies on the strength of a single flagged test is a good way to
introduce the bias you were checking for.

## Other tools

[`check_schema`](https://alexandercoppock.com/excheckr/reference/check_schema.md)
and its `assert_*` companions check that a cleaned dataset has the shape
a pipeline expects, rather than that the experiment behind it was sound.
The `write_*_code` functions emit copy-pasteable cleaning and checking
code.
[`stat_mode`](https://alexandercoppock.com/excheckr/reference/stat_mode.md)
computes a modal value for mode imputation, and
[`scale_by_control`](https://alexandercoppock.com/excheckr/reference/scale_by_control.md)
puts outcomes on a control-group-SD scale.

## See also

Useful links:

- <https://alexandercoppock.com/excheckr/>

- <https://github.com/acoppock/excheckr>

- Report bugs at <https://github.com/acoppock/excheckr/issues>

## Author

**Maintainer**: Alexander Coppock <acoppock@gmail.com>

Authors:

- Alexander Coppock <acoppock@gmail.com>
