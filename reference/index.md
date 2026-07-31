# Package index

## Checking one study

Run once per cleaned experimental dataset. Each check takes a `study_id`
and appends it to the output, so results can be stacked across studies.

- [`check_y_bounds()`](https://alexandercoppock.com/excheckr/reference/check_y_bounds.md)
  : Check that outcome variables are within \[0, 1\]
- [`check_missingness_nona()`](https://alexandercoppock.com/excheckr/reference/check_missingness_nona.md)
  : Check covariate missingness and imputed-version coverage
- [`check_balance()`](https://alexandercoppock.com/excheckr/reference/check_balance.md)
  : Check covariate balance across treatment conditions
- [`check_smd()`](https://alexandercoppock.com/excheckr/reference/check_smd.md)
  : Standardized mean differences between treatment arms
- [`check_attrition()`](https://alexandercoppock.com/excheckr/reference/check_attrition.md)
  : Check differential attrition across treatment conditions
- [`check_covariate_missingness()`](https://alexandercoppock.com/excheckr/reference/check_covariate_missingness.md)
  : Summarize and visualize missingness of covariates

## Checking many studies

Collapse per-study results and read them without over-reading them.
Balance p-values are uniform under a valid design, so a count of flags
is the wrong summary; the distribution is the right one.

- [`stack_checks()`](https://alexandercoppock.com/excheckr/reference/stack_checks.md)
  : Stack per-study check results into one list of tibbles
- [`report_checks()`](https://alexandercoppock.com/excheckr/reference/report_checks.md)
  : Triage a stacked set of design checks
- [`summarize_check_pvalues()`](https://alexandercoppock.com/excheckr/reference/summarize_check_pvalues.md)
  : Summarize a set of design-check p-values against the uniform
  reference
- [`plot_check_pvalues()`](https://alexandercoppock.com/excheckr/reference/plot_check_pvalues.md)
  : Plot design-check p-values against the uniform reference

## Data shape

Check that a cleaned dataset has the structure a pipeline expects, as
distinct from checking that the experiment behind it was sound.

- [`check_schema()`](https://alexandercoppock.com/excheckr/reference/check_schema.md)
  : Check that a cleaned data frame conforms to its schema
- [`assert_schema()`](https://alexandercoppock.com/excheckr/reference/assert_schema.md)
  : Assert a cleaned data frame conforms to its schema
- [`assert_key_unique()`](https://alexandercoppock.com/excheckr/reference/assert_key_unique.md)
  : Assert that a data frame's row key is unique
- [`assert_no_labelled()`](https://alexandercoppock.com/excheckr/reference/assert_no_labelled.md)
  : Assert that no column is a leaked haven_labelled

## Cleaning helpers

Utilities for building cleaned data, and code generators that emit
copy-pasteable cleaning and checking blocks.

- [`stat_mode()`](https://alexandercoppock.com/excheckr/reference/stat_mode.md)
  : Statistical mode

- [`scale_by_control()`](https://alexandercoppock.com/excheckr/reference/scale_by_control.md)
  : Scale outcome variables by control-group standard deviation (Glass's
  delta)

- [`check_s_scaling()`](https://alexandercoppock.com/excheckr/reference/check_s_scaling.md)
  :

  Check that `_s` (Glass's delta) variables are correctly standardized

- [`write_covariate_imputation_code()`](https://alexandercoppock.com/excheckr/reference/write_covariate_imputation_code.md)
  : Write covariate imputation code

- [`write_outcome_missingness_dummies_code()`](https://alexandercoppock.com/excheckr/reference/write_outcome_missingness_dummies_code.md)
  : Write outcome missingness code

- [`write_balance_check_code()`](https://alexandercoppock.com/excheckr/reference/write_balance_check_code.md)
  : Write balance check code

- [`write_attrition_check_code()`](https://alexandercoppock.com/excheckr/reference/write_attrition_check_code.md)
  : Write attrition check code

## Package

- [`excheckr`](https://alexandercoppock.com/excheckr/reference/excheckr-package.md)
  [`excheckr-package`](https://alexandercoppock.com/excheckr/reference/excheckr-package.md)
  : excheckr: Tools for Exploring and Checking Experimental Data
