# excheckr 0.1.0

First release.

`excheckr` runs the design checks an experiment should pass before its results
are believed, and stacks them across many studies so a corpus can be audited at
once.

## Checks

* `check_balance()` tests covariate balance, individually and jointly, and
  reports which quantity `F_stat` holds so a stacked corpus stays readable.
* `check_attrition()` tests differential attrition, returning both the simple
  and the interacted test from one call.
* `check_smd()` reports standardised mean differences, with weights.
* `check_covariate_missingness()`, `check_missingness_nona()`,
  `check_y_bounds()`, `check_s_scaling()` and `check_schema()` cover the
  remaining common failure modes.
* Every check takes `.by` for stratified checking, and carries `study_id`
  through, so results from many studies stack.

## Working with many studies

* `stack_checks()` combines results across studies and warns on mismatched
  schemas rather than silently binding them.
* `report_checks()`, `summarize_check_pvalues()` and `plot_check_pvalues()`
  summarise a stacked corpus.
* `write_balance_check_code()` and the other `write_*_code()` helpers generate
  per-study checking scripts.

## Notes on what the checks report

* A complete-cases outcome is a **pass**, not a missing test. Checks return a
  `status` (`tested`, `no_attrition`, `all_missing`, `not_estimable`) because
  the choice of denominator changes the headline figure threefold, from 2.4%
  prevalence to 7.2% uniformity, and that choice belongs to the analyst.
* Joint balance tests report `p_value_classical` alongside the robust Wald
  p-value. The robust form inverts a covariance matrix with q(q+1)/2 parameters,
  and with few observations per parameter the two can disagree by orders of
  magnitude. Seeing both makes the disagreement visible in the data rather than
  leaving it to be discovered later.
* Aliased covariates raise a warning that names the redundant term, what
  determines it, and the `covariates =` line that fixes the call. Nothing is
  pruned automatically, because choosing between two redundant covariates is an
  analysis decision.
