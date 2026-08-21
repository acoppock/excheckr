# excheckr 0.1.0

First release.

`excheckr` runs the design checks an experiment should pass before its results
are believed, and stacks them across many studies so a corpus can be audited at
once. It also audits the claims an article makes about that data, scoring each
number the text prints against the pipeline that is supposed to reproduce it.

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

## Auditing claims

* `claim()` records one number an article prints, scores it against the value
  the analysis pipeline produced, and writes one line of an audit trail.
  `claim_start()` opens the log, `claim_summary()` prints the verdict counts,
  `assert_claims()` runs the four gates that make the verdicts assertions
  rather than a report, and `claim_manifest()` returns one row per published
  statement asserted, which is what a coverage check reads.
* The expectation can be typed at the call site as the article's own typography
  (`published = "0.30"`, not `0.3`), or derived from an extraction of the
  published claims and an errata spine registered once in `claim_start()`. Both
  directions run the same verdict ladder. The typed direction simply leaves the
  rungs that need an erratum unreachable by construction, rather than by a mode
  flag.
* Passing verdicts are `match`, `corrected`, `range`, `derived`, `absent`,
  `shape` and `hedged`. Failing verdicts are `MISMATCH`, `STALE` (the pipeline
  still reproduces a value an erratum says it should now contradict), `DRIFT`
  (it contradicts the article but no longer equals the published correction)
  and `MISSING`.
* `format = "audit"` prints the columnar form the manuscript audits use. The
  value is right-aligned and the label follows it, so both columns line up over
  a long run without a width having to be declared. The rule above the summary
  is the project's own, printed at the call site, because the manuscript
  projects rule that line differently and neither should have to change to
  adopt this. The value column punctuates thousands the way the article's own
  statement does, so a year prints as `2012` and a respondent count on the next
  line prints as `1,776`.
* `claim_number()` and `claim_digits()` reduce published typography to a number
  and to the precision that number prints: thousands separators in the several
  forms an article writes them, percent signs, LaTeX math and braces, the
  unicode minus, a leading plus, and a bare leading decimal point. The union of
  these cases is what five hand-rolled claims files needed between them, and no
  one of the five handled more than the page its own paper happened to print.
* `claim_evidence()` prints a supporting note for a claim about shape, asserting
  nothing.

## Notes on what the claims audit reports

* Comparison happens at the precision the published value prints, separately
  for each statement, so a quantity stated in two documents at two precisions is
  compared correctly against both. A named `published` vector puts one pipeline
  value against several passages on one line, which is the only way a
  disagreement *between* two passages becomes visible at all.
* A value that is `NA` scores `absent` when `expect = "absent"` declared it so,
  and `MISSING` when nothing did. The exemption has to be declared at the call
  site, because an exemption that can be inferred from the value will one day be
  inferred from a bug.
* A claim with no published value scores `shape` when an extraction is
  registered, because the extraction is what says the sentence states no number.
  With no extraction registered the same call is an error, since there is nowhere
  else the expectation could have come from: a forgotten `published =` must not
  print, count as unasserted and pass. `expect = "shape"` declares the ones that
  genuinely state no number.
* `unasserted` counts what the file printed without comparing it, meaning the
  evidence notes plus the claims an `expect` exempted. A failing claim is
  asserted and disagreeing, so it is counted as mismatched and nowhere else.
* `digits` defaults to the precision the published value itself prints, which 77
  of 79 measured call sites had been typing by hand. It is required only where
  there is no published value to derive it from.
* The extraction and the errata spine are passed to `claim_start()` as data
  frames, never as file paths. Reading those CSVs with every column as character
  is the decision that keeps `0.30` from becoming `0.3`, and it belongs in the
  project that owns the deposit rather than buried in this package.
