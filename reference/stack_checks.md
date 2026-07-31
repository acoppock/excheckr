# Stack per-study check results into one list of tibbles

Reads every per-study check file in `dir` and binds them element-wise
into a single named list of tibbles. Each file is expected to hold a
named list of tibbles written by a per-study checking script, typically
the output of
[`check_y_bounds`](https://alexandercoppock.com/excheckr/reference/check_y_bounds.md),
[`check_missingness_nona`](https://alexandercoppock.com/excheckr/reference/check_missingness_nona.md),
[`check_balance`](https://alexandercoppock.com/excheckr/reference/check_balance.md),
and
[`check_attrition`](https://alexandercoppock.com/excheckr/reference/check_attrition.md)
with a `study_id` supplied.

## Usage

``` r
stack_checks(
  dir = "checks",
  pattern = "_checks\\.rds$",
  exclude = "^all_checks\\.rds$",
  elements = NULL,
  warn_schema = TRUE
)
```

## Arguments

- dir:

  Path to the directory holding the per-study check files (default
  `"checks"`).

- pattern:

  Regular expression identifying the per-study files (default
  `"_checks\\.rds$"`).

- exclude:

  Regular expression for files to skip, matched against the base name.
  Defaults to `"^all_checks\\.rds$"` so that re-running over a directory
  that already holds a stacked file is safe.

- elements:

  Character vector naming the list elements to stack, or `NULL`
  (default) to stack the union of element names found across all files.

- warn_schema:

  Logical. Warn when the files contributing one element disagree about
  their columns (default `TRUE`). Set `FALSE` in a project where that is
  expected: a corpus whose studies are stratified on different variables
  produces different grouping columns by design, and the warning cannot
  tell that apart from staleness.

## Value

A named list of tibbles, one per element name.

## Details

Elements that are `NULL` or have zero rows in a given file are dropped
before binding, so a study that contributes nothing to one check does
not break the stack for the others. An element present in some files but
not others is still returned, built from whichever files have it.

## Stale files hide in a successful stack

[`dplyr::bind_rows`](https://dplyr.tidyverse.org/reference/bind_rows.html)
unions column names and fills the gaps with `NA`. That tolerance is what
lets a study which skipped one check stack alongside studies that ran
it, and it is also how a stale file hides: when one study's results
predate a change in what a check returns, its rows arrive missing the
new columns while still carrying whatever the old version put in the
shared ones. The stack succeeds, the corpus looks fully re-run, and it
is not. A real instance: two per-study files a day older than the rest
left `estimable` as `NA` on eight rows and shifted the count of
informative attrition tests. Hence the warning, and hence `warn_schema`.

## See also

Other across-study summaries:
[`plot_check_pvalues()`](https://alexandercoppock.com/excheckr/reference/plot_check_pvalues.md),
[`report_checks()`](https://alexandercoppock.com/excheckr/reference/report_checks.md),
[`summarize_check_pvalues()`](https://alexandercoppock.com/excheckr/reference/summarize_check_pvalues.md)

## Examples

``` r
d <- tempfile()
dir.create(d)
saveRDS(
  list(ybounds = data.frame(study_id = "a", variable = "Y_x", in_bounds = TRUE)),
  file.path(d, "a_checks.rds")
)
saveRDS(
  list(ybounds = data.frame(study_id = "b", variable = "Y_x", in_bounds = FALSE)),
  file.path(d, "b_checks.rds")
)
stack_checks(d)
#> $ybounds
#>   study_id variable in_bounds
#> 1        a      Y_x      TRUE
#> 2        b      Y_x     FALSE
#> 
```
