# Check that a cleaned data frame conforms to its schema

Structural counterpart to the statistical `check_*` family
(`check_balance`, `check_attrition`, `check_y_bounds`, ...). Those check
whether an experiment is sound; this checks whether the cleaned data has
the shape the pipeline expects: a unique row key, a treatment and an
outcome column, weights, no raw `haven_labelled` columns leaking, and no
columns outside the declared contract.

## Usage

``` r
check_schema(
  data,
  key = "resp_id",
  meta = character(),
  treatment = "Z",
  outcome = "Y",
  covariate = "X_",
  extra_roles = character(),
  require_weights = TRUE,
  study_id = NULL
)
```

## Arguments

- data:

  A data frame or tibble.

- key:

  Character vector of columns that jointly identify a row (default
  `"resp_id"`). The check confirms they are all present and that their
  combination is unique.

- meta:

  Character vector of allowed non-role columns (study metadata such as
  `"topic"`, `"study_id"`). Columns that are neither key, weights, a
  role column, nor listed here are flagged as possible leaks.

- treatment, outcome, covariate:

  Prefixes identifying the treatment, outcome, and covariate columns
  (defaults `"Z"`, `"Y"`, `"X_"`). `treatment` and `outcome` match
  either a bare prefix (e.g. `"Z"`) or one followed by an underscore
  (`"Z_party"`).

- extra_roles:

  Character vector of additional role-column prefixes beyond
  covariate/treatment/outcome, matched as `"^<prefix>"`. Use for schemas
  with further role families, e.g. `"D_"` for belief / first-stage
  measures in the block-long beliefs schema. These columns count as
  roles, not as contract leaks, and are included in the all-NA check.

- require_weights:

  Logical. If `TRUE` (the default), a non-NA `weights` column is
  required (use `weights = 1` when a study has no survey weights).

- study_id:

  Optional character scalar appended as a `study_id` column, so results
  stack across studies.

## Value

A tibble with one row per check: `check` (name), `severity` (`"error"`
or `"warn"`), `pass` (logical), `detail` (offending columns, or `NA`),
and optionally `study_id`. Use `assert_schema` to turn failures into a
stop at save time.

## Details

Everything project-specific is an argument, so one tested implementation
serves every schema: the flat one-row-per-respondent schema
(`key = "resp_id"`), the within-subjects schema
(`key = c("resp_id", "topic")`), and the block-long schema
(`key = c("resp_id", "block", "wave")`). Conjoint data, whose role
prefixes and cardinality differ, keeps its own checker but can reuse
`assert_key_unique` and `assert_no_labelled`.

## See also

Other schema assertions:
[`assert_key_unique()`](https://alexandercoppock.com/excheckr/reference/assert_key_unique.md),
[`assert_no_labelled()`](https://alexandercoppock.com/excheckr/reference/assert_no_labelled.md),
[`assert_schema()`](https://alexandercoppock.com/excheckr/reference/assert_schema.md)

## Examples

``` r
dat <- data.frame(resp_id = 1:3, weights = 1,
                  X_age = c(20, 30, 40), Z_party = c(0, 1, 0), Y = c(0, 1, 1))
check_schema(dat)
#> # A tibble: 10 × 4
#>    check             severity pass  detail
#>    <chr>             <chr>    <lgl> <chr> 
#>  1 key_present       error    TRUE  NA    
#>  2 key_unique        error    TRUE  NA    
#>  3 weights_present   error    TRUE  NA    
#>  4 weights_no_na     error    TRUE  NA    
#>  5 has_treatment     error    TRUE  NA    
#>  6 has_outcome       error    TRUE  NA    
#>  7 no_labelled       error    TRUE  NA    
#>  8 no_extra_columns  warn     TRUE  NA    
#>  9 treatment_varies  warn     TRUE  NA    
#> 10 no_all_na_columns warn     TRUE  NA    
check_schema(dat, meta = "topic", study_id = "my_study")
#> # A tibble: 10 × 5
#>    check             severity pass  detail study_id
#>    <chr>             <chr>    <lgl> <chr>  <chr>   
#>  1 key_present       error    TRUE  NA     my_study
#>  2 key_unique        error    TRUE  NA     my_study
#>  3 weights_present   error    TRUE  NA     my_study
#>  4 weights_no_na     error    TRUE  NA     my_study
#>  5 has_treatment     error    TRUE  NA     my_study
#>  6 has_outcome       error    TRUE  NA     my_study
#>  7 no_labelled       error    TRUE  NA     my_study
#>  8 no_extra_columns  warn     TRUE  NA     my_study
#>  9 treatment_varies  warn     TRUE  NA     my_study
#> 10 no_all_na_columns warn     TRUE  NA     my_study
```
