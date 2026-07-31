# Assert a cleaned data frame conforms to its schema

Runs `check_schema` and turns the result into a hard stop, for use at
the tail of a cleaning script (before `write_rds`) so a nonconforming
study fails loudly where it is built. Any failing `"error"` check stops;
any failing `"warn"` check emits a warning. Returns the report
invisibly.

## Usage

``` r
assert_schema(data, ..., study_id = NULL)
```

## Arguments

- data:

  A data frame or tibble.

- ...:

  Passed to `check_schema`.

- study_id:

  Optional character scalar appended as a `study_id` column, so results
  stack across studies.

## Value

The `check_schema` report tibble, invisibly.

## See also

Other schema assertions:
[`assert_key_unique()`](https://alexandercoppock.com/excheckr/reference/assert_key_unique.md),
[`assert_no_labelled()`](https://alexandercoppock.com/excheckr/reference/assert_no_labelled.md),
[`check_schema()`](https://alexandercoppock.com/excheckr/reference/check_schema.md)

## Examples

``` r
dat <- data.frame(resp_id = 1:3, weights = 1, Z = c(0, 1, 0), Y = c(0, 1, 1))
assert_schema(dat)
```
