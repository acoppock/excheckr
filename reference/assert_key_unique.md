# Assert that a data frame's row key is unique

Hard-errors if the combination of `key` columns is duplicated. A small
composable primitive used by `check_schema` and reusable by bespoke
checkers (e.g. a conjoint `check_conjoint`).

## Usage

``` r
assert_key_unique(data, key)
```

## Arguments

- data:

  A data frame or tibble.

- key:

  Character vector of columns that jointly identify a row.

## Value

`TRUE`, invisibly.

## See also

Other schema assertions:
[`assert_no_labelled()`](https://alexandercoppock.com/excheckr/reference/assert_no_labelled.md),
[`assert_schema()`](https://alexandercoppock.com/excheckr/reference/assert_schema.md),
[`check_schema()`](https://alexandercoppock.com/excheckr/reference/check_schema.md)

## Examples

``` r
assert_key_unique(data.frame(resp_id = 1:3), key = "resp_id")
```
