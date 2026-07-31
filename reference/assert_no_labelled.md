# Assert that no column is a leaked haven_labelled

Hard-errors if any column still carries the `haven_labelled` class, i.e.
a raw labelled column read from a `.dta`/`.sav` that was never recoded
or stripped. A composable primitive used by `check_schema` and reusable
by bespoke checkers.

## Usage

``` r
assert_no_labelled(data)
```

## Arguments

- data:

  A data frame or tibble.

## Value

`TRUE`, invisibly.

## See also

Other schema assertions:
[`assert_key_unique()`](https://alexandercoppock.com/excheckr/reference/assert_key_unique.md),
[`assert_schema()`](https://alexandercoppock.com/excheckr/reference/assert_schema.md),
[`check_schema()`](https://alexandercoppock.com/excheckr/reference/check_schema.md)

## Examples

``` r
assert_no_labelled(data.frame(x = 1:3))
```
