# Statistical mode

Computes the most frequent (modal) value of a vector.

## Usage

``` r
stat_mode(x, na.rm = TRUE)
```

## Arguments

- x:

  A vector (numeric, character, or factor).

- na.rm:

  Logical. Should missing values be removed before computing the mode?
  Defaults to TRUE.

## Value

The most frequent value of \`x\`. If \`x\` is a factor, the result is
returned as a single-element factor with the same levels, compatible
with
[`tidyr::replace_na()`](https://tidyr.tidyverse.org/reference/replace_na.html).
If there are ties, the first occurring mode is returned. If all values
are missing, returns `NA`.

## Details

For factor input, the return value is a length-1 factor with the same
levels, which is directly compatible with
[`tidyr::replace_na()`](https://tidyr.tidyverse.org/reference/replace_na.html)
for mode-imputation of factor columns.

## Examples

``` r
stat_mode(c(1, 2, 2, 3, NA))
#> [1] 2
stat_mode(c("a", "b", "a", "c", "c"))
#> [1] "a"
stat_mode(factor(c("low", "high", "low", NA)))
#> [1] low
#> Levels: high low

# For factor columns, stat_mode returns a length-1 factor with the same levels.
# The result is directly compatible with tidyr::replace_na() -- no as.character()
# conversion needed:
#   replace_na(x, stat_mode(x))
# The base-R equivalent works without any extra packages:
x <- factor(c("low", "high", "low", NA), levels = c("low", "high"))
x[is.na(x)] <- stat_mode(x)
```
