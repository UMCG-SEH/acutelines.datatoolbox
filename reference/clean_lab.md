# Clean laboratory variables by removing '\<' and '\>' characters and converting to numeric Cleans all columns starting with "lab\_" (per Acutelines standard)

Clean laboratory variables by removing '\<' and '\>' characters and
converting to numeric Cleans all columns starting with "lab\_" (per
Acutelines standard)

## Usage

``` r
clean_lab(df)
```

## Arguments

- df:

  Data frame containing laboratory variables

## Value

Data frame with cleaned laboratory variables

## See also

Other postprocessing:
[`normalize_decimal_format()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/normalize_decimal_format.md)
