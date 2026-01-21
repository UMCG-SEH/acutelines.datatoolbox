# SOFA magic wrapper

Magic wrapper to automatically calculate the SOFA score on multiple
intervals (eg. hours, days) assuming the data is exported based on
Acutelines SOFA exports.

## Usage

``` r
sofa_magic_wrapper(
  df,
  interval = 24,
  timespan = 72,
  return_df = FALSE,
  naming.cols = "SOFA_<interval>h_<variable>",
  naming.result = "SOFAscore_<interval>h_<variable>"
)
```

## Arguments

- df:

  dataframe containing all the data

- interval:

  interval length, defaults to 24h

- timespan:

  number of intervals\*interval length, defaults to 72h

- return_df:

  Return all elements of the sofa score in the DF instead of only totals

- naming.cols:

  naming scheme to use to select columns. will be replaced by the
  interval and by the variable name

- naming.result:

  naming scheme to assign to column with total score

## Value

df with sofa scores

## Details

This function assumes data is exported using the Acutelines SOFA export
snippet, if not, manually define columns and use
[`sofa_total()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/sofa_total.md).

## See also

Other sofa:
[`sofa_cardiovascular()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/sofa_cardiovascular.md),
[`sofa_cns()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/sofa_cns.md),
[`sofa_coagulation()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/sofa_coagulation.md),
[`sofa_liver()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/sofa_liver.md),
[`sofa_renal()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/sofa_renal.md),
[`sofa_respiration()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/sofa_respiration.md),
[`sofa_total()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/sofa_total.md)
