# SOFA coagulation

This function calculates the coagulation part of the SOFA score.
Platelet count need to be provided in \*10^3 uL^-1.

## Usage

``` r
sofa_coagulation(plt)
```

## Arguments

- plt:

  Platelets (\*10^3 uL^-1)

## Value

partial sofa score

## See also

Other sofa:
[`sofa_cardiovascular()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/sofa_cardiovascular.md),
[`sofa_cns()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/sofa_cns.md),
[`sofa_liver()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/sofa_liver.md),
[`sofa_magic_wrapper()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/sofa_magic_wrapper.md),
[`sofa_renal()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/sofa_renal.md),
[`sofa_respiration()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/sofa_respiration.md),
[`sofa_total()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/sofa_total.md)
