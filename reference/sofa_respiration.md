# SOFA respiration

This function calculates the respiratory part of the SOFA score. It
needs either the PF-ratio or both PaO2 and FiO2. If PF-ratio is missing,
the function will calculate it from PaO2 and FiO2.

## Usage

``` r
sofa_respiration(mechanical_ventilation, pfratio, pao2, fio2)
```

## Arguments

- mechanical_ventilation:

  Mechanical Ventilation (1=TRUE, 0=FALSE)

- pfratio:

  P/F-ratio (kPa)

- pao2:

  PaO2 (kPa)

- fio2:

  FiO2

## Value

partial sofa score

## See also

Other sofa:
[`sofa_cardiovascular()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/sofa_cardiovascular.md),
[`sofa_cns()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/sofa_cns.md),
[`sofa_coagulation()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/sofa_coagulation.md),
[`sofa_liver()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/sofa_liver.md),
[`sofa_magic_wrapper()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/sofa_magic_wrapper.md),
[`sofa_renal()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/sofa_renal.md),
[`sofa_total()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/sofa_total.md)
