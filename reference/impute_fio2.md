# Impute FiO2

Impute FiO2 with oxygen supply. If FiO2 is missing, replace with FiO2
calculated from oxygen supply. If still missing, impute with 21%
(ambient air).

## Usage

``` r
impute_fio2(fio2, o2supply, o2system)
```

## Arguments

- fio2:

  Fraction inspired oxygen (%)

- o2supply:

  Oxygen supply (L/min)

- o2system:

  Oxygen supply method

## Value

fio2

## Details

FiO2 is missing in many cases. To fill those gaps, this function
converts the oxygen supply (L/min) to FiO2 based on the oxygen delivery
device in case the FiO2 is missing in the dataset. When also the oxygen
supply is missing, the FiO2 is imputed with 21%, the ambient air oxygen
percentage. This functions uses
[`o2supply_to_fio2()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/o2supply_to_fio2.md)

## See also

Other respiration:
[`clean_fio2()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/clean_fio2.md),
[`clean_spo2()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/clean_spo2.md),
[`impute_pao2()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/impute_pao2.md),
[`o2supply_to_fio2()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/o2supply_to_fio2.md),
[`pfratio_imputed()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/pfratio_imputed.md),
[`spo2_to_pao2()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/spo2_to_pao2.md)
