# Clean FiO2

Clean FiO2 values based on several rules.

## Usage

``` r
clean_fio2(fio2, o2supply, o2system)
```

## Arguments

- fio2:

  Fraction inspired oxygen (%)

- o2supply:

  Oxygen supply (L/min)

- o2system:

  Oxygen supply method

## Value

fio2 (%)

## Details

Rules include cleaning on impossible values and on common mistakes or
switch-ups when entering data in the EHR.

## Todo

Implement more sophisticated FiO2 cleaning algorithm.

## See also

Other respiration:
[`clean_spo2()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/clean_spo2.md),
[`impute_fio2()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/impute_fio2.md),
[`impute_pao2()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/impute_pao2.md),
[`o2supply_to_fio2()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/o2supply_to_fio2.md),
[`pfratio_imputed()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/pfratio_imputed.md),
[`spo2_to_pao2()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/spo2_to_pao2.md)

Other cleaning:
[`clean_spo2()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/clean_spo2.md)
