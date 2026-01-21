# Calculate P/F-ratio

Calculate P/F-ratio while cleaning and imputing PaO2 and FiO2.

## Usage

``` r
pfratio_imputed(pao2, spo2, fio2, o2supply, o2system)
```

## Arguments

- pao2:

  Arterial oxygen pressure (kPa)

- spo2:

  Peripheral oxygen saturation (%)

- fio2:

  Fraction inspired oxygen (%)

- o2supply:

  Oxygen supply (L/min)

- o2system:

  Oxygen supply method (ranked 0-50)

## Value

pfratio

## Details

This function uses
[`impute_pao2()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/impute_pao2.md),
[`clean_fio2()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/clean_fio2.md)
and
[`impute_fio2()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/impute_fio2.md).

## See also

Other respiration:
[`clean_fio2()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/clean_fio2.md),
[`clean_spo2()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/clean_spo2.md),
[`impute_fio2()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/impute_fio2.md),
[`impute_pao2()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/impute_pao2.md),
[`o2supply_to_fio2()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/o2supply_to_fio2.md),
[`spo2_to_pao2()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/spo2_to_pao2.md)
