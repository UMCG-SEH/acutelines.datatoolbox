# Impute PaO2 with SpO2

If PaO2 is missing replace with PaO2 calculated from SpO2.

## Usage

``` r
impute_pao2(pao2, spo2)
```

## Arguments

- pao2:

  Arterial oxygen pressure (kPa)

- spo2:

  Peripheral oxygen saturation (%)

## Value

pao2

## Details

PaO2 is missing in many cases, as arterial blood gass analysis is only
performed when needed. To fill those gaps, this function converts SpO2
(%) to PaO2 (kPa) in case the PaO2 is missing in the dataset. If SpO2 is
\>=97%, the PaO2 is set to 14.7 kPa, because the oxygen dissociation
curve is not valid to use in this range. This functions uses
[`spo2_to_pao2()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/spo2_to_pao2.md).

## See also

Other respiration:
[`clean_fio2()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/clean_fio2.md),
[`clean_spo2()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/clean_spo2.md),
[`impute_fio2()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/impute_fio2.md),
[`o2supply_to_fio2()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/o2supply_to_fio2.md),
[`pfratio_imputed()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/pfratio_imputed.md),
[`spo2_to_pao2()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/spo2_to_pao2.md)
