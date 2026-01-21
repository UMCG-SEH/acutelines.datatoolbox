# Clean SpO2

Clean SpO2 values based on several rules.

## Usage

``` r
clean_spo2(spo2)
```

## Arguments

- spo2:

  Peripheral oxygen saturation (%)

## Value

spo2 (%)

## Details

Rules include cleaning on impossible values and on common mistakes or
switch-ups when entering data in the EHR. Remove Spo2\<50 and convert
50-70 to 70%.

## See also

Other respiration:
[`clean_fio2()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/clean_fio2.md),
[`impute_fio2()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/impute_fio2.md),
[`impute_pao2()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/impute_pao2.md),
[`o2supply_to_fio2()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/o2supply_to_fio2.md),
[`pfratio_imputed()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/pfratio_imputed.md),
[`spo2_to_pao2()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/spo2_to_pao2.md)

Other cleaning:
[`clean_fio2()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/clean_fio2.md)
