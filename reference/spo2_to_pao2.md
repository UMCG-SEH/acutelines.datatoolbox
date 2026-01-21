# Convert SpO2 to PaO2

Convert SpO2 to PaO2 based on Brown / Ellis / Severinghaus equation

## Usage

``` r
spo2_to_pao2(spo2)
```

## Arguments

- spo2:

  SpO2 (%) to be converted

## Value

pao2 Calculated PaO2 (kPa)

## Details

Converts SpO2 percentages to PaO2 values in kPa. This is based on the
equations provides in two publications by Brown and Ellis.

## References

[10.1016/j.chest.2016.01.003](https://doi.org/10.1016/j.chest.2016.01.003)

[10.1152/jappl.1989.67.2.902](https://doi.org/10.1152/jappl.1989.67.2.902)

[10.1152/jappl.1979.46.3.599](https://doi.org/10.1152/jappl.1979.46.3.599)

## See also

Other respiration:
[`clean_fio2()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/clean_fio2.md),
[`clean_spo2()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/clean_spo2.md),
[`impute_fio2()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/impute_fio2.md),
[`impute_pao2()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/impute_pao2.md),
[`o2supply_to_fio2()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/o2supply_to_fio2.md),
[`pfratio_imputed()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/pfratio_imputed.md)
