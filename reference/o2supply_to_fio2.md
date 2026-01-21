# O2-supply to FiO2

Convert the O2-supply in L/min to FiO2.

## Usage

``` r
o2supply_to_fio2(o2supply, o2system)
```

## Arguments

- o2supply:

  Oxygen supply (L/min)

- o2system:

  Oxygen supply method 50 = tube 40 = optiflow 35 = cpap 30 =
  non-rebreather 25 = venturi mask 20 = nasal cannula 10 = nebulizer 0 =
  none (ambient air)

## Value

fio2 fraction inspirated oxygen

## Details

This function converts the oxygen supply in L/min as given per nasal
canula to FiO2. In some cases the FiO2 is reported in the O2 supply
field. To correct for this mistake, when O2 supply is above 15 it is
considered as FiO2.

## See also

Other respiration:
[`clean_fio2()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/clean_fio2.md),
[`clean_spo2()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/clean_spo2.md),
[`impute_fio2()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/impute_fio2.md),
[`impute_pao2()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/impute_pao2.md),
[`pfratio_imputed()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/pfratio_imputed.md),
[`spo2_to_pao2()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/spo2_to_pao2.md)
