# SOFA cardiovascular

This function calculates the cardiavascular part of the SOFA score.

## Usage

``` r
sofa_cardiovascular(
  map,
  dopamine,
  dobutamine,
  epinephrine,
  norepinephrine,
  norepinephrine_amp
)
```

## Arguments

- map:

  Mean Arterial Pressure

- dopamine:

  Dopamine dosage (ug/kg/min)

- dobutamine:

  Dobutamine dosage (ug/kg/min)

- epinephrine:

  Epinephrine dosage (ug/kg/min)

- norepinephrine:

  Norepinephrine dosage (ug/kg/min)

- norepinephrine_amp:

  Norepinephrine ampul administration (1=TRUE, 0=FALSE)

## Value

partial sofa score

## Details

Due to inconsistent registration of norepinephrine dosages, it's not
possible to distinguish between high (\>0.1) and low (\<=0.1) dosage of
norepinephrine. When norepinephrine is present 3.5 points are given.
Researchers can either round this to 4 (overestimate), 3 (underestimate)
or leave it as is.

## See also

Other sofa:
[`sofa_cns()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/sofa_cns.md),
[`sofa_coagulation()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/sofa_coagulation.md),
[`sofa_liver()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/sofa_liver.md),
[`sofa_magic_wrapper()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/sofa_magic_wrapper.md),
[`sofa_renal()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/sofa_renal.md),
[`sofa_respiration()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/sofa_respiration.md),
[`sofa_total()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/sofa_total.md)
