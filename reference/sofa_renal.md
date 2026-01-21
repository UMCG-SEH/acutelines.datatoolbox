# SOFA renal

This function calculates the renal part of the SOFA score.

## Usage

``` r
sofa_renal(creat)
```

## Arguments

- create:

  Creatinine (umol/L)

## Value

partial sofa score

## Details

This function ignores the urine output as defined in the SOFA score
Reliable measurements of urine output in a retrospective dataset of both
deteriorated and not deteriorated patients are scarce.

## See also

Other sofa:
[`sofa_cardiovascular()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/sofa_cardiovascular.md),
[`sofa_cns()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/sofa_cns.md),
[`sofa_coagulation()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/sofa_coagulation.md),
[`sofa_liver()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/sofa_liver.md),
[`sofa_magic_wrapper()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/sofa_magic_wrapper.md),
[`sofa_respiration()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/sofa_respiration.md),
[`sofa_total()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/sofa_total.md)
