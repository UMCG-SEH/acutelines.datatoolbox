# Total SOFA score

Calculate the total sofa score from Acutelines dataframe.

## Usage

``` r
sofa_total(df, column_mapping, return_df = FALSE)
```

## Arguments

- df:

  data frame

- column_mapping:

  map df columns to SOFA items. Follow this structure:

- return_df:

  if set to TRUE returns all individual elements of the SOFA

## Value

df column with sofa score per row

## See also

Other sofa:
[`sofa_cardiovascular()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/sofa_cardiovascular.md),
[`sofa_cns()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/sofa_cns.md),
[`sofa_coagulation()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/sofa_coagulation.md),
[`sofa_liver()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/sofa_liver.md),
[`sofa_magic_wrapper()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/sofa_magic_wrapper.md),
[`sofa_renal()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/sofa_renal.md),
[`sofa_respiration()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/sofa_respiration.md)

## Examples

``` r
if (FALSE) { # \dontrun{
column_mapping <- c(
 PaO2 = "SOFA_24h_PaO2",
 SpO2 = "SOFA_24h_SpO2",
 FiO2 = "SOFA_24h_FiO2",
 oxygen_supply = "SOFA_24h_oxygen_supply",
 oxygen_mode = "SOFA_24h_oxygen_mode",
 platelets = "SOFA_24h_platelets",
 bilirubin = "SOFA_24h_bilirubin",
 MAP = "SOFA_24h_MAP",
 dopamine = "SOFA_24h_dopamine",
 dobutamine = "SOFA_24h_dobutamine",
 epinephrine = "SOFA_24h_epinephrine",
 norepinephrine = "SOFA_24h_norepineprhine",
 norepinephrine_amp = "SOFA_24h_norepinephrine_amp",
 GCS = "SOFA_24h_GCS",
 creatinine = "SOFA_24h_creatinine"
 )
result <- sofa_total(df, column_mapping)
} # }
```
