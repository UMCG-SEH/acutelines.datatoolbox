# Calculate total NEWS score from individual components

Calculate total NEWS score from individual components

## Usage

``` r
news_total(hr, rf, temp, sbp, avpu, gcs, o2supp, spo2, return_df = FALSE)
```

## Arguments

- hr:

  Numeric value representing Heart Rate (bpm)

- rf:

  Numeric value representing Respiratory Frequency (breaths per minute)

- temp:

  Numeric value representing Temperature (°C)

- sbp:

  Numeric value representing Systolic Blood Pressure (mmHg)

- avpu:

  Character value representing AVPU scale ("A", "V", "P", "U", "D")

- gcs:

  Numeric value representing Glasgow Coma Scale (3-15)

- o2supp:

  Logical value indicating if oxygen supplementation is provided
  (TRUE/FALSE)

- spo2:

  Numeric value representing Oxygen Saturation (%)

- return_df:

  Logical value indicating if a detailed dataframe should be returned
  (TRUE/FALSE), default is FALSE

## Value

Total NEWS score as an integer, or a dataframe with individual component
scores and total score if return_df is TRUE

## See also

Other news:
[`news_consc()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/news_consc.md),
[`news_hr()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/news_hr.md),
[`news_o2supp()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/news_o2supp.md),
[`news_rf()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/news_rf.md),
[`news_sbp()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/news_sbp.md),
[`news_spo2()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/news_spo2.md),
[`news_temp()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/news_temp.md)
