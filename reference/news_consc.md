# Calculate NEWS score for Consciousness level (AVPU or GCS)

Prioritizes AVPU over GCS if both are provided.

## Usage

``` r
news_consc(avpu = NA, gcs = NA)
```

## Arguments

- avpu:

  Character value representing AVPU scale ("A", "V", "P", "U", "D")

- gcs:

  Numeric value representing Glasgow Coma Scale (3-15)

## Value

Numeric NEWS score for Consciousness level

## See also

Other news:
[`news_hr()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/news_hr.md),
[`news_o2supp()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/news_o2supp.md),
[`news_rf()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/news_rf.md),
[`news_sbp()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/news_sbp.md),
[`news_spo2()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/news_spo2.md),
[`news_temp()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/news_temp.md),
[`news_total()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/news_total.md)
