# NEWS score calculation

**DISCLAMER:** This function calculates the original National Early
Warning Score (NEWS) \[[1](#ref-News2012)\], not NEWS-2
\[[2](#ref-News2_2017)\]. The scoring rules differ slightly from NEWS-2,
specifically in oxygen saturation thresholds and mental status
classification.

## Usage

The NEWS can be calculated using the
[`news_total()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/news_total.md)
function. This function requires vital parameters as input, which are
then scored according to the NEWS criteria. By default `return_df` is
`FALSE` and can be left out, if set to `TRUE` the function will return
all the inidivual elements of the NEWS score instead of only the totals.
Assuming your vital parameters are stored in dataframe `df` with columns
`hr`, `rf`, `temp`, `sbp`, `avpu`, `gcs`, `o2supp`, and `spo2`, you can
calculate the NEWS score as follows:

``` r
result <- news_total <- function(df$hr, df$rf, df$temp, df$sbp, df$avpu, df$gcs, df$o2supp, df$spo2)
```

By default, the function only returns the total NEWS score, but you can
also return the individual components of the NEWS score by setting
`return_df = TRUE`. This will return a dataframe with all the individual
components of the NEWS score.

``` r
result <- news_total <- function(df$hr, df$rf, df$temp, df$sbp, df$avpu, df$gcs, df$o2supp, df$spo2, return_df = TRUE)
```

If all eight components scores are missing, the returned total NEWS
score will be `NA`.

## Calculations

### Heart rate (HR)

| Points | Criteria (bpm)  |
|--------|-----------------|
| 0      | 51–90           |
| 1      | 41–50 OR 91–110 |
| 2      | 111–130         |
| 3      | ≤ 40 OR ≥ 131   |

If HR is missing, `NA` is returned.

### Respiratory rate (RR)

| Points | Criteria (breaths/min) |
|--------|------------------------|
| 0      | 12–20                  |
| 1      | 9–11                   |
| 2      | 21–24                  |
| 3      | ≤ 8 OR ≥ 25            |

If RR is missing, `NA` is returned.

### Temperature (Temp)

| Points | Criteria (°C)          |
|--------|------------------------|
| 0      | 36.1–38.0              |
| 1      | 35.1–36.0 OR 38.1–39.0 |
| 2      | ≥ 39.1                 |
| 3      | ≤ 35.0                 |

If Temp is missing, `NA` is returned.

### Systolic blood pressure (SBP)

| Points | Criteria (mmHg) |
|--------|-----------------|
| 0      | 111–219         |
| 1      | 101–110         |
| 2      | 91–100          |
| 3      | ≤ 90 OR ≥ 220   |

If SBP is missing, `NA` is returned.

### Level of consciousness (AVPU/GCS)

Variables used:

- AVPU (Alert, Voice, Pain, Unresponsive, or “D” for “Drowsy”).
- GCS (Glasgow Coma Scale).

NOTE: AVPU is assessed first. Only if AVPU is missing is GCS used as a
substitute.

| Points | Criteria                                         |
|--------|--------------------------------------------------|
| 0      | AVPU = Alert (A) OR GCS = 15                     |
| 3      | AVPU = V, P, U, D OR (AVPU missing AND GCS ≤ 14) |

If both AVPU and GCS are missing, `NA` is returned.

### Oxygen supplementation (O2supp)

| Points | Criteria (binary)                         |
|--------|-------------------------------------------|
| 0      | No O₂ supplementation (value = FALSE)     |
| 2      | O₂ supplementation present (value = TRUE) |

If O2supp is missing, `NA` is returned.

### Oxygen saturation (SpO₂)

| Points | Criteria |
|--------|----------|
| 0      | ≥ 96%    |
| 1      | 94–95%   |
| 2      | 92–93%   |
| 3      | ≤ 91%    |

If SpO₂ is missing, `NA` is returned.

## References

1\.

Royal College of Physicians. *National Early Warning Score (NEWS):
Standardising the Assessment of Acute-Illness Severity in the NHS.
Report of a Working Party*.; 2012.

2\.

Royal College of Physicians. *National Early Warning Score (NEWS) 2:
Standardising the Assessment of Acute-Illness Severity in the NHS.
Updated Report of a Working Party.*; 2017.
<https://www.rcplondon.ac.uk/improving-care/resources/national-early-warning-score-news-2/>
