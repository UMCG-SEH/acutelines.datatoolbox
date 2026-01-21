# Package index

## SOFA score

Calculate (elements) of the SOFA score

- [`sofa_cardiovascular()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/sofa_cardiovascular.md)
  : SOFA cardiovascular
- [`sofa_cns()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/sofa_cns.md)
  : SOFA CNS
- [`sofa_coagulation()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/sofa_coagulation.md)
  : SOFA coagulation
- [`sofa_liver()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/sofa_liver.md)
  : SOFA liver
- [`sofa_magic_wrapper()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/sofa_magic_wrapper.md)
  : SOFA magic wrapper
- [`sofa_renal()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/sofa_renal.md)
  : SOFA renal
- [`sofa_respiration()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/sofa_respiration.md)
  : SOFA respiration
- [`sofa_total()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/sofa_total.md)
  : Total SOFA score

## Respiration

Collection of function to clean and impute respiratory parameters

- [`clean_fio2()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/clean_fio2.md)
  : Clean FiO2
- [`clean_spo2()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/clean_spo2.md)
  : Clean SpO2
- [`impute_fio2()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/impute_fio2.md)
  : Impute FiO2
- [`impute_pao2()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/impute_pao2.md)
  : Impute PaO2 with SpO2
- [`pfratio_imputed()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/pfratio_imputed.md)
  : Calculate P/F-ratio
- [`o2supply_to_fio2()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/o2supply_to_fio2.md)
  : O2-supply to FiO2
- [`spo2_to_pao2()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/spo2_to_pao2.md)
  : Convert SpO2 to PaO2

## NEWS score

Calculate (elements) of the National Early Warning Score (NEWS)

- [`news_consc()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/news_consc.md)
  : Calculate NEWS score for Consciousness level (AVPU or GCS)
- [`news_hr()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/news_hr.md)
  : Calculate NEWS score for Heart Rate (HR)
- [`news_o2supp()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/news_o2supp.md)
  : Calculate NEWS score for Oxygen Supplementation (O2 supp)
- [`news_rf()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/news_rf.md)
  : Calculate NEWS score for Respiratory Frequency (RF)
- [`news_sbp()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/news_sbp.md)
  : Calculate NEWS score for Systolic Blood Pressure (SBP)
- [`news_spo2()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/news_spo2.md)
  : Calculate NEWS score for Oxygen Saturation (SpO2)
- [`news_temp()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/news_temp.md)
  : Calculate NEWS score for Temperature (Temp)
- [`news_total()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/news_total.md)
  : Calculate total NEWS score from individual components

## Postprocessing functions

Functions to postprocess and clean data

- [`clean_lab()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/clean_lab.md)
  : Clean laboratory variables by removing '\<' and '\>' characters and
  converting to numeric Cleans all columns starting with "lab\_" (per
  Acutelines standard)
- [`normalize_decimal_format()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/normalize_decimal_format.md)
  : Normalize decimal format in a data frame Interprets both "." and ","
  as decimal indicators

## Helper functions

- [`cbrt()`](https://umcg-seh.github.io/acutelines.datatoolbox/reference/cbrt.md)
  : Calculate cube root of x
