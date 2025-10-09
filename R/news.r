# NEWS score calculation helper functions
# Acutelines 2025
#
# Initial code by CHO, later adapted to fit Acutelines datatoolbox

#' Calculate NEWS score for Heart Rate (HR)
#' 
#' @family news
#'
#' @param hr Numeric value representing Heart Rate (bpm)
#' 
#' @return Numeric NEWS score for HR
news_hr <- function(hr) {
  dplyr::case_when(
    hr <= 40 | hr >= 131 ~ 3,
    hr > 110 & hr <= 130 ~ 2,
    (hr > 40 & hr <= 50) | (hr >= 91 & hr <= 110) ~ 1,
    hr > 50 & hr <= 90 ~ 0,
    .default = NA_integer_ #Should be integer?
  )
}

#' Calculate NEWS score for Respiratory Frequency (RF)
#' 
#' @family news
#' @param rf Numeric value representing Respiratory Frequency (breaths per minute)
#' 
#' @return Numeric NEWS score for RF
news_rf <- function(rf) {
  dplyr::case_when(
    rf <= 8 | rf >= 25 ~ 3,
    rf >= 21 & rf <= 24 ~ 2,
    rf >= 9 & rf <= 11 ~ 1,
    rf >= 12 & rf <= 20 ~ 0,
    .default = NA_integer_
  )
}

#' Calculate NEWS score for Temperature (Temp)
#' 
#' @family news
#' @param temp Numeric value representing Temperature (°C)
#' 
#' @return Numeric NEWS score for Temp
news_temp <- function(temp) {
  dplyr::case_when(
    temp <= 35 ~ 3,
    temp > 39 ~ 2,
    (temp > 38 & temp <= 39) | (temp > 35 & temp <= 36) ~ 1,
    temp > 36 & temp <= 38 ~ 0,
    .default = NA_integer_
  )
}


#' Calculate NEWS score for Systolic Blood Pressure (SBP)
#' 
#' @family news
#' @param sbp Numeric value representing Systolic Blood Pressure (mmHg)
#' 
#' @return Numeric NEWS score for SBP
news_sbp <- function(sbp) {
  return(
    dplyr::case_when(
      sbp >= 220 | sbp <= 90 ~ 3,
      sbp > 90 & sbp <= 100 ~ 2,
      sbp > 100 & sbp <= 110 ~ 1,
      sbp > 110 & sbp < 220 ~ 0,
      .default = NA_integer_
    )
  )
}

#' Calculate NEWS score for Consciousness level (AVPU or GCS)
#' 
#' Prioritizes AVPU over GCS if both are provided.
#' 
#' @family news
#' 
#' @param avpu Character value representing AVPU scale ("A", "V", "P", "U", "D")
#' @param gcs Numeric value representing Glasgow Coma Scale (3-15)
#' 
#' @return Numeric NEWS score for Consciousness level
news_consc <- function(avpu = NA, gcs = NA) {
  dplyr::case_when(
    (avpu %in% c("V", "P", "U", "D")) | (is.na(avpu) & gcs <= 14 & !is.na(gcs)) ~ 3,
    avpu == "A" | (is.na(avpu) & gcs == 15 & !is.na(gcs)) ~ 0,
    .default = NA_integer_
  )
}


#' Calculate NEWS score for Oxygen Supplementation (O2 supp)
#' 
#' @family news
#' 
#' @param o2supp Logical value indicating if oxygen supplementation is provided (TRUE/FALSE)
#' 
#' @return Numeric NEWS score for O2 supplementation
news_o2supp <- function(o2supp) {
  dplyr::case_when(
    o2supp == TRUE ~ 2,
    o2supp == FALSE ~ 0,
    .default = NA_integer_
  )
}

#' Calculate NEWS score for Oxygen Saturation (SpO2)
#' 
#' @family news
#' 
#' @param spo2 Numeric value representing Oxygen Saturation (%)
#' 
#' @return Numeric NEWS score for SpO2
news_spo2 <- function(spo2) {
  spo2 = round(spo2) # Round to nearest integer, in case decimal input

  dplyr::case_when(
    spo2 <= 91 ~ 3,
    spo2 == 92 | spo2 == 93 ~ 2,
    spo2 == 94 | spo2 == 95 ~ 1,
    spo2 >= 96 ~ 0,
    .default = NA_integer_
  )
}

#' Calculate total NEWS score from individual components
#' 
#' @family news
#' 
#' @param hr Numeric value representing Heart Rate (bpm)
#' @param rf Numeric value representing Respiratory Frequency (breaths per minute)
#' @param temp Numeric value representing Temperature (°C)
#' @param sbp Numeric value representing Systolic Blood Pressure (mmHg)
#' @param avpu Character value representing AVPU scale ("A", "V", "P", "U", "D")
#' @param gcs Numeric value representing Glasgow Coma Scale (3-15)
#' @param o2supp Logical value indicating if oxygen supplementation is provided (TRUE/FALSE)
#' @param spo2 Numeric value representing Oxygen Saturation (%)
#' @param return_df Logical value indicating if a detailed dataframe should be returned (TRUE/FALSE), default is FALSE
#' 
#' @return Total NEWS score as an integer, or a dataframe with individual component scores and total score if return_df is TRUE
#' 
#' @export
news_total <- function(hr, rf, temp, sbp, avpu, gcs, o2supp, spo2, return_df = FALSE) {
  hr_score <- news_hr(hr)
  rf_score <- news_rf(rf)
  temp_score <- news_temp(temp)
  sbp_score <- news_sbp(sbp)
  consc_score <- news_consc(avpu, gcs)
  o2supp_score <- news_o2supp(o2supp)
  spo2_score <- news_spo2(spo2)

  if (all(is.na(c(hr_score, rf_score, temp_score, sbp_score,consc_score, o2supp_score, spo2_score)))) {
    NA_integer_
  } else {
    total_score <- sum(c(hr_score, rf_score, temp_score, sbp_score, consc_score, o2supp_score, spo2_score), na.rm = TRUE)
  }

  if(return_df) {
    return(data.frame(
      NEWS_hr_score = hr_score,
      NEWS_rf_score = rf_score,
      NEWS_temp_score = temp_score,
      NEWS_SBP_score = sbp_score,
      NEWS_consc_score = consc_score,
      NEWS_o2supp_score = o2supp_score,
      NEWS_SpO2_score = spo2_score,
      NEWS_total_score = total_score
    ))
  } else {
    return(total_score)
  }
}