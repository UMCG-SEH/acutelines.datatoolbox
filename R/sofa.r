# SOFA score calculation helper functions
# Acutelines 2024
#
# Revisions:
# v1 STH Initial script
# v2 TvdA Revising calculations
# v3 RvW Refactored code to functions, added Brown equation



#' Convert SpO2 to PaO2
#' 
#' Convert SpO2 to PaO2 based on Brown / Ellis equation
#' 
#' Converts SpO2 percentages to PaO2 values in kPa. This is based
#' on the equations provides in two publications by Brown and
#' Ellis.
#' DOI: \href{https://doi.org/10.1016/j.chest.2016.01.003}{10.1016/j.chest.2016.01.003}
#' DOI: \href{https://doi.org/0.1152/jappl.1979.46.3.599}{0.1152/jappl.1979.46.3.599}
#' 
#' @family helper
#' 
#' @param spo2 SpO2 (%) to be converted
#' 
#' @return pao2 Calculated PaO2 (kPa)
spo2_to_pao2 <- function(spo2) {
  S <- spo2/100
  a <- 11700/((1/S)-1)
  b <- sqrt(50^3+a^2)
  l <- cbrt(a + b)
  r <- cbrt(a - b)
  pao2_mmhg = l + r
  pao2_kpa = pao2_mmhg*0.133322
  return(pao2_kpa)
}

#' Calculate cube root of x
#' 
#' @family helper
#' @param x x
#' @return x^(1/3)
cbrt <- function(x){
  return(sign(x)*abs(x)^(1/3))
}

#' O2-supply to FiO2
#' 
#' Convert the O2-supply in L/min to FiO2.
#' 
#' This function converts the oxygen supply in L/min as given per nasal
#' canula to FiO2. In some cases the FiO2 is reported in the O2 suuply
#' field. To correct for this mistake, when O2 supply is below 16 it is
#' considered as FiO2.
#' 
#' @family helper
#' 
#' @param o2supply Oxygen supply (L/min)
#' 
#' @return fio2 fraction inspirated oxygen
o2supply_to_fio2 <- function(o2supply) {
  return(ifelse(o2supply<=15, (20+(4*o2supply)), o2supply))
}


#' Impute PaO2 with SpO2
#' 
#' If PaO2 is missing replace with PaO2 calculated from SpO2.
#' 
#' PaO2 is missing in many cases, as arterial blood gass analysis
#' is only performed when needed. To fill those gaps, this function
#' converts SpO2 (%) to PaO2 (kPa) in case the PaO2 is missing in
#' the dataset. This functions uses [spo2_to_pao2()]
#' 
#' @family helper
#' 
#' @param pao2 Arterial oxygen pressure (kPa)
#' @param spo2 Peripheral oxygen saturation (%)
#'
#' @return pao2
impute_pao2 <- function(pao2, spo2) {
  # Replace PaO2<7 (probably venous) with NA
  pao2[pao2 < 7] <- NA
  # Replace all missing values with estimated PaO2 based on SpO2
  pao2[is.na(pao2)] <- round(spo2_to_pao2(spo2[is.na(pao2)]),1)
  
  return(pao2)
}

#' Impute FiO2
#' 
#' Impute FiO2 with oxygen supply. If FiO2 is missing,
#' replace with FiO2 calculated from oxygen supply. If still
#' missing, impute with 21% (ambient air).
#' 
#' FiO2 is missing in many cases. To fill those gaps, this function
#' converts the oxygen supply by nasal cannula (L/min) to FiO2
#' in case the FiO2 is missing in the dataset. When also the
#' oxygen supply is missing, the FiO2 is imputed with 21%,
#' the ambient air oxygen percentage. This functions uses [o2supply_to_fio2()].
#' 
#' @family helper
#' 
#' @param fio2 Fraction inspired oxygen
#' @param o2supply Oxygen supply (L/min)
#' 
#' @return fio2
impute_fio2 <- function(fio2, o2supply) {
  # If FiO2 is <1 then it is probably a fraction, convert to percentage
  fio2 <- ifelse(fio2 < 1, fio2*100, fio2)

  # If FiO2 is <21 then it is probable oxygen supply in L/min, convert
  fio2 <- ifelse(fio2 > 1 & fio2 < 21, o2supply_to_fio2(fio2), fio2)
  #fio2[fio2 > 1 & fio2 < 21] <- o2supply_to_fio2(fio2[fio2 > 1 & fio2 < 21])

  # Replace all missing Fio2 with calculated FiO2 from O2 supply or 21%
  fio2[is.na(fio2)] <- o2supply_to_fio2(o2supply[is.na(fio2)])
  fio2[is.na(fio2)] <- 21

  return(fio2/100)
}

#' Calculate P/F-ratio
#' 
#' Calculate P/F-ratio while imputing missing PaO2 using SpO2 and
#' missing FiO2 using oxygen supply (in L/min)
#' 
#' This function uses the impute_pao2 and impute_fio2 helper functions.
#' 
#' @family helper
#' 
#' @param pao2 Arterial oxygen pressure (kPa)
#' @param spo2 Peripheral oxygen saturation (%)
#' @param fio2 Fraction inspired oxygen
#' @param o2supply Oxygen supply (L/min)
#' 
#' @return pfratio
pfratio_imputed <- function(pao2, spo2, fio2, oxygen_supply) {
  # Impute missing PaO2 with SpO2
  pao2 <- impute_pao2(pao2, spo2)

  # Impute missing FiO2 with O2-supply
  fio2 <- impute_fio2(fio2, oxygen_supply)

  return(pao2/fio2)
}

#' SOFA respiration
#' 
#' This function calculates the respiratory part of the SOFA
#' score. It needs either the PF-ratio or both PaO2 and FiO2.
#' If PF-ratio is missing, the function will calculate it from
#' PaO2 and FiO2.
#' 
#' @family sofa
#' 
#' @param mechanical_ventilation Mechanical Ventilation (1=TRUE, 0=FALSE)
#' @param pfratio P/F-ratio (kPa)
#' @param pao2 PaO2 (kPa)
#' @param fio2 FiO2
#' 
#' @return partial sofa score
sofa_respiration <- function(mechanical_ventilation, pfratio, pao2, fio2) {
  if(missing(pfratio)) {
    if(!missing(pao2) && !missing(fio2)) {
      pfratio <- pao2/fio2
    } else {
      stop('Not enough parameters for sofa_respiration')
    }
  }

  sofa_resp <- ifelse(pfratio < 13.3 & mechanical_ventilation == 1, 4, # <100 mmHg
                  ifelse(pfratio < 26.7 & mechanical_ventilation == 1, 3, # <200 mmHg
                    ifelse(pfratio < 40, 2, # <300 mmHg
                      ifelse(pfratio < 53.3, 1, # <400 mmHg
                        ifelse(pfratio >= 53.3, 0, NA) # >= 400 mmHg
                      )
                    )
                  )
                )

  return(sofa_resp)
}

#' SOFA coagulation
#' 
#' This function calculates the coagulation part of the SOFA
#' score. Platelet count need to be provided in *10^3 uL^-1.
#' 
#' @family sofa
#' 
#' @param plt Platelets (*10^3 uL^-1)
#' 
#' @return partial sofa score
sofa_coagulation <- function(plt) {
  sofa_plt <- ifelse(plt>149,0,
                     ifelse(plt>99,1,
                            ifelse(plt>49,2,
                                   ifelse(plt>19,3,
                                          ifelse(plt<20,4,NA)))))
  return(sofa_plt)
}

#' SOFA liver
#' 
#' This function calculates the liver part of the SOFA
#' score. Platelet count need to be provided in umol L^-1.
#' 
#' @family sofa
#' 
#' @param bili Bilirubin (umol L^-1)
#' 
#' @return partial sofa score
sofa_liver <- function(bili) {
  sofa_bili <- ifelse((bili<20),0,
                      ifelse((bili<33),1,
                             ifelse((bili<102),2,
                                    ifelse((bili<205),3,
                                           ifelse((bili>204),4,NA)))))
  return(sofa_bili)
}

#' SOFA cardiovacular
#' 
#' This function calculates the cardiavascular part of the SOFA
#' score.
#' 
#' Due to inconsistent registration of norepinephrine dosages,
#' it's not possible to distinguish between high (>0.1) and
#' low (<=0.1) dosage of norepinephrine.
#' When norephenrine is present 3.5 points are given.
#' Researchers can either round this to 4 (overestimate),
#' 3 (underestimate) or leave it as is.
#' 
#' @family sofa
#' 
#' @param map Mean Arterial Pressure
#' @param dopamine Dopamine dosage (ug/kg/min)
#' @param dobutamine Dobutamine dosage (ug/kg/min)
#' @param epinephrine Epinephrine dosage (ug/kg/min)
#' @param norepinephrine Norepinephrine dosage (ug/kg/min)
#' @param norepinephrine_amp Norepinephrine ampul
#'  administration (1=TRUE, 0=FALSE)
#' 
#' @return partial sofa score
sofa_cardiovascular <- function(map, dopamine, dobutamine, epinephrine,
                                norepinephrine, norepinephrine_amp) {
  sofa_cardio <- NA
  sofa_cardio[map>=70] <- 0
  sofa_cardio[map<70] <- 1
  sofa_cardio[(dopamine>0 | dobutamine>0)] <- 2
  sofa_cardio[(dopamine>=5.1 | epinephrine>0)] <- 3
  sofa_cardio[(norepinephrine>0 | norepinephrine_amp==1)] <- 3.5
  sofa_cardio[(dopamine>15 | epinephrine>0.1)] <- 4
  return(sofa_cardio)
}

#' SOFA CNS
#' 
#' This function calculates the Central Nervous System (CNS) part of the SOFA
#' score.
#' 
#' @family sofa
#' 
#' @param gcs Glascow Coma Scale
#' 
#' @return partial sofa score
sofa_cns <- function(gcs) {
  sofa_gcs <- ifelse(gcs<6,4,
                     ifelse(gcs<10,3,
                            ifelse(gcs<13,2,
                                   ifelse(gcs<15,1,
                                          ifelse(gcs>14,0,NA)))))
  return(sofa_gcs)
}

#' SOFA renal
#' 
#' This function calculates the renal part of the SOFA
#' score.
#' 
#' This function ignores the urine output as defined in the SOFA score
#' Reliable measurements of urine ouput in a retrospective dataset of
#' both deteriorated and not deteriorated patients are scarce.
#' 
#' @family sofa
#' 
#' @param create Creatinine
#' 
#' @return partial sofa score
sofa_renal <- function(creat) {
  sofa_creat <- ifelse(creat<110,0,
                       ifelse(creat<171,1,
                              ifelse(creat<301,2,
                                     ifelse(creat<441,3,
                                            ifelse(creat>440,4,NA)))))
  return(sofa_creat)
}

#' Total SOFA score
#' 
#' Calculate the total sofa score from Acutelines dataframe.
#' 
#' @family sofa
#' 
#' @param df data frame
#' @param column_mapping map df columns to SOFA items. Follow this structure:
#' @param return_df if set to TRUE returns all individual elements of the SOFA
#' 
#' @return df column with sofa score per row
#' 
#' @examples
#' \dontrun{
#' column_mapping <- c(
#'  PaO2 = "SOFA_24h_PaO2",
#'  SpO2 = "SOFA_24h_SpO2",
#'  FiO2 = "SOFA_24h_FiO2",
#'  oxygen_supply = "SOFA_24h_oxygen_supply",
#'  oxygen_mode = "SOFA_24h_oxygen_mode",
#'  platelets = "SOFA_24h_platelets",
#'  bilirubin = "SOFA_24h_bilirubin",
#'  MAP = "SOFA_24h_MAP",
#'  dopamine = "SOFA_24h_dopamine",
#'  dobutamine = "SOFA_24h_dobutamine",
#'  epinephrine = "SOFA_24h_epinephrine",
#'  norepinephrine = "SOFA_24h_norepineprhine",
#'  norepinephrine_amp = "SOFA_24h_norepinephrine_amp",
#'  GCS = "SOFA_24h_GCS",
#'  creatinine = "SOFA_24h_creatinine"
#'  )
#' result <- sofa_total(df, column_mapping)
#' }
#' 
#' @export
sofa_total <- function(df, column_mapping, return_df=FALSE){
  PaO2 = column_mapping["PaO2"]
  SpO2 = column_mapping["SpO2"]
  FiO2 = column_mapping["FiO2"]
  oxygen_supply = column_mapping["oxygen_supply"]
  oxygen_mode = column_mapping["oxygen_mode"]
  platelets = column_mapping["platelets"]
  bilirubin = column_mapping["bilirubin"]
  MAP = column_mapping["MAP"]
  dopamine = column_mapping["dopamine"]
  dobutamine = column_mapping["dobutamine"]
  epinephrine = column_mapping["epinephrine"]
  norepinephrine = column_mapping["norepinephrine"]
  norepinephrine_amp = column_mapping["norepinephrine_amp"]
  GCS = column_mapping["GCS"]
  creatinine = column_mapping["creatinine"]

  ## Respiration
  # Determine pfratio
  pfratio <- pfratio_imputed(df[[PaO2]], df[[SpO2]], df[[FiO2]], df[[oxygen_supply]])

  # Determine mechanical ventilation (by Acutelines scores: 35=CPAP, 40=optiflow, 50=tube)
  mechanical_ventilation <- ifelse(df[[oxygen_mode]] >= 35, 1, 0)

  # Calc sofa score
  score_respiration <- sofa_respiration(mechanical_ventilation, pfratio)

  ## Coagulation
  score_coagulation <- sofa_coagulation(df[[platelets]])

  ## Liver
  score_liver <- sofa_liver(df[[bilirubin]])

  ## Cardiovascular
  score_cardiovascular <- sofa_cardiovascular(df[[MAP]], df[[dopamine]], df[[dobutamine]], df[[epinephrine]], df[[norepinephrine]], df[[norepinephrine_amp]])

  ## CNS
  score_CNS <- sofa_cns(df[[GCS]])

  ## Renal
  score_renal <- sofa_renal(df[[creatinine]])

  # Sum
  score_total <- rowSums(cbind(score_respiration, score_coagulation, score_liver, score_cardiovascular, score_CNS, score_renal), na.rm = TRUE)

  if(return_df) {
    return <- data.frame(matrix(nrow=nrow(df)))
    return['SOFA_respiration'] <- score_respiration
    return['SOFA_coagulation'] <- score_coagulation
    return['SOFA_liver'] <- score_liver
    return['SOFA_cardiovascular'] <- score_cardiovascular
    return['SOFA_CNS'] <- score_CNS
    return['SOFA_renal'] <- score_renal
    return['SOFA_total'] <- score_total
    return(return)
  } else {
    return(score_total)
  }
  
}

#' SOFA magic wrapper
#' 
#' Magic wrapper to automatically calculate the SOFA score on
#' multiple intervals (eg. hours, days) assuming the data is
#' exported based on Acutelines SOFA exports.
#' 
#' This function assumes data is exported using the Acutelines SOFA
#' export snippet, if not, manually define columns and use [sofa_total()].
#' 
#' @family sofa
#' 
#' @param df dataframe containing all the data
#' @param interval interval length, defaults to 24h
#' @param timespan number of intervals*interval length, defaults to 72h
#' @param return_df Return all elements of the sofa score in the DF instead of only totals
#' @param naming.cols naming scheme to use to select columns. <interval> will be replaced by the interval and <variable> by the variable name
#' @param naming.result naming scheme to assign to column with total score
#' 
#' @return df with sofa scores
#' 
#' @export
sofa_magic_wrapper <- function(df, interval=24, timespan=72, return_df=FALSE,
                              naming.cols="SOFA_<interval>h_<variable>",
                              naming.result="SOFAscore_<interval>h_<variable>") {
  # Define the variables used in sofa_total()
  variables <- c("PaO2", "SpO2", "FiO2", "oxygen_supply", "oxygen_mode", "platelets","bilirubin", "MAP", "dopamine", "dobutamine", "epinephrine", "norepinephrine", "norepinephrine_amp", "GCS", "creatinine")

  # Iterate over each interval (hours generally)
  for(i in seq(interval, timespan, by=interval)) {
    # Substitute <interval> with current interval number
    varname <- gsub('<interval>', i, naming.cols)
    resultname <- gsub('<interval>', i, naming.result)

    # Create empty column_mapping varirable for sofa_total()
    column_mapping <- c()
    # Iterate over previously defined needed columns
    for(j in seq_along(variables)) {
      # Substitute <variable> with actual variable name and save in column_mapping
      column_mapping[[variables[j]]] <- gsub('<variable>', variables[j], varname)
    }

    # Pass the df and column_mapping for the interval to the sofa_total() function
    # and save the result in the dataframe
    df_temp <- sofa_total(df, unlist(column_mapping), return_df=TRUE)
    if(return_df) {
      df[[gsub('<variable>', 'respiration', resultname)]] <- df_temp$SOFA_respiration
      df[[gsub('<variable>', 'coagulation', resultname)]] <- df_temp$SOFA_coagulation
      df[[gsub('<variable>', 'liver', resultname)]] <- df_temp$SOFA_liver
      df[[gsub('<variable>', 'cardiovascular', resultname)]] <- df_temp$SOFA_cardiovascular
      df[[gsub('<variable>', 'CNS', resultname)]] <- df_temp$SOFA_CNS
      df[[gsub('<variable>', 'renal', resultname)]] <- df_temp$SOFA_renal
      df[[gsub('<variable>', 'total', resultname)]] <- df_temp$SOFA_total
    }
    df[[gsub('<variable>', 'total', resultname)]] <- df_temp$SOFA_total
  }

  return(df)
}