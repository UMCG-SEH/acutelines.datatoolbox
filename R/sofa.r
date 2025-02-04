# SOFA score calculation helper functions
# Acutelines 2024
#
# Revisions:
# v1 STH Initial script
# v2 TvdA Revising calculations
# v3 RvW Refactored code to functions, added Brown equation
# v4 RvW added mechanical ventilation, moved respiration cleaning to other file

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

#' SOFA cardiovascular
#' 
#' This function calculates the cardiavascular part of the SOFA
#' score.
#' 
#' Due to inconsistent registration of norepinephrine dosages,
#' it's not possible to distinguish between high (>0.1) and
#' low (<=0.1) dosage of norepinephrine.
#' When norepinephrine is present 3.5 points are given.
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
#' Reliable measurements of urine output in a retrospective dataset of
#' both deteriorated and not deteriorated patients are scarce.
#' 
#' @family sofa
#' 
#' @param create Creatinine (umol/L)
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
  pfratio <- pfratio_imputed(df[[PaO2]], df[[SpO2]], df[[FiO2]], df[[oxygen_supply]], df[[oxygen_mode]])

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