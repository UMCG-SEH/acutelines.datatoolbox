# Respiration related cleaning and imputing functions
# Acutelines 2024

#' Calculate cube root of x
#' 
#' @family helper
#' @param x x
#' @return x^(1/3)
cbrt <- function(x){
  return(sign(x)*abs(x)^(1/3))
}

#' Convert SpO2 to PaO2
#' 
#' Convert SpO2 to PaO2 based on Brown / Ellis equation
#' 
#' Converts SpO2 percentages to PaO2 values in kPa. This is based
#' on the equations provides in two publications by Brown and
#' Ellis.
#' 
#' @references \href{https://doi.org/10.1016/j.chest.2016.01.003}{10.1016/j.chest.2016.01.003}
#' @references \href{https://doi.org/10.1152/jappl.1979.46.3.599}{10.1152/jappl.1979.46.3.599}
#' 
#' @family respiration
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

#' O2-supply to FiO2
#' 
#' Convert the O2-supply in L/min to FiO2.
#' 
#' This function converts the oxygen supply in L/min as given per nasal
#' canula to FiO2. In some cases the FiO2 is reported in the O2 supply
#' field. To correct for this mistake, when O2 supply is above 15 it is
#' considered as FiO2.
#' 
#' @family respiration
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
#' the dataset. This functions uses [spo2_to_pao2()].
#' 
#' @family respiration
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

#' Clean FiO2
#' 
#' Clean FiO2 values based on several rules.
#' 
#' Rules include cleaning on impossible values and on common mistakes
#' or switch-ups when entering data in the EHR.
#' 
#' @section Todo:
#' Implement more sophisticated FiO2 cleaning algorithm.
#' 
#' @param fio2 Fraction inspired oxygen (%)
#' @param o2supply Oxygen supply (L/min)
#' 
#' @family respiration
#' @family cleaning
#' 
#' @return fio2 (%)
clean_fio2 <- function(fio2, o2supply) {
  # If FiO2 is <1 then it is probably a fraction, convert to percentage
  fio2 <- ifelse(fio2 < 1, fio2*100, fio2)

  # If FiO2 is <21 then it is probable oxygen supply in L/min, convert
  fio2 <- ifelse(fio2 > 1 & fio2 < 21, o2supply_to_fio2(fio2), fio2)

  return(fio2)
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
#' the ambient air oxygen percentage. This functions uses [o2supply_to_fio2()]
#' 
#' @family respiration
#' 
#' @param fio2 Fraction inspired oxygen (%)
#' @param o2supply Oxygen supply (L/min)
#' 
#' @return fio2
impute_fio2 <- function(fio2, o2supply) {
  # Replace all missing Fio2 with calculated FiO2 from O2 supply or 21%
  fio2[is.na(fio2)] <- o2supply_to_fio2(o2supply[is.na(fio2)])
  fio2[is.na(fio2)] <- 21

  return(fio2)
}

#' Calculate P/F-ratio
#' 
#' Calculate P/F-ratio while cleaning and imputing PaO2 and FiO2.
#' 
#' This function uses [impute_pao2()], [clean_fio2()] and [impute_fio2()].
#' 
#' @family respiration
#' 
#' @param pao2 Arterial oxygen pressure (kPa)
#' @param spo2 Peripheral oxygen saturation (%)
#' @param fio2 Fraction inspired oxygen (%)
#' @param o2supply Oxygen supply (L/min)
#' 
#' @return pfratio
pfratio_imputed <- function(pao2, spo2, fio2, oxygen_supply) {
  # Impute missing PaO2 with SpO2
  pao2 <- impute_pao2(pao2, spo2)

  # Clean fio2 values
  fio2 <- clean_fio2(fio2, oxygen_supply)

  # Impute missing FiO2 with O2-supply
  fio2 <- impute_fio2(fio2, oxygen_supply)

  return(pao2/(fio2/100))
}