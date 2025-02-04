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
#' Convert SpO2 to PaO2 based on Brown / Ellis / Severinghaus equation
#' 
#' Converts SpO2 percentages to PaO2 values in kPa. This is based
#' on the equations provides in two publications by Brown and
#' Ellis.
#' 
#' @references \href{https://doi.org/10.1016/j.chest.2016.01.003}{10.1016/j.chest.2016.01.003}
#' @references \href{https://doi.org/10.1152/jappl.1989.67.2.902}{10.1152/jappl.1989.67.2.902}
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
#' @param o2system Oxygen supply method
#'                  50 = tube
#'                  40 = optiflow
#'                  35 = cpap
#'                  30 = non-rebreather
#'                  25 = venturi mask
#'                  20 = nasal cannula   
#'                  10 = nebulizer
#'                  0 = none (ambient air) 
#' 
#' @return fio2 fraction inspirated oxygen
o2supply_to_fio2 <- function(o2supply, o2system) {
  #If only oxygen mode is available, the maximum FiO₂ for that mode is used, following the hospital guidelines.
  o2supply <- dplyr::case_when(
    # Nasal cannula
    o2system == 20 & o2supply == NA ~ 6,

    # Venturi mask and Non-rebreather
    (o2system == 25 | o2system == 30) & o2supply == NA ~ 15,

    # CPAP and Optiflow
    (o2system == 35 | o2system == 40) & o2supply == NA ~ 60,
  )
  
  fio2 <- dplyr::case_when(
    # Nasal cannula
    o2system == 20 ~ 20 + (4 * o2supply),

    # Simple face mask
    # Not implemented, not used in UMCG. Can be implemented for future datasets.
    
    # Venturi mask
    o2system == 25 & o2supply == 2 ~ 24,
    o2system == 25 & o2supply == 4 ~ 28,
    o2system == 25 & o2supply == 6 ~ 31,
    o2system == 25 & o2supply == 8 ~ 35,
    o2system == 25 & o2supply == 10 ~ 40,
    o2system == 25 & o2supply == 15 ~ 60,
    
    # Non-rebreather
    o2system == 30 & o2supply >=10 & o2supply <= 15 ~ 60 + (o2supply - 10) * 6,
    
    # CPAP and Optiflow
    (o2system == 35 | o2system == 40) & o2supply <= 60 ~ 30 + ((o2supply - 1) * 70)/59
    
    # Default case for missing values
    #TRUE ~ NA_real_
  )

  return(fio2)
}

#' Clean SpO2
#' 
#' Clean SpO2 values based on several rules.
#' 
#' Rules include cleaning on impossible values and on common mistakes
#' or switch-ups when entering data in the EHR.
#' Remove Spo2<50 and convert 50-70 to 70%.
#' 
#' @param spo2 Peripheral oxygen saturation (%)
#' 
#' @family respiration
#' @family cleaning
#' 
#' @return spo2 (%)
clean_spo2 <- function(spo2) {
  # Remove SpO2 below 50%, because unreliable
  spo2[spo2 < 50] <- NA

  # Convert 50-70 to 70% because SpO2 devices are not reliable in this range
  spo2[spo2 >= 50 & spo2 < 70] <- 70

  return(spo2)
}

#' Impute PaO2 with SpO2
#' 
#' If PaO2 is missing replace with PaO2 calculated from SpO2.
#' 
#' PaO2 is missing in many cases, as arterial blood gass analysis
#' is only performed when needed. To fill those gaps, this function
#' converts SpO2 (%) to PaO2 (kPa) in case the PaO2 is missing in
#' the dataset. If SpO2 is >=97%, the PaO2 is set to 14.7 kPa,
#' because the oxygen dissociation curve is not valid to use in this
#' range. This functions uses [spo2_to_pao2()].
#' 
#' @family respiration
#' 
#' @param pao2 Arterial oxygen pressure (kPa)
#' @param spo2 Peripheral oxygen saturation (%)
#'
#' @return pao2
impute_pao2 <- function(pao2, spo2) {
  # Replace PaO2<7 (probably venous) with NA to allow interpolation by SpO2
  pao2[pao2 < 7] <- NA

  # Get all SpO2 for missing PaO2 and SpO2 <97%
  spo2_missing_pao2 <- spo2[is.na(pao2) & !is.na(spo2) & spo2 < 97]

  # Replace all missing PaO2 values with estimated PaO2 based on SpO2 if SpO2 <97%
  pao2[is.na(pao2) & !is.na(spo2) & spo2 < 97] <- round(spo2_to_pao2(spo2_missing_pao2),1)

  # Set PaO2 to 14.7 kPa (110 mmHg) if SpO2 is 97% or higher
  pao2[is.na(pao2) & !is.na(spo2) & spo2 >= 97] <- 14.7
  
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
#' @param o2system Oxygen supply method
#' 
#' @family respiration
#' @family cleaning
#' 
#' @return fio2 (%)
clean_fio2 <- function(fio2, o2supply, o2system) {
  # If FiO2 is >100 then it is as measurement error by some devices, set to 100
  fio2[fio2>100] <- 100

  # If FiO2 = 0, set as missing
  fio2[fio2 == 0] <- NA

  # If FiO2 >= 85 and oxygen mode = 0, this is proibably SpO2, set as missing
  fio2[fio2 >= 85 & o2system == 0] <- NA

  # If FiO2 is <1 then it is probably a fraction, convert to percentage
  fio2 <- ifelse(fio2 < 1, fio2*100, fio2)

  # If FiO2 is between 1 and 21 set to NA
  fio2[fio2 > 1 & fio2 < 21] <- NA

  return(fio2)
}

#' Impute FiO2
#' 
#' Impute FiO2 with oxygen supply. If FiO2 is missing,
#' replace with FiO2 calculated from oxygen supply. If still
#' missing, impute with 21% (ambient air).
#' 
#' FiO2 is missing in many cases. To fill those gaps, this function
#' converts the oxygen supply (L/min) to FiO2 based on the oxygen delivery device
#' in case the FiO2 is missing in the dataset. When also the
#' oxygen supply is missing, the FiO2 is imputed with 21%,
#' the ambient air oxygen percentage. This functions uses [o2supply_to_fio2()]
#' 
#' @family respiration
#' 
#' @param fio2 Fraction inspired oxygen (%)
#' @param o2supply Oxygen supply (L/min)
#' @param o2system Oxygen supply method
#' 
#' @return fio2
impute_fio2 <- function(fio2, o2supply, o2system) {
  # Replace all missing Fio2 with calculated FiO2 from O2 supply
  fio2[is.na(fio2)] <- o2supply_to_fio2(o2supply[is.na(fio2)], o2system[is.na(fio2)])
  # If still missing set FiO2 to 21% (ambient air)
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
#' @param o2system Oxygen supply method (ranked 0-50)
#' 
#' @return pfratio
pfratio_imputed <- function(pao2, spo2, fio2, o2supply, o2system) {
  # Clean SpO2 values
  spo2 <- clean_spo2(spo2)

  # Impute missing PaO2 with SpO2
  pao2 <- impute_pao2(pao2, spo2)

  # Clean fio2 values
  fio2 <- clean_fio2(fio2, o2supply, o2system)

  # Impute missing FiO2 with O2-supply
  fio2 <- impute_fio2(fio2, o2supply, o2system)

  return(pao2/(fio2/100))
}