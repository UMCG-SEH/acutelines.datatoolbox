# acutelines.datatoolbox 1.0.0

* Added dplyr as a dependency to make use of dplyr::case_when(), which is more clear than cluttered ifelse structures.
* Fixed missing mechanical ventilation check when calculation the respiratory part of the SOFA-score. When no mechanical ventilation, the respiratory SOFA-score is now limited to 2 points.
* When FiO2 <1 consider it as a fraction instead of percentage. Then convert the fraction to percentage (corrected FiO2 = old FiO2 * 100).
* When FiO2 >1 and <21 consider it as missing.
* When FiO2 >100 this is a measurement error by some devices, set to 100.
* Revised oxygen supply to FiO2 conversion based on the device delivering the oxygen in combination with the flow.
* SpO2 >= 97% will now be treated as 96% and result in PaO2 of 14.7 kPa (110 mmHg). If SpO2 is below 50% values a discarded, as not reliable.
* Convert SpO2 50-70% to 70% because SpO2 devices are not reliable in this range.
* Fixed some minor bugs to resolve edge cases in using the magic wrapper.

# acutelines.datatoolbox 0.0.0.5

* Initial public release, includes SOFA score calculation.
