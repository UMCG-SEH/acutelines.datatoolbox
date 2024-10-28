# acutelines.datatoolbox 0.0.0.6

* Fixed missing mechanical ventilation check when calculation the respiratory part of the SOFA-score. When no mechanical ventilation, the respiratory SOFA-score is now limited to 2 points.
* When FiO2 <1 consider it as a fraction instead of percentage. Then convert the fraction to percentage (corrected FiO2 = old FiO2 * 100).
* When FiO2 >1 and <21 consider it as oxygen supply (L/min) instead of FiO2. Then convert this value to FiO2 with the same formula as missing FiO2.
* The oxygen supply to FiO2 convert relied on o2supply <16. In edge cases this could lead to problems van the oxygen supply is between 15 and 16 (decimals). Changed to o2supply <=15.
* Fixed some minor bugs to resolve edge cases in using the magic wrapper.

# acutelines.datatoolbox 0.0.0.5

* Initial public release, includes SOFA score calculation.