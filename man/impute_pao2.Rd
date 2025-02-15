% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/respiration.r
\name{impute_pao2}
\alias{impute_pao2}
\title{Impute PaO2 with SpO2}
\usage{
impute_pao2(pao2, spo2)
}
\arguments{
\item{pao2}{Arterial oxygen pressure (kPa)}

\item{spo2}{Peripheral oxygen saturation (\%)}
}
\value{
pao2
}
\description{
If PaO2 is missing replace with PaO2 calculated from SpO2.
}
\details{
PaO2 is missing in many cases, as arterial blood gass analysis
is only performed when needed. To fill those gaps, this function
converts SpO2 (\%) to PaO2 (kPa) in case the PaO2 is missing in
the dataset. If SpO2 is >=97\%, the PaO2 is set to 14.7 kPa,
because the oxygen dissociation curve is not valid to use in this
range. This functions uses \code{\link[=spo2_to_pao2]{spo2_to_pao2()}}.
}
\seealso{
Other respiration: 
\code{\link{clean_fio2}()},
\code{\link{clean_spo2}()},
\code{\link{impute_fio2}()},
\code{\link{o2supply_to_fio2}()},
\code{\link{pfratio_imputed}()},
\code{\link{spo2_to_pao2}()}
}
\concept{respiration}
