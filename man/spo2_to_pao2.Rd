% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/respiration.r
\name{spo2_to_pao2}
\alias{spo2_to_pao2}
\title{Convert SpO2 to PaO2}
\usage{
spo2_to_pao2(spo2)
}
\arguments{
\item{spo2}{SpO2 (\%) to be converted}
}
\value{
pao2 Calculated PaO2 (kPa)
}
\description{
Convert SpO2 to PaO2 based on Brown / Ellis / Severinghaus equation
}
\details{
Converts SpO2 percentages to PaO2 values in kPa. This is based
on the equations provides in two publications by Brown and
Ellis.
}
\references{
\href{https://doi.org/10.1016/j.chest.2016.01.003}{10.1016/j.chest.2016.01.003}

\href{https://doi.org/10.1152/jappl.1989.67.2.902}{10.1152/jappl.1989.67.2.902}

\href{https://doi.org/10.1152/jappl.1979.46.3.599}{10.1152/jappl.1979.46.3.599}
}
\seealso{
Other respiration: 
\code{\link{clean_fio2}()},
\code{\link{clean_spo2}()},
\code{\link{impute_fio2}()},
\code{\link{impute_pao2}()},
\code{\link{o2supply_to_fio2}()},
\code{\link{pfratio_imputed}()}
}
\concept{respiration}
