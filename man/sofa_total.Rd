% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sofa.r
\name{sofa_total}
\alias{sofa_total}
\title{Calculate the total sofa score from Acutelines df}
\usage{
sofa_total(df, column_mapping, return_df = FALSE)
}
\arguments{
\item{df}{data frame}

\item{column_mapping}{map df columns to SOFA items. Follow this strucure:}
}
\value{
df column with sofa score per row
}
\description{
Calculate the total sofa score from Acutelines df
}
\examples{
\dontrun{
column_mapping <- c(
 PaO2 = "SOFA_24h_PaO2",
 SpO2 = "SOFA_24h_SpO2",
 FiO2 = "SOFA_24h_FiO2",
 oxygen_supply = "SOFA_24h_oxygen_supply",
 platelets = "SOFA_24h_platelets",
 bilirubin = "SOFA_24h_bilirubin",
 MAP = "SOFA_24h_MAP",
 dopamine = "SOFA_24h_dopamine",
 dobutamine = "SOFA_24h_dobutamine",
 epinephrine = "SOFA_24h_epinephrine",
 norepinephrine = "SOFA_24h_norepineprhine",
 norepinephrine_amp = "SOFA_24h_norepinephrine_amp",
 GCS = "SOFA_24h_GCS",
 creatinine = "SOFA_24h_creatinine"
 )
result <- sofa_total(df, column_mapping)
}

}
