% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sofa.r
\name{sofa_magic_wrapper}
\alias{sofa_magic_wrapper}
\title{Magic wrapper to automatically calculate the SOFA score on multiple intervals (eg. hours, days)
assuming the data is exported using Acutelines SOFA SQL snippet.
Note: this function assumes data is exported using the Acutelines SOFA export snippet,
if not manually define columns and use sofa_total().}
\usage{
sofa_magic_wrapper(
  df,
  interval = 24,
  timespan = 72,
  return_df = FALSE,
  naming.cols = "SOFA_<interval>h_<variable>",
  naming.result = "SOFAscore_<interval>h_<variable>"
)
}
\arguments{
\item{df}{dataframe containing all the data}

\item{interval}{interval length, defaults to 24h}

\item{timespan}{number of intervals*interval length, defaults to 72h}

\item{return_df}{Return all elements of the sofa score in the DF instead of only totals}

\item{naming.cols}{naming scheme to use to select columns. \if{html}{\out{<interval>}} will be replaced by the interval and \if{html}{\out{<variable>}} by the variable name}

\item{naming.result}{naming scheme to assign to column with total score}
}
\value{
df with sofa scores
}
\description{
Magic wrapper to automatically calculate the SOFA score on multiple intervals (eg. hours, days)
assuming the data is exported using Acutelines SOFA SQL snippet.
Note: this function assumes data is exported using the Acutelines SOFA export snippet,
if not manually define columns and use sofa_total().
}
