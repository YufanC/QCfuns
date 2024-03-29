% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qcfgetmaxdigit.r
\name{getmaxdigit}
\alias{getmaxdigit}
\title{Get Maximum Digit}
\usage{
getmaxdigit(data, var, max_digit)
}
\arguments{
\item{data}{input dataframe}

\item{var}{a continuous variable in the input dataframe}

\item{max_digit}{maximum number of digit to report}
}
\description{
Function to get maximum decimal place for continuous variable
}
\examples{
adsl <- data.frame(
  USUBJID = 1:10,
  TRT01P = factor(sample(c("A", "B", "C"), 10, replace = TRUE)),
  AGE = sample(18:65, 10, replace = TRUE))
  
getmaxdigit(adsl, "AGE", 1)
}
