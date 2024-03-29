% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qcfvgetdigit.r
\name{getdigit}
\alias{getdigit}
\title{Get Digit}
\usage{
getdigit(var, max_digit, criteria = 10)
}
\arguments{
\item{var}{input dataframe}

\item{max_digit}{maximum number of digit to report}

\item{criteria}{values with number of decimal places over the criteria will be rounded to the number of decimal places that equals to the criteria. Default = 10}
}
\description{
Function to get the decimal place of a numeric column
}
\examples{
age <- sample(18:65, 10, replace = TRUE)

getdigit(age , 1)
}
