% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qc_demo.r
\name{qc_demo}
\alias{qc_demo}
\title{Create Demographic Rows}
\usage{
qc_demo(
  input,
  colvar = "TRT01P",
  stats_list = c("Mean_SD", "Median", "Range"),
  var_list,
  drop_var_list = NULL,
  max_digit = 2
)
}
\arguments{
\item{input}{input dataframe}

\item{colvar}{column variable}

\item{stats_list}{stats variables to display. Accepted values are \code{c("Mean_SD", "Mean", "SD", "Median", "Range", "Min", "Max", "Geo_mean", "Geo_CV", "Geo_CL")} and the order of variables matters}

\item{var_list}{variable list in demo table. Please see details.}

\item{drop_var_list}{Variable that don't keep all levels in the output}

\item{max_digit}{maximum number of decimal place to report}
}
\value{
dataframe with demographic rows
}
\description{
Function to create demographic rows
}
\details{
\code{var_list} should be a dataframe with desired variable names as column names,
the label of each variable display in tables as the first row, and optionally,
the number of decimal places to keep in the output as the second row. If the 
label row of a variable is empty, the row text and N row for this variable
will be missing.
}
\examples{
age <- sample(18:65, 10, replace = TRUE)

adsl <- data.frame(
  USUBJID = 1:10,
  TRT01P = factor(sample(c("A", "B", "C"), 10, replace = TRUE)),
  AGE = age,
  AGEGR1 = ifelse(age < 45, "< 45", ">= 45"),
  SEX = factor(sample(c("Female", "Male"), 10, replace = TRUE)))
  
### Create variable list based on DPS and assign labels to them.
### Leave "" For variables that concatenate to the corresponding categorical variables
var_list <- data.frame(
  AGE = "Age, years",
  AGEGR1 = "",
  SEX = "Sex"
  )

tab1 <- qc_demo(adsl, colvar = "TRT01P", var_list = var_list)
tab1
}
