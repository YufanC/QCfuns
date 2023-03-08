##################################################################################################
# Original Reporting Effort:  THR1003/DBR_CSR/RE_CSR
# Program Name             :  qcfdemofuns.r
# R Version                :  4.1.0
# Short Description        :  R functions to create demographic table
# Author                   :  Yufan Chen
# Date                     :  APR 30,2021
# Input                    :  Getdigit(): get the maximum decimal place for a variable
#                             data:          input dataset
#                             var:           variable within input dataset 
#                             
# Output                   :  
# Remarks                  :  Required packages: dplyr, tidyr
# function Sample Call     :

# Modification History
#Rev        Modified By                   Reporting Effort         Date      Description
##################################################################################################
### Get max decimal place for continuous variable
Getdigit <- function(data, var){
  if (sum((data[[var]] %% 1) == 0) != length(data[[var]])){
    value <- as.character(data[[var]])[(data[[var]] %% 1) != 0]
    max(nchar(matrix(unlist(strsplit(value, ".", fixed = T)), ncol = 2, byrow = T)[, 2]))
  } else {
    return(0)
  }
}