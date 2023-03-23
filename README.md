# QCfuns
Collection of R QC Functions

## Installation
``` r
# install.packages("devtools")
devtools::install_github("YufanC/QCfuns")
```
## Example QC script
``` r
#################################################################################
# Original Reporting Effort:  Reporting effort
# Program Name             :  Program name
# R Version                :  R version 4.2.1 (2022-06-23)
# Short Description        :  Description
# Author                   :  Author
# Date                     :  2023-03-23
# input                    :
# Output                   :  
# Remarks                  :

# Modification History
#Rev       Modified By         Reporting Effort         Date      Description
#################################################################################
rm(list=ls()) 
#################################################################################
###                           Prepping environment                            ###
#################################################################################
library(haven)
library(dplyr)
library(stringr)
library(tidyr)
library(QCfuns)

files.sources <- list.files(path = read_path(rptdrv, "qc"), pattern = "^\qcf.*\.r", full.names = T)
sapply(files.sources, source)

###########################
###  Derive QC dataset  ###
###########################

adsl <- read_sas(read_path(a_in, "adsl.sas7bdat")) %>% 
  mutate(across(where(is.character), zap_empty))
  
###########################
###  Generate QC table  ###
###########################

###################
### Read in RTF ###
###################

tableid <- "tableid"
tab_rtf <- rtf2df(tableid)

############################
### Compare two datasets ###
############################

compare2xlsx(qc = tab_final, rtf = tab_rtf, path = qc, filename = tableid)
```