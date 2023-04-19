# QCfuns
Collection of R QC Functions

## Installation
``` r
# install.packages("devtools")
devtools::install_github("YufanC/QCfuns")
```
## Example QC script
``` r
# Add qcscript to snippet
QCfuns::addQCscript()

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

#################################################################################
###                           Prepping environment                            ###
#################################################################################
library(haven)
library(dplyr)
library(stringr)
library(tidyr)
library(QCfuns)

files.sources <- list.files(path = read_path(rptdrv, "qc"), pattern = "qcf.*\\.r", full.names = T)
sapply(files.sources, source)

###########################
###  Derive QC dataset  ###
###########################

adsl <- read_sas(read_path(a_in, "adsl.sas7bdat")) %>% 
  mutate(across(where(is.character), zap_empty))
  
###########################
###  Generate QC table  ###
###########################

first_row <- qc_cntrow1(adsl, "TRT01P", row_text = "Analysis set: Safety")

tab1 <- qc_cat_row(adsl, "TRT01P", rowvar = "SEX", N_row = first_row$N_row)

tab_qc <- bind_rows(first_row$row1, tab1) %>% 
  mutate(across(everything(), ~replace(., is.na(.), "")))

###################
### Read in RTF ###
###################

tableid <- "tableid"
tab_rtf <- qc_rtf2df(tableid)

############################
### Compare two datasets ###
############################

qc_compare2xlsx(qc = tab_qc, rtf = tab_rtf, path = qc, filename = tableid)
```