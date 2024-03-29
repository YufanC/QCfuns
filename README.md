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
QCfuns::add_snippets()

### Header ###################################################################
# Original Reporting Effort:  ${1:Reporting effort}
# Program Name             :  ${2:Program name}
# R Version                :  `r R.Version()$version.string`
# Short Description        :  ${3:Description}
# Author                   :  ${4:Author}
# Date                     :  `r Sys.Date()`
# input                    :
# Output                   :  
# Remarks                  :

### Modification History #####################################################
#Rev       Modified By         Reporting Effort         Date      Description

### Prepping environment #####################################################
library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(QCfuns)

files.sources <- list.files(path = read_path(rptdrv, "qc"), 
                            pattern = "^qcf.*\\\.r", full.names = T)
sapply(files.sources, source)

### Derive QC dataset ########################################################

adsl <- read_sas(read_path(a_in, "adsl.sas7bdat")) %>% 
  mutate(across(where(is.character), zap_empty))
  
### Generate QC table ########################################################

first_row <- qc_cntrow1(adsl, "TRT01P", row_text = "Analysis set: Safety")

tab1 <- qc_cat_row(adsl, "TRT01P", rowvar = "SEX")

tab_qc <- bind_rows(first_row\$row1, tab1) %>% 
  mutate(across(everything(), ~replace(., is.na(.), "")))

### Read in RTF ##############################################################

tableid <- "${5:tableid}"
tab_rtf <- qc_rtf2df(tableid, path = opath[["PREPROD"]])

### Compare two datasets #####################################################

qc_comparedf(qc = tab_qc, rtf = tab_rtf, path = qc[["PDEV"]], 
             filename = tableid)
```