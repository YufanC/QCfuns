##################################################################################################
# Original Reporting Effort:  THR1003/DBR_CSR/RE_CSR
# Program Name             :  compare2xlsx
# R Version                :  4.1.0
# Short Description        :  R functions to compare two dataframe and output the result to xlsx
# Author                   :  Yufan Chen
# Date                     :  Oct 21,2021
# Input                    :  qc: the dataframe you create in QC
#                             rtf: the dataframe you created by read in rtf
#                             path: the path to put xlsx format compare result
#                             filename: tableid
#                             version: add version control by current date. Default = TURE
# Output                   :  
# Remarks                  :  Required packages: arsenal, openxlsx
# Function Sample Call     :  compare2xlsx(qc = tab_qc, rtf = tab_rtf, path = qc, filename = "tableid")
# Modification History
#Rev        Modified By                   Reporting Effort         Date      Description
##################################################################################################

compare2xlsx <- function(qc, rtf, path = qc, filename, version = TRUE) {
  
  # Get rid of the leading and trailing whitespace from the character string
  rtfdf <- as.data.frame(sapply(rtf, trimws))
  qcdf <- as.data.frame(sapply(qc, trimws))
  colnames(qcdf) <- colnames(rtfdf)
  
  row.names(rtfdf) <- NULL
  row.names(qcdf) <- NULL
  
  # Assign the two datasets to global environment
  assign("rtfdf", rtfdf, envir = .GlobalEnv)
  assign("qcdf", qcdf, envir = .GlobalEnv)
  
  result <- summary(comparedf(rtfdf, qcdf))[-9]
  result$diffs.table$values.x <- unlist(result$diffs.table$values.x)
  result$diffs.table$values.y <- unlist(result$diffs.table$values.y)
  
  # create workbook
  wb <- createWorkbook()
  
  #Iterate the same way as PavoDive, slightly different (creating an anonymous function inside Map())
  Map(function(data, nameofsheet){     
    
    addWorksheet(wb, nameofsheet)
    writeData(wb, nameofsheet, data)
    
  }, result, names(result))
  
  if (version == FALSE) {
    filename1 <- paste0(filename, ".xlsx")
  } else {
    filename1 <- paste0("qc", filename, "_", as.character(Sys.Date()), ".xlsx")
  }
  
  # Save workbook to excel file 
  saveWorkbook(wb, file = file.path(write_path(path), filename1), overwrite = TRUE)
  
  # output the summary of compare result in the log
  print(summary(comparedf(rtfdf, qcdf)))
  
}

