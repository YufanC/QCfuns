##################################################################################################
# Original Reporting Effort:  THR1003/DBR_CSR/RE_CSR
# Program Name             :  rtf2df
# R Version                :  4.1.0
# Short Description        :  R functions to read in rtf and convert it to dataframe
# Author                   :  Yufan Chen
# Date                     :  Oct 21,2021
# Input                    :  filename: the rtf filename you want to read in
#                             level: Choose from "PDEV", "PREPROD" or "PROD". If left blank,
#                                    find the rtf file in output folder on different levels
#                                    in sequence of "PDEV", "PREPROD" and "PROD"
# Output                   :  
# Remarks                  :  Required packages: dplyr, striprtf
# Function Sample Call     :  rtf2df("tsidem01", "PREPROD")

# Modification History
#Rev        Modified By                   Reporting Effort         Date      Description
##################################################################################################

#' Convert rtf to dataframe
#'
#' read in rtf and convert it to dataframe
#' @param filename the rtf filename you want to read in
#' @param level Choose from "PDEV", "PREPROD" or "PROD". If left blank, find the rtf file in output folder on different levels in sequence of "PDEV", "PREPROD" and "PROD"
#' @return a dataframe containing all the information from the rtf
#' @examples 
#' dat <- rtf2df("tsidem01", "PREPROD");
#' @export

rtf2df <- function(filename, level = NULL){
  
  if (is.null(level)) {
    a_rtf <- striprtf::read_rtf(read_path(opath, paste0(str_to_lower(filename), ".rtf")))
  } else {
    a_rtf <- striprtf::read_rtf(read_path(opath[level], paste0(str_to_lower(filename), ".rtf")))
  }
  
  # Get the valuable lines 
  sep_count <- str_count(a_rtf, "\\|")
  
  # Remove \n
  a_rtf <- str_replace_all(a_rtf, "\\n", " ")
  
  # Convert to dataframe
  dat0 <- read.table(text = a_rtf[sep_count == max(sep_count)], header = T, sep = "|", strip.white = T, colClasses = "character", quote = "")
  
  dat1 <- dat0[, c(-1, -ncol(dat0))]
  
  # Convert NA and "-" to ""
  dat2 <- dat1 %>% 
    mutate(across(1:length(dat1), ~replace(., is.na(.), "")),
           across(1:length(dat1), ~replace(., .=="-", "")))
  
  # Delete rows with all values equal to ""
  dat3 <- dat2[rowSums(dat2 == "") != ncol(dat2), ]
  
  return(dat3)
  
}
