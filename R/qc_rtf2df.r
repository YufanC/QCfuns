#' Convert RTF files to Data frame
#'
#' Read in RTF files and convert it to data frame
#' @param filename the RTF file name you want to read in
#' @param level Choose from "PDEV", "PREPROD" or "PROD". The default is to find the RTF file in output folder on different levels in sequence of "PDEV", "PREPROD" and "PROD"
#' @return a data frame containing all data information from the RTF excluding titles and footnotes
#' @examplesIf exists("opath") 
#' dat <- qc_rtf2df("tsidem01", "PREPROD")
#' dat
#' @export
#' @import dplyr
#' @import stringr
#' @importFrom striprtf read_rtf
qc_rtf2df <- function(filename = "tsidem01", level = NULL){
  
  if (is.null(level)) {
    a_rtf <- striprtf::read_rtf(envsetup::read_path(opath, paste0(str_to_lower(filename), ".rtf")))
  } else {
    a_rtf <- striprtf::read_rtf(envsetup::read_path(opath[level], paste0(str_to_lower(filename), ".rtf")))
  }
  
  # Get the valuable lines 
  sep_count <- stringr::str_count(a_rtf, "\\|")
  
  # Remove \n
  a_rtf <- stringr::str_replace_all(a_rtf, "\\n", " ")
  
  # Convert to dataframe
  dat0 <- read.table(text = a_rtf[sep_count == max(sep_count)], header = T, sep = "|", strip.white = T, colClasses = "character", quote = "")
  
  dat1 <- dat0[, c(-1, -ncol(dat0))]
  
  # Convert NA and "-" to ""
  dat2 <- dat1 %>% 
    mutate(across(1:length(dat1), ~replace(., is.na(.), "")),
           across(1:length(dat1), ~replace(., .=="-", "")))
  
  # Delte rows with the first column equal to ""
  dat3 <- dat2[dat2[[1]] != "", ]
  rownames(dat3) <- NULL
  
  return(dat3)
  
}
