#' Convert RTF files to Data frame
#'
#' Read in RTF files and convert it to data frame
#' @param filename the RTF file name you want to read in
#' @param path the folder path of RTF file
#' @return a data frame containing all data information from the RTF excluding titles and footnotes
#' @examples
#' \dontrun{
#' dat <- qc_rtf2df(filename = "tsidem01", path = opath[["PREPROD"]])
#' dat
#' }
#' @export
#' @import dplyr
#' @import stringr
#' @importFrom striprtf read_rtf
#' @importFrom utils read.table tail
qc_rtf2df <- function(filename, path = NULL){
  
  assertthat::assert_that(file.exists(file.path(path, paste0(str_to_lower(filename), ".rtf"))))
  
  a_rtf <- striprtf::read_rtf(file.path(path, paste0(str_to_lower(filename), ".rtf")))
  
  # Get the valuable lines 
  sep_count <- stringr::str_count(a_rtf, "\\|")
  # Remove \n
  a_rtf <- stringr::str_replace_all(a_rtf, "\\n", " ")
  
  ### rtfs created by SAS and R are different
  if (length(unique(sep_count)) == 1){
    ### for rtf created by R
    # Remove title row and path row
    a_rtf <- a_rtf[-c(1, length(a_rtf))]
    seq_white_space <- which(stringr::str_detect(a_rtf, "\\*\\|   \\|"))
    
    if (length(seq_white_space) > 2){
      start_row <- max(seq_white_space[seq_len(match(FALSE, diff(seq_white_space) == 1))])
    } else if(length(seq_white_space) == 0){
      start_row <- 1
    } else {
      start_row <- max(seq_white_space)
    }
    
    # Convert to dataframe
    dat0 <- read.table(text = a_rtf[start_row:length(a_rtf)], header = T, sep = "|", strip.white = T, colClasses = "character", quote = "", comment.char = "")
    
    # Remove the first and last column
    dat1 <- dat0[, c(-1, -ncol(dat0))]
    
    # Remove footnote which are the same in all columns
    footnote_rows <- apply(dat1, 1, function(row) all(stringr::str_remove(row, ".*:") == stringr::str_remove(row[1], ".*:"), stringr::str_length(stringr::str_remove(stringr::str_trim(row[1]), ".*:")) > 0))
    dat1 <- dat1[!footnote_rows, ]
    
  } else {
    ### for rtf created by SAS
    # Convert to dataframe
    dat0 <- read.table(text = a_rtf[sep_count == max(sep_count)], header = T, sep = "|", strip.white = T, colClasses = "character", quote = "", comment.char = "")
    
    dat1 <- dat0[, c(-1, -ncol(dat0))]
  }
  
  # Convert NA and "-" to ""
  dat2 <- dat1 %>% 
    mutate(across(1:length(dat1), ~replace(., is.na(.), "")),
           across(1:length(dat1), ~replace(., .=="-", "")))
  
  # Delete rows with all values equal to ""
  dat3 <- dat2[rowSums(dat2 == "") != ncol(dat2), ]
  rownames(dat3) <- NULL
  
  return(dat3)
  
}
