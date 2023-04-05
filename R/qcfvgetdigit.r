#' Get Digit
#'
#' Function to get the decimal place of a numeric column 
#' @param var input dataframe 
#' @param max_digit maximum number of digit to report
#' @param criteria values with number of decimal places over the criteria will be rounded to the number of decimal places that equals to the criteria. Default = 10
#' @examples 
#' age <- sample(18:65, 10, replace = TRUE)
#' 
#' getdigit(age , 1)
#' @export
getdigit <- function(var, max_digit, criteria = 10) {
  
  # Convert to character
  char1 <- unlist(strsplit(trimws(formatC(var, digits = criteria)), ".", fixed = T))
  
  # Get length the digit part
  digit <- ifelse(length(char1) == 2, nchar(char1[2]), 0)
  
  if (digit > max_digit){
    return(max_digit)
  } else {
    return(digit)
  }
}

