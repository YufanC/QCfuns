#' Get Maximum Digit
#'
#' Function to get maximum decimal place for continuous variable
#' @param data input dataframe 
#' @param var a continuous variable in the input dataframe
#' @param max_digit maximum number of digit to report
#' @examples 
#' adsl <- data.frame(
#'   USUBJID = 1:10,
#'   TRT01P = sample(c("A", "B", "C"), 10, replace = TRUE),
#'   AGE = sample(18:65, 10, replace = TRUE))
#'   
#' getmaxdigit(adsl, "AGE", 1)
#' @export
getmaxdigit <- function(data, var, max_digit){
  if (sum((data[[var]] %% 1) == 0, na.rm = T) != length(data[[var]][!is.na(data[[var]])])){
    value <- as.character(data[[var]])[(data[[var]] %% 1) != 0]
    maxdigit <- max(nchar(matrix(unlist(strsplit(value, ".", fixed = T)), ncol = 2, byrow = T)[, 2]), na.rm = T)
    
    if (maxdigit > max_digit){
      return(max_digit)
    } else {
      return(maxdigit)
    }
    
  } else {
    return(0)
  }
}