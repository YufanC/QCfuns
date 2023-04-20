#' Create Demographic Rows
#'
#' Function to create demographic rows 
#' 
#' \code{var_list} should be a dataframe with desired variable names as column names,
#' the label of each variable display in tables as the first row, and optionally,
#' the number of decimal places to keep in the output as the second row. If the 
#' label row of a variable is empty, the row text and N row for this variable
#' will be missing.
#' @inheritParams qc_cntrow1
#' @inheritParams qc_num_row
#' @param var_list variable list in demo table. Please see details.
#' @param drop_var_list Variable that don't keep all levels in the output
#' @param max_digit maximum number of decimal place to report
#' @return dataframe with demographic rows 
#' @examples 
#' age <- sample(18:65, 10, replace = TRUE)
#' 
#' adsl <- data.frame(
#'   USUBJID = 1:10,
#'   TRT01P = factor(sample(c("A", "B", "C"), 10, replace = TRUE)),
#'   AGE = age,
#'   AGEGR1 = ifelse(age < 45, "< 45", ">= 45"),
#'   SEX = factor(sample(c("Female", "Male"), 10, replace = TRUE)))
#'   
#' ### Create variable list based on DPS and assign labels to them.
#' ### Leave "" For variables that concatenate to the corresponding categorical variables
#' var_list <- data.frame(
#'   AGE = "Age, years",
#'   AGEGR1 = "",
#'   SEX = "Sex"
#'   )
#' 
#' tab1 <- qc_demo(adsl, colvar = "TRT01P", var_list = var_list)
#' tab1
#' @export
### create demo table 
qc_demo <- function(input, colvar = "TRT01P", stats_list = c("Mean_SD", "Median", "Range"), var_list, drop_var_list = NULL, max_digit = 2){
  
  tab_final <- data.frame()
  
  ### Create final dataset
  for (i in 1:length(colnames(var_list))){
    
    # Number of decimal place in the original data
    if (class(input[[colnames(var_list)[i]]]) %in% c("integer", "numeric")){
      digit0 <- ifelse(suppressWarnings(is.na(as.numeric(var_list[2, i]))), 
                       getmaxdigit(input, colnames(var_list)[i], max_digit),
                       as.numeric(var_list[2, i]))
    } 
    
    # Build rows for numeric and categorical variables separately
    if (class(input[[colnames(var_list)[i]]]) %in% c("integer", "numeric")){
      tab <- qc_num_row(input, colvar, colnames(var_list)[i], stats_list = stats_list, 
                        digit = digit0, row_text = var_list[1, i])
    } else if (class(input[[colnames(var_list)[i]]]) %in% c("factor", "character")){
      
      # combine group variable with its continuous counterpart
      if (colnames(var_list)[i] %in% colnames(var_list[which(str_trim(var_list[1, ]) == "")])) {
        tab <- qc_cat_row(input, colvar, colnames(var_list)[i], 
                          keep = !(colnames(var_list)[i] %in% drop_var_list)) %>% 
          filter(!row_text %in% c("Sex", "N"))
      } else {
        tab <- qc_cat_row(input, colvar, colnames(var_list)[i], 
                          keep = !(colnames(var_list)[i] %in% drop_var_list), 
                          row_text = var_list[1, i])
      }
     
    }
    # Append them together 
    tab_final <- bind_rows(tab_final, tab)
  }
  
  return(tab_final)
  
}
