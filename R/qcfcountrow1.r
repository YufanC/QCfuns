##################################################################################################
# Original Reporting Effort:  FDC SCS
# Program Name             :  qcfcountrow1.r
# R Version                :  4.0.5
# Short Description        :  R functions to compute count for analysis set row
# Author                   :  Yufan Chen
# Date                     :  Dec 09,2022
# Input                    :  input(R): input dataframe 
#                             colvar(R): column variable 
#                             popfl(R): population flag (="Y")
#                             row_text: row text 
# Output                   :  
# Remarks                  :  Required packages: dplyr, tidyr
# Function Sample Call     :  countpct(input = adae, colvar = "TRT01PN", popfl = SAFFL,
#                                      row_text = "Analysis set: Safety")
# Modification History
#Rev        Modified By                   Reporting Effort         Date      Description
##################################################################################################

countrow1 <- function(input, colvar, row_text = NULL, subset = NULL){
  first_row <- input %>% 
    filter(eval(parse(text = subset))) %>% 
    group_by(.data[[colvar]], .drop = F) %>% 
    count() %>% 
    mutate(N_trt = n) %>% 
    select(-n)
  
  first_row1 <- first_row %>% 
    distinct(.data[[colvar]], N_trt) %>% 
    pivot_wider(names_from = .data[[colvar]],
                values_from = N_trt)
  
  first_row2 <- cbind(row_text, first_row1)
  
  colnames(first_row2) <- c("row_text", levels(as.factor(pull(first_row, colvar))))
  
  return(list(first_row, first_row2))
}
