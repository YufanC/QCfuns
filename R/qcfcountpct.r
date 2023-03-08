##################################################################################################
# Original Reporting Effort:  FDC SCS
# Program Name             :  qcfcountpct.r
# R Version                :  4.0.5
# Short Description        :  R functions to compute count and percentage of character variables
# Author                   :  Yufan Chen
# Date                     :  Dec 09,2022
# Input                    :  input(R): input dataframe 
#                             colvar(R): column variable 
#                             row_text: row text 
#                             N_row(R): dataframe with N 
#                             subset: subset criteria
# Output                   :  
# Remarks                  :  Required packages: dplyr, tidyr
# Function Sample Call     :  countpct(input = adae, colvar = "TRT01PN", N_row = first_row,
#                                      row_text = "Subjects with 1 or more AEs", subset = "SEX == "Female")
# Modification History
#Rev        Modified By                   Reporting Effort         Date      Description
##################################################################################################

countpct <- function(input, colvar, row_text, N_row, subset = NULL){
  ### first N row
  row1 <- input %>%
    group_by(.data[[colvar]], .drop = F) %>% 
    summarise(n = ifelse(is.null(subset), n_distinct(USUBJID), n_distinct(USUBJID[eval(parse(text = subset))])), .groups = "drop") %>% 
    left_join(., N_row, by = colvar) %>% 
    mutate(pct = (round(n * 100 / N_trt, 1)),
           col = ifelse(pct == 0, "0", paste0(n, ' (', formatC(pct, format = "f", digits = 1), '%)')))
  
  row2 <- row1 %>% 
    select(.data[[colvar]], col) %>% 
    pivot_wider(names_from = .data[[colvar]],
                values_from = col)
  
  row3 <- cbind(row_text, row2)
  
  colnames(row3) <- c("row_text", levels(as.factor(pull(N_row, colvar))))
  
  return(row3)
  
}