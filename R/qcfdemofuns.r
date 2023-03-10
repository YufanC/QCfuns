##################################################################################################
# Original Reporting Effort:  THR1003/DBR_CSR/RE_CSR
# Program Name             :  qcfdemofuns.r
# R Version                :  4.1.0
# Short Description        :  R functions to create demographic table
# Author                   :  Yufan Chen
# Date                     :  APR 30,2021
# Input                    :  cat_row(), num_row() and demo: rows for categorical and continuous variables
#                             input:         input dataset
#                             colvar:        column variable
#                             rowvar:        row variable
#                             N_row:         dataframe with N
#                             keep:          if = TRUE, keep all levels
#                             digit:         the original digit of the continuous variable
#                             var_list:      variable list in demo table
#                             con_var_list:  List of continuous variables that concatenate to the corresponding categorical variables
#                             drop_var_list: Variable that don't keep all levels in the output
#                             
# Output                   :  
# Remarks                  :  Required packages: dplyr, tidyr
# function Sample Call     :

# Modification History
#Rev        Modified By                   Reporting Effort         Date      Description
##################################################################################################

### Categorical variable rows
cat_row <- function(data, colvar, rowvar, N_row, keep = TRUE){
  # Calculate count and percentage
  tab1 <- data %>% 
    filter(!is.na(.data[[rowvar]])) %>% 
    group_by(.data[[colvar]], .data[[rowvar]], .drop = !keep) %>% 
    summarise(n = n_distinct(USUBJID), .groups = "drop") %>%
    left_join(N_row, by = colvar) %>% 
    mutate(pct = (round(n * 100 / N_trt, 1)),
           col = ifelse(pct == 0, "0", paste0(n, ' (', formatC(pct, format = "f", digits = 1), '%)')))
  
  # Transpose data
  tab2 <- tab1 %>%
    select(.data[[rowvar]], .data[[colvar]], col) %>%
    pivot_wider(names_from = .data[[colvar]], 
                values_from = col)
  
  # Total counts row
  tab3 <- tab1 %>% 
    distinct(.data[[colvar]], N_trt) %>% 
    pivot_wider(names_from = .data[[colvar]],
                values_from = N_trt)
  
  tab4 <- cbind("N", tab3)
  
  colnames(tab2) <- c("row_text", levels(as.factor(pull(data, colvar))))
  colnames(tab4) <- c("row_text", levels(as.factor(pull(data, colvar))))
  
  tab5 <- rbind(tab4, tab2)
  
  return(tab5)
}

### Numeric variable rows
num_row <- function(data, colvar, rowvar, digit){
  # Calculate mean, median and range
  tab1 <- data %>% 
    filter(!is.na(.data[[rowvar]])) %>% 
    group_by(.data[[colvar]], .drop = F) %>% 
    summarise(Mean = mean(.data[[rowvar]], na.rm = T), 
              SD = sd(.data[[rowvar]], na.rm = T), 
              Median = median(.data[[rowvar]], na.rm = T), 
              Min = min(.data[[rowvar]], na.rm = T), 
              Max = max(.data[[rowvar]], na.rm = T), 
              N_trt = n(),
              .groups = "drop") %>% 
    arrange(.data[[colvar]]) %>% 
    mutate('Mean (SD)' = ifelse(!is.na(SD), 
                                paste0(formatC(Mean, format = "f", digits = (digit + 1)), 
                                       ' (', formatC(SD, format = "f", digits = (digit + 2)), ')'), 
                                paste0(formatC(Mean, format = "f", digits = (digit + 1)), 
                                       ' (-)')),
           Median = formatC(Median, format = "f", digits = (digit + 1)),
           Range = paste0("(", formatC(Min, format = "f", digits = digit), "; ", 
                          formatC(Max, format = "f", digits = digit), ")"))
  
  tab2 <- tab1 %>%
    select(.data[[colvar]], 'Mean (SD)', Median, Range)
  
  # Transpose dataset
  tab3 <- as.data.frame(t(tab2), row.names = F)
  
  colnames(tab3) <- tab3 %>% 
    slice(1) %>% 
    unlist()
  
  tab3 <- tab3 %>% 
    slice(-1)
  
  tab4 <- cbind(colnames(tab2)[-1], tab3)
  
  # Total counts row
  tab5 <- tab1 %>% 
    distinct(.data[[colvar]], N_trt) %>% 
    pivot_wider(names_from = .data[[colvar]],
                values_from = N_trt)
  
  tab6 <- cbind("N", tab5)
  
  colnames(tab4) <- c("row_text", levels(as.factor(pull(data, colvar))))
  colnames(tab6) <- c("row_text", levels(as.factor(pull(data, colvar))))
  
  tab7 <- rbind(tab6, tab4)
  
  return(tab7)
}

### create demo table 
demo <- function(input, colvar, N_row, var_list, con_var_list, drop_var_list){
  
  tab_final <- data.frame()
  
  ### Create final dataset
  for (i in 1:length(var_list)){
    
    # Number of decimal place in the original data
    if (class(input[[var_list[i]]]) == "numeric"){
      digit0 <- Getdigit(input, var_list[i])
    } 
    
    # Build rows for numeric and categorical variables separately
    if (class(input[[var_list[i]]]) == "numeric"){
      tab <- num_row(input, colvar, var_list[i], digit0)
    } else if (class(input[[var_list[i]]]) %in% c("factor", "character")){
      tab <- cat_row(input, colvar, var_list[i], N_row, keep = !(var_list[i] %in% drop_var_list))
    }
    # Append them together, combine group variable with its continuous counterpart
    if (var_list[i] %in% con_var_list){
      tab <- tab[-1, ]
      tab_final <- bind_rows(tab_final, tab)
    } else {
      tab_final <- bind_rows(tab_final, data.frame(row_text = names(var_list)[i]), tab)
    }
  }
  
  return(tab_final)
  
}