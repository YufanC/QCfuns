##################################################################################################
# Original Reporting Effort:  THR1003/DBR_CSR/RE_CSR
# Program Name             :  qcfchg.r
# R Version                :  4.0.5
# Short Description        :  R functions to create change from basline over time table,
#                             table will be ordered by param, treatment group and visit
# Author                   :  Yufan Chen
# Date                     :  Mar 07,2023
# Input                    :  cat_row(), num_row() and demo: rows for categorical and continuous variables
#                             input:         input dataset
#                             val:           measurement variable
#                             chg:           change from baseline measurement variable
#                             param:         parameter variable
#                             trt:           treatment group variable
#                             visit:         visit variable
#                             keep:          if = TRUE, keep all levels
#                             
# Output                   :  
# Remarks                  :  Required packages: dplyr, tidyr
# function Sample Call     :

# Modification History
#Rev        Modified By                   Reporting Effort         Date      Description
##################################################################################################

chgfb <- function(input, val, chg, param, trt, visit, keep = FALSE){
  # Measured Value
  tab1 <- input %>% 
    filter(!is.na(.data[[val]])) %>% 
    # Get the original decimal place
    mutate(digit0 = Getdigit(., val)) %>% 
    group_by(.data[[param]], .data[[trt]], .data[[visit]], .drop = !keep) %>% 
    summarise(N      = ifelse(sum(!is.na(.data[[val]])) == 0, NaN, sum(!is.na(.data[[val]]))),
              Mean   = formatC(mean(.data[[val]], na.rm = T), format = "f", digits = max(digit0) + 1), 
              SD     = formatC(sd(.data[[val]], na.rm = T), format = "f", digits = max(digit0) + 2), 
              Median = formatC(median(.data[[val]], na.rm = T), format = "f", digits = max(digit0) + 1), 
              Min    = formatC(min(.data[[val]], na.rm = T), format = "f", digits = max(digit0)), 
              Max    = formatC(max(.data[[val]], na.rm = T), format = "f", digits = max(digit0)),
              CV     = formatC(sd(.data[[val]], na.rm = T)/mean(.data[[val]], na.rm = T)*100, format = "f", digits = max(digit0) + 1),
              .groups = "drop") %>% 
    mutate(across(everything(), ~replace(., trimws(.)=="NaN"|trimws(.)=="NA"|trimws(.)=="Inf"|trimws(.)=="-Inf", NA)))
  
  # Percent Change from Baseline
  tab2 <- input %>% 
    filter(!is.na(.data[[chg]])) %>% 
    # Get the original decimal place
    mutate(digit0 = Getdigit(., chg)) %>%
    group_by(.data[[param]], .data[[trt]], .data[[visit]], .drop = !keep) %>% 
    summarise(N      = ifelse(sum(!is.na(.data[[chg]])) == 0, NaN, sum(!is.na(.data[[chg]]))),
              Mean   = formatC(mean(.data[[chg]], na.rm = T), format = "f", digits = max(digit0) + 1), 
              SD     = formatC(sd(.data[[chg]], na.rm = T), format = "f", digits = max(digit0) + 2), 
              Median = formatC(median(.data[[chg]], na.rm = T), format = "f", digits = max(digit0) + 1), 
              Min    = formatC(min(.data[[chg]], na.rm = T), format = "f", digits = max(digit0)), 
              Max    = formatC(max(.data[[chg]], na.rm = T), format = "f", digits = max(digit0)),
              CV     = formatC(sd(.data[[chg]], na.rm = T)/mean(.data[[chg]], na.rm = T)*100, format = "f", digits = max(digit0) + 1),
              .groups = "drop") %>% 
    mutate(across(everything(), ~replace(., trimws(.)=="NaN"|trimws(.)=="NA"|trimws(.)=="Inf"|trimws(.)=="-Inf", NA))) %>% 
    ungroup()
  
  tab3 <- tab1 %>% 
    left_join(., tab2, by = c(param, trt, visit))
  
  ### Reorder the rows and combine them together
  param_row <- distinct(select(input, .data[[param]])) %>% 
    arrange(.data[[param]]) %>% 
    mutate(ord_param = 1:n(),
           ord_trt = 0,
           ord_visit = 0)
  
  trt_row <- distinct(select(input, .data[[param]], .data[[trt]])) %>% 
    arrange(.data[[trt]]) %>%
    left_join(param_row, by = param) %>% 
    mutate(ord_trt = 1:n(),
           ord_visit = 0)
  
  visit_row <- tab3 %>% 
    arrange(.data[[param]], .data[[trt]], .data[[visit]]) %>% 
    left_join(trt_row, by = c(param, trt)) %>% 
    group_by(.data[[trt]], .drop = F) %>% 
    mutate(ord_visit = 1:n()) %>% 
    ungroup()
  
  tab_combine <- bind_rows(param_row, trt_row, visit_row) %>% 
    arrange(ord_param, ord_trt, ord_visit) %>% 
    mutate(across(c(.data[[param]], .data[[trt]], .data[[visit]]), ~as.character(.)),
           row_text = if_else(ord_trt == 0, .data[[param]], 
                              if_else(ord_visit == 0, .data[[trt]], .data[[visit]]))) %>% 
    select(row_text, everything(), -c(.data[[param]], .data[[trt]], .data[[visit]], ord_param, ord_trt, ord_visit))
  
  return(tab_combine)
}