#' Create change from basline over time table
#'
#' create change from basline over time table, table can be ordered by param, treatment group and visit
#' @param input input dataframe 
#' @param val measurement variable
#' @param chg change from baseline measurement variable
#' @param rowvar row variable (can be a set of two variables such as c("TRT01P", "AVISIT") or three variables such as c("PARAM", "TRT01P", "AVISIT"))
#' @param stats_list stats variables to display. Accepted values are c("N", "Mean", "SD", "Median", "Min", "Max", "CV", "Base_mean") and the order of variables matters
#' @param max_digit largest number of digit to display 
#' @param keep if = TRUE, keep all levels. Default = FALSE
#' @return change from baseline table
#' @examples 
#' chgfb(adlb, "AVAL", "CHG", rowvar = c("PARAM", "TRT01P", "AVISIT"), max_digit = 3, keep = FALSE)
#' @export
chgfb <- function(input, val = "AVAL", chg = "CHG", rowvar = c("PARAM", "TRT01P", "AVISIT"), stats_list = c("N", "Mean", "SD", "Median", "Min", "Max"), max_digit = 2, keep = TRUE){
  
  if(length(rowvar) == 2){
    
    # Measured Value
    tab1 <- input %>% 
      filter(!is.na(.data[[val]])) %>% 
      # Get the original decimal place
      rowwise() %>% 
      mutate(digit0 = ifelse(getdigit(.data[[val]]) >= max_digit, 2, getdigit(.data[[val]]))) %>% 
      ungroup() %>% 
      group_by(.data[[rowvar[1]]], .data[[rowvar[2]]], .drop = !keep) %>% 
      summarise(N      = ifelse(sum(!is.na(.data[[val]])) == 0, 0, sum(!is.na(.data[[val]]))),
                Mean   = formatC(mean(.data[[val]], na.rm = T), format = "f", digits = max(digit0) + 1), 
                SD     = formatC(sd(.data[[val]], na.rm = T), format = "f", digits = max(digit0) + 2), 
                Median = formatC(median(.data[[val]], na.rm = T), format = "f", digits = max(digit0) + 1), 
                Min    = formatC(min(.data[[val]], na.rm = T), format = "f", digits = max(digit0)), 
                Max    = formatC(max(.data[[val]], na.rm = T), format = "f", digits = max(digit0)),
                CV     = formatC(sd(.data[[val]], na.rm = T)/mean(.data[[val]], na.rm = T), format = "f", digits = max(digit0) + 1),
                .groups = "drop") %>% 
      mutate(across(everything(), ~replace(., trimws(.)=="NaN"|trimws(.)=="NA"|trimws(.)=="Inf"|trimws(.)=="-Inf", NA))) %>% 
      select(rowvar[1], rowvar[2], stats_list[!stats_list %in% "Base_mean"])
    
    # Change from Baseline
    tab2 <- input %>% 
      filter(!is.na(.data[[chg]])) %>% 
      # Get the original decimal place
      rowwise() %>% 
      mutate(digit0 = ifelse(chg != "PCHG", 
                             ifelse(getdigit(.data[[chg]]) >= max_digit, 2, getdigit(.data[[chg]])),
                             ifelse(getdigit(.data[[val]]) >= max_digit, 2, getdigit(.data[[val]])))) %>% 
      ungroup() %>% 
      group_by(.data[[rowvar[1]]], .data[[rowvar[2]]], .drop = !keep) %>% 
      summarise(Base_mean  = formatC(mean(BASE, na.rm = T), format = "f", digits = max(digit0) + 1), 
                N      = ifelse(sum(!is.na(.data[[chg]])) == 0, 0, sum(!is.na(.data[[chg]]))),
                Mean   = formatC(mean(.data[[chg]], na.rm = T), format = "f", digits = max(digit0) + 1), 
                SD     = formatC(sd(.data[[chg]], na.rm = T), format = "f", digits = max(digit0) + 2), 
                Median = formatC(median(.data[[chg]], na.rm = T), format = "f", digits = max(digit0) + 1), 
                Min    = formatC(min(.data[[chg]], na.rm = T), format = "f", digits = max(digit0)), 
                Max    = formatC(max(.data[[chg]], na.rm = T), format = "f", digits = max(digit0)),
                CV     = formatC(sd(.data[[chg]], na.rm = T)/mean(.data[[chg]], na.rm = T), format = "f", digits = max(digit0) + 1),
                .groups = "drop") %>% 
      mutate(across(everything(), ~replace(., trimws(.)=="NaN"|trimws(.)=="NA"|trimws(.)=="Inf"|trimws(.)=="-Inf", NA))) %>% 
      select(rowvar[1], rowvar[2], stats_list)
    
    tab3 <- tab1 %>% 
      left_join(., tab2, by = c(rowvar[1], rowvar[2]))
    
    ### Reorder the rows and combine them together
    param_row <- input %>% 
      group_by(.data[[rowvar[1]]], .drop = !keep) %>%
      summarise(.groups = "drop") %>% 
      arrange(.data[[rowvar[1]]]) %>% 
      mutate(ord_param = 1:n(),
             ord_visit = 0)
    
    visit_row <- tab3 %>% 
      arrange(.data[[rowvar[1]]], .data[[rowvar[2]]]) %>% 
      left_join(param_row, by = rowvar[1]) %>% 
      group_by(.data[[rowvar[1]]], .drop = F) %>% 
      mutate(ord_visit = 1:n()) %>% 
      ungroup()
    
    tab_combine <- bind_rows(param_row, visit_row) %>% 
      arrange(ord_param, ord_visit) %>% 
      mutate(across(c(.data[[rowvar[1]]], .data[[rowvar[2]]]), ~as.character(.)),
             row_text = if_else(ord_visit == 0, .data[[rowvar[1]]], .data[[rowvar[2]]])) %>% 
      select(row_text, everything(), -c(.data[[rowvar[1]]], .data[[rowvar[2]]], ord_param, ord_visit))
    
  } else if (length(rowvar) == 3) {
    
    # Measured Value
    tab1 <- input %>% 
      filter(!is.na(.data[[val]])) %>% 
      # Get the original decimal place
      rowwise() %>% 
      mutate(digit0 = ifelse(getdigit(.data[[val]]) >= max_digit, 2, getdigit(.data[[val]]))) %>% 
      ungroup() %>% 
      group_by(.data[[rowvar[1]]], .data[[rowvar[2]]], .data[[rowvar[3]]], .drop = !keep) %>% 
      summarise(N      = ifelse(sum(!is.na(.data[[val]])) == 0, 0, sum(!is.na(.data[[val]]))),
                Mean   = formatC(mean(.data[[val]], na.rm = T), format = "f", digits = max(digit0) + 1), 
                SD     = formatC(sd(.data[[val]], na.rm = T), format = "f", digits = max(digit0) + 2), 
                Median = formatC(median(.data[[val]], na.rm = T), format = "f", digits = max(digit0) + 1), 
                Min    = formatC(min(.data[[val]], na.rm = T), format = "f", digits = max(digit0)), 
                Max    = formatC(max(.data[[val]], na.rm = T), format = "f", digits = max(digit0)),
                CV     = formatC(sd(.data[[val]], na.rm = T)/mean(.data[[val]], na.rm = T), format = "f", digits = max(digit0) + 1),
                .groups = "drop") %>% 
      mutate(across(everything(), ~replace(., trimws(.)=="NaN"|trimws(.)=="NA"|trimws(.)=="Inf"|trimws(.)=="-Inf", NA))) %>% 
      select(rowvar[1], rowvar[2], rowvar[3], stats_list[!stats_list %in% "Base_mean"])
    
    # Change from Baseline
    tab2 <- input %>% 
      filter(!is.na(.data[[chg]])) %>% 
      # Get the original decimal place
      rowwise() %>% 
      mutate(digit0 = ifelse(chg != "PCHG", 
                             ifelse(getdigit(.data[[chg]]) >= max_digit, 2, getdigit(.data[[chg]])),
                             ifelse(getdigit(.data[[val]]) >= max_digit, 2, getdigit(.data[[val]])))) %>% 
      ungroup() %>% 
      group_by(.data[[rowvar[1]]], .data[[rowvar[2]]], .data[[rowvar[3]]], .drop = !keep) %>% 
      summarise(Base_mean  = formatC(mean(BASE, na.rm = T), format = "f", digits = max(digit0) + 1), 
                N      = ifelse(sum(!is.na(.data[[chg]])) == 0, 0, sum(!is.na(.data[[chg]]))),
                Mean   = formatC(mean(.data[[chg]], na.rm = T), format = "f", digits = max(digit0) + 1), 
                SD     = formatC(sd(.data[[chg]], na.rm = T), format = "f", digits = max(digit0) + 2), 
                Median = formatC(median(.data[[chg]], na.rm = T), format = "f", digits = max(digit0) + 1), 
                Min    = formatC(min(.data[[chg]], na.rm = T), format = "f", digits = max(digit0)), 
                Max    = formatC(max(.data[[chg]], na.rm = T), format = "f", digits = max(digit0)),
                CV     = formatC(sd(.data[[chg]], na.rm = T)/mean(.data[[chg]], na.rm = T), format = "f", digits = max(digit0) + 1),
                .groups = "drop") %>% 
      mutate(across(everything(), ~replace(., trimws(.)=="NaN"|trimws(.)=="NA"|trimws(.)=="Inf"|trimws(.)=="-Inf", NA))) %>% 
      select(rowvar[1], rowvar[2], rowvar[3], stats_list)
    
    tab3 <- tab1 %>% 
      left_join(., tab2, by = c(rowvar[1], rowvar[2], rowvar[3]))
    
    ### Reorder the rows and combine them together
    param_row <- input %>% 
      group_by(.data[[rowvar[1]]], .drop = !keep) %>%
      summarise(.groups = "drop") %>% 
      arrange(.data[[rowvar[1]]]) %>% 
      mutate(ord_param = 1:n(),
             ord_trt = 0,
             ord_visit = 0)
    
    trt_row <- input %>% 
      group_by(.data[[rowvar[1]]], .data[[rowvar[2]]], .drop = !keep) %>%
      summarise(.groups = "drop") %>% 
      arrange(.data[[rowvar[2]]]) %>%
      left_join(param_row, by = rowvar[1]) %>%
      group_by(.data[[rowvar[1]]]) %>% 
      mutate(ord_trt = 1:n(),
             ord_visit = 0) %>% 
      ungroup()
    
    visit_row <- tab3 %>% 
      arrange(.data[[rowvar[1]]], .data[[rowvar[2]]], .data[[rowvar[3]]]) %>% 
      left_join(trt_row, by = c(rowvar[1], rowvar[2])) %>% 
      group_by(.data[[rowvar[1]]], .data[[rowvar[2]]], .drop = F) %>% 
      mutate(ord_visit = 1:n()) %>% 
      ungroup()
    
    tab_combine <- bind_rows(param_row, trt_row, visit_row) %>% 
      arrange(ord_param, ord_trt, ord_visit) %>% 
      mutate(across(c(.data[[rowvar[1]]], .data[[rowvar[2]]], .data[[rowvar[3]]]), ~as.character(.)),
             row_text = if_else(ord_trt == 0, .data[[rowvar[1]]], 
                                if_else(ord_visit == 0, .data[[rowvar[2]]], .data[[rowvar[3]]]))) %>% 
      select(row_text, everything(), -c(.data[[rowvar[1]]], .data[[rowvar[2]]], .data[[rowvar[3]]], ord_param, ord_trt, ord_visit))
    
  } else {
    warning("rowvar has to have 2 or 3 elements")
  }
  
  return(tab_combine)
}