#' Create change from basline over time table
#'
#' create change from basline over time table, table can be ordered by param, treatment group and visit
#' 
#' The number of decimal places the results will keep depends on a variable "digit" 
#' in the input dataset. If digit variable does not exist in input dataset, the number
#' of decimal places will depend on the number of decimal places of AVAL/CHG
#' @param input input dataframe 
#' @param val measurement variable
#' @param chg change from baseline measurement variable
#' @param rowvar row variable (can be a set of two variables such as 
#' \code{c("TRT01P", "AVISIT")} or three variables such as \code{c("PARAM", "TRT01P", "AVISIT")})
#' @param stats_list stats variables to display. Accepted values are 
#' \code{c("N", "Mean", "SD", "Median", "Min", "Max", "CV", "Base_mean")} and the order of variables matters
#' @param max_digit largest number of digit to display 
#' @param keep if = TRUE, keep all levels. Default = TRUE
#' @return change from baseline table
#' @examples 
#' adsl <- data.frame(
#'   USUBJID = 1:10,
#'   TRT01P = factor(sample(c("A", "B", "C"), 10, replace = TRUE)))
#'   
#' param <- data.frame(PARAM = c("Test1", "Test2"))
#' visit <- data.frame(AVISIT = c("Baseline", "Visit1", "Visit2"))
#' 
#' adlb0 <- merge(adsl, param)
#' adlb0$BASE <- sample(1:100, 20)
#' 
#' adlb <- merge(adlb0, visit)
#' adlb$AVAL <- sample(1:100, 60)
#' adlb$CHG <- ifelse(adlb$AVISIT == "Baseline", NA, adlb$AVAL - adlb$BASE)
#' adlb$digit <- ifelse(adlb$PARAM == "Test1", 0, 2)
#' 
#' tab1 <- qc_chgfb(adlb, "AVAL", "CHG", rowvar = c("PARAM", "TRT01P", "AVISIT"), 
#'                  max_digit = 0, keep = FALSE)
#' tab1
#' @export
qc_chgfb <- function(input, val = "AVAL", chg = "CHG", rowvar = c("PARAM", "TRT01P", "AVISIT"), stats_list = c("N", "Mean", "SD", "Median", "Min", "Max"), max_digit = 2, keep = TRUE){
  
  assertthat::assert_that(not_empty(input))
  assertthat::assert_that(assertthat::has_name(input, c(val, chg, rowvar)))
  assertthat::assert_that(stats_accept(stats_list))
  
  ### Get maxium digit
  tab_digit <- input %>% 
    filter(!is.na(.data[[val]])) %>% 
    # Get the original decimal place
    rowwise() %>% 
    mutate(digit0 = ifelse(exists("digit", input), digit, NA),
           digit0 = ifelse(!is.na(digit0), digit0,
                           getdigit(.data[[val]], max_digit = max_digit))) %>% 
    ungroup() %>% 
    group_by(.data[[rowvar[1]]]) %>% 
    summarise(digit0 = max(digit0)) %>% 
    select(all_of(rowvar[1]), digit0) %>% 
    ungroup()
  
  if(length(rowvar) == 2){
    
    # Measured Value
    tab1 <- input %>% 
      filter(!is.na(.data[[val]])) %>% 
      left_join(tab_digit, by = rowvar[1]) %>%
      group_by(.data[[rowvar[1]]], .data[[rowvar[2]]], .drop = !keep) %>% 
      summarise(N      = ifelse(sum(!is.na(.data[[val]])) == 0, 0, sum(!is.na(.data[[val]]))),
                Mean   = formatC(round_sas(mean(.data[[val]], na.rm = T), max(digit0) + 1), format = "f", digits = max(digit0) + 1), 
                SD     = formatC(round_sas(sd(.data[[val]], na.rm = T), max(digit0) + 2), format = "f", digits = max(digit0) + 2), 
                Median = formatC(round_sas(median(.data[[val]], na.rm = T), max(digit0) + 1), format = "f", digits = max(digit0) + 1), 
                Min    = formatC(round_sas(min(.data[[val]], na.rm = T), max(digit0)), format = "f", digits = max(digit0)), 
                Max    = formatC(round_sas(max(.data[[val]], na.rm = T), max(digit0)), format = "f", digits = max(digit0)),
                CV     = formatC(round_sas(sd(.data[[val]], na.rm = T)/mean(.data[[val]], na.rm = T), max(digit0) + 1), format = "f", digits = max(digit0) + 1),
                .groups = "drop") %>% 
      mutate(across(everything(), ~replace(., trimws(.)=="NaN"|trimws(.)=="NA"|trimws(.)=="Inf"|trimws(.)=="-Inf", NA))) %>% 
      select(all_of(c(rowvar, stats_list[!stats_list %in% "Base_mean"])))
    
    # Change from Baseline
    tab2 <- input %>% 
      filter(!is.na(.data[[chg]])) %>% 
      left_join(tab_digit, by = rowvar[1]) %>%
      group_by(.data[[rowvar[1]]], .data[[rowvar[2]]], .drop = !keep) %>% 
      summarise(Base_mean  = formatC(round_sas(mean(BASE, na.rm = T), max(digit0) + 1), format = "f", digits = max(digit0) + 1), 
                N      = ifelse(sum(!is.na(.data[[chg]])) == 0, 0, sum(!is.na(.data[[chg]]))),
                Mean   = formatC(round_sas(mean(.data[[chg]], na.rm = T), max(digit0) + 1), format = "f", digits = max(digit0) + 1), 
                SD     = formatC(round_sas(sd(.data[[chg]], na.rm = T), max(digit0) + 2), format = "f", digits = max(digit0) + 2), 
                Median = formatC(round_sas(median(.data[[chg]], na.rm = T), max(digit0) + 1), format = "f", digits = max(digit0) + 1), 
                Min    = formatC(round_sas(min(.data[[chg]], na.rm = T), max(digit0)), format = "f", digits = max(digit0)), 
                Max    = formatC(round_sas(max(.data[[chg]], na.rm = T), max(digit0)), format = "f", digits = max(digit0)),
                CV     = formatC(round_sas(sd(.data[[chg]], na.rm = T)/mean(.data[[chg]], na.rm = T), max(digit0) + 1), format = "f", digits = max(digit0) + 1),
                .groups = "drop") %>% 
      mutate(across(everything(), ~replace(., trimws(.)=="NaN"|trimws(.)=="NA"|trimws(.)=="Inf"|trimws(.)=="-Inf", NA))) %>% 
      select(all_of(c(rowvar, stats_list)))
    
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
             row_text = if_else(ord_visit == 0, .data[[rowvar[1]]], .data[[rowvar[2]]]),
             N.y = ifelse(row_text == "Baseline", NA, N.y)) %>% 
      select(row_text, everything(), -all_of(rowvar), -c(ord_param, ord_visit))
    
  } else if (length(rowvar) == 3) {
    
    # Measured Value
    tab1 <- input %>% 
      filter(!is.na(.data[[val]])) %>% 
      left_join(tab_digit, by = rowvar[1]) %>%
      group_by(.data[[rowvar[1]]], .data[[rowvar[2]]], .data[[rowvar[3]]], .drop = !keep) %>% 
      summarise(N      = ifelse(sum(!is.na(.data[[val]])) == 0, 0, sum(!is.na(.data[[val]]))),
                Mean   = formatC(round_sas(mean(.data[[val]], na.rm = T), max(digit0) + 1), format = "f", digits = max(digit0) + 1), 
                SD     = formatC(round_sas(sd(.data[[val]], na.rm = T), max(digit0) + 2), format = "f", digits = max(digit0) + 2), 
                Median = formatC(round_sas(median(.data[[val]], na.rm = T), max(digit0) + 1), format = "f", digits = max(digit0) + 1), 
                Min    = formatC(round_sas(min(.data[[val]], na.rm = T), max(digit0)), format = "f", digits = max(digit0)), 
                Max    = formatC(round_sas(max(.data[[val]], na.rm = T), max(digit0)), format = "f", digits = max(digit0)),
                CV     = formatC(round_sas(sd(.data[[val]], na.rm = T)/mean(.data[[val]], na.rm = T), max(digit0) + 1), format = "f", digits = max(digit0) + 1),
                .groups = "drop") %>% 
      mutate(across(everything(), ~replace(., trimws(.)=="NaN"|trimws(.)=="NA"|trimws(.)=="Inf"|trimws(.)=="-Inf", NA))) %>% 
      select(all_of(c(rowvar, stats_list[!stats_list %in% "Base_mean"])))
    
    # Change from Baseline
    tab2 <- input %>% 
      filter(!is.na(.data[[chg]])) %>% 
      left_join(tab_digit, by = rowvar[1]) %>%
      group_by(.data[[rowvar[1]]], .data[[rowvar[2]]], .data[[rowvar[3]]], .drop = !keep) %>% 
      summarise(Base_mean  = formatC(round_sas(mean(BASE, na.rm = T), max(digit0) + 1), format = "f", digits = max(digit0) + 1), 
                N      = ifelse(sum(!is.na(.data[[chg]])) == 0, 0, sum(!is.na(.data[[chg]]))),
                Mean   = formatC(round_sas(mean(.data[[chg]], na.rm = T), max(digit0) + 1), format = "f", digits = max(digit0) + 1), 
                SD     = formatC(round_sas(sd(.data[[chg]], na.rm = T), max(digit0) + 2), format = "f", digits = max(digit0) + 2), 
                Median = formatC(round_sas(median(.data[[chg]], na.rm = T), max(digit0) + 1), format = "f", digits = max(digit0) + 1), 
                Min    = formatC(round_sas(min(.data[[chg]], na.rm = T), max(digit0)), format = "f", digits = max(digit0)), 
                Max    = formatC(round_sas(max(.data[[chg]], na.rm = T), max(digit0)), format = "f", digits = max(digit0)),
                CV     = formatC(round_sas(sd(.data[[chg]], na.rm = T)/mean(.data[[chg]], na.rm = T), max(digit0) + 1), format = "f", digits = max(digit0) + 1),
                .groups = "drop") %>% 
      mutate(across(everything(), ~replace(., trimws(.)=="NaN"|trimws(.)=="NA"|trimws(.)=="Inf"|trimws(.)=="-Inf", NA))) %>% 
      select(all_of(c(rowvar, stats_list)))
    
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
      group_by(.data[[rowvar[1]]], .drop = !keep) %>% 
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
                                if_else(ord_visit == 0, .data[[rowvar[2]]], .data[[rowvar[3]]])),
             N.y = ifelse(row_text == "Baseline", NA, N.y)) %>% 
      select(row_text, everything(), -all_of(rowvar), -c(ord_param, ord_trt, ord_visit))
    
  } else {
    stop("rowvar has to have 2 or 3 elements")
  }
  
  return(tab_combine)
}
