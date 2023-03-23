#' Compute count and percentage of SOC/PT
#'
#' compute count and percentage of SOC(class)/PT by column variable with an ordering
#' @param input input dataframe 
#' @param colvar column variable 
#' @param rowvar row variable (can be single variable such as "AEDECOD" or multiple variable such as c("AEBODSYS", "AEDECOD"))
#' @param row_text row text 
#' @param N_row dataframe with N 
#' @param col_order ordering column name. e.g. "n_5". Default is NULL and PTs will be ordered by row-wise sum 
#' @return a dataframe containing count and percentage of SOC/PT by colvar
#' @examples 
#' aedecod <- sample(paste0("PT", 1:3), 10, replace = T)
#' 
#' adae <- data.frame(
#'   USUBJID = 1:10,
#'   TRT01P = sample(c("A", "B", "C"), 10, replace = T),
#'   SEX = as.factor(sample(c("Female", "Male"), 10, replace = T)),
#'   AEBODSYS = ifelse(aedecod == "PT1", "SOC1", "SOC2"),
#'   AEDECOD = aedecod)
#' 
#' ### Create analysis row first
#' first_row <- qc_cntrow1(input = adae, colvar = "TRT01P", row_text = "Analysis set: Safety")
#' 
#' tab1 <- qc_cntpctpt(input = adae, colvar = "TRT01P", rowvar = c("AEBODSYS", "AEDECOD"),
#'            row_text = "Subjects with 1 or more AEs", N_row = first_row[[1]])
#' tab1
#' @export
qc_cntpctpt <- function(input, colvar = "TRT01P", rowvar = c("AEBODSYS", "AEDECOD"), row_text = "Subjects with 1 or more AEs", N_row, col_order = NULL, subset = NULL){
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
  
  # row variables - if first level variable exist
  if (length(rowvar) == 2) {
    tab1 <- input %>% 
      group_by(.data[[colvar]], .data[[rowvar[1]]]) %>% 
      summarise(n = ifelse(is.null(subset), n_distinct(USUBJID), n_distinct(USUBJID[eval(parse(text = subset))])), .groups = "drop") %>% 
      left_join(., N_row, by = colvar) %>% 
      mutate(pct = (round(n * 100 / N_trt, 1)),
             col = ifelse(pct == 0, "0", paste0(n, ' (', formatC(pct, format = "f", digits = 1), '%)')))
    
    tab2 <- tab1 %>%
      select(.data[[colvar]], .data[[rowvar[1]]], col, n) %>%
      pivot_wider(names_from = colvar, 
                  values_from = c(col, n)) %>% 
      rowwise() %>% 
      mutate(across(starts_with("n"), ~replace(., is.na(.), 0)),
             order1 = ifelse(is.null(col_order), rowSums(across(starts_with("n")), na.rm = T), as.numeric(.data[[col_order]])),
             order2 = Inf) %>% 
      select(-starts_with("n"))
    
    tab3 <- input %>% 
      group_by(.data[[colvar]], .data[[rowvar[1]]], .data[[rowvar[2]]]) %>% 
      summarise(n = ifelse(is.null(subset), n_distinct(USUBJID), n_distinct(USUBJID[eval(parse(text = subset))])), .groups = "drop") %>% 
      left_join(., N_row, by = colvar) %>% 
      mutate(pct = (round(n * 100 / N_trt, 1)),
             col = ifelse(pct == 0, "0", paste0(n, ' (', formatC(pct, format = "f", digits = 1), '%)')))
    
    tab4 <- tab3 %>%
      select(.data[[colvar]], .data[[rowvar[1]]], .data[[rowvar[2]]], col, n) %>%
      pivot_wider(names_from = colvar, 
                  values_from = c(col, n)) %>% 
      rowwise() %>% 
      mutate(across(starts_with("n"), ~replace(., is.na(.), 0)),
             order2 = ifelse(is.null(col_order), rowSums(across(starts_with("n")), na.rm = T), as.numeric(.data[[col_order]]))) %>% 
      select(-starts_with("n")) %>% 
      ungroup()
    
    tab5 <- tab4 %>% 
      left_join(select(tab2, .data[[rowvar[1]]], order1), by = rowvar[1]) 
    
    tab_final <- bind_rows(tab2, tab5) %>% 
      arrange(desc(order1), .data[[rowvar[1]]], desc(order2), .data[[rowvar[2]]]) %>% 
      mutate(row_text = if_else(order2 == Inf, .data[[rowvar[1]]], .data[[rowvar[2]]])) %>% 
      select(-c( .data[[rowvar[1]]], .data[[rowvar[2]]], order1, order2)) %>% 
      relocate(row_text) %>% 
      mutate(across(everything(), ~replace(., is.na(.), "0")))
    
  } else if (length(rowvar) == 1){
    # row variables - if only one level variable exist
    tab1 <- input %>% 
      group_by(.data[[colvar]], .data[[rowvar]]) %>% 
      summarise(n = ifelse(is.null(subset), n_distinct(USUBJID), n_distinct(USUBJID[eval(parse(text = subset))])), .groups = "drop") %>% 
      left_join(., N_row, by = colvar) %>% 
      mutate(pct = (round(n * 100 / N_trt, 1)),
             col = ifelse(pct == 0, "0", paste0(n, ' (', formatC(pct, format = "f", digits = 1), '%)')))
    
    tab_final <- tab1 %>%
      select(.data[[colvar]], .data[[rowvar]], col, n) %>%
      pivot_wider(names_from = colvar, 
                  values_from = c(col, n)) %>% 
      rowwise() %>% 
      mutate(across(starts_with("n"), ~replace(., is.na(.), 0)),
             n_order = ifelse(is.null(.data[[col_order]]), rowSums(across(starts_with("n")), na.rm = T), as.numeric(.data[[col_order]]))) %>% 
      arrange(desc(order1)) %>% 
      select(-starts_with("n")) %>% 
      mutate(across(everything(), ~replace(., is.na(.), "0"))) %>% 
      ungroup()
    
  } else {
    stop("rowvar has to have 1 or 2 elements")
  }
  
  colnames(tab_final) <- c("row_text", levels(as.factor(pull(N_row, colvar))))
  
  if (nrow(tab_final) == 0){
    tab2[1,1] <- "No data to report"
  }
  
  tab_combine <- bind_rows(row3, tab_final)
  
  return(tab_combine)
  
}