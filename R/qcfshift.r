#' Create Shift table
#'
#' create shift table, accept up to 3 levels of row variables 
#' @param input input dataframe 
#' @param acat measurement variable
#' @param chg change from baseline measurement variable
#' @param rowvar row variable (can be a set of two variables such as c("TRT01P", "AVISIT") or three variables such as c("PARAM", "TRT01P", "AVISIT"))
#' @param stats_list stats variables to display. Accepted values are c("N", "Mean", "SD", "Median", "Min", "Max", "CV", "Base_mean") and the order of variables matters
#' @param max_digit largest number of digit to display 
#' @param keep if = TRUE, keep all levels. Default = FALSE
#' @return shift table by rowvar
#' @examples 
#' adsl <- data.frame(
#'   USUBJID = 1:10,
#'   TRT01P = sample(c("A", "B", "C"), 10, replace = T))
#'   
#' param <- c("Test1", "Test2")
#' visit <- c("Baseline", "Visit1", "Visit2")
#' 
#' adlb0 <- merge(adsl, param)
#' adlb <- merge(adlb0, visit)
#' 
#' adlb$ANRIND <- factor(sample(c("Low", "Medium", "High"), 60), levels = c("Low", "Medium", "High"))
#' adlb$BNRIND <- factor(sample(c("Low", "Medium", "High"), 60), levels = c("Low", "Medium", "High"))
#' 
#' tab1 <- qc_shift(adlb, "ANRIND", "BNRIND", rowvar = c("PARAM", "TRT01P", "AVISIT"))
#' tab1
#' @export
qc_shift <- function(input, acat = "ANRIND", bcat = "BNRIND", rowvar = c("PARAM", "AVISIT", "TRT01P"), drop_zero = TRUE){
  
  if(length(rowvar) == 1){
    
    # Create frequency table
    tab1 <- table(input[[acat]], input[[bcat]], input[[rowvar[1]]], useNA = "always")
    tab2 <- ftable(addmargins(tab1), row.vars = c(3,1))
    tab3 <- as.matrix(tab2)
    
    # Get the row names and split it to several cols
    rnames <- t(as.data.frame(str_split(row.names(tab3), "_")))
    
    # Combine the row names and frequency table together
    tab4 <- as.data.frame(cbind(rnames, tab3))
    colnames(tab4) <- c(rowvar, acat, levels(input[[bcat]]), "NA_col", "Sum")
    
    # Get rid of the NA and sum rows and NA cols
    tab5 <- tab4 %>% 
      filter(!.data[[rowvar[1]]] %in% c("NA", "Sum") & .data[[acat]] != "NA") %>% 
      mutate(across(c(NA_col, Sum), as.numeric), 
             Total = Sum - NA_col) %>%  # Divide NA counts from total
      select(-c(NA_col, Sum)) %>% 
      mutate(across(where(is.numeric), as.character))
    
    row.names(tab5) <- NULL
    
    ### Reorder the rows and combine them together
    
    param_row <- input %>% 
      group_by(.data[[rowvar[1]]], .drop = FALSE) %>%
      summarise(N = n_distinct(USUBJID), .groups = "drop") %>% 
      arrange(.data[[rowvar[1]]]) %>% 
      mutate(ord_param = 1:n(),
             ord_ind = 0,
             N = ifelse(N == 0 & drop_zero, NA, N))
    
    ind_row <- tab5 %>% 
      arrange(.data[[rowvar[1]]]) %>% 
      left_join(param_row, by = rowvar[1]) %>% 
      group_by(.data[[rowvar[1]]], .drop = F) %>% 
      mutate(ord_ind = 1:n(),
             across(c(levels(input[[bcat]]), "Total"), ~ifelse(is.na(N), NA, .))) %>% 
      ungroup() %>% 
      select(-N)
    
    tab_combine <- bind_rows(param_row, ind_row) %>% 
      arrange(ord_param, ord_ind) %>% 
      mutate(across(all_of(c(rowvar, acat)), ~as.character(.)),
             row_text = if_else(ord_ind == 0, .data[[rowvar[1]]],
                                if_else(.data[[acat]] == "Sum", "Total", str_to_title(.data[[acat]])))) %>% 
      select(row_text, everything(), -c(ord_param, ord_ind), -all_of(c(rowvar, acat)))
    
    return(tab_combine)
    
  } else if (length(rowvar) == 2){
    # Create frequency table
    tab1 <- table(input[[acat]], input[[bcat]], input[[rowvar[2]]], input[[rowvar[1]]], useNA = "always")
    tab2 <- ftable(addmargins(tab1), row.vars = c(4,3,1))
    tab3 <- as.matrix(tab2)
    
    # Get the row names and split it to several cols
    rnames <- t(as.data.frame(str_split(row.names(tab3), "_")))
    
    # Combine the row names and frequency table together
    tab4 <- as.data.frame(cbind(rnames, tab3))
    colnames(tab4) <- c(rowvar, acat, levels(input[[bcat]]), "NA_col", "Sum")
    
    # Get rid of the NA and sum rows and NA cols
    tab5 <- tab4 %>% 
      filter(!.data[[rowvar[2]]] %in% c("NA", "Sum") & !.data[[rowvar[1]]] %in% c("NA", "Sum") & .data[[acat]] != "NA") %>% 
      mutate(across(c(NA_col, Sum), as.numeric), 
             Total = Sum - NA_col) %>%  # Divide NA counts from total
      select(-c(NA_col, Sum)) %>% 
      mutate(across(where(is.numeric), as.character))
    
    row.names(tab5) <- NULL
    
    ### Reorder the rows and combine them together
    
    param_row <- input %>% 
      group_by(.data[[rowvar[1]]], .drop = FALSE) %>%
      summarise(.groups = "drop") %>% 
      arrange(.data[[rowvar[1]]]) %>% 
      mutate(ord_param = 1:n(),
             ord_visit = 0,
             ord_ind = 0)
    
    visit_row <- input %>% 
      group_by(.data[[rowvar[1]]], .data[[rowvar[2]]], .drop = FALSE) %>%
      summarise(N = n_distinct(USUBJID), .groups = "drop") %>% 
      arrange(.data[[rowvar[2]]]) %>%
      left_join(param_row, by = rowvar[1]) %>%
      group_by(.data[[rowvar[1]]], .drop = FALSE) %>% 
      mutate(ord_visit = 1:n(),
             N = ifelse(N == 0 & drop_zero, NA, N)) %>% 
      ungroup()
    
    ind_row <- tab5 %>% 
      arrange(.data[[rowvar[1]]], .data[[rowvar[2]]]) %>% 
      left_join(visit_row, by = c(rowvar[1], rowvar[2])) %>% 
      group_by(.data[[rowvar[1]]], .data[[rowvar[2]]], .drop = F) %>% 
      mutate(ord_ind = 1:n(),
             across(c(levels(input[[bcat]]), "Total"), ~ifelse(is.na(N), NA, .))) %>% 
      ungroup() %>% 
      select(-N)
    
    tab_combine <- bind_rows(param_row, visit_row, ind_row) %>% 
      arrange(ord_param, ord_visit, ord_ind) %>% 
      mutate(across(all_of(c(rowvar, acat)), ~as.character(.)),
             row_text = if_else(ord_visit == 0, .data[[rowvar[1]]], 
                                if_else(ord_ind == 0, .data[[rowvar[2]]],
                                        if_else(.data[[acat]] == "Sum", "Total", str_to_title(.data[[acat]]))))) %>% 
      select(row_text, everything(), -c(ord_param, ord_visit, ord_ind), -all_of(c(rowvar, acat)))
    
    return(tab_combine)
    
  } else if (length(rowvar) == 3){
    # Create frequency table
    tab1 <- table(input[[acat]], input[[bcat]], input[[rowvar[3]]], input[[rowvar[2]]], input[[rowvar[1]]], useNA = "always")
    tab2 <- ftable(addmargins(tab1), row.vars = c(5,4,3,1))
    tab3 <- as.matrix(tab2)
    
    # Get the row names and split it to several cols
    rnames <- t(as.data.frame(str_split(row.names(tab3), "_")))
    
    # Combine the row names and frequency table together
    tab4 <- as.data.frame(cbind(rnames, tab3))
    colnames(tab4) <- c(rowvar, acat, levels(input[[bcat]]), "NA_col", "Sum")
    
    # Get rid of the NA and sum rows and NA cols
    tab5 <- tab4 %>% 
      filter(!.data[[rowvar[3]]] %in% c("NA", "Sum") & !.data[[rowvar[2]]] %in% c("NA", "Sum") &
             !.data[[rowvar[1]]] %in% c("NA", "Sum") & .data[[acat]] != "NA") %>% 
      mutate(across(c(NA_col, Sum), as.numeric), 
             Total = Sum - NA_col) %>%  # Divide NA counts from total
      select(-c(NA_col, Sum)) %>% 
      mutate(across(where(is.numeric), as.character))
    
    row.names(tab5) <- NULL
    
    ### Reorder the rows and combine them together
    
    param_row <- input %>% 
      group_by(.data[[rowvar[1]]], .drop = FALSE) %>%
      summarise(.groups = "drop") %>% 
      arrange(.data[[rowvar[1]]]) %>% 
      mutate(ord_param = 1:n(),
             ord_visit = 0,
             ord_trt = 0,
             ord_ind = 0)
    
    visit_row <- input %>% 
      group_by(.data[[rowvar[1]]], .data[[rowvar[2]]], .drop = FALSE) %>%
      summarise(.groups = "drop") %>% 
      arrange(.data[[rowvar[2]]]) %>%
      left_join(param_row, by = rowvar[1]) %>%
      group_by(.data[[rowvar[1]]], .drop = FALSE) %>% 
      mutate(ord_visit = 1:n()) %>% 
      ungroup()
    
    trt_row <- input %>% 
      group_by(.data[[rowvar[1]]], .data[[rowvar[2]]], .data[[rowvar[3]]], .drop = FALSE) %>%
      summarise(N = n_distinct(USUBJID), .groups = "drop") %>% 
      ungroup() %>% 
      left_join(visit_row, by = c(rowvar[1], rowvar[2])) %>% 
      arrange(.data[[rowvar[1]]], .data[[rowvar[2]]], .data[[rowvar[3]]]) %>% 
      group_by(.data[[rowvar[1]]], .data[[rowvar[2]]], .drop = FALSE) %>% 
      mutate(ord_trt = 1:n(),
             N = ifelse(N == 0 & drop_zero, NA, N)) %>% 
      ungroup()
    
    ind_row <- tab5 %>% 
      arrange(.data[[rowvar[1]]], .data[[rowvar[2]]], .data[[rowvar[3]]]) %>% 
      left_join(trt_row, by = c(rowvar[1], rowvar[2], rowvar[3])) %>% 
      group_by(.data[[rowvar[1]]], .data[[rowvar[2]]], .data[[rowvar[3]]], .drop = F) %>% 
      mutate(ord_ind = 1:n(),
             across(c(levels(input[[bcat]]), "Total"), ~ifelse(is.na(N), NA, .))) %>% 
      ungroup() %>% 
      select(-N)
    
    tab_combine <- bind_rows(param_row, visit_row, trt_row, ind_row) %>% 
      arrange(ord_param, ord_visit, ord_trt, ord_ind) %>% 
      mutate(across(all_of(c(rowvar, acat)), ~as.character(.)),
             row_text = if_else(ord_visit == 0, .data[[rowvar[1]]], 
                                if_else(ord_trt == 0, .data[[rowvar[2]]], 
                                        if_else(ord_ind == 0, .data[[rowvar[3]]],
                                                if_else(.data[[acat]] == "Sum", "Total", str_to_title(.data[[acat]])))))) %>% 
      select(row_text, everything(), -c(ord_param, ord_trt, ord_visit, ord_ind), -all_of(c(rowvar, acat)))
    
    return(tab_combine)
    
  } else {
    stop("rowvar has to have 1, 2 or 3 elements")
  }

}
