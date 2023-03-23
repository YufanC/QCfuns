
#' Create Rows for Categorical Variables
#'
#' Function to create rows for categorical variables in demographic table
#' @inheritParams qc_cntrow1
#' @param rowvar row variable
#' @param N_row a dataframe with denominator N
#' @param keep if = TRUE, keep all factor levels
#' @return dataframe with demographic rows 
#' @examples 
#' adsl <- data.frame(
#'   USUBJID = 1:10,
#'   TRT01P = sample(c("A", "B", "C"), 10, replace = T),
#'   SEX = as.factor(sample(c("Female", "Male"), 10, replace = T)))
#' 
#' ### Create analysis row first
#' first_row <- qc_cntrow1(input = adsl, colvar = "TRT01P", row_text = "Analysis set: Safety")
#'   
#' tab1 <- qc_cat_row(adsl, "TRT01P", "SEX", N_row = first_row[[1]])
#' tab1
#' @export
### Categorical variable rows
qc_cat_row <- function(input, colvar = "TRT01P", rowvar = "SEX", N_row, keep = TRUE){
  # Calculate count and percentage
  tab1 <- input %>% 
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
  
  colnames(tab2) <- c("row_text", levels(as.factor(pull(input, colvar))))
  colnames(tab4) <- c("row_text", levels(as.factor(pull(input, colvar))))
  
  tab5 <- rbind(tab4, tab2)
  
  return(tab5)
}


#' Create Rows for Continuous Variables
#'
#' Function to create rows for continuous variables in demographic table
#' @inheritParams qc_cntrow1
#' @param rowvar row variable
#' @param stats_list stats variables to display. Accepted values are c("Mean_SD", "Median", "Range", "Geo_mean", "Geo_CV", "Geo_CL") and the order of variables matters
#' @param digit number of decimal place to report
#' @return dataframe with demographic rows 
#' @examples 
#' adsl <- data.frame(
#'   USUBJID = 1:10,
#'   TRT01P = sample(c("A", "B", "C"), 10, replace = T),
#'   AGE = sample(18:65, 10, replace = T))
#'   
#' tab1 <- qc_num_row(input = adsl, colvar = "TRT01P", rowvar = "AGE", 
#'                         stats_list = c("Mean_SD", "Median", "Range"), digit = 0)
#' tab1
#' @export
### Numeric variable rows
qc_num_row <- function(input, colvar = "TRT01P", rowvar = "AGE", stats_list, digit){
  # Calculate statistics
  tab1 <- input %>% 
    filter(!is.na(.data[[rowvar]])) %>% 
    group_by(.data[[colvar]], .drop = F) %>% 
    summarise(Mean = mean(.data[[rowvar]], na.rm = T), 
              SD = sd(.data[[rowvar]], na.rm = T), 
              Median = median(.data[[rowvar]], na.rm = T), 
              Min = min(.data[[rowvar]], na.rm = T), 
              Max = max(.data[[rowvar]], na.rm = T), 
              N_trt = n(),
              Geo_mean = ifelse(n() < 1, NA,
                                formatC(exp(mean(log(.data[[rowvar]][.data[[rowvar]] > 0]), na.rm = T)), format = "f", digits = (digit + 1))),
              Geo_CV = ifelse(n() <= 1, NA,
                              formatC(sqrt(exp(sd(log(.data[[rowvar]][.data[[rowvar]] > 0]), na.rm = T)^2) - 1), format = "f", digits = (digit + 1))),
              Gmean_LL = ifelse(n() <= 1, NA,
                                exp(mean(log(.data[[rowvar]][.data[[rowvar]] > 0]), na.rm = T) - qt(0.975,df=n()-1)*sd(log(.data[[rowvar]][.data[[rowvar]] > 0]))/sqrt(n()))),
              Gmean_HL = ifelse(n() <= 1, NA,
                                exp(mean(log(.data[[rowvar]][.data[[rowvar]] > 0]), na.rm = T) + qt(0.975,df=n()-1)*sd(log(.data[[rowvar]][.data[[rowvar]] > 0]))/sqrt(n()))),
              .groups = "drop") %>% 
    arrange(.data[[colvar]]) %>% 
    mutate(Mean_SD = ifelse(!is.na(SD), 
                            paste0(formatC(Mean, format = "f", digits = (digit + 1)), 
                                   ' (', formatC(SD, format = "f", digits = (digit + 2)), ')'), 
                            paste0(formatC(Mean, format = "f", digits = (digit + 1)), 
                                   ' (-)')),
           Median = formatC(Median, format = "f", digits = (digit + 1)),
           Range = paste0("(", formatC(Min, format = "f", digits = digit), "; ", 
                          formatC(Max, format = "f", digits = digit), ")"),
           Geo_CL = ifelse(is.na(Gmean_LL), NA, 
                           paste0("(", formatC(Gmean_LL, format = "f", digits = (digit + 1)), "; ", 
                                  formatC(Gmean_HL, format = "f", digits = (digit + 1)), ")")))
  
  tab2 <- tab1 %>%
    select(.data[[colvar]], all_of(stats_list))
  
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
  
  colnames(tab4) <- c("row_text", levels(as.factor(pull(input, colvar))))
  colnames(tab6) <- c("row_text", levels(as.factor(pull(input, colvar))))
  
  tab7 <- rbind(tab6, tab4) %>% 
    mutate(row_text = case_when(
      row_text == "Mean_SD" ~ 'Mean (SD)',
      row_text == "Geo_mean" ~ 'Geometric mean',
      row_text == "Geo_CV" ~ 'Geometric CV',
      row_text == "Geo_CL" ~ '95% CL of geometric mean',
      TRUE ~ row_text
    ))
  
  return(tab7)
}


#' Create Demographic Rows
#'
#' Function to create demographic rows 
#' @inheritParams qc_cntrow1
#' @param N_row a dataframe with denominator N
#' @inheritParams qc_num_row
#' @param var_list variable list in demo table
#' @param con_var_list List of continuous variables that concatenate to the corresponding categorical variables
#' @param drop_var_list Variable that don't keep all levels in the output
#' @param max_digit maximum number of decimal place to report
#' @return dataframe with demographic rows 
#' @examples 
#' age <- sample(18:65, 10, replace = T)
#' 
#' adsl <- data.frame(
#'   USUBJID = 1:10,
#'   TRT01P = sample(c("A", "B", "C"), 10, replace = T),
#'   AGE = age,
#'   AGEGR1 = ifelse(age < 45, "< 45", ">= 45"),
#'   SEX = as.factor(sample(c("Female", "Male"), 10, replace = T)))
#'   
#' ### Create variable list based on DPS
#' var_list <- c("AGE", "AGEGR1", "SEX")
#'              
#' ### list of variable labels displayed in the table
#' names(var_list) <- c("Age, years", "", "Sex")
#'                     
#' ### List of continuous variables that concatenate to the corresponding categorical variables
#' var_list1 <- "AGEGR1"
#'
#' ### Create analysis row first
#' first_row <- qc_cntrow1(input = adsl, colvar = "TRT01P", row_text = "Analysis set: Safety")
#' 
#' tab1 <- qc_demo(adsl, colvar = "TRT01P", N_row = first_row[[1]], var_list = var_list, 
#'                con_var_list = var_list1)
#' tab1
#' @export
### create demo table 
qc_demo <- function(input, colvar = "TRT01P", N_row, stats_list = c("Mean_SD", "Median", "Range"), var_list = c("AGE", "AGEGR1", "SEX"), con_var_list = "AGEGR1", drop_var_list = NULL, max_digit = 2){
  
  tab_final <- data.frame()
  
  ### Create final dataset
  for (i in 1:length(var_list)){
    
    # Number of decimal place in the original data
    if (class(input[[var_list[i]]]) %in% c("integer", "numeric")){
      digit0 <- getmaxdigit(input, var_list[i], max_digit)
    } 
    
    # Build rows for numeric and categorical variables separately
    if (class(input[[var_list[i]]]) %in% c("integer", "numeric")){
      tab <- qc_num_row(input, colvar, var_list[i], stats_list = stats_list, digit0)
    } else if (class(input[[var_list[i]]]) %in% c("factor", "character")){
      tab <- qc_cat_row(input, colvar, var_list[i], N_row, keep = !(var_list[i] %in% drop_var_list))
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