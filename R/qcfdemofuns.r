
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
#'   TRT01P = factor(sample(c("A", "B", "C"), 10, replace = TRUE)),
#'   SEX = factor(sample(c("Female", "Male"), 10, replace = TRUE)))
#' 
#' ### Create analysis row first
#' first_row <- qc_cntrow1(input = adsl, colvar = "TRT01P", row_text = "Analysis set: Safety")
#'   
#' tab1 <- qc_cat_row(adsl, "TRT01P", "SEX", N_row = first_row$N_row)
#' tab1
#' @export
### Categorical variable rows
qc_cat_row <- function(input, colvar = "TRT01P", rowvar = "SEX", row_text = "Sex", N_row, keep = TRUE){
  
  assertthat::assert_that(not_empty(input))
  assertthat::assert_that(assertthat::has_name(input, c(colvar, rowvar)))
  assertthat::assert_that(is.factor(input[[colvar]]))
  
  # Calculate count and percentage
  tab1 <- input %>% 
    filter(!is.na(.data[[rowvar]])) %>% 
    group_by(.data[[colvar]], .data[[rowvar]], .drop = !keep) %>% 
    summarise(n = n_distinct(USUBJID), .groups = "drop") %>%
    left_join(N_row, by = colvar) %>% 
    mutate(pct = (round_sas(n * 100 / N_trt, 1)),
           col = ifelse(pct == 0, "0", paste0(n, ' (', formatC(pct, format = "f", digits = 1), '%)')))
  
  # Transpose data
  tab2 <- tab1 %>%
    select(all_of(c(rowvar, colvar)), col) %>%
    pivot_wider(names_from = all_of(colvar), 
                values_from = col)
  
  # Total counts row
  tab3 <- tab1 %>% 
    distinct(.data[[colvar]], N_trt) %>% 
    pivot_wider(names_from = all_of(colvar),
                values_from = N_trt) %>% 
    mutate(across(where(is.numeric), as.character))
  
  tab4 <- cbind("N", tab3)
  
  colnames(tab2) <- c("row_text", levels(input[[colvar]]))
  colnames(tab4) <- c("row_text", levels(input[[colvar]]))
  
  tab5 <- bind_rows(data.frame(row_text = row_text), tab4, tab2)
  
  return(tab5)
}


#' Create Rows for Continuous Variables
#'
#' Function to create rows for continuous variables in demographic table
#' @inheritParams qc_cntrow1
#' @param rowvar row variable
#' @param stats_list stats variables to display. Accepted values are \code{c("Mean_SD", "Median", "Range", "Geo_mean", "Geo_CV", "Geo_CL")} and the order of variables matters
#' @param digit number of decimal place to report
#' @return dataframe with demographic rows 
#' @examples 
#' adsl <- data.frame(
#'   USUBJID = 1:10,
#'   TRT01P = factor(sample(c("A", "B", "C"), 10, replace = TRUE)),
#'   AGE = sample(18:65, 10, replace = TRUE))
#'   
#' tab1 <- qc_num_row(input = adsl, colvar = "TRT01P", rowvar = "AGE", 
#'                         stats_list = c("Mean_SD", "Median", "Range"), digit = 0)
#' tab1
#' @export
### Numeric variable rows
qc_num_row <- function(input, colvar = "TRT01P", rowvar = "AGE", row_text = "Age, years", stats_list, digit){
  
  assertthat::assert_that(not_empty(input))
  assertthat::assert_that(assertthat::has_name(input, c(colvar, rowvar)))
  assertthat::assert_that(stats_accept_num(stats_list))
  assertthat::assert_that(is.factor(input[[colvar]]))
  
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
                                formatC(round_sas(exp(mean(log(.data[[rowvar]][.data[[rowvar]] > 0]), na.rm = T)), digit + 1), 
                                        format = "f", digits = (digit + 1))),
              Geo_CV = ifelse(n() <= 1, NA,
                              formatC(round_sas(sqrt(exp(sd(log(.data[[rowvar]][.data[[rowvar]] > 0]), na.rm = T)^2) - 1), digit + 1), 
                                      format = "f", digits = (digit + 1))),
              Gmean_LL = ifelse(n() <= 1, NA,
                                exp(mean(log(.data[[rowvar]][.data[[rowvar]] > 0]), na.rm = T) - qt(0.975,df=n()-1)*sd(log(.data[[rowvar]][.data[[rowvar]] > 0]))/sqrt(n()))),
              Gmean_HL = ifelse(n() <= 1, NA,
                                exp(mean(log(.data[[rowvar]][.data[[rowvar]] > 0]), na.rm = T) + qt(0.975,df=n()-1)*sd(log(.data[[rowvar]][.data[[rowvar]] > 0]))/sqrt(n()))),
              .groups = "drop") %>% 
    arrange(.data[[colvar]]) %>% 
    mutate(Mean_SD = ifelse(is.na(Mean), NA,
                            ifelse(!is.na(SD), 
                                   paste0(formatC(round_sas(Mean, digit + 1), format = "f", digits = (digit + 1)), 
                                          ' (', formatC(SD, format = "f", digits = (digit + 2)), ')'), 
                                   paste0(formatC(Mean, format = "f", digits = (digit + 1)), 
                                          ' (-)'))),
           Median = ifelse(is.na(Median), NA, 
                           formatC(round_sas(Median, digit + 1), format = "f", digits = (digit + 1))),
           Range = ifelse(is.na(Min)|is.na(Max), NA,
                          paste0("(", formatC(round_sas(Min, digit), format = "f", digits = digit), "; ", 
                          formatC(round_sas(Max, digit), format = "f", digits = digit), ")")),
           Geo_CL = ifelse(is.na(Gmean_LL)|is.na(Gmean_HL), NA, 
                           paste0("(", formatC(round_sas(Gmean_LL, digit + 1), format = "f", digits = (digit + 1)), "; ", 
                                  formatC(round_sas(Gmean_HL, digit + 1), format = "f", digits = (digit + 1)), ")")))
  
  tab2 <- tab1 %>%
    select(all_of(c(colvar, stats_list)))
  
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
    pivot_wider(names_from = all_of(colvar),
                values_from = N_trt) %>% 
    mutate(across(where(is.numeric), as.character))
  
  tab6 <- cbind("N", tab5)
  
  colnames(tab4) <- c("row_text", levels(input[[colvar]]))
  colnames(tab6) <- c("row_text", levels(input[[colvar]]))
  
  tab7 <- bind_rows(data.frame(row_text = row_text), tab6, tab4) %>% 
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
#' 
#' \code{var_list} should be a dataframe with desired variable names as column names,
#' the label of each variable display in tables as the first row, and optionally,
#' the number of decimal places to keep in the output as the second row. If the 
#' label row of a variable is empty, the row text and N row for this variable
#' will be missing.
#' @inheritParams qc_cntrow1
#' @param N_row a dataframe with denominator N
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
#' ### Create analysis row first
#' first_row <- qc_cntrow1(input = adsl, colvar = "TRT01P", row_text = "Analysis set: Safety")
#' 
#' tab1 <- qc_demo(adsl, colvar = "TRT01P", N_row = first_row$N_row, var_list = var_list)
#' tab1
#' @export
### create demo table 
qc_demo <- function(input, colvar = "TRT01P", N_row, stats_list = c("Mean_SD", "Median", "Range"), var_list, drop_var_list = NULL, max_digit = 2){
  
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
        tab <- qc_cat_row(input, colvar, colnames(var_list)[i], N_row = N_row, 
                                  keep = !(colnames(var_list)[i] %in% drop_var_list)) %>% 
          filter(!row_text %in% c("Sex", "N"))
      } else {
        tab <- qc_cat_row(input, colvar, colnames(var_list)[i], N_row = N_row, 
                          keep = !(colnames(var_list)[i] %in% drop_var_list), row_text = var_list[1, i])
      }
     
    }
    # Append them together 
    tab_final <- bind_rows(tab_final, tab)
  }
  
  return(tab_final)
  
}
