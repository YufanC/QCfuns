#' Create Rows for Continuous Variables
#'
#' Function to create rows for continuous variables in demographic table
#' @inheritParams qc_cntrow1
#' @param rowvar row variable
#' @param stats_list stats variables to display. Accepted values are \code{c("Mean_SD", "Mean", "SD", "Median", "Range", "Min", "Max", "Geo_mean", "Geo_CV", "Geo_CL")} and the order of variables matters
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
#' @importFrom stats addmargins ftable median qt sd
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
              Min = ifelse(n() == 0, NA, min(.data[[rowvar]], na.rm = T)), 
              Max = ifelse(n() == 0, NA, max(.data[[rowvar]], na.rm = T)), 
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
           Mean = ifelse(is.na(Mean), NA, 
                         formatC(round_sas(Mean, digit + 1), format = "f", digits = (digit + 1))),
           SD = ifelse(is.na(SD), NA, 
                       formatC(round_sas(SD, digit + 1), format = "f", digits = (digit + 1))),
           Median = ifelse(is.na(Median), NA, 
                           formatC(round_sas(Median, digit + 1), format = "f", digits = (digit + 1))),
           Range = ifelse(is.na(Min)|is.na(Max), NA,
                          paste0("(", formatC(round_sas(Min, digit), format = "f", digits = digit), "; ", 
                                 formatC(round_sas(Max, digit), format = "f", digits = digit), ")")),
           Min = ifelse(is.na(Min), NA, 
                        formatC(round_sas(Min, digit), format = "f", digits = digit)),
           Max = ifelse(is.na(Max), NA, 
                        formatC(round_sas(Max, digit), format = "f", digits = digit)),
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