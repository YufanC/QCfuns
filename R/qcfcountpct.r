#' Compute count and percentage
#'
#' compute count and percentage by column variable
#' @param input input dataframe 
#' @param colvar column variable 
#' @param row_text row text 
#' @param N_row dataframe with N 
#' @param subset subset criteria. Default = NULL
#' @return a dataframe containing count and percentage by colvar
#' @examples 
#' countpct(input = adae, colvar = "TRT01PN", N_row = first_row, row_text = "Subjects with 1 or more AEs", subset = "SEX == "Female")
#' @export
#' @import tidyr
cntpct <- function(input, colvar, row_text, N_row, subset = NULL){
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