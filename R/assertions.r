### assert input dataset has more than 0 rows
not_empty <- function(x){
  nrow(x) > 0
}

assertthat::on_failure(not_empty) <- function(call, env) {
  paste0(deparse(call$x), " has 0 row. Use 'subset' arguement instead of doing subset on ", deparse(call$x))
}

### assert values in stats_list are accepted values in qc_chg
stats_accept <- function(x){
  all(x %in% c("N", "Mean", "SD", "Median", "Min", "Max", "CV", "Base_mean"))
}

assertthat::on_failure(stats_accept) <- function(call, env) {
  paste0(deparse(call$x), ' must be values from ("N", "Mean", "SD", "Median", "Min", "Max", "CV", "Base_mean")')
}

### assert values in stats_list are accepted values in qc_num_row
stats_accept_num <- function(x){
  all(x %in% c("Mean_SD", "Mean", "SD", "Median", "Range", "Min", "Max", "Geo_mean", "Geo_CV", "Geo_CL"))
}

assertthat::on_failure(stats_accept_num) <- function(call, env) {
  paste0(deparse(call$x), ' must be values from ("Mean_SD", "Median", "Range", "Geo_mean", "Geo_CV", "Geo_CL")')
}