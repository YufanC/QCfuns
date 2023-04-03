#' @title Round a numeric vector; halves will be rounded up as in SAS.
#'
#' @description
#' Please see janitor::round_half_up
#' 
#' @param x a numeric vector to round.
#' @param digits how many digits should be displayed after the decimal point?
#' @export
#' @examples
#' round_half_up(12.5)
#' round_half_up(1.125, 2)
#' round_half_up(1.125, 1)
#' round_half_up(-0.5, 0) # negatives get rounded away from zero
#'
round_sas <- function(x, digits = 0) {
  posneg <- sign(x)
  z <- abs(x) * 10 ^ digits
  z <- z + 0.5 + sqrt(.Machine$double.eps)
  z <- trunc(z)
  z <- z / 10 ^ digits
  z * posneg
}