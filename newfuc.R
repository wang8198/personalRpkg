#'
#' The log of sum of exponential
#'
#' @param x A vector of consecutive positive integers
#'
#' @return the log of sum of exponential of the input vector
#'
#' @examples
#'
#' log_summed_exps((seq(1:1000)))
#'
#' @export
#'

log_summed_exps <- function(x){
  max_x <- max(x)
  sum_exp <- sum(exp(x - max_x))
  log_sum_exp <- log(sum_exp)
  output_sum <- max_x + log_sum_exp
  return(output_sum)
}


