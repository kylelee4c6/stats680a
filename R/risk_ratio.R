#' Risk ratio
#'
#' This function allows you to compute the risk ratio of a 2x2 matrix.
#'
#' @param data A 2x2 matrix.
#' @param confidence The confidence level in decimal form.
#' @return results The output will be the point estimate followed by the
#'   the confidence interval of the risk ratio stored in a list
#'
#' @examples 
#' x <- matrix(c(10,5,10,75),nrow = 2, ncol = 2)
#' relative.rsk(x,.95)
#'

risk.ratio <- function(data, confidence) {
  p1 <- data[1,1]/(data[1,1] + data[1,2])
  p2 <- data[2,1]/(data[2,1] + data[2,2])
  n1 <- data[1,1]
  n2 <- data[2,1]
  
  # Compute point estimate
  risk.ratio <- p1/p2
  crit.value <- qnorm(1-(1-confidence)/2)
  
  # Calculate variance of log(p1/p2)
  var.log.p1.p2 <- (1-p1)/(n1) + (1-p2)/(n2)
  lower.bound <- risk.ratio*exp(-crit.value*sqrt(var.log.p1.p2))
  upper.bound <- risk.ratio*exp(crit.value*sqrt(var.log.p1.p2))
  
  results <- list(statistic = risk.ratio, lower = lower.bound, upper = upper.bound)
  
  cat("Risk Ratio: ", risk.ratio, "\n",confidence, "% Confidence Interval: ", "[",lower.bound, ",", upper.bound, "]")
  return(results)  
} 

