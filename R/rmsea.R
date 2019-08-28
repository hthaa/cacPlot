#' Root Mean Squared Error of Approximation (RMSEA).
#' @description Calculates the Root Mean Squared Error of Approximation (RMSEA) by way of the Chi-squared statistic, its Degrees of Freedom, and the sample size.
#' @param chi2 Chi-squared.
#' @param df The Chi-squared statistics Degrees of Freedom.
#' @param ss The sample size.
#' @return A single value: The RMSEA statistic.
#' @export
RMSEA <- function(chi2, df, n) {
  return(sqrt((chi2 - df)/(df * (n - 1))))
}
