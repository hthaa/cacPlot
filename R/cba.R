#' Calculate Cronbachs Alpha from supplied variables.
#'
#' @description Calculates Cronbachs Alpha, a very commonly used index for assessing the reliability / internal consistency of a sumscore. Often interpreted as the mean correlation across all possible split-half alternate forms of the test.
#' @param x A data-frame or matrix of numerical values where rows are across-items within-respondent observation vectors, and columns are within-item across-respondents observation vectors.
#' @note Missing values are treated by passing \code{na.rm = TRUE} to the \code{var} function call.
#' @note Be aware that this function does not issue a warning if there are negative correlations between variables in the supplied data-set.
#' @return Cronbachs Alpha for the sumscore of supplied variables.
#' @examples
#' data <- expand.table(LSAT7)
#' cba(data)
#' @export

cba <- function(x) {
  (ncol(x) / (ncol(x) - 1)) *
    (1 - (sum(diag(var(x, na.rm = TRUE))) /
            sum(var(x, na.rm = TRUE))))
}
