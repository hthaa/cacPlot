#' Sample-generated moments.
#'
#' @description Moments describe the shape of distributions, and the four most familiar are generally known by their plebeian names: Mean, Variance, Skewness, and Kurtosis. For the further initiated, these might be recognized as the first, second, third, and fourth order moments, respectively. The potential number of orders is infinite, but only the first four are commonly taken into account.
#' @description In addition to orders, moments can be further classified into types, most commonly labelled the "Raw", "Central", and "Standardized" moments. The most familiar moments are usually not expressed in the same types. That is, the first-order moment (mean) is expressed as Raw-, the second-order (variance) as Central-, and the third- and fourth-order (skewness and kurtosis, respectively) as Standardized-type.
#'
#' @param x A vector of numerical values (i.e., a variable).
#' @param type A character vector indicating which types of moments are to be calculated. Permissible values are "raw", "central", and "standardized". The default is to compute all types.
#' @param orders A numeric value indicating the number of moment orders are to be calculated for each type (e.g., 1 yields mean, 2 yields mean and variance). The default is to compute the first four moments.
#' @param correct A logical value indicating whether the calculations for central- and standardized moments should take degrees of freedom into account (i.e., bias correction).
#'
#' @note Missing values are treated by way of \code{na.omit}.
#'
#' @return A list of up to three elements (depending on how many moment types were asked for), each containing one entry for every type-moment order that was to be calculated.
#'
#' @examples
#' data <- expand.table(LSAT7)
#' var1_moments <- moments(data[, 1])
#' # Retrieve the third-order standardized moment (i.e., skewness) for the
#' # first variable of the LSAT7 data-set.
#' var1_moments$standardized[[3]]
#'
#' @export

moments <- function(x, type = c("raw", "central", "standardized"), orders = 4, correct = TRUE) {
  x <- na.omit(x)
  types <- 1
  momentorders <- list()
  if (any(type == "raw")) {
    mu <- list(rep(vector(length = 1), orders))
    for (i in 1:orders) {
      mu[i] <- sum(x^i) / length(x)
    }
    momentorders[[length(momentorders) + 1]] <- mu
    names(momentorders)[types] <- "raw"
    types <- types + 1
  }
  if (any(type == "central")) {
    sigma <- list(rep(vector(length = 1), orders))
    for (i in 1:orders) {
      if (correct) {
        sigma[i] <- sum((x - mean(x))^i) / (length(x) - 1)
      } else {
        sigma[i] <- sum((x - mean(x))^i) / (length(x))
      }
    }
    momentorders[[length(momentorders) + 1]] <- sigma
    names(momentorders)[types] <- "central"
    types <- types + 1
  }
  if (any(type == "standardized")) {
    gamma <- list(rep(vector(length = 1), orders))
    for (i in 1:orders) {
      if(correct) {
        gamma[i] <- sum(((x - mean(x))^i) / sqrt(var(x))^i) / (length(x) - 1)
      } else {
        gamma[i] <- sum(((x - mean(x))^i) / sqrt(var(x))^i) / length(x)
      }
    }
    momentorders[[length(momentorders) + 1]] <- gamma
    names(momentorders)[types] <- "standardized"
  }
  return(momentorders)
}

skewness <- function(x) {
  moments(x, type = "standardized", orders = 3, correct = FALSE)[[1]][[3]]
}

kurtosis <- function(x) {
  moments(x, type = "standardized", orders = 4, correct = FALSE)[[1]][[4]]
}

ex.kurtosis <- function(x) {
  kurtosis(x) - 3
}
