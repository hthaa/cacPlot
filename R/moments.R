moments <- function(x, type = c("raw", "central", "standardized"), orders = 4, correct = TRUE) {
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
        gamma[i] <- sum(((x - mean(x))^i) / sqrt(var(x))^i) / (length(x) - 2)
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
