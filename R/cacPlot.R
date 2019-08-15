#' Classification Accuracy and Consistency Plotting.
#'
#' @description Plotting tool for gauging the classification accuracy or consistency of classifications based on IRT ability estimates, where classification accuracy and consistency indices are computed by means of the \code{cacIRT} package (Lathrop, 2015).
#'
#' @param x A data.frame or matrix with rows representing respondents and columns representing items, or a mirt-model object of class "SingleGroupClass".
#' @param ablty A vector of ability estimates. Required specification of standard error in \code{ablty.se}.
#' @param ablty.se A vector of standard errors of estimates corresponding to the values in the \code{ablty} vector.
#' @param stat A character-value indicating whether to color-code observations with respect to their expected consistency or accuracy. Permissible values are "c", "cc", "consistency" or "Consistency" for expected classification consistency, and "a", "ca", "accuracy" or "Accuracy" for expected classification accuracy.
#' @param mdl If a dataset was supplied as input, specifies which model to fit to the data by way of the \code{mirt} package (Chalmers, 2012). See \code{?mirt} for options.
#' @param cutoff The cutoff value relative to which expected classification consistency or accuracy for observations are to be calculated and illustrated.
#' @param ci Plot confidence intervals around each observation point?
#' @param cSEM Plot the conditional standard errors of the estimates?
#' @param xRng The range of the plotted x-axis.
#' @param yRng The range of the plotted y-axis.
#' @param grid Include a grid in the plot?
#' @param lbls Include labels in the plot?
#' @param rel.wdth The relative widths of the main plot and the color gradient legend.
#' @param colorblindFriendly Make gradient colorblind friendly?
#' @return A graph plotting observations with color gradients indicating expected classification consistency and accuracy relative to a defined cutoff point.
#' @references R. Philip Chalmers (2012). mirt: A Multidimensional Item Response Theory Package for the R Environment. Journal of Statistical Software, 48(6), 1-29.
#' @references Quinn N. Lathrop (2015). cacIRT: Classification Accuracy and Consistency under Item Response Theory. R package version 1.4.
#' @examples
#' # Color-blind friendly plotting of classification consistency based on feeding
#' # cacPlot a dataset, where the cutoff-point is set to 0.5.
#' data <- expand.table(LSAT7[2:31, ])
#' cacPlot(data, stat = "c", cutoff = .5, colorblindFriendly = TRUE)
#'
#' # Plotting of classification accuracy based on feeding cacPlot a mirt
#' # model-object, along with plotting of the conditional standard error
#' # of measurement (cSEM).
#' data <- expand.table(LSAT7[2:31, ])
#' LSAT7.mod <- mirt(data, model = 1, itemtype = "Rasch")
#' cacPlot(LSAT7.mod, stat = "a", cSEM = TRUE)
#'
#' # Plotting of classification consistency based on feeding cacPlot a
#' # vector of raw ability estimates, and a vector with the standard
#' # errors corresponding to those ability estimates.
#' ability_estimates <- fscores(mirt(expand.table(LSAT7[2:31, ]), 1, "Rasch"),
#'     "ML", response.pattern = expand.table(LSAT7[2:31, ]))[, c("F1", "SE_F1")]
#' cacPlot(ablty = ability_estimates[, "F1"], ablty.se = ability_estimates[, "SE_F1"])
#' @export

cacPlot <- function(x = NULL, ablty = NULL, ablty.se = NULL, stat = "ca", mdl = "Rasch", cutoff = 0, ci = TRUE, cSEM = FALSE, xRng = c(-3, 3), yRng = c(0, 1), grid = TRUE, lbls = TRUE, lgd = TRUE, rel.wdth = c(7, 1), colorblindFriendly = FALSE) {
  if (!is.null(ablty) & is.null(ablty.se)) {
    stop("Raw ability estimates must be accompanied by standard errors to compute classification accuracy or consistency.")
  }
  if (stat == "cc" | stat == "c" | stat == "consistency") {
    stat <- "Consistency"
  }
  if (stat == "ca" | stat == "a" | stat == "accuracy") {
    stat <- "Accuracy"
  }
  layout(matrix(c(2, 1), ncol = 2), c(rel.wdth[1], rel.wdth[2]), c(1, 1))

  par(mar = c(4, 1, 3, 3), las = 1, cex = 1)
  plot(NULL, xlim = c(0, 1), ylim = c(.5, 1), axes = FALSE, xlab = "", ylab = "")
  abline(h = seq(.5, 1, .0001), col = sapply(seq(.5, 1, .0001), cacGradient, cp = colorblindFriendly))
  axis(4, c(.5, .75, 1))
  par(mar = c(4, 3, 3, 1))
  if (class(x) == "SingleGroupClass") {
    mod <- x
  } else {
    if (class(x) == "data.frame" | class(x) == "matrix") {
      mod <- mirt::mirt(data = x, model = 1, itemtype = mdl)
    }
  }
  ab.est <- if (!is.null(x))  {
    mirt::fscores(mod, method = "ML", response.pattern = mod@Data$data)[, c("F1", "SE_F1")]
  } else {
    matrix(c(ablty, ablty.se), ncol = 2)
  }
  cac <- cacIRT::class.Rud(cutoff, ability = ab.est[, 1], se = ab.est[, 2], D = 1)
  plot(NULL, xlim = c(xRng[1], xRng[2]), ylim = c(yRng[1], yRng[2]), xlab = "", ylab = "")
  if (grid) grid()
  if (cSEM) {
    par(new = TRUE)
    plot(seq(xRng[1], xRng[2], .001), 1 / sqrt(mirt::testinfo(mod, seq(xRng[1], xRng[2], .001))),
         xlim = c(xRng[1], xRng[2]), ylim = c(yRng[1], yRng[2]), type = "l", lty = 2, lwd = 2, xlab = "", ylab = "")
  }
  abline(v = cutoff, lty = 3, lwd = 2)
  curve(dnorm(x), from = xRng[1], to = xRng[2], lwd = 2, n = 1001, add = TRUE)
  cac.col <- sapply(cac$Conditional[[stat]], cacGradient, cp = colorblindFriendly)
  points(matrix(c(ab.est[, 1], sapply(ab.est[, 1], dnorm)), ncol = 2), pch = 19, col = cac.col)
  if (ci) {
    coords <- matrix(c(ab.est[, 1], ab.est[, 2], ab.est[, 1] - 1.96*ab.est[, 2], ab.est[, 1] + 1.96*ab.est[, 2], sapply(ab.est[, 1], dnorm)), ncol = 5)
    apply(coords, 1, function(x) {
      lines(x[c(3, 4)], rep(x[5], 2), col = cacGradient(cacIRT::class.Rud(cutoff, D = 1, ability = x[1], se = x[2])$Conditional[[stat]],
                                                        colorblindFriendly), lwd = 2)
      lines(rep(x[3], 2), c(x[5] - (yRng[2] - yRng[1]) * .015, x[5] + (yRng[2] - yRng[1]) * .015),
            col = cacGradient(cacIRT::class.Rud(cutoff, D = 1, ability = x[1], se = x[2])$Conditional[[stat]], colorblindFriendly), lwd = 2)
      lines(rep(x[4], 2), c(x[5] - (yRng[2] - yRng[1]) * .015, x[5] + (yRng[2] - yRng[1]) * .015),
            col = cacGradient(cacIRT::class.Rud(cutoff, D = 1, ability = x[1], se = x[2])$Conditional[[stat]], colorblindFriendly), lwd = 2)
    }
    )
  }
  if (lgd) {
    legend("topleft", bty = "n", lty = c(1, 2, 3), lwd = c(2, 2, 2), pch = c(19, NA_integer_, NA_integer_),
           legend = c("Obs. w/ 95% CI", "cSEM", paste("Cutoff (", cutoff, ")", sep = "")), merge = TRUE)
  }
  if (lbls) {
    title(main = paste("Expected Classification ", stat, " at Cutoff = ", cutoff, ".", sep = ""))
    par(cex = 1.5)
    title(xlab = expression(Theta))
  }
}
