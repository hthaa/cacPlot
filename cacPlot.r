cacPlot <- function(x, ablty = NULL, ablty.se = NULL, stat = "cc", mdl = "Rasch", cutoff = 0, cSEM = TRUE, xRng = c(-3, 3), yRng = c(0, 1), grid = TRUE, lbls = TRUE, lgd = TRUE, grp = NULL, colorblindFriendly = FALSE) {
  library(mirt)
  library(cacIRT)
  if (!is.null(ablty) & is.null(ablty.se)) stop("Raw ability estimates must be accompanied by standard errors to compute classification accuracy or consistency.")
  layout(matrix(c(1, 2), ncol = 2), c(5, 1), c(1, 1))
  par(mar = c(4, 3, 3, 1))
  if (class(x) == "SingleGroupClass") {
    mod <- x
  } else {
    if (class(x) == "data.frame" | class(x) == "matrix") {
      mod <- mirt(data = x, model = 1, itemtype = mdl)
    }
  }
  ab.est <- fscores(mod, method = "ML", response.pattern = mod@Data$data)[, c("F1", "SE_F1")]
  cac <- class.Rud(cutoff, ability = ab.est[, 1], se = ab.est[, 2], D = 1)
  plot(NULL, xlim = c(xRng[1], xRng[2]), ylim = c(yRng[1], yRng[2]), xlab = "", ylab = "")
  if (grid) grid()
  if (cSEM) {
    par(new = TRUE)
    plot(seq(xRng[1], xRng[2], .001), 1 / sqrt(testinfo(mod, seq(xRng[1], xRng[2], .001))), 
         xlim = c(xRng[1], xRng[2]), ylim = c(yRng[1], yRng[2]), type = "l", lty = 2, lwd = 2, xlab = "", ylab = "")
  }
  abline(v = cutoff, lty = 3, lwd = 2)
  curve(dnorm(x), from = xRng[1], to = xRng[2], lwd = 2, n = 1001, add = TRUE)
  for (i in 1:nrow(ab.est)) {
    if (stat == "cc" | stat == "c" | stat == "consistency") { 
      stat <- "Consistency" 
    }
    if (stat == "ca" | stat == "a" | stat == "accuracy") { 
      stat <- "Accuracy" 
    }
      col <- rgb(
        red = if (cac$Conditional[[stat]][i] <= .75) {
          1
        } else {
          (1 - cac$Conditional[[stat]][i]) * 4
        },
        green = if (!colorblindFriendly) {
          if (cac$Conditional[[stat]][i] >= .75) {
            1
          } else {
            (cac$Conditional[[stat]][i] - .5) * 4
          }
        } else {
          0
        },
        blue = if (colorblindFriendly) {
          if (cac$Conditional[[stat]][i] >= .75) {
            1
          } else {
            (cac$Conditional[[stat]][i] - .5) * 4
          }
        } else {
          0
        }
      )
    
    # Point estimate of Theta.
    points(ab.est[i, 1], dnorm(ab.est[i, 1]), pch = 19, col = col)
    # 95% CI.
    lines(c(ab.est[i, 1] - 1.96*ab.est[i, 2], ab.est[i, 1] + 1.96*ab.est[i, 2]), rep(dnorm(ab.est[i, 1]), 2), lwd = 2, col = col)
    lines(rep(ab.est[i, 1] - 1.96*ab.est[i, 2], 2), c(dnorm(ab.est[i, 1] - (yRng[2] - yRng[1]) * .03), dnorm(ab.est[i, 1] + (yRng[2] - yRng[1]) * .03)), lwd = 2, col = col)
    lines(rep(ab.est[i, 1] + 1.96*ab.est[i, 2], 2), c(dnorm(ab.est[i, 1] - (yRng[2] - yRng[1]) * .03), dnorm(ab.est[i, 1] + (yRng[2] - yRng[1]) * .03)), lwd = 2, col = col)
  }
  if (lgd) {
    legend("topleft", bty = "n", lty = c(1, 2, 3), lwd = c(2, 2, 2), pch = c(19, NA_integer_, NA_integer_), legend = c("Obs. w/ 95% CI", "cSEM", paste("Cutoff (", cutoff, ")", sep = "")), merge = TRUE) 
  }
  if (lbls) {
    title(main = paste("Expected Classification ", stat, " at Cutoff = ", cutoff, ".", sep = ""))
    par(cex = 1.5)
    title(xlab = expression(Theta))
  }
  par(mar = c(4, 1, 3, 3), las = 1, cex = 1)
  plot(NULL, xlim = c(0, 1), ylim = c(.5, 1), axes = FALSE, xlab = "", ylab = "")
  axis(4, c(.5, .75, 1))
}
