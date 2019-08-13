cacGradient <- function(x, cp = FALSE) {
  rgb(
    red = if (x <= .75) {
      1
    } else {
      (1 - x) * 4
    },
    green = if (!cp) {
      if (x >= .75) {
        1
      } else {
        (x - .5) * 4
      }
    } else {
      0
    },
    blue = if (cp) {
      if (x >= .75) {
        1
      } else {
        (x - .5) * 4
      }
    } else {
      0
    }
  )
}

cacPlot <- function(x, ablty = NULL, ablty.se = NULL, stat = "cc", mdl = "Rasch", cutoff = 0, ci = TRUE, cSEM = TRUE, xRng = c(-3, 3), yRng = c(0, 1), grid = TRUE, lbls = TRUE, lgd = TRUE, grp = NULL, rel.wdth = c(7, 1), colorblindFriendly = FALSE) {
  library(mirt)
  library(cacIRT)
  if (!is.null(ablty) & is.null(ablty.se)) {
    stop("Raw ability estimates must be accompanied by standard errors to compute classification accuracy or consistency.")
  } 
  if (stat == "cc" | stat == "c" | stat == "consistency") { 
    stat <- "Consistency" 
  }
  if (stat == "ca" | stat == "a" | stat == "accuracy") { 
    stat <- "Accuracy" 
  }
  layout(matrix(c(1, 2), ncol = 2), c(rel.wdth[1], rel.wdth[2]), c(1, 1))
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
  cac.col <- sapply(cac$Conditional[[stat]], cacGradient, cp = colorblindFriendly)
  points(matrix(c(ab.est[, 1], sapply(ab.est[, 1], dnorm)), ncol = 2), pch = 19, col = cac.col)
  if (ci) {
    coords <- matrix(c(ab.est[, 1] - 1.96*ab.est[, 2], ab.est[, 1] + 1.96*ab.est[, 2], sapply(ab.est[, 1], dnorm)), ncol = 3)
    apply(coords, 1, function(x) {
      lines(x[-3], rep(x[3], 2), col = cacGradient(class.Rud(cutoff, ability = ((x[1] + x[2]) / 2), se = x[3])$Conditional[[stat]], 
                                                   colorblindFriendly), lwd = 2)
      lines(rep(x[1], 2), c(x[3] - (yRng[2] - yRng[1]) * .015, x[3] + (yRng[2] - yRng[1]) * .015), 
            col = cacGradient(class.Rud(cutoff, ability = ((x[1] + x[2]) / 2), se = x[3])$Conditional[[stat]], colorblindFriendly), lwd = 2)
      lines(rep(x[2], 2), c(x[3] - (yRng[2] - yRng[1]) * .015, x[3] + (yRng[2] - yRng[1]) * .015), 
            col = cacGradient(class.Rud(cutoff, ability = ((x[1] + x[2]) / 2), se = x[3])$Conditional[[stat]], colorblindFriendly), lwd = 2)
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
  par(mar = c(4, 1, 3, 3), las = 1, cex = 1)
  plot(NULL, xlim = c(0, 1), ylim = c(.5, 1), axes = FALSE, xlab = "", ylab = "")
  abline(h = seq(.5, 1, .0001), col = sapply(seq(.5, 1, .0001), cacGradient, cp = colorblindFriendly))
  axis(4, c(.5, .75, 1))
}
