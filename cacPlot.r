cacPlot <- function(x, mdl = "Rasch", cutoff = 0, cSEM = TRUE, xRng = c(-3, 3), yRng = c(0, 1), grid = TRUE, lgd = TRUE, grp = NULL, colorblindFriendly = FALSE) {
  library(mirt)
  library(cacIRT)
  if (class(x) == "SingleGroupClass") {
    mod <- x
  } else {
    if (class(x) == "data.frame" | class(x) == "matrix") {
      mod <- mirt(data = x, model = 1, itemtype = mdl)
    }
  }
  ab.est <- fscores(mod, method = "ML", response.pattern = mod@Data$data)[, c("F1", "SE_F1")]
  cac <- class.Rud(cutoff, ability = ab.est[, 1], se = ab.est[, 2], D = 1)
  cc.col <- rgb(
    red = if (cac$Conditional$Consistency[i] <= .75) {
      1
      } else {
        (1 - cac$Conditional$Consistency[i]) * 4
    },
    green = if (!colorblindFriendly) {
      if (cac$Conditional$Consistency[i] >= .75) {
        1
        } else {
          (cac$Conditional$Consistency[i] - .5) * 4
          }
      } else {
        0
    },
    blue = if (colorblindFriendly) {
      if (cac$Conditional$Consistency[i] >= .75) {
        1
      } else {
        (cac$Conditional$Consistency[i] - .5) * 4
        }
      } else {
        0
    }
  )
  ca.col <- rgb(
    red = if (cac$Conditional$Accuracy[i] <= .75) {
      1
      } else {
        (1 - cac$Conditional$Accuracy[i]) * 4
        },
    green = if (!colorblindFriendly) {
      if (cac$Conditional$Accuracy[i] >= .75) {
        1
        } else {
          (cac$Conditional$Accuracy[i] - .5) * 4
          }
      } else {
        0
        },
    blue = if (colorblindFriendly) {
      if (cac$Conditional$Accuracy[i] >= .75) {
        1
        } else {
          (cac$Conditional$Accuracy[i] - .5) * 4
          }
      } else {
        0
        }
    )
  plot(NULL, xlim = c(xRng[1], xRng[2]), ylim = c(yRng[1], yRng[2]), xlab = expression(Theta), ylab = "Density")
  if (grid) grid()
  if (cSEM) {
    par(new = TRUE)
    plot(seq(xRng[1], xRng[2], .001), 1 / sqrt(testinfo(mod, seq(xRng[1], xRng[2], .001))), 
         xlim = c(xRng[1], xRng[2]), ylim = c(yRng[1], yRng[2]), type = "l", lty = 2, lwd = 2, xlab = "", ylab = "")
  }
  abline(v = cutoff, lty = 3, lwd = 2)
  curve(dnorm(x), from = xRng[1], to = xRng[2], lwd = 2, add = TRUE)
  for (i in 1:nrow(ab.est)) {
    cc.col <- rgb(
      red = if (cac$Conditional$Consistency[i] <= .75) {
        1
      } else {
        (1 - cac$Conditional$Consistency[i]) * 4
      },
      green = if (!colorblindFriendly) {
        if (cac$Conditional$Consistency[i] >= .75) {
          1
        } else {
          (cac$Conditional$Consistency[i] - .5) * 4
        }
      } else {
        0
      },
      blue = if (colorblindFriendly) {
        if (cac$Conditional$Consistency[i] >= .75) {
          1
        } else {
          (cac$Conditional$Consistency[i] - .5) * 4
        }
      } else {
        0
      }
    )
    # Point estimate of Theta.
    points(ab.est[i, 1], dnorm(ab.est[i, 1]), pch = 19, col = cc.col)
    # 95% CI.
    lines(c(ab.est[i, 1] - 1.96*ab.est[i, 2], ab.est[i, 1] + 1.96*ab.est[i, 2]), rep(dnorm(ab.est[i, 1]), 2), lwd = 2, col = cc.col)
    lines(rep(ab.est[i, 1] - 1.96*ab.est[i, 2], 2), c(dnorm(ab.est[i, 1] - .025), dnorm(ab.est[i, 1] + .025)), lwd = 2, col = cc.col)
    lines(rep(ab.est[i, 1] + 1.96*ab.est[i, 2], 2), c(dnorm(ab.est[i, 1] - .025), dnorm(ab.est[i, 1] + .025)), lwd = 2, col = cc.col)
  }
  if (lgd) {
    legend("topleft", bty = "n", lty = c(1, 2, 3), lwd = c(2, 2, 2), pch = c(19, NA_integer_, NA_integer_), legend = c("Obs. w/ 95% CI", "cSEM", "Cutoff"), merge = TRUE) 
  }
}
