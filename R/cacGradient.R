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
