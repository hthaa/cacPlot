#' Confusion Matrix for a test.
#'
#' @description Given a set of scores, their standard errors of measurement and the cutoff value of a test, calculate a confusion matrix indicating true and false positives and negatives under the assumption that sampling errors distribute normally around true-values.
#' @param score A vector of test-scores.
#' @param se A vector of standard errors associated with the test-scores.
#' @param cut A value indicating the cutoff-score of the test.
#' @return A confusion matrix indicating the true- and false positive and negative rates of a test, given a specific cut-score and under the assumption that sampling errors distribute normally around true-values.
#' @references Rudner, L. (2005). Expected Classification Accuracy. Practical Assessment, Research and Evaluation. 10(13), p. 1-5.
#' @export

cmat <- function(score, se, cut) {
  fn <- sum(apply(cbind(score[score < cut], se[score < cut]), 1, function(x) { 1 - pnorm(cut, x[1], x[2]) })) / length(score < cut)
  tp <- sum(apply(cbind(score[score < cut], se[score < cut]), 1, function(x) { pnorm(cut, x[1], x[2]) })) / length(score < cut)
  fp <- sum(apply(cbind(score[score >= cut], se[score >= cut]), 1, function(x) { pnorm(cut, x[1], x[2]) })) / length(score >= cut)
  tn <- sum(apply(cbind(score[score >= cut], se[score >= cut]), 1, function(x) { 1 - pnorm(cut, x[1], x[2]) })) / length(score >= cut)
  matrix(c(tp, fp, tp + fp, tn, fn, tn + fn, tp + tn, fp + fn, tp + fp + tn + fn), nrow = 3,
         dimnames = list(c("True", "False", "Total"), c("Positive", "Negative", "Total")))
}
