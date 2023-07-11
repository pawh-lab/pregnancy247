#' Estimate the number of sit to stand transitions
#'
#' This function estimates the number of sit to stand transitions, where values
#' of `0` indicates sitting, `1` indicates standings, and `2` indicates
#' stepping.
#'
#' @param posture A numeric vector (in any EPOCH), where `0` indicates sitting,
#' `1` indicates standings, and `2` indicates stepping.
#'
#' @details This is essentially the same as [activpalProcessing::breaks.AP()];
#' however, if there are **NO* sit to stand transitations a value of `0` is
#' returned instead of `NA`.
#'
#' @return A numeric value indicating the number of breaks from sitting
#'
#' @examples
#' x <- sample(x = c(0, 1, 2), size = 100, replace = TRUE, prob = rep(1/3, 3))
#' breaks_ap(x)
#'
#' @export breaks_ap
breaks_ap <- function(posture) {
  y <- posture
  n <- length(y) #nolint
  mmm <- length(y)
  one <- y[-mmm]
  two <- y[-1]
  trans_up <- (one == 0) & (two != 0)
  num_up_ap <- sum(trans_up, na.rm = TRUE)
  if (num_up_ap == 0) {
    num_up_ap <- 0
  }
  return(num_up_ap = num_up_ap)
}