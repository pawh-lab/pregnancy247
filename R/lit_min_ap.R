#' Estimate time in light intensity activity
#'
#' This function estimates time spent in light intensity activity. Light
#' intensity activity is activity in the standing or stepping posture.
#'
#' @param mets A numeric vector of MET values.
#' @param posture A numeric vector of values `0`-`2` indicating: `0` for
#' sitting, `1` for standing, and `2` for stepping.
#' @param epoch A numeric value indicating what epoch (in seconds) the data are
#' in (e.g. `60` = 1 minute epochs). The default value is `1` second.
#'
#' @details This function is an adaptation of
#' [activpalProcessing::lit.min.AP()], where the dependencies on certain MET
#' values to determine standing and stepping is excluded.
#'
#' @return A numeric value indicating hours spent in light intensity activity.
#'
#' @examples
#' set.seed(1997)
#' n <- 100
#' mets <- stats::rgamma(n = n, shape = 2)
#' posture <- rep(NA, n)
#' for (i in seq_along(posture)) {
#'   if (mets[i] < 1.5) {
#'     posture[i] <- sample(c(0, 1), 1, TRUE, c(0.75, 0.25))
#'   } else if (mets[i] >= 1.5 && mets[i] < 3) {
#'     posture[i] <- sample(c(0, 1, 2), 1, TRUE, c(0.25, 0.5, 0.25))
#'   } else {
#'     posture[i] <- sample(c(1, 2), 1, TRUE, c(0.25, 0.75))
#'   }
#' }
#' lit_min_ap(mets = mets, posture = posture)
#' @export lit_min_ap
lit_min_ap <- function(mets, posture, epoch = 1) {
  lit_mins_temp <- sum((posture == 2) & (mets  < 3)) / (60 / epoch)
  lit_mins_temp <- lit_mins_temp +     
    sum((posture == 0) & (mets  < 3) & (mets >= 1.5)) / (60 / epoch)
  lit_mins_stand <- sum(posture == 1 & (mets  < 3)) / (60 / epoch)
  lit_mins <- lit_mins_temp + lit_mins_stand
  if (lit_mins == 0) {
    lit_mins <- NA
  }
  return(lit_mins)
}

