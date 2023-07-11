#' Levels of physical activity
#'
#' Determines how many moderate, vigrous, and moderate-vigrous physical activity
#' minutes occurred for a given interval.
#'
#' @name mpa_min_ap
#'
#' @param mets A numeric vectors of METs (metabolic equivalents).
#' @param epoch Length of interval, default is `1` second.
#'
#' @details These functions are similar to the
#' [activpalProcessing::mvpa.min.AP()] function but the METs cutoff values used
#' to define the levels of activity have be adjusted.
#' * METs between [3, 6) define moderate physical activity
#' * METs greater than or equal to 6 define vigrous physical activity
#' * METs greater than or equal to 3 define moderate-vigrous physical activity
#'
#' @return Numeric vector denoting the number of minutes.
#'
#' @examples
#' x <- runif(100, 1, 10)
#' mpa_min_ap(x)
#' vpa_min_ap(x)
#' mvp_min_ap(x)
#'
#' @rdname mpa_min_ap
#' @export mpa_min_ap
mpa_min_ap <- function(mets, epoch = 1) {
  mpa_mins <- sum((mets >= 3) & (mets < 6)) / (60 / epoch)
  if (is.na(mpa_mins)) {
    mpa_mins <- 0
  }
  return(mpa_mins)
}

#' @rdname mpa_min_ap
#' @export vpa_min_ap
vpa_min_ap <- function(mets, epoch = 1) {
  vpa_mins <- sum(mets >= 6) / (60 / epoch)
  if (is.na(vpa_mins)) {
    vpa_mins <- 0
  }
  return(vpa_mins)
}

#' @rdname mpa_min_ap
#' @export mvpa_min_ap
mvpa_min_ap <- function(mets, epoch = 1) {
  mvpa_mins <- sum(mets >= 3) / (60 / epoch)
  if (is.na(mvpa_mins)) {
    mvpa_mins <- 0
  }
  return(mvpa_mins)
}