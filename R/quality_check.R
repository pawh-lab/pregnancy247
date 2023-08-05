#' Outlier screening for sleep or nap Actiwatch data
#'
#' Importing, screening, and exporting sleep or nap Actiwatch data.
#'
#' @inheritParams read_msleep
#' @param nap Logical denoting if nap data is to be imported and screened.
#' Default is FALSE.
#' @inheritParams check_msleep
#'
#' @details If nap data is being screened then sleep data is imported as well
#' for checking of overlapping nap and sleep windows. This function relies
#' soley on the other `msleep` functions. So, for further details, see the
#' documentation of the functions listed below.
#'
#' @seealso [read_msleep()], [check_msleep()], [write_msleep()],
#' [write_all_msleep()]
#'
#' @examples
#' \dontrun{
#' quality_check(subject = "1000-AB", trimester = 1)
#' }
#' @export quality_check
quality_check <- function(subject, trimester, nap = FALSE, cutpoint = 30) {
  # Importing data ####
  dat <- read_msleep(subject = subject, trimester = trimester, nap = nap)

  # Checking data ####
  if (nap) {
    msleep <- read_msleep(subject = subject, trimester = trimester, nap = FALSE)
    dat <- check_msleep(
      x = dat, y = msleep,
      nap = nap, cutpoint = cutpoint
    )
  } else {
    dat <- check_msleep(x = dat, cutpoint = cutpoint)
  }

  # Exporting ####
  write_msleep(x = dat, subject = subject, trimester = trimester, nap = nap)
  write_all_msleep(x = dat, subject = subject, trimester = trimester, nap = nap)
}