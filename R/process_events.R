#' Manipulate the events data recorded by activPAL
#'
#' Manipulate and clean the events data record by activPAL and imported by
#' `read_events()`.
#'
#' @param data A `data.events` object produced by `read_events()`.
#'
#' @details This functions rearranges the variables produced by the activPAL
#' software and selects the variables related to:
#' * `time`,
#' * `datacount`,
#' * `interval`,
#' * `activity`,
#' * `cummulativesteps`, and
#' * `methrs`.
#'
#' The naming of variables is based on the archived package named
#' [`activpalProcessing`](https://cran.r-project.org/src/contrib/Archive/activpalProcessing/)
#' created by [Kate Lynden](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5469371/).
#'
#' Beyond variable selection, this function specifically:
#' * doubles the `cumulativesteps` as the activPAL device is only on one leg,
#' * transforms the `time` variable to a date-time (POSIXct) numeric vector
#' based on the UTC time-zone, and
#' * creates several new variables to indicate:
#'    * wear day based on sleep onset (`wear_day`),
#'    * sleep windows (`sleep_loop`),
#'    * nap windows (`nap_loop`),
#'    * wake windows (`wake_loop`), and
#'    * work windows (`work_loop`)
#'
#' that are all missing values to be filled in later by the appropriate data.
#'
#' @return A `process.data.events` object, which is a `data.frame` containing
#' the variables discussed in the **Details**.
#'
#' @seealso [lubridate::with_tz()]
#'
#' @examples
#' \dontrun{
#' dat <- read_events("0001-AB", "3")
#' process_dat <- process_events(data = dat)
#' }
#'
#' @export process_events
process_events <- function(data) {
  if (!("data.events" %in% class(data))) {
    stop("data must be a data.events object imported by read_events")
  }

  # Select useable variables & rename it ####
  dat <- data[, c("Time", "Data.Count", "Duration..s.", "Event.Type", "Cumulative.Step.Count", "Activity.Score..MET.h.")]
  names(dat) <- c("time", "datacount", "interval", "activity", "cumulativesteps", "methrs")

  # Updating cumulative steps ####
  # Doubling the number of cumulative steps because the ActivPal is only placed
  # on one leg, so accounting for the other leg steps is needed.
  dat$cumulativesteps <- c(dat$cumulativesteps) * 2

  # Adjusting the way time is expressed ####
  dat$time <- as.POSIXct(as.Date(dat$time, origin = "1899-12-30"))
  dat$time <- lubridate::with_tz(dat$time, "UTC")

  # Adding empty loop variables ####
  dat$wear_day <- NA
  dat$sleep_loop <- NA
  dat$nap_loop <- NA
  dat$wake_loop <- NA
  dat$work_loop <- NA

  # Returning processed data set ####
  class(dat) <- c("process.data.events", "data.frame")
  return(dat)
}
