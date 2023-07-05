process_events <- function(data) {
  if (!("data.events" %in% class(data))) {
    stop("data must be a data.events object imported by read_events")
  }

  # Arrange variables ####
  dat <- dplyr::bind_cols(
    data[, c(1, 3, 5, 4, 7, 8, 9, 10, 11)],
    data[, c(2, 6, 12, 13, 14, 15, 16, 17)]
  )

  # Select useable variables ####
  dat <- dat[, 1:6]
  names(dat) <- c(
    "time", "datacount", "interval",
    "activity", "cumulativesteps", "methrs"
  )

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