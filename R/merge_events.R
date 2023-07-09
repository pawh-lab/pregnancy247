#' Merge event data together
#'
#' Merge events data set produced by [process_events()] with any extra events
#' that needed to be included by an external source, such as the Actiwatch and
#' sleep diary.
#' 
#' @param data A `process.data.events` object produced by [process_events()]
#' @param add_events A `data.frame` of events to merge with `data` that has the
#' same variable names as `data`.
#' @param start_time A date-time (POSIXct) value that denotes first date and
#' time that should be recorded in merged data set. Default is `NA` to indicate
#' that the merge data set does not need to be subsetted.
#' @param end_time A date-time (POSIXct) value that denotes last date and time
#' that should be recorded in merged data set. Default is `NA` to indicate
#' that the merge data set does not need to be subsetted.
#' @param off_times A `list` containing two objects of equal length denoting the
#' beginning and ending times of when the monitors were removed. For the
#' Pregnancy 24/7 study, the [windows_monitor()] function was used to find these
#' times. Default is `NULL`, which indicates that is no monitor off times.
#' @param good_days An integer vector denoting the wear days that have valid
#' data. The default is `1:9`, as for the Pregnancy 24/7 study the wear period
#' last nine days.
#' @param remove_days A logical denoting if merge data set should be subsetted
#' by the `good_days`. Default is FALSE.
#'
#' @details After `data` and `add_events` have been merged together, all missing
#' values created by merging the two data set together are filled in by their
#' appropriate values. For the looping variables, usually, the last value is
#' carried forwarded. As for the activity variables, it depends on how the
#' original variable was created as for how the missing values are filled in.
#'
#' If there are monitor off times, the looping variables are replaced with the
#' value of `99` to indicate a special type of missing values related to
#' monitor wear.
#'
#' There are two more objectives of this function.
#' 1. Round the `time` variable to the nearest second with
#' [lubridate::round_date()] and
#' 2. create a new variable called `event` that denotes the event in recorded.
#'
#' @return A `merge.data.events` object, which is a `data.frame` with all of the
#' variables in `data` plus the `event` variable.
#'
#' @seealso [lubridate::round_date()]
#'
#' @examples
#' # See process_data() for an example of how this function is used.
#'
#' @export merge_events
merge_events <- function(
  data, add_events, start_time = NA, end_time = NA, off_times = NULL,
  good_days = 1:9, remove_days = FALSE
) {

  # Checking parameters ####
  if (!("process.data.events" %in% class(data))) {
    stop("data is not a process.data.events object which is needed")
  }

  if (!all(colnames(add_events) %in% colnames(data))) {
    stop("add_events does not contain all the necessary variables for merging")
  }

  if (!is.na(start_time)) {
    if (!lubridate::is.POSIXct(start_time) || length(start_time) != 1) {
      stop("start_time must be a date-time object of length 1")
    }
  }

  if (!is.na(end_time)) {
    if (!lubridate::is.POSIXct(end_time) || length(end_time) != 1) {
      stop("end_time must be a date-time object of length 1")
    }
  }

  if (!is.null(off_times)) {
    if (!("list" == class(off_times)) || length(off_times) != 2) {
      stop("off_times must be a list of length 2 denoting monitor off/on times")
    }
  }

  if (!all(is.integer(good_days))) {
    stop("good_days must be integer values denoting valid wear days")
  }

  if (!is.logical(remove_days)) {
    stop("remove_days must be a logical indicating removal of invalid days")
  }

  # Merging sleep data with ActivPal data
  col_order <- c(
    "time", "datacount", "interval", "activity", "cumulativesteps", "methrs",
    "wear_day", "sleep_loop", "nap_loop", "wake_loop", "work_loop"
  )
  add_events <- add_events[, col_order]
  dat <- dplyr::bind_rows(data, add_events)
  dat <- dat[order(dat$time, dat$datacount), ]
  dat <- dat[!duplicated(dat$time), ]
  dat <- dat[order(dat$time, dat$datacount), ]

  ## Removing times occur before and after wear period ####
  ## Removing the first rows of data if it occurred before the monitors were
  ## initial put on
  if (!is.na(start_time)) {
    dat <- subset(dat, subset = time >= start_time)
  }
  ## Subsetting the data to exclude any data that comes after time a subject
  ## wakes up on the final day of the wear period.
  if (!is.na(end_time)) {
    dat <- subset(dat, subset = time <= end_time)
  }

  # Fill in missing values ####
  t <- dim(dat)[1]

  ## Loop variables ####

  ### Wear day ####
  if (sum(is.na(dat$wear)) != 0) {
    dat <- tidyr::fill(dat, wear_day, .direction = "down")
    ### Day 1 before first possible sleep ####
    dat$wear_day[is.na(dat$wear_day)] <- 1
  }

  ### Sleep loop  ####
  if (sum(is.na(dat$sleep_loop)) != 0) {
    dat <- tidyr::fill(dat, sleep_loop, .direction = "down")
    ### Day 1 before first possible sleep ####
    dat$sleep_loop[is.na(dat$sleep_loop)] <- 0
  }

  ### Nap loop ####
  if (sum(is.na(dat$nap_loop)) != 0) {
    dat <- tidyr::fill(dat, nap_loop, .direction = "down")
    ### Day 1 before first possible sleep ####
    dat$nap_loop[is.na(dat$nap_loop)] <- 0
  }

  ### Wake loop ####
  if (sum(is.na(dat$wake_loop)) != 0) {
    dat <- tidyr::fill(dat, wake_loop, .direction = "down")
    ### Day 1 before first possible sleep ####
    dat$wake_loop[is.na(dat$wake_loop)] <- 1
  }

  ### Work loop ####
  if (sum(is.na(dat$work_loop)) != 0) {
    dat <- tidyr::fill(dat, work_loop, .direction = "down")
    ### Day 1 before first possible sleep ####
    dat$work_loop[is.na(dat$work_loop)] <- 0
  }

  ### Adjusting to indicate that the monitor was taken off ####
  if (!is.null(off_times)) {
    if (all(off_times[[1]] < off_times[[2]])) {
      monitor_off <- off_times[[1]]
      monitor_on <- off_times[[2]]
    } else {
      monitor_off <- off_times[[2]]
      monitor_on <- off_times[[1]]
    }
    hold1 <- monitor_off[!is.na(monitor_off) & !is.na(monitor_on)]
    hold2 <- monitor_on[!is.na(monitor_off) & !is.na(monitor_on)]
    if (length(hold1) > 0) {
      for (k in seq_along(hold1)) {
        for (j in seq_len(nrow(dat))) {
          if (dat$time[j] >= hold1[k] && dat$time[j] <= hold2[k]) {
            dat$sleep_loop[j] <- 99
            dat$nap_loop[j] <- 99
            dat$wake_loop[j] <- 99
          }
        }
      }
    }
  }

  ## Events data ####

  ### Interval ####
  for (j in seq_along(dat$time)) {
    if (is.na(dat$interval[j])) {
      dat$interval[j] <- as.numeric(
        difftime(dat$time[j + 1], dat$time[j],
        units = "secs"
        )
      )
      dat$interval[j - 1] <- as.numeric(
        difftime(dat$time[j], dat$time[j - 1],
        units = "secs"
        )
      )
    } else {
      dat$interval[j] <- dat$interval[j]
    }
  }

  #### Last observation ####
  dat$interval[t] <- 1

  ### Met hours ####
  for (j in seq_along(dat$time)) {
    if (is.na(dat$methrs[j]) && (dat$wear_day[j] %in% good_days)) {
      mets <- dat$methrs[j - 1] / (dat$interval[j - 1] / 3600)
      first_position_time <- as.numeric(
        difftime(dat$time[j], dat$time[j - 1],
        units = "hours"
        )
      )
      second_position_time <- as.numeric(
        difftime(dat$time[j + 1], dat$time[j],
        units = "hours"
        )
      )
      totaltime <- dat$interval[j - 1]
      first_factor <- (first_position_time / totaltime) * mets
      second_factor <- (second_position_time / totaltime) * mets
      dat$methrs[j - 1] <- first_factor * first_position_time
      dat$methrs[j] <- second_factor * second_position_time
    } else {
      dat$methrs[j] <- dat$methrs[j]
    }
  }

  #### Last observation ####
  dat$methrs[t] <- dat$methrs[t - 1]

  ### Cumulative steps ####
  for (j in seq_along(dat$cumulativesteps)) {
    if (is.na(dat$cumulativesteps[j]) && (dat$wear_day[j] %in% good_days)) {
      dat$cumulativesteps[j] <- dat$cumulativesteps[j - 1]
    } else {
      dat$cumulativesteps[j] <- dat$cumulativesteps[j]
    }
  }

  ### Activity ####
  dat$activity[is.na(dat$activity)] <- 0

  ### Data count ####
  for (j in seq_along(dat$datacount)) {
    if (is.na(dat$datacount[j]) && (dat$wear_day[j] %in% good_days)) {
      dat$datacount[j] <- dat$datacount[j - 1] + (dat$interval[j - 1] * 10)
    } else {
      dat$datacount[j] <- dat$datacount[j]
    }
  }

  ## Adjusting con-current event times ####
  ## This is being done to create matching times within a 1 second interval
  dat$time <- lubridate::round_date(dat$time, unit = "second")
  dat$time <- as.POSIXct(dat$time, tz = "UTC")

  ## Numbering the events ####
  dat$event <- seq_along(nrow(dat))
  dat <- dplyr::relocate(dat, event, .before = time)

  # Returning merged data set ####
  ## Excluding invalid wear days ####
  if (remove_days) {
    dat <- subset(dat, subset = wear_day %in% good_days)
  }
  class(dat) <- c("merge.data.events", "data.frame")
  return(dat)
}