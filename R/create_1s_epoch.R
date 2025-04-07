#' Create a 1 second EPOCH data set
#'
#' Transforming the events data to a one second EPOCH data that contains
#' expanded metrics on physical activity.
#'
#' @param data Either a `process.data.events` or `merge.data.events` data set to
#' be converted to a one second EPOCH data set
#' @inheritParams merge_events
#'
#' @details The `cumulativesteps` and `methrs` variables are renamed to conform
#' to previous work done in this field.
#' * `cumulativesteps` to `steps`
#' * `methrs` to `met.hrs`
#'
#' One second, thirty second, and sixty second variants of MET hours are created
#' and named accordingly.
#' * `mets1` for one second
#' * `mets30` for thirty second
#' * `mets60` for sixty second
#'
#' @return A `one.epoch.data` object, which is a data.frame that has the same
#' variables as the `data` parameter plus `mets1`, `mets30`, `mets60`, and
#' * `date` which is just date version of the `time` variable and
#' * `ap.posture` which is a replicate of the `activity` variable to conform to
#' previous work.
#'
#' @examples
#' \dontrun{
#' process_dat <- process_events(data = dat)
#' sec_by_sec <- create_1s_epoch(data = process_dat)
#' }
#'
#' @export create_1s_epoch
create_1s_epoch <- function(data, good_days = 1:9, remove_days = FALSE) {
  # Checking parameters ####
  check1 <- "merge.data.events" %in% class(data)
  check2 <- "process.data.events" %in% class(data)
  if (!(check1 || check2)) {
    stop("data must be either a process.data.event or merge.data.events object")
  }

  if (!all(is.integer(good_days))) {
    stop("good_days must be integer values denoting valid wear days")
  }

  if (!is.logical(remove_days)) {
    stop("remove_days must be a logical indicating removal of invalid days")
  }

  # Creating 1 second epoch data set ####

  ## Find the initial starting time ####
  start_time <- strptime(data$time[1], format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

  ## Creating time variables ####
  ## for various EPOCH files based on different number of seconds
  secs <- as.numeric((max(data$time) - min(data$time)) * 86400)
  times <- as.POSIXct(start_time + (0:secs), tz = "UTC")

  ## Creating a basis 1 second epoch dataset ####
  ## This dataset will use the event data converted to a sec by sec dataset
  sec_by_sec <- data.frame(
    event = NA,
    time = lubridate::round_date(times, unit = "second"),
    datacount = NA,
    interval = NA,
    activity = NA,
    cumulativesteps = NA,
    methrs = NA,
    wear_day = NA,
    sleep_loop = NA,
    nap_loop = NA,
    wake_loop = NA,
    work_loop = NA
  )
  sec_by_sec$time <- as.POSIXct(sec_by_sec$time, tz = "UTC")

  ## Creating then second by second dataset ####
  ## Note that all the time in the events file will be duplicated since the sec
  ## by sec dataset is based on the events file.
  ## Considering there is duplication, the duplicates will need to be removed.
  sec_by_sec <- dplyr::bind_rows(data, sec_by_sec)
  sec_by_sec <- sec_by_sec[order(sec_by_sec$time, sec_by_sec$datacount), ]
  hold1 <- (!duplicated(sec_by_sec$time) | !is.na(sec_by_sec$event))
  sec_by_sec <- sec_by_sec[hold1, ]
  sec_by_sec <- sec_by_sec[order(sec_by_sec$time, sec_by_sec$datacount), ]

  ## Cleaning up second by second data ####

  ### Removing previous row numbers ####
  rownames(sec_by_sec) <- NULL

  ### Filling in missing values ####
  sec_by_sec <- tidyr::fill(
    sec_by_sec,
    event,
    datacount,
    interval,
    activity,
    cumulativesteps,
    methrs,
    wear_day,
    sleep_loop,
    nap_loop,
    wake_loop,
    work_loop,
    .direction = "down"
  )

  ### Renaming some variables ####
  sec_by_sec <- dplyr::rename(
    sec_by_sec,
    steps = cumulativesteps,
    met.hours = methrs
  )

  ### Calculating variants of MET hours ####
  n <- as.numeric(nrow(sec_by_sec))

  #### 1 second met values ####
  sec_by_sec$mets1 <- (sec_by_sec$met.hours * 3600) / sec_by_sec$interval
  sec_by_sec <- dplyr::relocate(sec_by_sec, mets1, .after = met.hours)

  #### 30-second epoch ####
  condition30 <- lubridate::second(sec_by_sec$time) == 0 | lubridate::second(sec_by_sec$time) == 30
  sec_by_sec$epoch_30 <- NA
  sec_by_sec$epoch_30[1] <- 0
  sec_by_sec$epoch_30[condition30] <- seq_len(sum(condition30))
  sec_by_sec <- tidyr::fill(sec_by_sec, epoch_30, .direction = "down")
  
    ### 30-second met values ###
    met30 <- aggregate(sec_by_sec$mets1, list(group = sec_by_sec$epoch_30), FUN = "mean")
    sec_by_sec$mets30 <- NA
    sec_by_sec$mets30 <- met30$x[match(sec_by_sec$epoch_30, met30$group)]
    sec_by_sec <- dplyr::relocate(sec_by_sec, mets30, .after = mets1)

  #### 60-second epoch ####
  condition60 <- lubridate::second(sec_by_sec$time) == 0
  sec_by_sec$epoch_60 <- NA
  sec_by_sec$epoch_60[1] <- 0
  sec_by_sec$epoch_60[condition60] <- seq_len(sum(condition60))
  sec_by_sec <- tidyr::fill(sec_by_sec, epoch_60, .direction = "down")
  sec_by_sec <- dplyr::relocate(sec_by_sec, epoch_60, .after = epoch_30)
  
    ### 60-second met values ###
    met60 <- aggregate(sec_by_sec$mets1, list(group = sec_by_sec$epoch_60), FUN = "mean")
    sec_by_sec$mets60 <- NA
    sec_by_sec$mets60 <- met60$x[match(sec_by_sec$epoch_60, met60$group)]
    sec_by_sec <- dplyr::relocate(sec_by_sec, mets60, .after = mets30)
  
  #### Rounding met values
  sec_by_sec$mets1 <- signif(sec_by_sec$mets1, 3)
  sec_by_sec$mets30 <- signif(sec_by_sec$mets30, 3)
  sec_by_sec$mets60 <- signif(sec_by_sec$mets60, 3)

  #### Adding date variable ####
  sec_by_sec$date <- as.Date(
    x = substring(format(sec_by_sec$time), 1, 10),
    format = "%Y-%m-%d"
  )
  sec_by_sec <- dplyr::relocate(sec_by_sec, date, .after = time)

  #### Adding an activity variable ####
  sec_by_sec$ap.posture <- sec_by_sec$activity
  sec_by_sec <- dplyr::relocate(sec_by_sec, ap.posture, .before = activity)

  # Returning 1-second epoch data set ####
  ## Excluding invalid wear days ####
  if (remove_days) {
    sec_by_sec <- subset(sec_by_sec, subset = wear_day %in% good_days)
  }
  class(sec_by_sec) <- c("one.epoch.data", "data.frame")
  return(sec_by_sec)
}
