create_1s_epoch <- function(data, good_days = 1:9, remove_days = FALSE) {
  # Checking parameters ####
  if (!("merge.data.events" %in% class(data))) {
    stop("data must be a merge.data.events object created by merge_events")
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
    time = lubridate::round_date(times, units = "second"),
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
  sec_by_sec$met.hours <- sec_by_sec$met.hours / sec_by_sec$interval
  sec_by_sec$mets1 <- (sec_by_sec$met.hours * 3600) / sec_by_sec$interval
  sec_by_sec <- dplyr::relocate(sec_by_sec, mets1, .after = met.hours)

  #### 30 second met values ####
  time30 <- start_time + (30 * rep(0:floor((n / 30)), each = 30, length = n))
  sec_by_sec$mets30 <- rep(
    tapply(sec_by_sec$mets1, time30, mean), each = 30,
    length = n
  )
  sec_by_sec <- dplyr::relocate(sec_by_sec, mets30, .after = mets1)

  #### 60 second met values ####
  time60 <- start_time + (60 * rep(0:floor((n / 60)), each = 60, length = n))
  sec_by_sec$mets60 <- rep(
    tapply(sec_by_sec$mets1, time60, mean), each = 60,
    length = n
  )
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

  # Returning 1 second epoch data set ####
  ## Excluding invalid wear days ####
  if (remove_days) {
    sec_by_sec <- subset(sec_by_sec, subset = wear_day %in% good_days)
  }
  class(sec_by_sec) <- c("one.epoch.data", "data.frame")
  return(sec_by_sec)
}