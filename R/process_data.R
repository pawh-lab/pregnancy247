process_data <- function(
  subject, trimester, sleep_source,
  day1 = TRUE, day2 = TRUE, day3 = TRUE,
  day4 = TRUE, day5 = TRUE, day6 = TRUE,
  day7 = TRUE, day8 = TRUE, day9 = TRUE
) {
  # Check R version ####
  if (as.integer(R.version$major) != 4 && as.numeric(R.version$minor) < 1) {
    stop("R version must be 4.1 or higer.")
  }

  # Check current working directory ####
  check_dir()

  # Check parameter values ####
  if (!is.character(subject)) {
    stop("subject must be a character string denoting subject ID.")
  }
  if (nchar(trimester) != 1 || !(as.integer(trimester) %in% c(1, 2, 3))) {
    stop("trimester must be either 1, 2, 3 denoting pregnancy trimester.")
  }
  valid_days <- c(day1, day2, day3, day4, day5, day6, day7, day8, day9)
  if (!all(is.logical(valid_days))) {
    stop("day(i) must be a logical denoting valid processing day.")
  }
  if (!is.character(sleep_source)) {
    stop("sleep_source must be a character file name for the sleep data.")
  }

  # Import raw data ####

  ## Actiwatch and sleep diary data ####
  sleep <- read_sleep(
    file = sleep_source, subject = subject, trimester = trimester
  )

  ## activPAL (events file) data ####
  dat <- read_events(subject = subject, trimester = trimester)

  # Procssing of imported data ####

  ## Setting subject ID of interest for processing
  subject_id <- paste0(subject, trimester)

  ## Determining which days have valid data
  names(valid_days) <- paste0("day", 1:9)
  ### Listing the good days of data
  good_days <- which(valid_days)
  ### Creating a vector of all the days
  all_days <- 1:9
  names(all_days) <- paste0("day", all_days)

  ## Actiwatch and sleep diary data ####

  ### Checking if shift-worker ####
  if (!is.na(sleep$diary_24nosleep)) {
    if (sleep$diary_24nosleep == 1) {
      resp <- menu(
        choices = c("Yes", "No"),
        graphics = TRUE,
        title = paste0(
          subject_id, " is a shift worker or went more than",
          " 24 hours without sleep.",
          "\nWould you like to stop the data processing?"
        )
      )
      resp <- ifelse(resp == 1L, "Yes", "No")
      if (resp == "Yes") {
        stop(
          paste0(
            "Data processing stopped because ", subject_id,
            " is a shift worker or went more than 24 hours without sleep.",
            "\nFurther manual data processing is needed."
          )
        )
      }
    }
  }

  ### Diary dates ####
  diary_dates <- unlist(sleep[1, grep("^diary_date", colnames(sleep))])
  diary_dates <- as.character(as.Date(diary_dates, format = "%m/%d/%Y"))
  na_dates <- !is.na(diary_dates) # this denotes whether a date is not missing
  diary_dates <- diary_dates[na_dates]

  ### Windows ###
  ### Finding windows of certain activities recorded by the subject or Actiwatch

  #### Sleep ####
  wind_sleep <- windows_sleep(sleep = sleep)
  sleep_start <- wind_sleep$start
  sleep_end <- wind_sleep$end

  #### Nap ####
  wind_nap <- windows_nap(sleep = sleep)
  nap_start <- wind_nap$start
  nap_end <- wind_nap$end

  #### Work ####
  wind_work <- windows_work(sleep = sleep)
  work_start <- wind_work$start
  work_end <- wind_work$end

  #### Monitor off ####
  wind_monitor <- windows_monitor(sleep = sleep)
  monitor_off <- wind_monitor$start
  monitor_on <- wind_monitor$end

  ### Preparing sleep data for merging ####

  #### Creating temporary data set based on sleep data ####
  temp_sleep <- data.frame(
    time = c(
      sleep_start, sleep_end,
      nap_start, nap_end,
      work_start, work_end,
      monitor_off, monitor_on,
      sleep_end + 1, nap_end + 1
    ),
    datacount = NA,
    interval = NA,
    activity = NA,
    cumulativesteps = NA,
    methrs = NA
  )

  #### Loop variables ####
  nr_sleep <- nrow(temp_sleep)
  len_sleep <- length(c(sleep_start, sleep_end))
  len_nap <- length(c(nap_start, nap_end))
  ##### Wear day ####
  temp_sleep$wear_day <- c(
    all_days[-1],
    rep(NA, (nr_sleep - length(all_days[-1])))
  )
  ##### Sleep loop ####
  temp_sleep$sleep_loop <- c(
    rep(1, len_sleep),
    rep(0, nr_sleep - len_sleep)
  )
  ##### Nap loop ####
  temp_sleep$nap_loop <- c(
    rep(0, len_sleep),
    rep(1, len_nap),
    rep(0, nr_sleep - (len_sleep + len_nap))
  )
  ##### Wake loop ####
  temp_sleep$wake_loop <- c(
    rep(0, len_sleep + len_nap),
    rep(1, nr_sleep - (len_sleep + len_nap))
  )
  ##### Work loop ----
  temp_sleep$work_loop <- c(
    rep(0, len_sleep + len_nap),
    rep(1, length(c(work_start, work_end))),
    rep(0, length(c(monitor_off, monitor_on, sleep_end, nap_end)))
  )

  #### Removing NA times ####
  temp_sleep <- temp_sleep[!is.na(temp_sleep$time), ]
  temp_sleep <- temp_sleep[order(temp_sleep$time), ]

  ## activPAL (events) data ####
  process_dat <- process_events(data = dat)

  # Merge data ####

  ## Creating start time variable ####
  start_time <- as.POSIXct(
    paste(
      diary_dates[1],
      paste0(
        substr(sleep$diary_time1, 1, 2), ":",
        substr(sleep$diary_time1, 4, 5)
      )
    ),
    tz = "UTC"
  )

  ## Creating end time variable ####
  end_time <- sleep_end[!is.na(sleep_end)]
  end_time <- end_time[length(end_time)]

  ## Merging sleep data with ActivPal data ####
  merge_dat <- merge_events(
    data = process_dat, add_events = temp_sleep,
    start_time = start_time, end_time = end_time,
    off_times = wind_monitor, good_days = good_days,
    remove_days = TRUE
  )

  # Creating 1 second epoch data set ####
  sec_by_sec <- create_1s_epoch(
    data = merge_dat, good_days = good_days, remove_days = TRUE
  )

  # Remove invalid wear days from events data set ####
  # This is being done now because we needed the invalid days to create the
  # 1 second epoch data set.
  merge_dat <- subset(merge_dat, subset = wear_day %in% good_days)

  # Graph activity ####
  ## Collecting all the sleeping and napping times ####
  sleep_times <- data.frame(
    sleep_start = c(
      sleep_start[-length(sleep_start)],
      nap_start[-length(nap_start)]
    ),
    sleep_stop = c(
      sleep_end[-length(sleep_end)],
      nap_end[-length(nap_end)]
    ),
    day = c(all_days[-1], all_days[-length(all_days)]),
    label = c(
      rep("sleep", length(all_days[-1])),
      rep("nap", length(all_days[-length(all_days)]))
    )
  )
  ## Removing any of sleeping/napping times that occurred on invalid days
  sleep_times <- subset(sleep_times, subset = day %in% good_days)

  ## Obtaining plot ####
  g <- plot_1s_epoch(data = sec_by_sec, sleep_times = sleep_times)

  # Calculating activity measures ####
  # Resulting data set will be called variables
  summ_daily <- summarize_daily(
    subject = subject, trimester = trimester,
    data_events = merge_dat, data_1s_epoch = sec_by_sec,
    all_days = all_days, good_days = good_days
  )
  variables <- summ_daily$daily
  avgs <- summ_daily$avgs

  # Exporting ####
  write_events(x = merge_dat, subject = subject, trimester = trimester)
  write_1s_epoch(x = sec_by_sec, subject = subject, trimester = trimester)
  write_daily(x = variables, subject = subject, trimester = trimester)
  write_avgs(x = avgs, subject = subject, trimester = trimester)
  write_all_avgs(x = avgs, subject = subject, trimester = trimester)
  save_plot_1s_epoch(
    x = g, subject = subject, trimester = trimester,
    width = 11, height = 8.5
  )
}