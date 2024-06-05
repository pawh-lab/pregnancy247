#' Summarize daily activity
#'
#' The activities during the wear period are summarized using events and 1
#' second EPOCH data for the `subject` of interest during the `trimester` of
#' interest.
#'
#' @inheritParams read_sleep
#' @param data_events Either a `process.data.events` or `merge.data.events` data
#' set.
#' @param data_1s_epoch A `one.epoch.data` object created by
#' [create_1s_epoch()].
#' @param all_days An integer vector denoting the all the possible wear days.
#' The default is 1:9, as for the Pregnancy 24/7 study the wear period last nine
#' days.
#' @inheritParams create_1s_epoch
#'
#' @details See the vingettes for a full description of the variables created.
#'
#' @return A named `list` with objects:
#' * `daily` a `data.frame` containing daily summary of activities for all the
#' possible wear days
#' * `avgs` a `data.frame` containing the averages of the activities during the
#' wear period.
#'
#' @examples
#' \dontrun{
#' process_dat <- process_events(data = dat)
#' sec_by_sec <- create_1s_epoch(data = process_dat)
#' summ_daily <- summarize_daily(
#'  subject = "0001-AB", trimester = "1",
#'  data_events = process_dat, data_1s_epoch = sec_by_sec
#' )
#' }
#'
#' @export summarize_daily
summarize_daily <- function(
  subject, trimester, data_events, data_1s_epoch,
  all_days = 1:9, good_days = 1:9
) {

  # Checking parameter values ####
  check1 <- "merge.data.events" %in% class(data_events)
  check2 <- "process.data.events" %in% class(data_events)
  if (!(check1 || check2)) {
    stop("data must be either a process.data.event or merge.data.events object")
  }

  if (!("one.epoch.data" %in% class(data_1s_epoch))) {
    stop("data must be a one.epoch.data produced by create_1s_epoch")
  }

  cap_wear_days <- base::intersect(
    data_events$wear_day, data_1s_epoch$wear_day
  )
  if (!all(unique(data_events$wear_day) %in% cap_wear_days)) {
    stop("Missing wear days in data_events compared to data_1s_epoch")
  }

  if (!all(unique(data_1s_epoch$wear_day) %in% cap_wear_days)) {
    stop("Missing wear days in data_1s_epoch compared to data_events")
  }

  if (!all(unique(good_days) %in% cap_wear_days)) {
    stop("Valid wear days must match to data_events and data_")
  }

  # Splitting the second by second and activity data into wear day data ####
  # during the awake periods

  ## Sec by sec data ####
  day_total <- split(
    x = data_1s_epoch[data_1s_epoch$wake_loop != 99, ],
    f = data_1s_epoch$wear_day[data_1s_epoch$wake_loop != 99]
  )
  day <- lapply(
    X = seq_along(day_total),
    FUN = function(j) {
      day_total[[j]][day_total[[j]]$wake_loop == 1, ]
    }
  )
  names(day) <- good_days

  ## activPAL (events) data ####
  day_dat <- split(
    x = data_events,
    f = data_events$wear_day
  )
  day_dat <- lapply(
    X = seq_along(day_dat),
    FUN = function(j) {
      day_dat[[j]][day_dat[[j]]$wake_loop == 1, ]
    }
  )
  names(day_dat) <- good_days

  # Setting up resulting data.frame ####
  variables <- data.frame(wear_day = good_days)

  # Obtaining activity variables ####

  ## Calendar variables ####
  ### Day ####
  #### Number of days wearing the device
  variables$days <- rep(
    difftime(
      data_events[length(data_events$time), ]$time, data_events[1, ]$time,
      units = "days"
    ),
    length(good_days)
  )

  #### Weekday ####
  w_days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
  #### Number of weekdays wearing the device
  variables$weekday <- rep(
    sum(ifelse(unique(weekdays(data_events$time)) %in% w_days, 1, 0)),
    length(good_days)
  )

  #### Weekend ####
  w_end_days <- c("Saturday", "Sunday")
  #### Number of weekend days wearing the device
  variables$weekend <- rep(
    sum(ifelse(unique(weekdays(data_events$time)) %in% w_end_days, 1, 0)),
    length(good_days)
  )

  #### Primary day of week ####
  #### This was determined by using the median time of each day. Note that the
  #### final day won't have a day of week value because we are only capturing
  #### the sleep data on that day
  variables$day_of_week <- NA
  for (j in seq_along(good_days)) {
    variables$day_of_week[j] <- weekdays(stats::median(day_total[[j]]$time))
  }

  ## Activity variables ####

  ### Position and activity ####
  ### Calculating the number of certain activity per time period of interest
  ### Sedentary time is calculated with a METs cap with 1s, 30s, and 60s epoch
  ### to be consistent with LPA and MVPA.
  variables$steps <- NA
  variables$step_min <- 0
  variables$stand_min <- 0
  variables$upright_min <- NA
  variables$sed1_min <- 0
  variables$sed30_min <- 0
  variables$sed60_min <- 0
  for (j in seq_along(good_days)) {
    if (nrow(day[[j]]) != 0) {
      #### Steps per day
      steps_end <- day[[j]][length(day[[j]]$wear_day), ]$steps
      steps_start <- day[[j]][1, ]$steps
      variables$steps[j] <-  steps_end - steps_start

      #### Steps per minute per day
      variables$step_min[j] <- sum(ifelse(day[[j]]$activity == 2, 1, 0)) / 60

      #### Stand minutes per day
      variables$stand_min[j] <- sum(ifelse(day[[j]]$activity == 1, 1, 0)) / 60

      #### Upright minutes per day
      variables$upright_min[j] <- variables$stand_min[j] + variables$step_min[j]

      #### Sedentary minutes per day
      variables$sed1_min[j] <- sum(
        ifelse(day[[j]]$activity == 0 & day[[j]]$mets1 < 1.5, 1, 0)) / 60
      variables$sed30_min[j] <- sum(
        ifelse(day[[j]]$activity == 0 & day[[j]]$mets30 < 1.5, 1, 0)) / 60
      variables$sed60_min[j] <- sum(
        ifelse(day[[j]]$activity == 0 & day[[j]]$mets60 < 1.5, 1, 0)) / 60
    }
  }

  ### Activity bouts ####
  ### Calculating the number of bouts by certain activity per time
  ### period of interest
  variables$sed30_bout <- 0
  variables$sed60_bout <- 0
  variables$sed30b_min <- 0
  variables$sed60b_min <- 0
  for (j in seq_along(good_days)) {
    if (nrow(day[[j]]) != 0) {
      #### Number of sit bouts > 30 minutes per day
      variables$sed30_bout[j] <- activpalProcessing::prolonged.sed.bouts.num(
        posture = day[[j]]$ap.posture,
        epoch = 1, n = 30
      )

      #### Number of sit bouts > 60 minutes per day
      variables$sed60_bout[j] <- activpalProcessing::prolonged.sed.bouts.num(
        posture = day[[j]]$ap.posture,
        epoch = 1, n = 60
      )

      #### Time spent in sitting bouts > 30 minutes per day
      variables$sed30b_min[j] <- activpalProcessing::prolonged.sed.bouts.min(
        posture = day[[j]]$ap.posture,
        epoch = 1, n = 30
      )

      #### Time spent in sitting bouts > 60 minutes per day
      variables$sed60b_min[j] <- activpalProcessing::prolonged.sed.bouts.min(
        posture = day[[j]]$ap.posture,
        epoch = 1, n = 60
      )
    }
  }

  ### Descriptive measures for movement behaviors ####
  variables$mean_sed_bout <- 0
  variables$sed1_perc <- 0
  variables$sed30_perc <- 0
  variables$sed60_perc <- 0
  variables$sitstand <- 0
  time_awake <- variables$sed1_min + variables$stand_min + variables$step_min
  stand_perc <- rep(0, length(good_days))
  step_perc <- rep(0, length(good_days))
  for (j in seq_along(good_days)) {
    if (nrow(day_dat[[j]]) != 0) {
      #### Mean sedentary bout length per minute
      sed_int <- day_dat[[j]][day_dat[[j]]$activity == 0, ]$interval
      variables$mean_sed_bout[j] <- mean(sed_int) / 60
    }

    #### Total time spend sedentary, standing, and stepping expressed as percent
    #### of wake and wear times. Calculated by dividing total sedentary time
    #### (sed_min) by the total time subject was awake and wear the ActivPal
    variables$sed1_perc[j] <- variables$sed1_min[j] / time_awake[j]
    variables$sed30_perc[j] <- variables$sed30_min[j] / time_awake[j]
    variables$sed60_perc[j] <- variables$sed60_min[j] / time_awake[j]
    stand_perc[j] <- variables$stand_min[j] / time_awake[j]
    step_perc[j] <- variables$step_min[j] / time_awake[j]

    if (nrow(day[[j]]) != 0) {
      ##### Number of sit to stand transitions per day
      variables$sitstand[j] <- breaks_ap(day[[j]]$ap.posture)
    }
  }

  #### Adjusting for 0 / 0 producing NaN
  variables$sed1_perc <- ifelse(
    is.nan(variables$sed1_perc), 0, variables$sed1_perc
  )
  variables$sed30_perc <- ifelse(
    is.nan(variables$sed30_perc), 0, variables$sed30_perc
  )
  variables$sed60_perc <- ifelse(
    is.nan(variables$sed60_perc), 0, variables$sed60_perc
  )
  stand_perc <- ifelse(is.nan(stand_perc), 0, stand_perc)
  step_perc <- ifelse(is.nan(step_perc), 0, step_perc)

  ### Physical activity measures ####
  variables$met_hrs <- NA
  variables$lpa1_min <- 0
  variables$lpa30_min <- 0
  variables$lpa60_min <- 0
  variables$mpa1_min <- 0
  variables$mpa30_min <- 0
  variables$mpa60_min <- 0
  variables$vpa1_min <- 0
  variables$vpa30_min <- 0
  variables$vpa60_min <- 0
  variables$mvpa1_min <- 0
  variables$mvpa30_min <- 0
  variables$mvpa60_min <- 0
  variables$lpa1_perc <- NA
  variables$lpa30_perc <- NA
  variables$lpa60_perc <- NA
  variables$mpa1_perc <- NA
  variables$mpa30_perc <- NA
  variables$mpa60_perc <- NA
  variables$vpa1_perc <- NA
  variables$vpa30_perc <- NA
  variables$vpa60_perc <- NA
  variables$mvpa1_perc <- NA
  variables$mvpa30_perc <- NA
  variables$mvpa60_perc <- NA
  variables$guideline_minbout_10 <- 0
  variables$guideline_numbout_10 <- 0
  for (j in seq_along(good_days)) {
    if (nrow(day[[j]]) != 0) {
      #### ActivPal estimate of met-hours per day which is the sum of met-hours
      #### awake and sleep
      variables$met_hrs[j] <- sum(day_dat[[j]]$methrs, na.rm = TRUE)

      #### Total time in light intensity (1.5 to < 3 METS) activity per day
      variables$lpa1_min[j] <- lit_min_ap(
        mets = day[[j]]$mets1,
        posture = day[[j]]$ap.posture,
        epoch = 1
      )
      variables$lpa30_min[j] <- lit_min_ap(
        mets = day[[j]]$mets30,
        posture = day[[j]]$ap.posture,
        epoch = 1
      )
      variables$lpa60_min[j] <- lit_min_ap(
        mets = day[[j]]$mets60,
        posture = day[[j]]$ap.posture,
        epoch = 1
      )

      #### Total time in moderate intensity (>= 3 to 6 METS) activity per day
      variables$mpa1_min[j] <- mpa_min_ap(mets = day[[j]]$mets1, epoch = 1)
      variables$mpa30_min[j] <- mpa_min_ap(mets = day[[j]]$mets30, epoch = 1)
      variables$mpa60_min[j] <- mpa_min_ap(mets = day[[j]]$mets60, epoch = 1)

      #### Total time in vigorous intensity (>= 6 METS) activity per day
      variables$vpa1_min[j] <- vpa_min_ap(mets = day[[j]]$mets1, epoch = 1)
      variables$vpa30_min[j] <- vpa_min_ap(mets = day[[j]]$mets30, epoch = 1)
      variables$vpa60_min[j] <- vpa_min_ap(mets = day[[j]]$mets60, epoch = 1)

      #### Total time in moderate-to-vigorous intensity (>= 3 METS) activity
      #### per day
      variables$mvpa1_min[j] <- mvpa_min_ap(day[[j]]$mets1, epoch = 1)
      variables$mvpa30_min[j] <- mvpa_min_ap(day[[j]]$mets30, epoch = 1)
      variables$mvpa60_min[j] <- mvpa_min_ap(day[[j]]$mets60, epoch = 1)

      ##### Estimating the minutes spent in bouts of activity that qualify
      #### towards meeting the physical activity guidelines (10 minutes)
      minbout <- activpalProcessing::guideline.bouts.min(mets = day[[j]]$mets1)
      variables$guideline_minbout_10[j] <- minbout

      #### Estimating the nubmer of bouts to activity that qualify towards
      #### meeting the physical activity guidelines (10 minutes)
      numbout <- activpalProcessing::guideline.bouts.num(mets = day[[j]]$mets1)
      variables$guideline_numbout_10[j] <- numbout

      #### Total time spend in LPA expressed as a percent of waking time and
      #### wear time
      variables$lpa1_perc[j] <- variables$lpa1_min[j] / time_awake[j]
      variables$lpa30_perc[j] <- variables$lpa30_min[j] / time_awake[j]
      variables$lpa60_perc[j] <- variables$lpa60_min[j] / time_awake[j]

      #### Total time spend in MPA expressed as a percent of waking time and
      #### wear time
      variables$mpa1_perc[j] <- variables$mpa1_min[j] / time_awake[j]
      variables$mpa30_perc[j] <- variables$mpa30_min[j] / time_awake[j]
      variables$mpa60_perc[j] <- variables$mpa60_min[j] / time_awake[j]

      #### Total time spend in VPA expressed as a percent of waking time and
      #### wear time
      variables$vpa1_perc[j] <- variables$vpa1_min[j] / time_awake[j]
      variables$vpa30_perc[j] <- variables$vpa30_min[j] / time_awake[j]
      variables$vpa60_perc[j] <- variables$vpa60_min[j] / time_awake[j]

      #### Total time spend in MVPA expressed as a percent of waking time and
      #### wear time
      variables$mvpa1_perc[j] <- variables$mvpa1_min[j] / time_awake[j]
      variables$mvpa30_perc[j] <- variables$mvpa30_min[j] / time_awake[j]
      variables$mvpa60_perc[j] <- variables$mvpa60_min[j] / time_awake[j]
    }
  }

  ### Cadence grouping ####

  #### Finding initial cadence values
  cadence30 <- list()
  cadence60 <- list()
  for (j in seq_along(good_days)) {
    fc <- dplyr::filter(data_1s_epoch, wear_day == good_days[j])
    rownum <- seq(1, nrow(fc))
    group_column_30 <- ceiling(rownum / 30)
    group_column_60 <- ceiling(rownum / 60)
    fc30 <- cbind(fc, group_column_30)
    fc60 <- cbind(fc, group_column_60)
    cadence30[[j]] <- dplyr::group_by(fc30, group_column_30) |>
      dplyr::summarise(cadence = 2 * (max(steps) - min(steps)))
    cadence60[[j]] <- dplyr::group_by(fc60, group_column_60) |>
      dplyr::summarise(cadence = max(steps) - min(steps))
  }

  ##### Finding stepping time and amount
  variables$step_time_75_30 <- 0
  variables$step_num_75_30 <- 0
  variables$step_time_100_30 <- 0
  variables$step_num_100_30 <- 0
  variables$step_time_75_60 <- 0
  variables$step_num_75_60 <- 0
  variables$step_time_100_60 <- 0
  variables$step_num_100_60 <- 0
  for (j in seq_along(good_days)) {
    ## Stepping time cadence >= 75 for 30 and 60s
    variables$step_time_75_30[j] <- sum(
      ifelse(cadence30[[j]]$cadence >= 75, 1, 0)
    )
    variables$step_time_75_60[j] <- sum(
      ifelse(cadence60[[j]]$cadence >= 75, 1, 0)
    )

    ## Number of steps of cadence >= 75 for 30 and 60s
    variables$step_num_75_30[j] <- sum(
      cadence30[[j]][cadence30[[j]]$cadence >= 75, ]$cadence
    )
    variables$step_num_75_60[j] <- sum(
      cadence60[[j]][cadence60[[j]]$cadence >= 75, ]$cadence
    )

    ## Stepping time cadence >= 100 for 30 and 60s
    variables$step_time_100_30[j] <- sum(
      ifelse(cadence30[[j]]$cadence >= 100, 1, 0)
    )
    variables$step_time_100_60[j] <- sum(
      ifelse(cadence60[[j]]$cadence >= 100, 1, 0)
    )

    ## Number of steps of cadence >= 100 for 30 and 60s
    variables$step_num_100_30[j] <- sum(
      cadence30[[j]][cadence30[[j]]$cadence >= 100, ]$cadence
    )
    variables$step_num_100_60[j] <- sum(
      cadence60[[j]][cadence60[[j]]$cadence >= 100, ]$cadence
    )
  }

  ## Activity data during sleep windows ####
  ### Finding sleep windows ####
  sleep_window <- list()
  for (j in seq_along(good_days)) {
    sleep_window[[j]] <- day_total[[j]][day_total[[j]]$sleep_loop == 1, ]
  }

  ### Position and activity ####
  #### Calculating the number of certain activity per time period of interest
  variables$steps_sleep <- NA
  variables$step_min_sleep <- NA
  variables$stand_min_sleep <- NA
  variables$upright_min_sleep <- NA
  variables$sed_min_sleep <- NA
  for (j in seq_along(good_days)) {
    #### Steps per day
    if (nrow(sleep_window[[j]]) != 0) {
      steps_end <- sleep_window[[j]][dim(sleep_window[[j]])[1], ]$steps
      steps_start <- sleep_window[[j]][1, ]$steps
      variables$steps_sleep[j] <-  steps_end - steps_start

      #### Steps per minute per day
      step_sleep <- ifelse(sleep_window[[j]]$activity == 2, 1, 0)
      variables$step_min_sleep[j] <- sum(step_sleep) / 60

      #### Stand minutes per day
      stand_sleep <- ifelse(sleep_window[[j]]$activity == 1, 1, 0)
      variables$stand_min_sleep[j] <- sum(stand_sleep) / 60

      #### Upright minutes per day
      stand_min_sleep <- variables$stand_min_sleep[j]
      step_min_sleep <- variables$step_min_sleep[j]
      variables$upright_min_sleep[j] <- stand_min_sleep + step_min_sleep

      #### Sedentary minutes per day (i.e., how many minutes did they sleep per
      #### day not counting naps)
      sed_sleep <- ifelse(sleep_window[[j]]$activity == 0, 1, 0)
      variables$sed_min_sleep[j] <- sum(sed_sleep) / 60
    }
  }

  ### Sit to stand transitions ####
  variables$sitstand_sleep <- NA
  for (j in seq_along(good_days)) {
    if (nrow(sleep_window[[j]]) != 0) {
      variables$sitstand_sleep[j] <- breaks_ap(sleep_window[[j]]$ap.posture)
    }
  }

  ## Obtaining total wear time ####

  ### Awake time ####
  variables$wakewear_min <- time_awake

  ### Sleep time ####
  variables$sleepwear_min <- with(
    data = variables,
    expr = sed_min_sleep + stand_min_sleep + step_min_sleep
  )
  variables$sleepwear_min[is.na(variables$sleepwear_min)] <- 0

  #### Nap time ----
  nap_window <- list()
  nap_window <- lapply(
    X = day_total,
    FUN = function(x) {
      x[which(x$nap_loop == 1), ]
    }
  )
  variables$napwear_min <- unlist(
    lapply(
      X = nap_window,
      FUN = function(x) {
        sum(x$nap_loop) / 60
      }
    )
  )

  #### Total wear time ####
  variables$wear_min <- with(
    data = variables,
    expr = wakewear_min + sleepwear_min + napwear_min
  )
  
  nonwear_window <- list()
  for (j in seq_along(good_days)) {
    nonwear_window[[j]] <- day_total[[j]][day_total[[j]]$sleep_loop != 0 & day_total[[j]]$wake_loop == 99, ]
  }
  
  variables$nonwear_min <- ifelse(length(nonwear_window) != 0, nrow(nonwear_window)/60, 0)

  # Determining valid wear days ####
  # This is based on total device wear time
  # A day is valid if there is 20 hours of wear time (20 * 60 minutes)
  variables <- variables[order(variables$wear_day), ]
  variables$valid_day <- NA
  variables$valid_day[1] <- ifelse(variables$wear_day[1] == 1, 0, NA)
  variables$valid_day[nrow(variables)] <- ifelse(
    test = variables$wear_day[nrow(variables)] == 9,
    yes = 0,
    no = NA
  )
  variables$valid_day <- ifelse(
    test = variables$nonwear_min/variables$wear_min <= 0.166666 & is.na(variables$valid_day),
    yes = 1,
    no = 0
  )
  if (variables$wear_day[1] == 1) {
    if (sum(variables$valid_day) < 7 && variables$nonwear_min[1]/variables$wear_min[1] <= 0.166666 ) {
      variables$valid_day[1] <- 1
    } else {
      variables$valid_day[1] <- variables$valid_day[1]
    }
  }

  # Relocating variables ####
  variables <- dplyr::relocate(variables, days, .after = wear_day)
  variables <- dplyr::relocate(variables, weekday, .after = days)
  variables <- dplyr::relocate(variables, weekend, .after = weekday)
  variables <- dplyr::relocate(variables, day_of_week, .after = weekend)
  variables <- dplyr::relocate(variables, steps, .after = day_of_week)
  variables <- dplyr::relocate(variables, wear_min, .after = steps)
  variables <- dplyr::relocate(variables, nonwear_min, .after = wear_min)
  variables <- dplyr::relocate(variables, wakewear_min, .after = nonwear_min)
  variables <- dplyr::relocate(variables, sleepwear_min, .after = wakewear_min)
  variables <- dplyr::relocate(variables, napwear_min, .after = sleepwear_min)
  variables <- dplyr::relocate(variables, valid_day, .after = napwear_min)
  variables <- dplyr::relocate(variables, step_min, .after = valid_day)
  variables <- dplyr::relocate(variables, stand_min, .after = step_min)
  variables <- dplyr::relocate(variables, upright_min, .after = stand_min)
  variables <- dplyr::relocate(variables, sed1_min, .after = upright_min)
  variables <- dplyr::relocate(variables, sed30_min, .after = sed1_min)
  variables <- dplyr::relocate(variables, sed60_min, .after = sed30_min)
  variables <- dplyr::relocate(variables, sed30_bout, .after = sed60_min)
  variables <- dplyr::relocate(variables, sed60_bout, .after = sed30_bout)
  variables <- dplyr::relocate(variables, sed30b_min, .after = sed60_bout)
  variables <- dplyr::relocate(variables, sed60b_min, .after = sed30b_min)
  variables <- dplyr::relocate(variables, mean_sed_bout, .after = sed60b_min)
  variables <- dplyr::relocate(variables, sed1_perc, .after = mean_sed_bout)
  variables <- dplyr::relocate(variables, sed30_perc, .after = sed1_perc)
  variables <- dplyr::relocate(variables, sed60_perc, .after = sed30_perc)
  variables <- dplyr::relocate(variables, sitstand, .after = sed60_perc)
  variables <- dplyr::relocate(variables, met_hrs, .after = sitstand)
  variables <- dplyr::relocate(variables, lpa1_min, .after = met_hrs)
  variables <- dplyr::relocate(variables, mpa1_min, .after = lpa1_min)
  variables <- dplyr::relocate(variables, vpa1_min, .after = mpa1_min)
  variables <- dplyr::relocate(variables, mvpa1_min, .after = vpa1_min)
  variables <- dplyr::relocate(variables, lpa30_min, .after = mvpa1_min)
  variables <- dplyr::relocate(variables, mpa30_min, .after = lpa30_min)
  variables <- dplyr::relocate(variables, vpa30_min, .after = mpa30_min)
  variables <- dplyr::relocate(variables, mvpa30_min, .after = vpa30_min)
  variables <- dplyr::relocate(variables, lpa60_min, .after = mvpa30_min)
  variables <- dplyr::relocate(variables, mpa60_min, .after = lpa60_min)
  variables <- dplyr::relocate(variables, vpa60_min, .after = mpa60_min)
  variables <- dplyr::relocate(variables, mvpa60_min, .after = vpa60_min)
  variables <- dplyr::relocate(
    variables, guideline_minbout_10, .after = mvpa60_min
  )
  variables <- dplyr::relocate(
    variables, guideline_numbout_10, .after = guideline_minbout_10
  )
  variables <- dplyr::relocate(
    variables, lpa1_perc, .after = guideline_numbout_10
  )
  variables <- dplyr::relocate(variables, mpa1_perc, .after = lpa1_perc)
  variables <- dplyr::relocate(variables, vpa1_perc, .after = mpa1_perc)
  variables <- dplyr::relocate(variables, mvpa1_perc, .after = vpa1_perc)
  variables <- dplyr::relocate(variables, lpa30_perc, .after = mvpa1_perc)
  variables <- dplyr::relocate(variables, mpa30_perc, .after = lpa30_perc)
  variables <- dplyr::relocate(variables, vpa30_perc, .after = mpa30_perc)
  variables <- dplyr::relocate(variables, mvpa30_perc, .after = vpa30_perc)
  variables <- dplyr::relocate(variables, lpa60_perc, .after = mvpa30_perc)
  variables <- dplyr::relocate(variables, mpa60_perc, .after = lpa60_perc)
  variables <- dplyr::relocate(variables, vpa60_perc, .after = mpa60_perc)
  variables <- dplyr::relocate(variables, mvpa60_perc, .after = vpa60_perc)
  variables <- dplyr::relocate(variables, step_time_75_30, .after = mvpa60_perc)
  variables <- dplyr::relocate(
    variables, step_num_75_30, .after = step_time_75_30
  )
  variables <- dplyr::relocate(
    variables, step_time_100_30, .after = step_num_75_30
  )
  variables <- dplyr::relocate(
    variables, step_num_100_30, .after = step_time_100_30
  )
  variables <- dplyr::relocate(
    variables, step_time_75_60, .after = step_num_100_30
  )
  variables <- dplyr::relocate(
    variables, step_num_75_60, .after = step_time_75_60
  )
  variables <- dplyr::relocate(
    variables, step_time_100_60, .after = step_num_75_60
  )
  variables <- dplyr::relocate(
    variables, step_num_100_60, .after = step_time_100_60
  )
  variables <- dplyr::relocate(variables, steps_sleep, .after = step_num_100_60)
  variables <- dplyr::relocate(variables, step_min_sleep, .after = steps_sleep)
  variables <- dplyr::relocate(
    variables, stand_min_sleep, .after = step_min_sleep
  )
  variables <- dplyr::relocate(
    variables, upright_min_sleep, .after = stand_min_sleep
  )
  variables <- dplyr::relocate(
    variables, sed_min_sleep, .after = upright_min_sleep
  )
  variables <- dplyr::relocate(
    variables, sitstand_sleep, .after = sed_min_sleep
  )

  # Averages of per day activities ####
  # Find the average values for all the variables in the variables dataset
  # given that the wear day is a valid day
  hold1 <- variables[variables$valid_day == 1, ]
  hold2 <- subset(hold1, select = -c(wear_day, day_of_week, valid_day))
  hold2$days <- as.numeric(hold2$days)
  avgs <- dplyr::summarise_all(.tbl = hold2, .funs = mean)
  avgs$valid_day <- sum(hold1$valid_day)
  avgs$subject_id <- subject
  avgs$trimester <- as.integer(trimester)
  avgs <- dplyr::relocate(avgs, valid_day, .before = days)
  avgs <- dplyr::relocate(avgs, subject_id, .before = valid_day)
  avgs <- dplyr::relocate(avgs, trimester, .after = subject_id)

  ## Adding in days labeled as invalid due missing activity data ####
  hold1 <- all_days[!(all_days %in% good_days)]
  if (length(hold1) != 0) {
    hold2 <- data.frame(
      matrix(
        data = NA,
        nrow = length(hold1), ncol = dim(variables)[2]
      )
    )
    names(hold2) <- names(variables)
    hold2$wear_day <- hold1
    variables <- dplyr::bind_rows(variables, hold2)
    variables <- variables[order(variables$wear_day), ]
  }

  # Returning daily summary and averages data sets ####
  return(list(daily = variables, avgs = avgs))
}