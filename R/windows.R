#' Determine event windows for activities
#'
#' Determine sleep, nap, work, and monitor wear windows recorded by the
#' Actiwatch or by the subject in their diary.
#'
#' @name windows
#'
#' @param sleep A `data.sleep` object produced by [read_sleep()].
#' @param interval For `windows_nap()`, a character taking a value of
#' `"first"` or `"second"` to denote if first or second napping windows. Default
#' is `"first"`.
#'
#' @details These functions require that a `data.sleep` object is used as this
#' type of object contains the specific variable names to determine the daily
#' windows of sleep, naps, work, and monitor wear.
#'
#' The sleep and nap windows may be soley on times recorded by the Actiwatch,
#' which tracks information based on activity, light, and event markers.
#' Variables that end with `input` will denote how the sleep and nap windows
#' should be recorded. Below is a list of the input values and their relation
#' to how the times are to be record.
#' * `1` denotes event marker time
#' * `2` denotes white light time
#' * `3` denotes diary record time
#' * `4` denotes activity time
#'
#' Each of the windows are initialing created with diary dates recorded by the
#' subject. The dates and times are altered based on time of day in relation to
#' the initial date. As an example, suppose subject 0001-AB records information
#' in their diary on 01-01-2023, including their sleep onset and wake times.
#' With the Pregnancy 24/7 study, the start of each day begins with sleep onset.
#' So, the sleep onset and wake times initially start with the same date and
#' need to be altered so a wake time of 6:00 AM is actually recorded to occur on
#' 01-02-2023 and not 01-01-2023. Or, if the sleep onset time was at 12:00 AM,
#' the date is recorded as 01-02-2023.
#'
#' `windows_work()` and `windows_monitor()` are soley based on times recorded
#' by the subject in their log.
#'
#' The window creation functions also may require user input if their is a
#' missing date or time. It will prompt you with a question whether to proceed
#' with processing the windows given a certain day has missing data. If it is
#' known had of time that certain days will have invalid data the missing data
#' may not be problem, as the invalid data will be excluded later.
#'
#' The `interval` parameter for `windows_nap` allows for recording two naps a
#' day during the wear period. It is assumed the second napping intervals have
#' variable names that end in `_b#`, where `#` denotes the wear day.
#'
#' @return A `list` containing two numeric vectors named `start` and `end` that
#' are formatted as date-time (`POSIXct`) based on UTC time-zone.
#'
#' @examples
#' \dontrun{
#' wind_sleep <- windows_sleep(sleep = sleep)
#' sleep_start <- wind_sleep$start
#' sleep_end <- wind_sleep$end
#' }
#'
#' \dontrun{
#' wind_nap <- windows_nap(sleep = sleep)
#' nap_start <- wind_nap$start
#' nap_end <- wind_nap$end
#' }
#'
#' \dontrun{
#' wind_work <- windows_work(sleep = sleep)
#' work_start <- wind_work$start
#' work_end <- wind_work$end
#' }
#'
#' \dontrun{
#' wind_monitor <- windows_monitor(sleep = sleep)
#' monitor_off <- wind_monitor$start
#' monitor_on <- wind_monitor$end
#' }
#'
#' @rdname windows
#' @export windows_sleep
windows_sleep <- function(sleep) {
  # Checking parameter ####
  if (!("data.sleep" %in% class(sleep))) {
    stop("sleep must be a data.sleep object imported with read_sleep.")
  }

  # Diary dates ####
  all_days <- 1:9
  diary_dates <- unlist(sleep[1, grep("^diary_date", colnames(sleep))])
  diary_dates <- as.character(as.Date(diary_dates, format = "%m/%d/%Y"))
  na_dates <- !is.na(diary_dates) # this denotes whether a date is not missing
  diary_dates <- diary_dates[na_dates]

  # Sleep inputs ####
  # night denotes sleep window starts
  # morn denotes sleep window ends

  ## Input ####
  ## How sleep windows should be defined
  night_input <- unlist(sleep[1, grep("^night_input_", colnames(sleep))])
  night_input <- night_input[na_dates]
  morn_input <- unlist(sleep[1, grep("^morn_input_", colnames(sleep))])
  morn_input <- morn_input[na_dates]

  ## Type of inputs ####

  ### Event marker ####
  night_marker <- unlist(sleep[1, grep("^night_marker_", colnames(sleep))])
  night_marker <- night_marker[na_dates]
  night_marker <- ifelse(
    nchar(night_marker) <= 7,
    paste0("0", night_marker),
    night_marker
  )
  morn_marker <- unlist(sleep[1, grep("^morn_marker_", colnames(sleep))])
  morn_marker <- morn_marker[na_dates]
  morn_marker <- ifelse(
    nchar(morn_marker) <= 7,
    paste0("0", morn_marker),
    morn_marker
  )

  ### Light ####
  night_light <- unlist(sleep[1, grep("^night_light_", colnames(sleep))])
  night_light <- night_light[na_dates]
  night_light <- ifelse(
    nchar(night_light) <= 7,
    paste0("0", night_light),
    night_light
  )
  morn_light <- unlist(sleep[1, grep("^morn_light_", colnames(sleep))])
  morn_light <- morn_light[na_dates]
  morn_light <- ifelse(
    nchar(morn_light) <= 7,
    paste0("0", morn_light),
    morn_light
  )

  ### Activity ####
  night_activity <- unlist(sleep[1, grep("^night_activity_", colnames(sleep))])
  night_activity <- night_activity[na_dates]
  night_activity <- ifelse(
    nchar(night_activity) <= 7,
    paste0("0", night_activity),
    night_activity
  )
  morn_activity <- unlist(sleep[1, grep("^morn_activity_", colnames(sleep))])
  morn_activity <- morn_activity[na_dates]
  morn_activity <- ifelse(
    nchar(morn_activity) <= 7,
    paste0("0", morn_activity),
    morn_activity
  )

  ### Diary ####
  night_diary <- unlist(sleep[1, grep("^diary_trysleep", colnames(sleep))])
  night_diary <- night_diary[na_dates]
  hold1 <- nchar(night_diary) == 4 & substr(night_diary, 2, 2) %in% c(":", ";")
  hold2 <- nchar(night_diary) == 5 & substr(night_diary, 3, 3) %in% c(":", ";")
  night_diary <- ifelse(hold1 | hold2, paste0(night_diary, ":00"), night_diary)
  night_diary <- ifelse(
    nchar(night_diary) <= 7,
    paste0("0", night_diary),
    night_diary
  )
  morn_diary <- unlist(sleep[1, grep("^diary_stopsleep", colnames(sleep))])
  morn_diary <- morn_diary[na_dates]
  hold1 <- nchar(morn_diary) == 4 & substr(morn_diary, 2, 2) %in% c(":", ";")
  hold2 <- nchar(morn_diary) == 5 & substr(morn_diary, 3, 3) %in% c(":", ";")
  morn_diary <- ifelse(hold1 | hold2, paste0(morn_diary, ":00"), morn_diary)
  morn_diary <- ifelse(
    nchar(morn_diary) <= 7,
    paste0("0", morn_diary),
    morn_diary
  )

  # Determining windows ####
  sleep_start <- as.POSIXct(rep(NA, length(diary_dates)), tz = "UTC")
  sleep_end <- as.POSIXct(rep(NA, length(diary_dates)), tz = "UTC")
  for (j in seq_along(diary_dates)) {
    ## Start of window ####
    if (!is.na(night_input[j])) {
      if (night_input[j] == 1) {
        sleep_start[j] <- as.POSIXct(
          paste0(
            diary_dates[j], " ",
            substr(night_marker[j], 1, 2), ":",
            substr(night_marker[j], 4, 5)
          ),
          tz = "UTC"
        )
      } else if (night_input[j] == 2) {
        sleep_start[j] <- as.POSIXct(
          paste0(
            diary_dates[j], " ",
            substr(night_light[j], 1, 2), ":",
            substr(night_light[j], 4, 5)
          ),
          tz = "UTC"
        )
      } else if (night_input[j] == 3) {
        sleep_start[j] <- as.POSIXct(
          paste0(
            diary_dates[j], " ",
            substr(night_diary[j], 1, 2), ":",
            substr(night_diary[j], 4, 5)
          ),
          tz = "UTC"
        )
      } else if (night_input[j] == 4) {
        sleep_start[j] <- as.POSIXct(
          paste0(
            diary_dates[j], " ",
            substr(night_activity[j], 1, 2), ":",
            substr(night_activity[j], 4, 5)
          ),
          tz = "UTC"
        )
      }
    } else {
      sleep_start[j] <- NA
    }

    ## End of window
    if (!is.na(morn_input[j])) {
      if (morn_input[j] == 1) {
        sleep_end[j] <- as.POSIXct(
          paste0(
            diary_dates[j], " ",
            substr(morn_marker[j], 1, 2), ":",
            substr(morn_marker[j], 4, 5)
          ),
          tz = "UTC"
        )
      } else if (morn_input[j] == 2) {
        sleep_end[j] <- as.POSIXct(
          paste0(
            diary_dates[j], " ",
            substr(morn_light[j], 1, 2), ":",
            substr(morn_light[j], 4, 5)
          ),
          tz = "UTC"
        )
      } else if (morn_input[j] == 3) {
        sleep_end[j] <- as.POSIXct(
          paste0(
            diary_dates[j], " ",
            substr(morn_diary[j], 1, 2), ":",
            substr(morn_diary[j], 4, 5)
          ),
          tz = "UTC"
        )
      } else if (morn_input[j] == 4) {
        sleep_end[j] <- as.POSIXct(
          paste0(
            diary_dates[j], " ",
            substr(morn_activity[j], 1, 2), ":",
            substr(morn_activity[j], 4, 5)
          ),
          tz = "UTC"
        )
      }
    } else {
      sleep_end[j] <- NA
    }

    ## Adjusting windows ####
    ## to account change in day due time
    ## Windows are based on diary dates which are static dates
    if (!is.na(sleep_start[j]) && !is.na(sleep_end[j])) {
      noon <- as.POSIXct(paste0(as.Date(sleep_start[j]), " ", "12:00:00"))
      if (sleep_start[j] < noon) {
        sleep_start[j] <- sleep_start[j] + 86400
      }
      if (sleep_start[j] > sleep_end[j]) {
        sleep_end[j] <- sleep_end[j] + 86400
      } else {
        sleep_end[j] <- sleep_end[j]
      }
      if (j >= 2) {
        if (sleep_start[j] <= sleep_end[j - 1]) {
          sleep_start[j] <- sleep_start[j] + 86400
          sleep_end[j] <- sleep_end[j] + 86400
        } else {
          sleep_start[j] <- sleep_start[j]
          sleep_end[j] <- sleep_end[j]
        }
      } else if (j == 1) {
        time1 <- as.POSIXct(
          paste(diary_dates[1], sleep$diary_time1),
          tz = "UTC"
        )
        if (sleep_start[j] <= time1) {
          sleep_start[j] <- sleep_start[j] + 86400
          sleep_end[j] <- sleep_end[j] + 86400
        } else {
          sleep_start[j] <- sleep_start[j]
          sleep_end[j] <- sleep_end[j]
        }
      }
    }

    ## Handling missing windows ####
    hold1 <- is.na(sleep_start[j]) & !is.na(sleep_end[j])
    hold2 <- !is.na(sleep_start[j]) & is.na(sleep_end[j])
    if ((hold1 || hold2)) {
      jj <- all_days[j]
      resp <- utils::menu(
        choices = c("Yes", "No"),
        graphics = TRUE,
        title = paste0(
          "Day ", jj, " has a missing sleep time.",
          "\nWould you like to stop the data processing?"
        )
      )
      resp <- ifelse(resp == 1L, "Yes", "No")
      if (resp == "Yes") {
        stop(paste0("Day ", jj, " has a missing sleep time."))
      }
    }
  }
  return(list(start = sleep_start, end = sleep_end))
}

#' @rdname windows
#' @export windows_nap
windows_nap <- function(sleep, interval = "first") {
  # Checking parameter ####
  if (!("data.sleep" %in% class(sleep))) {
    stop("sleep must be a data.sleep object imported with read_sleep.")
  }

  if (!(interval %in% c("first", "second"))) {
    stop("interval must be a character with a value of first or second.")
  }

  # Switching interval to match variable names ####
  interval <- ifelse(interval == "first", "", "b") # used for most
  int <- ifelse(interval == "b", "_b", "") # needed for different names

  # Diary dates ####
  all_days <- 1:9
  diary_dates <- unlist(sleep[1, grep("^diary_date", colnames(sleep))])
  diary_dates <- as.character(as.Date(diary_dates, format = "%m/%d/%Y"))
  na_dates <- !is.na(diary_dates) # this denotes whether a date is not missing
  diary_dates <- diary_dates[na_dates]

  # Nap taken indicator ####
  nams <- grep(paste0("^diary_nap", int, "\\d"), colnames(sleep))
  diary_nap <- unlist(sleep[1, nams])
  diary_nap <- as.integer(diary_nap)
  diary_nap <- diary_nap[na_dates]

  # Nap inputs ####
  # nap_start denotes nap window starts
  # nap_end denotes nap window ends

  ## Input ####
  ## How sleep windows should be defined
  nams <- grep(paste0("^nap_start_input_", interval, "\\d"), colnames(sleep))
  nap_start_input <- c(sleep[1, nams])
  nap_start_input <- nap_start_input[na_dates]
  nams <- grep(paste0("^nap_end_input_", interval, "\\d"), colnames(sleep))
  nap_end_input <- c(sleep[1, nams])
  nap_end_input <- nap_end_input[na_dates]

  ## Type of inputs ####

  ### Event marker ####
  nams <- grep(paste0("^nap_start_marker_", interval, "\\d"), colnames(sleep))
  nap_start_marker <- c(sleep[1, nams])
  nap_start_marker <- nap_start_marker[na_dates]
  nap_start_marker <- ifelse(
    nchar(nap_start_marker) <= 7,
    paste0("0", nap_start_marker),
    nap_start_marker
  )
  nams <- grep(paste0("^nap_end_marker_", interval, "\\d"), colnames(sleep))
  nap_end_marker <- c(sleep[1, nams])
  nap_end_marker <- nap_end_marker[na_dates]
  nap_end_marker <- ifelse(
    nchar(nap_end_marker) <= 7,
    paste0("0", nap_end_marker),
    nap_end_marker
  )

  ### Light ####
  nams <- grep(paste0("^nap_start_light_", interval, "\\d"), colnames(sleep))
  nap_start_light <- c(sleep[1, nams])
  nap_start_light <- nap_start_light[na_dates]
  nap_start_light <- ifelse(
    nchar(nap_start_light) <= 7,
    paste0("0", nap_start_light),
    nap_start_light
  )
  nams <- grep(paste0("^nap_end_light_", interval, "\\d"), colnames(sleep))
  nap_end_light <- c(sleep[1, nams])
  nap_end_light <- nap_end_light[na_dates]
  nap_end_light <- ifelse(
    nchar(nap_end_light) <= 7,
    paste0("0", nap_end_light),
    nap_end_light
  )
  ### Activity ####
  nams <- grep(paste0("^nap_start_activity_", interval, "\\d"), colnames(sleep))
  nap_start_activity <- c(sleep[1, nams])
  nap_start_activity <- nap_start_activity[na_dates]
  nap_start_activity <- ifelse(
    nchar(nap_start_activity) <= 7,
    paste0("0", nap_start_activity),
    nap_start_activity
  )
  nams <- grep(paste0("^nap_end_activity_", interval, "\\d"), colnames(sleep))
  nap_end_activity <- c(sleep[1, nams])
  nap_end_activity <- nap_end_activity[na_dates]
  nap_end_activity <- ifelse(
    nchar(nap_end_activity) <= 7,
    paste0("0", nap_end_activity),
    nap_end_activity
  )

  ### Diary ####
  nams <- grep(paste0("^diary_napstrt", int, "\\d"), colnames(sleep))
  nap_start_diary <- c(sleep[1, nams])
  nap_start_diary <- nap_start_diary[na_dates]
  hold1 <- nchar(nap_start_diary) == 4 & substr(nap_start_diary, 2, 2) %in% c(":", ";") #nolint
  hold2 <- nchar(nap_start_diary) == 5 & substr(nap_start_diary, 3, 3) %in% c(":", ";") #nolint
  nap_start_diary <- ifelse(
    hold1 | hold2,
    paste0(nap_start_diary, ":00"),
    nap_start_diary
  )
  nap_start_diary <- ifelse(
    nchar(nap_start_diary) <= 7,
    paste0("0", nap_start_diary),
    nap_start_diary
  )
  nams <- grep(paste0("^diary_napend", int, "\\d"), colnames(sleep))
  nap_end_diary <- c(sleep[1, nams])
  nap_end_diary <- nap_end_diary[na_dates]
  hold1 <- nchar(nap_end_diary) == 4 & substr(nap_end_diary, 2, 2) %in% c(":", ";") #nolint
  hold2 <- nchar(nap_end_diary) == 5 & substr(nap_end_diary, 3, 3) %in% c(":", ";") #nolint
  nap_end_diary <- ifelse(
    hold1 | hold2,
    paste0(nap_end_diary, ":00"),
    nap_end_diary
  )
  nap_end_diary <- ifelse(
    nchar(nap_end_diary) <= 7,
    paste0("0", nap_end_diary),
    nap_end_diary
  )

  # Determining windows ####
  nap_start <- as.POSIXct(rep(NA, length(diary_dates)), tz = "UTC")
  nap_end <- as.POSIXct(rep(NA, length(diary_dates)), tz = "UTC")
  for (j in 1:(length(diary_nap) - 1)) {
    ## Start of window
    if (diary_nap[j] == 1 && !is.na(diary_nap[j])) {
      if (nap_start_input[j] == 1) {
        nap_start[j] <- as.POSIXct(
          paste(
            diary_dates[j],
            paste0(
              substr(nap_start_marker[j], 1, 2), ":",
              substr(nap_start_marker[j], 4, 5)
            )
          ),
          tz = "UTC"
        ) + 1
      } else if (nap_start_input[j] == 2) {
        nap_start[j] <- as.POSIXct(
          paste(
            diary_dates[j],
            paste0(
              substr(nap_start_light[j], 1, 2), ":",
              substr(nap_start_light[j], 4, 5)
            )
          ),
          tz = "UTC"
        ) + 1
      } else if (nap_start_input[j] == 3) {
        nap_start[j] <- as.POSIXct(
          paste(
            diary_dates[j],
            paste0(
              substr(nap_start_diary[j], 1, 2), ":",
              substr(nap_start_diary[j], 4, 5)
            )
          ),
          tz = "UTC"
        ) + 1
      } else if (nap_start_input[j] == 4) {
        nap_start[j] <- as.POSIXct(
          paste(
            diary_dates[j],
            paste0(
              substr(nap_start_activity[j], 1, 2), ":",
              substr(nap_start_activity[j], 4, 5)
            )
          ),
          tz = "UTC"
        ) + 1
      }
    } else {
      nap_start[j] <- NA
    }

    ## End of window
    if (diary_nap[j] == 1 && !is.na(diary_nap[j])) {
      if (nap_end_input[j] == 1) {
        nap_end[j] <- as.POSIXct(
          paste(
            diary_dates[j],
            paste0(
              substr(nap_end_marker[j], 1, 2), ":",
              substr(nap_end_marker[j], 4, 5)
            )
          ),
          tz = "UTC"
        ) + 1
      } else if (nap_end_input[j] == 2) {
        nap_end[j] <- as.POSIXct(
          paste(
            diary_dates[j],
            paste0(
              substr(nap_end_light[j], 1, 2), ":",
              substr(nap_end_light[j], 4, 5)
            )
          ),
          tz = "UTC"
        ) + 1
      } else if (nap_end_input[j] == 3) {
        nap_end[j] <- as.POSIXct(
          paste(
            diary_dates[j],
            paste0(
              substr(nap_end_diary[j], 1, 2), ":",
              substr(nap_end_diary[j], 4, 5)
            )
          ),
          tz = "UTC"
        ) + 1
      } else if (nap_end_input[j] == 4) {
        nap_end[j] <- as.POSIXct(
          paste(
            diary_dates[j],
            paste0(
              substr(nap_end_activity[j], 1, 2), ":",
              substr(nap_end_activity[j], 4, 5)
            )
          ),
          tz = "UTC"
        ) + 1
      }
    } else {
      nap_end[j] <- NA
    }

    ## Adjusting windows ####
    ## to account change in day due time
    ## Windows are based on diary dates which are static dates
    if (!is.na(nap_start[j]) && !is.na(nap_end[j])) {
      if (nap_start[j] > nap_end[j]) {
        nap_end[j] <- nap_end[j] + 86400
      } else {
        nap_end[j] <- nap_end[j]
      }
    }

    ## Handling missing windows ####
    hold1 <- is.na(nap_start[j]) & !is.na(nap_end[j])
    hold2 <- !is.na(nap_start[j]) & is.na(nap_end[j])
    if ((hold1 || hold2)) {
      jj <- all_days[j]
      resp <- utils::menu(
        choices = c("Yes", "No"),
        graphics = TRUE,
        title = paste0(
          "Day ", jj, " has a missing nap time.",
          "\nWould you like to stop the data processing?"
        )
      )
      resp <- ifelse(resp == 1L, "Yes", "No")
      if (resp == "Yes") {
        stop(paste0("Day ", jj, " has a missing nap time."))
      }
    }
  }

  return(list(start = nap_start, end = nap_end))
}

#' @rdname windows
#' @export windows_work
windows_work <- function(sleep) {
  # Checking parameter ####
  if (!("data.sleep" %in% class(sleep))) {
    stop("sleep must be a data.sleep object imported with read_sleep.")
  }

  # Diary dates ####
  all_days <- 1:9
  diary_dates <- unlist(sleep[1, grep("^diary_date", colnames(sleep))])
  diary_dates <- as.character(as.Date(diary_dates, format = "%m/%d/%Y"))
  na_dates <- !is.na(diary_dates) # this denotes whether a date is not missing
  diary_dates <- diary_dates[na_dates]

  # Start ####
  work_start_diary <- c(sleep[1, grep("^diary_workstart", colnames(sleep))])
  work_start_diary <- work_start_diary[na_dates]
  hold1 <- nchar(work_start_diary) == 4 & substr(work_start_diary, 2, 2) %in% c(":", ";") #nolint
  hold2 <- nchar(work_start_diary) == 5 & substr(work_start_diary, 3, 3) %in% c(":", ";") #nolint
  work_start_diary <- ifelse(
    hold1 | hold2,
    paste0(work_start_diary, ":00"),
    work_start_diary
  )
  work_start_diary <- ifelse(
    nchar(work_start_diary) <= 7,
    paste0("0", work_start_diary),
    work_start_diary
  )

  # End ####
  work_end_diary <- c(sleep[1, grep("^diary_workstop", colnames(sleep))])
  work_end_diary <- work_end_diary[na_dates]
  hold1 <- nchar(work_end_diary) == 4 & substr(work_end_diary, 2, 2) %in% c(":", ";") #nolint
  hold2 <- nchar(work_end_diary) == 5 & substr(work_end_diary, 3, 3) %in% c(":", ";") #nolint
  work_end_diary <- ifelse(
    hold1 | hold2,
    paste0(work_end_diary, ":00"),
    work_end_diary
  )
  work_end_diary <- ifelse(
    nchar(work_end_diary) <= 7,
    paste0("0", work_end_diary),
    work_end_diary
  )

  # Determining windows ####
  work_start <- as.POSIXct(rep(NA, length(diary_dates)), tz = "UTC")
  work_end <- as.POSIXct(rep(NA, length(diary_dates)), tz = "UTC")
  for (j in 1:(length(diary_dates) - 1)) {
    if (!is.na(work_start_diary[j])) {
      ## Start of window
      work_start[j] <- as.POSIXct(
        paste(
          diary_dates[j],
          paste0(
            substr(work_start_diary[j], 1, 2), ":",
            substr(work_start_diary[j], 4, 5)
          )
        ),
        tz = "UTC"
      )
    }

    ## End of window
    if (!is.na(work_end_diary[j])) {
      work_end[j] <- as.POSIXct(
        paste(
          diary_dates[j],
          paste0(
            substr(work_end_diary[j], 1, 2), ":",
            substr(work_end_diary[j], 4, 5)
          )
        ),
        tz = "UTC"
      )
    }

    ##### Adjusting the date for the end of the work window. Due to the start
    ##### and end of the work window being based on the same diary entry date,
    ##### the end of the work window may begin before the start of the work
    ##### window.
    if (!is.na(work_start[j]) && !is.na(work_end[j])) {
      if (work_start[j] > work_end[j]) {
        work_end[j] <- work_end[j] + 86400
      } else {
        work_end[j] <- work_end[j]
      }
    }

    ##### Handling missing entries of work start/end times. If a subject doesn't
    ##### doesn't list a work start and work stop time the data processor will
    ##### be asked whether data processing should stop.
    hold1 <- is.na(work_start[j]) & !is.na(work_end[j])
    hold2 <- !is.na(work_start[j]) & is.na(work_end[j])
    if ((hold1 || hold2)) {
      jj <- all_days[j]
      resp <- utils::menu(
        choices = c("Yes", "No"),
        graphics = TRUE,
        title = paste0(
          "Day ", jj, " has a missing work time.",
          "\nWould you like to stop the data processing?"
        )
      )
      resp <- ifelse(resp == 1L, "Yes", "No")
      if (resp == "Yes") {
        stop(paste0("Day ", jj, " has a missing work time."))
      }
    }
  }

  return(list(start = work_start, end = work_end))
}

#' @rdname windows
#' @export windows_monitor
windows_monitor <- function(sleep) {
  # Checking parameter ####
  if (!("data.sleep" %in% class(sleep))) {
    stop("sleep must be a data.sleep object imported with read_sleep.")
  }

  # Diary dates ####
  all_days <- 1:9
  diary_dates <- unlist(sleep[1, grep("^diary_date", colnames(sleep))])
  diary_dates <- as.character(as.Date(diary_dates, format = "%m/%d/%Y"))
  na_dates <- !is.na(diary_dates) # this denotes whether a date is not missing
  diary_dates <- diary_dates[na_dates]

  # Monitor off ####
  monitor_off_diary <- c(sleep[1, grep("^diary_monitorsofftime", colnames(sleep))]) #nolint
  monitor_off_diary <- monitor_off_diary[na_dates[-9]]
  hold1 <- nchar(monitor_off_diary) == 4 & substr(monitor_off_diary, 2, 2) %in% c(":", ";") #nolint
  hold2 <- nchar(monitor_off_diary) == 5 & substr(monitor_off_diary, 3, 3) %in% c(":", ";") #nolint
  monitor_off_diary <- ifelse(
    hold1 | hold2,
    paste0(monitor_off_diary, ":00"),
    monitor_off_diary
  )
  monitor_off_diary <- ifelse(
    nchar(monitor_off_diary) <= 7,
    paste0("0", monitor_off_diary),
    monitor_off_diary
  )

  # Monitor on ####
  monitor_on_diary <- c(sleep[1, grep("^diary_monitorsontime", colnames(sleep))]) #nolint
  monitor_on_diary <- monitor_on_diary[na_dates[-9]]
  hold1 <- nchar(monitor_on_diary) == 4 & substr(monitor_on_diary, 2, 2) %in% c(":", ";") #nolint
  hold2 <- nchar(monitor_on_diary) == 5 & substr(monitor_on_diary, 3, 3) %in% c(":", ";") #nolint
  monitor_on_diary <- ifelse(
    hold1 | hold2,
    paste0(monitor_on_diary, ":00"),
    monitor_on_diary
  )
  monitor_on_diary <- ifelse(
    nchar(monitor_on_diary) <= 7,
    paste0("0", monitor_on_diary),
    monitor_on_diary
  )

  # Determining windows ####
  monitor_off <- as.POSIXct(rep(NA, length(diary_dates)), tz = "UTC")
  monitor_on <- as.POSIXct(rep(NA, length(diary_dates)), tz = "UTC")
  for (j in 1:(length(diary_dates) - 1)) {
    if (!is.na(monitor_off_diary[j])) {
      ## Start of window
      monitor_off[j] <- as.POSIXct(
        paste(
          diary_dates[j],
          paste0(
            substr(monitor_off_diary[j], 1, 2), ":",
            substr(monitor_off_diary[j], 4, 5)
          )
        ),
        tz = "UTC"
      )
    }

    ## End of window
    if (!is.na(monitor_on_diary[j])) {
      monitor_on[j] <- as.POSIXct(
        paste(
          diary_dates[j],
          paste0(
            substr(monitor_on_diary[j], 1, 2), ":",
            substr(monitor_on_diary[j], 4, 5)
          )
        ),
        tz = "UTC"
      )
    }

    ##### Adjusting the date for the end of the monitor off window. Due to the
    ##### start and end of the monitor off window being based on the same diar
    ##### entry date, the end of the monitor off window may begin before the
    ##### start of the monitor off window.
    if (!is.na(monitor_off[j]) && !is.na(monitor_on[j])) {
      if (monitor_off[j] > monitor_on[j]) {
        monitor_on[j] <- monitor_on[j] + 86400
      } else {
        monitor_on[j] <- monitor_on[j]
      }
    }

    ##### Handling missing entries of monitor off/on times. If a subject doesn't
    ##### doesn't list a monitor off and monitor on time the data processor will
    ##### be asked whether data processing should stop.
    hold1 <- is.na(monitor_off[j]) & !is.na(monitor_on[j])
    hold2 <- !is.na(monitor_off[j]) & is.na(monitor_on[j])
    if ((hold1 || hold2)) {
      jj <- all_days[j]
      resp <- utils::menu(
        choices = c("Yes", "No"),
        graphics = TRUE,
        title = paste0(
          "Day ", jj, " has a missing monitor time.",
          "\nWould you like to stop the data processing?"
        )
      )
      resp <- ifelse(resp == 1L, "Yes", "No")
      if (resp == "Yes") {
        stop(paste0("Day ", jj, " has a missing monitor time."))
      }
    }
  }

  return(list(start = monitor_off, end = monitor_on))
}