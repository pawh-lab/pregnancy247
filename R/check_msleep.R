#' Screening the sleep or nap data.
#'
#' Screening the sleep or nap Actiwatch data for each subject imported with
#' [pregnancy247::read_msleep()] for outlier values based on the expertise of
#' [Christopher E. Kline](https://www.education.pitt.edu/people/cek51).
#'
#' @param x A `msleep` object imported with [pregnancy247::read_msleep()].
#' @param y An optional `msleep` object that is used for screening nap data
#' collected by Actiwatch. Default is `NULL`, which coincides with the default
#' value of `nap`.
#' @param nap Logical denoting if nap data is to be screened for outlying
#' values. Default is `FALSE`.
#' @param cutpoint A numeric value denoting the amount of time in minutes to
#' see if nap and sleep overlap. Default is `30` minutes.
#'
#' @details The parameter `y` should only be specified if `nap = TRUE` and it
#' should be the sleep data exported by Actiwatch. Also, `cutpoint` is only used
#' if `nap = TRUE`. Overlapping nap and sleep windows is a part of the screening
#' of nap data collected by Actiwatch.
#'
#' @return A `msleep` object, which is a `data.frame` containing two additional
#' variables `flag1` and `flag2`. The `flag1` variable contains values of 0, 1,
#' or missing, where 0 denotes no issues with this observation, 1 denotes at
#' least one issue with this observation, and missing denotes a value is missing
#' for this observation. The `flag2` values provides a comment for the value of
#' `flag1.`
#'
#' @examples
#' \dontrun{
#' # sleep
#' msleep <- read_msleep(subject = "1000-AB", trimester = 3)
#' msleep_test <- check_msleep(x = msleep)
#' # nap
#' mnap <- read_msleep(subject = "1000-AB", trimester = 3, nap = TRUE)
#' mnap_test <- check(x = mnap, y = msleep, nap = TRUE)
#' }
#'
#' @export check_msleep
check_msleep <- function(x, y = NULL, nap = FALSE, cutpoint = 30) {
  # Checking parmeter values ####
  if (!("msleep" %in% class(x))) {
    stop("x must be msleep object imported by pregnancy247::read_msleep")
  }

  if (!is.logical(nap)) {
    stop("nap must be a logical (TRUE/FALSE) denoting if screening of nap data")
  }

  if (is.null(y) && nap) {
    stop("y must be specificied if nap = TRUE")
  }

  if (!is.null(y) && !("msleep" %in% class(y))) {
    stop("y must be msleep object imported by pregnancy247::read_msleep")
  }

  if (nap && (!is.numeric(cutpoint) || length(cutpoint) != 1)) {
    stop("cutpoint must be numeric vector of length 1 if nap = TRUE")
  }

  # Screening data ####
  n <- seq_len(nrow(x))
  x$flag1 <- rep(0, nrow(x))
  x$flag2 <- rep("", nrow(x))

  ## Invalid times ####
  ### Off-wrist ####
  for (i in n) {
    if (!is.na(x$off_wrist[i]) && (x$off_wrist[i] > 0)) {
      x$flag1[i] <- 1
      if (x$flag2[i] == "") {
        x$flag2[i] <- "Invalid time for off-wrist"
      } else {
        x$flag2[i] <- paste0(x$flag2[i], ", invalid time for off-wrist")
      }
    } else if (is.na(x$off_wrist[i])) {
      if (x$flag1[i] != 1 || is.na(x$flag1[i])) {
        x$flag1[i] <- NA
      } else {
        x$flag1[i] <- x$flag1[i]
      }
      if (x$flag2[i] == "") {
        x$flag2[i] <- "Missing off-wrist"
      } else {
        x$flag2[i] <- paste0(x$flag2[i], ", missing off-wrist")
      }
    } else {
      x$flag1[i] <- x$flag1[i]
      x$flag2[i] <- x$flag2[i]
    }
  }

  ### Inv time ac ####
  for (i in n) {
    if (!is.na(x$inv_time_ac[i]) && (x$inv_time_ac[i] > 0)) {
      x$flag1[i] <- 1
      if (x$flag2[i] == "") {
        x$flag2[i] <- "Invalid time for inv time ac"
      } else {
        x$flag2[i] <- paste0(x$flag2[i], ", invalid time for inv time ac")
      }
    } else if (is.na(x$inv_time_ac[i])) {
      if (x$flag1[i] != 1 || is.na(x$flag1[i])) {
        x$flag1[i] <- NA
      } else {
        x$flag1[i] <- x$flag1[i]
      }
      if (x$flag2[i] == "") {
        x$flag2[i] <- "Missing inv time ac"
      } else {
        x$flag2[i] <- paste0(x$flag2[i], ", missing inv time ac")
      }
    } else {
      x$flag1[i] <- x$flag1[i]
      x$flag2[i] <- x$flag2[i]
    }
  }

  ### Inv time sw ####
  for (i in n) {
    if (!is.na(x$inv_time_sw[i]) && x$inv_time_sw[i] > 0) {
      x$flag1[i] <- 1
      if (x$flag2[i] == "") {
        x$flag2[i] <- "Invalid time for inv time sw"
      } else {
        x$flag2[i] <- paste0(x$flag2[i], ", invalid time for inv time sw")
      }
    } else if (is.na(x$inv_time_sw[i])) {
      if (x$flag1[i] != 1 || is.na(x$flag1[i])) {
        x$flag1[i] <- NA
      } else {
        x$flag1[i] <- x$flag1[i]
      }
      if (x$flag2[i] == "") {
        x$flag2[i] <- "Missing inv time sw"
      } else {
        x$flag2[i] <- paste0(x$flag2[i], ", missing inv time sw")
      }
    } else {
      x$flag1[i] <- x$flag1[i]
      x$flag2[i] <- x$flag2[i]
    }
  }

  ## Rest intervals ####
  rest <- subset(x, subset = interval_type == "REST")
  n_rest <- seq_len(nrow(rest))

  ### Duration ####
  if (!nap) {
    for (i in n_rest) {
      if (!is.na(rest$duration[i]) && (rest$duration[i] >= 720 || rest$duration[i] <= 120)) { # nolint
        rest$flag1[i] <- 1
        if (rest$flag2[i] == "") {
          rest$flag2[i] <- "Duration >= 720 min or Duration <= 120 min"
        } else {
          rest$flag2[i] <- paste0(
            rest$flag2[i], ", duration >= 720 min or duration <= 120 min"
          )
        }
      } else if (is.na(rest$duration[i])) {
        if (rest$flag1[i] != 1 || is.na(rest$flag1[i])) {
          rest$flag1[i] <- NA
        } else {
          rest$flag1[i] <- rest$flag1[i]
        }
        if (rest$flag2[i] == "") {
          rest$flag2[i] <- "Missing duration"
        } else {
          rest$flag2[i] <- paste0(rest$flag2[i], ", missing duration")
        }
      } else {
        rest$flag1[i] <- rest$flag1[i]
        rest$flag2[i] <- rest$flag2[i]
      }
    }
  }


  ### Fragmentation ####
  for (i in n_rest) {
    if (!is.na(rest$fragmentation[i]) && (rest$fragmentation[i] <= 1.50)) {
      rest$flag1[i] <- 1
      if (rest$flag2[i] == "") {
        rest$flag2[i] <- "Fragmentation <= 1.50"
      } else {
        rest$flag2[i] <- paste0(rest$flag2[i], ", fragmentation <= 1.50")
      }
    } else if (is.na(rest$fragmentation[i])) {
      if (rest$flag1[i] != 1 || is.na(rest$flag1[i])) {
        rest$flag1[i] <- NA
      } else {
        rest$flag1[i] <- rest$flag1[i]
      }
      if (rest$flag2[i] == "") {
        rest$flag2[i] <- "Missing fragmentation"
      } else {
        rest$flag2[i] <- paste0(rest$flag2[i], ", missing fragmentation")
      }
    } else {
      rest$flag1[i] <- rest$flag1[i]
      rest$flag2[i] <- rest$flag2[i]
    }
  }

  ### Number of wake bouts ####
  for (i in n_rest) {
    if (!is.na(rest$wake_bouts_num[i]) && (rest$wake_bouts_num[i] >= 100)) {
      rest$flag1[i] <- 1
      if (rest$flag2[i] == "") {
        rest$flag2[i] <- "Number of wake bouts >= 100"
      } else {
        rest$flag2[i] <- paste0(rest$flag2[i], ", number of wake bouts >= 100")
      }
    } else if (is.na(rest$wake_bouts_num[i])) {
      if (rest$flag1[i] != 1 || is.na(rest$flag1[i])) {
        rest$flag1[i] <- NA
      } else {
        rest$flag1[i] <- rest$flag1[i]
      }
      if (rest$flag2[i] == "") {
        rest$flag2[i] <- "Missing number of wake bouts"
      } else {
        rest$flag2[i] <- paste0(rest$flag2[i], ", missing number of wake bouts")
      }
    } else {
      rest$flag1[i] <- rest$flag1[i]
      rest$flag2[i] <- rest$flag2[i]
    }
  }

  ### Timing of naps ####
  if (nap) {
    #### Pulling REST intervals from sleep data ####
    yy <- subset(y, subset = interval_type == "REST")

    #### Obtaining start and end of REST intervals in sleep and nap data ####
    yy_stimes <- as.POSIXct(
      x = paste(yy$start_date, yy$start_time),
      format = "%m/%d/%Y %H:%M:%OS",
      tz = "UTC"
    )
    rest_stimes <- as.POSIXct(
      x = paste(rest$start_date, rest$start_time),
      format = "%m/%d/%Y %H:%M:%OS",
      tz = "UTC"
    )
    yy_etimes <- as.POSIXct(
      x = paste(yy$end_date, yy$end_time),
      format = "%m/%d/%Y %H:%M:%OS",
      tz = "UTC"
    )
    rest_etimes <- as.POSIXct(
      x = paste(rest$end_date, rest$end_time),
      format = "%m/%d/%Y %H:%M:%OS",
      tz = "UTC"
    )

    #### Checking ####
    ##### Nap start times occur within 30 minutes of the prior rest interval
    ##### ending
    for (i in seq_along(rest_stimes)) {
      for (j in seq_along(yy_etimes)) {
        dtime <- difftime(yy_etimes[j], rest_stimes[i], units = "mins")
        dtime <- as.numeric(dtime)
        if (dtime >= (-1 * cutpoint) && dtime <= 0) {
          rest$flag1[i] <- 1
          if (rest$flag2[i] == "") {
            rest$flag2[i] <- paste0(
              "Nap start time occur within 30 minutes of the",
              " prior rest interval ending"
            )
          } else {
            rest$flag2[i] <- paste0(
              rest$flag2[i], ", Nap start time occur within 30 minutes of the",
              " prior rest interval ending"
            )
          }
        } else {
          rest$flag1[i] <- rest$flag1[i]
          rest$flag2[i] <- rest$flag2[i]
        }
      }
    }

    ##### Nap end times occur within 30 minutes of the subsequent rest interval
    ##### beginning
    for (i in seq_along(rest_etimes)) {
      for (j in seq_along(yy_stimes)) {
        dtime <- difftime(yy_stimes[j], rest_etimes[i], units = "mins")
        dtime <- as.numeric(dtime)
        if (dtime >= 0 && dtime <= cutpoint) {
          rest$flag1[i] <- 1
          if (rest$flag2[i] == "") {
            rest$flag2[i] <- paste0(
              "Nap end times occur within 30 minutes of the",
              " subsequent rest interval beginning"
            )
          } else {
            rest$flag2[i] <- paste0(
              rest$flag2[i], ", Nap end times occur within 30 minutes of the",
              " subsequent rest interval beginning"
            )
          }
        } else {
          rest$flag1[i] <- rest$flag1[i]
          rest$flag2[i] <- rest$flag2[i]
        }
      }
    }
  }


  ## Sleep intervals ####
  sleep <- subset(x, subset = interval_type == "SLEEP")
  n_sleep <- seq_len(nrow(sleep))

  ### Onset latency
  for (i in n_sleep) {
    if (!is.na(sleep$onset_latency[i]) && (sleep$onset_latency[i] > 150)) {
      sleep$flag1[i] <- 1
      if (sleep$flag2[i] == "") {
        sleep$flag2[i] <- "Onset latency greater than 150"
      } else {
        sleep$flag2[i] <- paste0(sleep$flag2[i], ", onset latency greater than 150") # nolint
      }
    } else if (is.na(sleep$onset_latency[i])) {
      if (sleep$flag1[i] != 1 || is.na(sleep$flag1[i])) {
        sleep$flag1[i] <- NA
      } else {
        sleep$flag1[i] <- sleep$flag1[i]
      }
      if (sleep$flag2[i] == "") {
        sleep$flag2[i] <- "Missing onset latency"
      } else {
        sleep$flag2[i] <- paste0(sleep$flag2[i], ", missing onset latency")
      }
    } else {
      sleep$flag1[i] <- sleep$flag1[i]
      sleep$flag2[i] <- sleep$flag2[i]
    }
  }

  ### Efficiency ####
  for (i in n_sleep) {
    if (!is.na(sleep$efficiency[i]) && (sleep$efficiency[i] <= 40.0)) {
      sleep$flag1[i] <- 1
      if (sleep$flag2[i] == "") {
        sleep$flag2[i] <- "Efficiency <= 40%"
      } else {
        sleep$flag2[i] <- paste0(sleep$flag2[i], ", efficiency <= 40%")
      }
    } else if (is.na(sleep$efficiency[i])) {
      if (sleep$flag1[i] != 1 || is.na(sleep$flag1[i])) {
        sleep$flag1[i] <- NA
      } else {
        sleep$flag1[i] <- sleep$flag1[i]
      }
      if (sleep$flag2[i] == "") {
        sleep$flag2[i] <- "Missing efficiency"
      } else {
        sleep$flag2[i] <- paste0(sleep$flag2[i], ", missing efficiency")
      }
    } else {
      sleep$flag1[i] <- sleep$flag1[i]
      sleep$flag2[i] <- sleep$flag2[i]
    }
  }

  ### WASO ####
  for (i in n_sleep) {
    if (!is.na(sleep$waso[i]) && (sleep$waso[i] >= 180.0)) {
      sleep$flag1[i] <- 1
      if (sleep$flag2[i] == "") {
        sleep$flag2[i] <- "WASO >= 180 min"
      } else {
        sleep$flag2[i] <- paste0(sleep$flag2[i], ", WASO >= 180 min")
      }
    } else if (is.na(sleep$waso[i])) {
      if (sleep$flag1[i] != 1 || is.na(sleep$flag1[i])) {
        sleep$flag1[i] <- NA
      } else {
        sleep$flag1[i] <- sleep$flag1[i]
      }
      if (sleep$flag2[i] == "") {
        sleep$flag2[i] <- "Missing WASO"
      } else {
        sleep$flag2[i] <- paste0(sleep$flag2[i], ", missing WASO")
      }
    } else {
      sleep$flag1[i] <- sleep$flag1[i]
      sleep$flag2[i] <- sleep$flag2[i]
    }
  }

  # Returning ####
  x <- dplyr::bind_rows(rest, sleep)
  return(x)
}