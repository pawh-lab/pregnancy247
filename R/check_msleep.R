check_msleep <- function(x, nap = FALSE) {
  # Checking parmeter values ####
  if (!("msleep" %in% class(x))) {
    stop("x must be msleep object imported by pregnancy247::read_msleep")
  }

  if (!is.logical(nap)) {
    stop("nap must be a logical (TRUE/FALSE) denoting if screening of nap data")
  }

  # Changing variables names ####
  names(x) <- create_dict(x)

  # Screening data ####
  n <- seq_len(nrow(x))
  flag1 <- rep(0, nrow(x))
  flag2 <- rep("", nrow(x))

  ## Invalid times ####

  ### Off-wrist ####
  for (i in n) {
    if (!is.na(x$Off_Wrist[i]) && (x$Off_Wrist[i] > 0)) {
      flag1[i] <- 1
      if (flag2[i] == "") {
        flag2[i] <- "Invalid time for off-wrist"
      } else {
        flag2[i] <- paste0(flag2[i], ", invalid time for off-wrist")
      }
    } else if (is.na(x$Off_Wrist[i])) {
      if (flag1[i] != 1 || is.na(flag1[i])) {
        flag1[i] <- NA
      } else {
        flag1[i] <- flag1[i]
      }
      if (flag2[i] == "") {
        flag2[i] <- "Missing off-wrist"
      } else {
        flag2[i] <- paste0(flag2[i], ", missing off-wrist")
      }
    } else {
      flag1[i] <- flag1[i]
      flag2[i] <- flag2[i]
    }
  }

  ### Inv time ac ####
  for (i in n) {
    if (!is.na(x$Inv_Time_AC[i]) && (x$Inv_Time_AC[i] > 0)) {
      flag1[i] <- 1
      if (flag2[i] == "") {
        flag2[i] <- "Invalid time for inv time ac"
      } else {
        flag2[i] <- paste0(flag2[i], ", invalid time for inv time ac")
      }
    } else if (is.na(x$Inv_Time_AC[i])) {
      if (flag1[i] != 1 || is.na(flag1[i])) {
        flag1[i] <- NA
      } else {
        flag1[i] <- flag1[i]
      }
      if (flag2[i] == "") {
        flag2[i] <- "Missing inv time ac"
      } else {
        flag2[i] <- paste0(flag2[i], ", missing inv time ac")
      }
    } else {
      flag1[i] <- flag1[i]
      flag2[i] <- flag2[i]
    }
  }

  ### Inv time sw ####
  for (i in n) {
    if (!is.na(x$Inv_Time_SW[i]) && x$Inv_Time_SW[i] > 0) {
      flag1[i] <- 1
      if (flag2[i] == "") {
        flag2[i] <- "Invalid time for inv time sw"
      } else {
        flag2[i] <- paste0(flag2[i], ", invalid time for inv time sw")
      }
    } else if (is.na(x$Inv_Time_SW[i])) {
      if (flag1[i] != 1 || is.na(flag1[i])) {
        flag1[i] <- NA
      } else {
        flag1[i] <- flag1[i]
      }
      if (flag2[i] == "") {
        flag2[i] <- "Missing inv time sw"
      } else {
        flag2[i] <- paste0(flag2[i], ", missing inv time sw")
      }
    } else {
      flag1[i] <- flag1[i]
      flag2[i] <- flag2[i]
    }
  }

  ## Unlikely rest/sleep values ####

  ### Onset latency ####
  for (i in n) {
    if (!is.na(x$Onset_Latency[i]) && (x$Onset_Latency[i] > 150)) {
      flag1[i] <- 1
      if (flag2[i] == "") {
        flag2[i] <- "Onset latency greater than 150"
      } else {
        flag2[i] <- paste0(flag2[i], ", onset latency greater than 150")
      }
    } else if (is.na(x$Onset_Latency[i])) {
      if (flag1[i] != 1 || is.na(flag1[i])) {
        flag1[i] <- NA
      } else {
        flag1[i] <- flag1[i]
      }
      if (flag2[i] == "") {
        flag2[i] <- "Missing onset latency"
      } else {
        flag2[i] <- paste0(flag2[i], ", missing onset latency")
      }
    } else {
      flag1[i] <- flag1[i]
      flag2[i] <- flag2[i]
    }
  }

  ### Duration ####
  for (i in n) {
    if (!is.na(x$Duration[i]) && (x$Duration[i] >= 720 || x$Duration[i] <= 120)) { #nolint
      flag1[i] <- 1
      if (flag2[i] == "") {
        flag2[i] <- "Duration >= 720 min or Duration <= 120 min"
      } else {
        flag2[i] <- paste0(
          flag2[i], ", duration >= 720 min or duration <= 120 min"
        )
      }
    } else if (is.na(x$Duration[i])) {
      if (flag1[i] != 1 || is.na(flag1[i])) {
        flag1[i] <- NA
      } else {
        flag1[i] <- flag1[i]
      }
      if (flag2[i] == "") {
        flag2[i] <- "Missing duration"
      } else {
        flag2[i] <- paste0(flag2[i], ", missing duration")
      }
    } else {
      flag1[i] <- flag1[i]
      flag2[i] <- flag2[i]
    }
  }

  ### Efficiency ####
  for (i in n) {
    if (!is.na(x$Efficiency[i]) && (x$Efficiency[i] <= 40.0)) {
      flag1[i] <- 1
      if (flag2[i] == "") {
        flag2[i] <- "Efficiency <= 40%"
      } else {
        flag2[i] <- paste0(flag2[i], ", efficiency <= 40%")
      }
    } else if (is.na(x$Efficiency[i])) {
      if (flag1[i] != 1 || is.na(flag1[i])) {
        flag1[i] <- NA
      } else {
        flag1[i] <- flag1[i]
      }
      if (flag2[i] == "") {
        flag2[i] <- "Missing efficiency"
      } else {
        flag2[i] <- paste0(flag2[i], ", missing efficiency")
      }
    } else {
      flag1[i] <- flag1[i]
      flag2[i] <- flag2[i]
    }
  }

  ### WASO ####
  for (i in n) {
    if (!is.na(x$WASO[i]) && (x$WASO[i] >= 180.0)) {
      flag1[i] <- 1
      if (flag2[i] == "") {
        flag2[i] <- "WASO >= 180 min"
      } else {
        flag2[i] <- paste0(flag2[i], ", WASO >= 180 min")
      }
    } else if (is.na(x$WASO[i])) {
      if (flag1[i] != 1 || is.na(flag1[i])) {
        flag1[i] <- NA
      } else {
        flag1[i] <- flag1[i]
      }
      if (flag2[i] == "") {
        flag2[i] <- "Missing WASO"
      } else {
        flag2[i] <- paste0(flag2[i], ", missing WASO")
      }
    } else {
      flag1[i] <- flag1[i]
      flag2[i] <- flag2[i]
    }
  }

  ### Fragmentation ####
  for (i in n) {
    if (!is.na(x$Fragmentation[i]) && (x$Fragmentation[i] <= 1.50)) {
      flag1[i] <- 1
      if (flag2[i] == "") {
        flag2[i] <- "Fragmentation <= 1.50"
      } else {
        flag2[i] <- paste0(flag2[i], ", fragmentation <= 1.50")
      }
    } else if (is.na(x$Fragmentation[i])) {
      if (flag1[i] != 1 || is.na(flag1[i])) {
        flag1[i] <- NA
      } else {
        flag1[i] <- flag1[i]
      }
      if (flag2[i] == "") {
        flag2[i] <- "Missing fragmentation"
      } else {
        flag2[i] <- paste0(flag2[i], ", missing fragmentation")
      }
    } else {
      flag1[i] <- flag1[i]
      flag2[i] <- flag2[i]
    }
  }

  ### Number of wake bouts ####
  for (i in n) {
    if (!is.na(x$num_Wake_Bouts[i]) && (x$num_Wake_Bouts[i] >= 100)) {
      flag1[i] <- 1
      if (flag2[i] == "") {
        flag2[i] <- "Number of wake bouts >= 100"
      } else {
        flag2[i] <- paste0(flag2[i], ", number of wake bouts >= 100")
      }
    } else if (is.na(x$num_Wake_Bouts[i])) {
      if (flag1[i] != 1 || is.na(flag1[i])) {
        flag1[i] <- NA
      } else {
        flag1[i] <- flag1[i]
      }
      if (flag2[i] == "") {
        flag2[i] <- "Missing number of wake bouts"
      } else {
        flag2[i] <- paste0(flag2[i], ", missing number of wake bouts")
      }
    } else {
      flag1[i] <- flag1[i]
      flag2[i] <- flag2[i]
    }
  }

  ### Start of nap ####
  if (nap) {
    start_time <- substr(x$Start_Time, 12, 19)
    hour <- as.numeric(substr(start_time, 1, 2))
    for (i in n) {
      if (!is.na(hour[i]) && (hour[i] > 20 || hour < 10)) {
        flag1[i] <- 1
        if (flag2[i] == "") {
          flag2[i] <- "Start time > 20:00:00 or < 10:00:00"
        } else {
          flag2[i] <- paste0(flag2[i], ", start time > 20:00:00 or < 10:00:00")
        }
      } else if (is.na(hour[i])) {
        if (flag1[i] != 1 || is.na(flag1[i])) {
          flag1[i] <- NA
        } else {
          flag1[i] <- flag1[i]
        }
        if (flag2[i] == "") {
          flag2[i] <- "Missing start time"
        } else {
          flag2[i] <- paste0(flag2[i], ", missing start time")
        }
      } else {
        flag1[i] <- flag1[i]
        flag2[i] <- flag2[i]
      }
    }
  }

  # Returning screened object ####
  x$flag1 <- flag1
  x$flag2 <- flag2
  return(x)
}