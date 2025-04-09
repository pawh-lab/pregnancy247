#' Summarize Daily Physical Activity and Sleep Data
#'
#' The functions summarize daily & weekly physical activity and sleep data, 
#' including steps, sedentary behavior, light physical activity (LPA), moderate-to-vigorous physical activity (MVPA), and sleep metrics.
#' @param data A data frame containing the following columns: 
#' `subject`, `trimester`, `wear_day`, `date`, `sleep_loop`, `nap_loop`, 
#' `activity`, `steps`, `epoch_30`, `epoch_60`, 
#' `mets1`, `mets30`, `mets60`, and `time`.
#' @return A data frame summarizing daily physical activity and sleep metrics for the given subject and day.
#' @examples
#' # Example usage
#' daily_summary <- dailysum(data)
#' @export
dailysum <- function(data){
  # Participants characteristics
  subject <- data$subject[1]
  trimester <- data$trimester[1]
  wearday <- data$wear_day[1]
  date <- names(which.max(table(data$date)))
  day <- weekdays(as.Date(names(which.max(table(data$date)))))
  weekday <- ifelse(weekdays(as.Date(names(which.max(table(data$date))))) %in% c("Saturday", "Sunday"), 0, 1)
  
  nonwear <- sum(is.na(data$sleep_loop) | is.na(data$nap_loop) & data$activity == 0) / 60
  
  # Steps
  range <- function(data){max(data) - min(data)}
  if(sum(data$sleep_loop, na.rm = TRUE) == 0){
    steps_sleep <- 0
  }else{
    steps_sleep <- range(data$steps[which(data$sleep_loop == 1)])}
  
  steps <- range(data$steps) - steps_sleep
  stepping_min <- sum(data[which(data$sleep_loop == 0 & data$nap_loop == 0),]$activity == 2, na.rm = T)/60
  steps30 <- aggregate(data$steps, list(group = data$epoch_30), FUN = range)
  steps60 <- aggregate(data$steps, list(group = data$epoch_60), FUN = range)
  steps75.30 <- sum(steps30$data >= 37.5) / 2
  steps75.60 <- sum(steps60$data >= 75)
  steps100.30 <- sum(steps30$data >= 50) / 2
  steps100.60 <- sum(steps60$data >= 100)
  
  data$mets1 <- ifelse(data$activity == 0, 1.25, data$mets1)
  
  # 24-hour behaviors
  sleep <- sum(data$sleep_loop, na.rm = T)/60
  nap <- sum(data$nap_loop, na.rm = T)/60
  sed <- sum(data[which(data$sleep_loop == 0 & data$nap_loop == 0),]$activity == 0, na.rm = T)/60
  stand <- sum(data[which(data$sleep_loop == 0 & data$nap_loop == 0),]$activity == 1, na.rm = T)/60
  
  l <- rep(1, length(data$mets1)) # Index variable
  lpa1 <- sum(l[which( (data$sleep_loop == 0 & data$nap_loop == 0) & 
                         data$mets1 > 1.25 & data$mets1 < 3)], na.rm = T) / 60
  lpa30 <- sum(l[which( (data$sleep_loop == 0 & data$nap_loop == 0 & data$activity !=0) & 
                          data$mets30 > 1.25 & data$mets30 < 3)], na.rm = T) / 60
  lpa60 <- sum(l[which( (data$sleep_loop == 0 & data$nap_loop == 0 & data$activity !=0) & 
                          data$mets60 > 1.25 & data$mets60 < 3)], na.rm = T) / 60
  mvpa1 <- sum(l[which( (data$sleep_loop == 0 & data$nap_loop == 0) & data$mets1 >= 3)], na.rm = T) / 60
  mvpa30 <- sum(l[which( (data$sleep_loop == 0 & data$nap_loop == 0 & data$activity !=0) & 
                           data$mets30 >= 3)], na.rm = T) / 60
  mvpa60 <- sum(l[which( (data$sleep_loop == 0 & data$nap_loop == 0 & data$activity !=0) & 
                           data$mets60 >= 3)], na.rm = T) / 60
  
  # Sleep variables
  s <- subset(data, sleep_loop == 1)
  stand_sleep <- sum(s$activity == 1, na.rm = T)/60
  stepping_sleep <- sum(s$activity == 2, na.rm = T)/60
  sed.bouts_sleep <- bouts(s$mets1, wake = NULL, bout_length = 1/60, thresh_lower = 1, thresh_upper = 1.25, tol = 0)
  sed.breaks_sleep <- sum(rle(sed.bouts_sleep)$values)
  
  # Valid_day
  daytime <- round(abs(as.numeric(difftime(max(data$time), min(data$time), "UTC", units = "mins"))), 0)
  valid_day <- ifelse(((sleep + sed + nap + lpa1 + mvpa1)/daytime >= 20/24) & (steps >= 500 & stepping_min >= 1), 1, 0)
  
  # Bout
  sed.breaks <- bouts(data$mets1, wake = data$wake_loop, bout_length = 1/60, thresh_lower = 1, thresh_upper = 1.25, tol = 0)
  sed.breaks <- sum(rle(sed.breaks)$values)
  sed.bout30m <- bouts(data$mets1, wake = data$wake_loop, bout_length = 30, thresh_lower = 1.25, thresh_upper = 1.25, tol = 0)
  sed.bout60m <- bouts(data$mets1, wake = data$wake_loop, bout_length = 60, thresh_lower = 1.25, thresh_upper = 1.25, tol = 0)
  sed.bout30m.min <- sum(sed.bout30m)/60
  sed.bout60m.min <- sum(sed.bout60m)/60
  sed.bout30m.num <- sum(rle(sed.bout30m)$values)
  sed.bout60m.num <- sum(rle(sed.bout60m)$values)
  mvpa.10bout1 <- bouts(data$mets1, wake = data$wake_loop, bout_length = 10, thresh_lower = 3, thresh_upper = 6, 
                        tol = 2, tol_lower = 1.25, tol_upper = 7)
  mvpa.10bout.min1 <- sum(mvpa.10bout1)/60
  mvpa.10bout.num1 <- sum(rle(mvpa.10bout1)$values)
  mvpa.10bout30 <- bouts(data$mets30, wake = data$wake_loop, bout_length = 10, thresh_lower = 3, thresh_upper = 6, 
                         tol = 2, tol_lower = 1.25, tol_upper = 7)
  mvpa.10bout.min30 <- sum(mvpa.10bout30)/60
  mvpa.10bout.num30 <- sum(rle(mvpa.10bout30)$values)
  mvpa.10bout60 <- bouts(data$mets60, wake = data$wake_loop, bout_length = 10, thresh_lower = 3, thresh_upper = 6, 
                         tol = 2, tol_lower = 1.25, tol_upper = 7)
  mvpa.10bout.min60 <- sum(mvpa.10bout60)/60
  mvpa.10bout.num60 <- sum(rle(mvpa.10bout60)$values)
  
  # Create summary data
  daily <- data.frame(subject, trimester, wearday, date, day, weekday, valid_day, daytime,
                     weartime = (sleep + sed + nap + lpa1 + mvpa1) , wakeware = (sed + lpa1 + mvpa1), nonwear, 
                     steps, stepping_min, 
                     sleep, nap, sed, stand, upright = (stand + stepping_min),
                     sed.breaks, sed.bout30m.min, sed.bout60m.min, sed.bout30m.num, sed.bout60m.num,
                     lpa1, lpa30, lpa60,
                     mvpa1, mvpa30, mvpa60,
                     mvpa.10bout.min1, mvpa.10bout.num1, mvpa.10bout.min30, mvpa.10bout.num30, mvpa.10bout.min60, mvpa.10bout.num60,
                     steps75.30, steps75.60, steps100.30, steps100.60,
                     steps_sleep, stand_sleep, stepping_sleep, upright_sleep = (stand_sleep + stepping_sleep), sed.breaks_sleep
  )
  return(daily)
}

#' Summarize Weekly Physical Activity and Sleep Data
#'
#' This function summarizes weekly physical activity and sleep metrics,
#' including steps, sedentary behavior, light physical activity (LPA), moderate-to-vigorous physical activity (MVPA), and sleep metrics.
#' @param data A data frame that processed via daily processing function.
#' @return A data frame summarizing weekly physical activity and sleep metrics for the given subject and visit.
#' @examples
#' # Example usage
#' weekly_summary <- weeklysum(data)
#' @export
weeklysum <- function(data){
  xsub <- subset(data, wearday %in% 2:8 & valid_day == 1) ## exclude day 1 from the data
  record_id <- data$subject[1]
  validdays <- sum(xsub$valid_day)
  trimester <- data$trimester[1]
  weekday <- sum(xsub$weekday == 1)
  weekend <- sum(xsub$weekday == 0)
  weartime <- mean(xsub$weartime, na.rm = T)
  wakeware <- mean(xsub$wakeware, na.rm = T)
  nonwear <- mean(xsub$nonwear, na.rm = T)
  
  steps <- mean(xsub$steps, na.rm = T)
  
  sleep <- mean(xsub$sleep, na.rm = T)
  nap <- mean(xsub$nap, na.rm = T)
  stand <- mean(xsub$stand, na.rm = T)
  stepping <- mean(xsub$stepping_min, na.rm = T)
  upright <- mean(xsub$upright, na.rm = T)
  
  # Sleep
  steps_sleep <- mean(xsub$steps_sleep, na.rm = T)
  stand_sleep <- mean(xsub$stand_sleep, na.rm = T)
  stepping_sleep <- mean(xsub$stepping_sleep, na.rm = T)
  upright_sleep <- mean(xsub$upright_sleep, na.rm = T)
  sed.breaks_sleep <- mean(xsub$sed.breaks_sleep, na.rm = T)
  
  # Sedentary
  sed <- mean(xsub$sed, na.rm = T)
  sed.breaks <- mean(xsub$sed.breaks, na.rm = T)
  sed.bout30m.min <- mean(xsub$sed.bout30m.min, na.rm = T)
  sed.bout60m.min <- mean(xsub$sed.bout60m.min, na.rm = T)
  sed.bout30m.num <- mean(xsub$sed.bout30m.num, na.rm = T)
  sed.bout60m.num <- mean(xsub$sed.bout60m.num, na.rm = T) 
  
  
  # 1-sec epoch summary
  lpa1 <- mean(xsub$lpa1, na.rm = T)
  mvpa1 <- mean(xsub$mvpa1, na.rm = T)
  mvpa.10bout.min1 <- mean(xsub$mvpa.10bout.min1, na.rm = T)
  mvpa.10bout.num1 <- mean(xsub$mvpa.10bout.num1, na.rm = T)
  
  # 30-sec epoch summary
  lpa30 <- mean(xsub$lpa30, na.rm = T)
  mvpa30 <- mean(xsub$mvpa30, na.rm = T)
  mvpa.10bout.min30 <- mean(xsub$mvpa.10bout.min30, na.rm = T)
  mvpa.10bout.num30 <- mean(xsub$mvpa.10bout.num30, na.rm = T)
  steps75.30 <- mean(xsub$steps75.30, na.rm = T)
  steps100.30 <- mean(xsub$steps100.30, na.rm = T)
  
  # 60-sec epoch summary
  lpa60 <- mean(xsub$lpa60, na.rm = T)
  mvpa60 <- mean(xsub$mvpa60, na.rm = T)
  mvpa.10bout.min60 <- mean(xsub$mvpa.10bout.min60, na.rm = T)
  mvpa.10bout.num60 <- mean(xsub$mvpa.10bout.num60, na.rm = T)
  steps75.60 <- mean(xsub$steps75.60, na.rm = T)
  steps100.60 <- mean(xsub$steps100.60, na.rm = T)
  
  # Behavior Proportion
  if(weartime > 0 ){
  sleep_perc <- sleep / weartime
  nap_perc <- nap / weartime
  sed_perc <- sed / weartime
  lpa30_perc <- lpa30 / weartime
  mvpa30_perc <- mvpa30 / weartime
  } else {
  print("There is no wear time.")
  sleep_perc <- NA
  nap_perc <- NA
  sed_perc <- NA
  lpa30_perc <- NA
  mvpa30_perc <- NA
  }
  weekly <- data.frame(record_id, trimester, validdays, weekday, weekend, weartime, nonwear, wakeware, 
                     steps, sleep, nap, stand, stepping, upright, 
                     sed, sed.breaks, sed.bout30m.min, sed.bout30m.num, sed.bout60m.min, sed.bout60m.num, 
                     lpa1, lpa30, lpa60,
                     mvpa1, mvpa30, mvpa60, 
                     mvpa.10bout.min1, mvpa.10bout.num1, mvpa.10bout.min30, mvpa.10bout.num30, mvpa.10bout.min60, mvpa.10bout.num60,
                     steps75.30, steps75.60, steps100.30, steps100.60,
                     sleep_perc, nap_perc, sed_perc, lpa30_perc, mvpa30_perc,
                     steps_sleep, stand_sleep, stepping_sleep, upright_sleep, sed.breaks_sleep
  )
  return(weekly)
}

#' Identify Bouts of Physical Activity Behaviors
#'
#' This function identifies bouts of physical activity behaviors based on metabolic equivalent tasks (METs) data. 
#' It accounts for periods of wear time and applies thresholds to determine valid bouts. 
#' This is also a simpler R version of bout function from accelerometry package.
#' 
#' @param mets Numeric vector of METs data.
#' @param wake Optional numeric vector indicating wear time (1 for wear, 0 for non-wear). If NULL, all data is considered valid wear time.
#' @param bout_length Numeric value indicating the minimum length of a bout in minutes. Default is 10 minutes.
#' @param thresh_lower Numeric value indicating the lower threshold for valid METs. Default is 1.25.
#' @param thresh_upper Numeric value indicating the upper threshold for valid METs. Default is 6.
#' @param tol Numeric value indicating the tolerance for non-valid METs within a bout. Default is 0.
#' @param tol_lower Numeric value indicating the lower threshold for tolerance METs. Default is 1.25.
#' @param tol_upper Numeric value indicating the upper threshold for tolerance METs. Default is 3.
#' @param epoch Numeric value indicating the epoch length in minutes. Default is 1 minute.
#' @return Integer vector indicating bouts of physical activity (1 for bout, 0 for non-bout).
#' @examples
#' mets <- c(1.5, 2.0, 1.0, 5.0, 6.0, 1.0, 2.5, 3.0, 1.0, 1.5)
#' wake <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
#' bouts(mets, wake, bout_length = 10, thresh_lower = 1.25, thresh_upper = 6, tol = 0, epoch = 1)
#' @export
bouts <- function(mets, wake = NULL, bout_length = 10, thresh_lower = 1.25, thresh_upper = 6, 
                  tol = 0, tol_lower = 1.25, tol_upper = 3, epoch = 1){
  n <- length(mets)
  if(n <= 600){return(0)}
  else{
    bout_length <- bout_length * (60/epoch)
    tol <- tol * (60/epoch)
    out <- integer(n)  # Initialize output vector with zeros
    
    if (is.null(wake)) {
      wear <- rep(1, n)
    } else {
      wear <- wake
    }
    
    valid_mets <- (wear == 1) & (mets >= thresh_lower) & (mets <= thresh_upper)
    
    if (tol == 0) {
      counter <- 0
      for (i in seq_len(n)) {
        if (valid_mets[i] & !is.na(valid_mets[i])) {
          counter <- counter + 1
        } else {
          if (counter >= bout_length) {
            out[(i - counter):(i - 1)] <- 1
          }
          counter <- 0
        }
        if (i == n && counter >= bout_length) {
          out[(i - counter + 1):i] <- 1
        }
      }
    } else if (tol > 0) {
      for (i in 1:(n - bout_length + 1)) {
        if (!valid_mets[i] & !is.na(valid_mets[i])) next
        tolcount <- 0
        tolcount2 <- 0
        last <- 0
        for (j in i:(i + bout_length - 1)) {
          if (valid_mets[j]& !is.na(valid_mets[j])) {
            tolcount2 <- 0
            last <- j
          } else {
            tolcount <- tolcount + 1
            tolcount2 <- tolcount2 + 1
          }
          if (tolcount == tol + 1) break
        }
        if (tolcount < tol + 1) {
          if (i + bout_length == n) {
            out[i:last] <- 1
            break
          }
          for (k in (i + bout_length):n) {
            if (valid_mets[k] & !is.na(valid_mets[k])){
              tolcount2 <- 0
              last <- k
            } else {
              tolcount2 <- tolcount2 + 1
            }
            if (tolcount2 == tol + 1 || k == n) {
              out[i:last] <- 1
              break
            }
          }
        }
      }
    }
  }  
  return(out)
}