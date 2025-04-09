#' Process data for the Pregnancy 24/7 study
#'
#' Process the Actiwatch, sleep diary, and activPAL data for the Pregnancy 24/7
#' study into 1 second epoch file and screening plot.
#'
#' @inheritParams read_sleep
#' @param plotsave Logical value indicating whether to save the plot as a PDF. 
#' Default is `TRUE`.
#' @param day1 Logical denoting this a valid day of wear for day 1 of the wear
#' period. Default is `TRUE`.
#' @param day2 Logical denoting this a valid day of wear for day 2 of the wear
#' period. Default is `TRUE`.
#' @param day3 Logical denoting this a valid day of wear for day 3 of the wear
#' period. Default is `TRUE`.
#' @param day4 Logical denoting this a valid day of wear for day 4 of the wear
#' period. Default is `TRUE`.
#' @param day5 Logical denoting this a valid day of wear for day 5 of the wear
#' period. Default is `TRUE`.
#' @param day6 Logical denoting this a valid day of wear for day 6 of the wear
#' period. Default is `TRUE`.
#' @param day7 Logical denoting this a valid day of wear for day 7 of the wear
#' period. Default is `TRUE`.
#' @param day8 Logical denoting this a valid day of wear for day 8 of the wear
#' period. Default is `TRUE`.
#' @param day9 Logical denoting this a valid day of wear for day 9 of the wear
#' period. Default is `TRUE`.
#'
#' @details This function is based on the specific needs of the Pregnancy 24/7
#' study. Thus, the decision to only include parameters `day1` through `day9`
#' to denote valid wear days was because the wear period for Pregnancy 24/7 only
#' lasted nine days. If added `day` parameters are needed, this function can be
#' adjusted for that change by either
#' * adding a specific `day` parameter or
#' * creating a new parameter that is a logical vector that is length of wear
#' period and removing the `day` parameters.
#'
#' The `day` parameters only need to be specified when a day is invalid and to
#' make that specification have `day = FALSE`.
#'
#' The entire creation of this function depends on the other functions in the
#' `pregnancy247` package. There is no `R` object returned by this function, but
#' files are written to their appropriate locations based on the `subject` and
#' `trimester` parameters.
#' 
#' Due to nature of the observational data, there is an inclusion of a second
#' interval of napping for each wear day. This is implemented through
#' `windows_nap()`.
#'
#' @examples
#' \dontrun{
#' # 0001-AB ####
#' ## Trimester 1 ####
#' process_data_1sec(subject = "0001-AB", trimester = 1)
#'  
#'
#' }
#'
#' @export process_data_1sec
process_data_1sec <- function(subject, trimester, plotsave = TRUE, 
                              day1 = TRUE, day2 = TRUE, day3 = TRUE,
                              day4 = TRUE, day5 = TRUE, day6 = TRUE,
                              day7 = TRUE, day8 = TRUE, day9 = TRUE){
  # Check R version ####
  if (as.integer(R.version$major) != 4 && as.numeric(R.version$minor) < 1) {
    stop("R version must be 4.1 or higher.")
  }
  
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
  # Import raw data ####
  
  ## Actiwatch and sleep diary data
  print("Choose Sleep data file")
  sleep_source <- choose.files(caption = "Choose Sleep Data file")
  
  if(isFALSE(grepl(pattern = "SleepD", sleep_source))){
    while(!grepl(pattern = "SleepD", sleep_source)){
      print("Please choose Sleep Data file")
      sleep_source <- choose.files(caption = "Choose Sleep Data file")
    }
  }
  
  sleep <- read_sleep(
    file = sleep_source, subject = subject, trimester = trimester
  )
  
  ## activPAL (events file) data
  print("Choose activPAL eventEx file")
  PAL_source <- choose.files(caption = "Choose activPAL eventEx file") # input eventEx file source
  
    # Check eventEx file algorithm
    if(isFALSE(grepl(pattern = "VANE", PAL_source))){
      while(!grepl(pattern = "VANE", PAL_source)){
        print("Please choose eventEx file processed using VANE algorithm")
        PAL_source <- choose.files(caption = "Choose activPAL eventEX file")
      }
    }
  
    if(isFALSE(grepl(pattern = paste0(subject, trimester), PAL_source))){
      while(!grepl(pattern = paste0(subject, trimester), PAL_source)){
        print("Please choose the eventEx file corresponding to the selected subject and trimester")
        PAL_source <- choose.files(caption = "Choose activPAL eventEX file")
      }
    }
  
  dat <- read_events(subject = subject, trimester = trimester, file = PAL_source) # read eventEx file
  
  # Processing of imported data ####
  
  ## Subject ID
  subject_id <- paste0(subject, trimester)
  
  ## Determining which days have valid days
  names(valid_days) <- paste0("day", 1:9)
  ## Listing the good days of data
  good_days <- which(valid_days)
  ## Creating a vector of all the days
  all_days <- 1:9
  names(all_days) <- paste0("day", all_days)
  
  ## Actiwatch and sleep diary data ####
  
  ### if 24 hours without sleep is true####
  if(!is.na(sleep$diary_24nosleep)){
    if(sleep$diary_24nosleep == 1){
      resp <- utils::menu(
        choices = c("Yes", "No"),
        graphics = FALSE, 
        title = paste0(
          subject_id, " has days where more than 24 hours have passed without sleep.",
          "\nWould you like to stop the data processing?"
        )
      )
      if(resp == 1){
        stop(
          paste0("Manual data processing is required.")
        )
      } 
    }
  }
  
  ### Diary Dates ####
  diary_dates <- unlist(sleep[1, grep("^diary_date", colnames(sleep))])
  diary_dates <- as.character(as.Date(diary_dates, tryFormats = c("%m/%d/%Y", "%Y-%m-%d")))
  na_dates <- !is.na(diary_dates) # this denotes whether a date is not missing
  diary_dates <- diary_dates[na_dates]
  
  ### Windows ####
  ### Finding windows of certain activities recorded by the subject or Actiwatch
  
  #### Sleep ####
  wind_sleep <- windows_sleep(sleep = sleep)
  sleep_start <- wind_sleep$start
  sleep_end <- wind_sleep$end
  
  #### Nap ####
  
  ##### First nap interval
  wind_nap <- windows_nap(sleep = sleep)
  nap_start <- wind_nap$start
  nap_end <- wind_nap$end
  
  ##### Second nap interval
  wind_nap_b <- windows_nap(sleep = sleep, interval = "second")
  nap_start_b <- wind_nap_b$start
  nap_end_b <- wind_nap_b$end
  
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
      nap_start_b, nap_end_b,
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
  len_nap <- length(c(nap_start, nap_end, nap_start_b, nap_end_b))
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
    off_times = wind_monitor, good_days = good_days
  )
  
  # Creating 1 second epoch data set ####
  sec_by_sec <- create_1s_epoch(
    data = merge_dat, good_days = good_days, remove_days = TRUE
  )
  
  # Graph ####
  sleep_times <- data.frame(
    sleep_start = c(
      sleep_start[-length(sleep_start)],
      nap_start[-length(nap_start)],
      nap_start_b[-length(nap_start_b)]
    ),
    sleep_stop = c(
      sleep_end[-length(sleep_end)],
      nap_end[-length(nap_end)],
      nap_end_b[-length(nap_end_b)]
    ),
    day = c(all_days[-1], rep(all_days[-length(all_days)], times = 2)),
    label = c(
      rep("sleep", length(all_days[-1])),
      rep("nap", 2 * length(all_days[-length(all_days)]))
    )
  )
  ## Removing any of sleeping/napping times that occurred on invalid days
  sleep_times <- subset(sleep_times, subset = day %in% good_days)
  sleep_times <- subset(sleep_times, !is.na(sleep_start) & !is.na(sleep_stop))
  
  ## Obtaining plot ####
  
    g <- plot_1s_epoch(data = sec_by_sec, sleep_times = sleep_times)
    # Save graph pdf
    if(plotsave){
      plot_dir <- if(exists("choose.dir")){
                    choose.dir(caption = "Choose the directory to save the plot")
                  } else {
                    tcltk::tk_choose.dir(caption = "Choose the directory to save the plot")
                  }
      file_name <- paste0(plot_dir, "/", subject_id, "graph.pdf")
      save_plot_1s_epoch(x = g, subject = subject, trimester, file = file_name, width = 11, height = 8.5)
    }
  
  # Save one-sec epoch file ####
  sec_dir <- if(exists("choose.dir")){
               choose.dir(caption = "Choose the directory to save the 1-sec epoch file")
             } else {
                tcltk::tk_choose.dir(caption = "Choose the directory to save the 1-sec epoch file")
             }
  data.table::fwrite(sec_by_sec, 
                     file = paste0(sec_dir, "/", subject_id, "_1sec.csv"), 
                     row.names = FALSE)
}
