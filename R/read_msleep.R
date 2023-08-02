#' Import master sleep and nap data.
#'
#' Importing the raw ...
#'
#' @details Empty strings (`""`), missing values (`NA`), and not a number
#' (`NaN`) values exported by the ActivWATCH device are all considered as
#' missing values for this importing process.
#'
#' @return A `msleep` object, which is a `data.frame` containing the imported
#' master sleep or nap data.
#'
#' @seealso `utils::read.csv()`
#'
#' @examples
#' # rest <- read_msleep()
#'
#' @export read_msleep
read_msleep <- function(subject, trimester, nap = FALSE, ...) {
  # Checking parameter values ####
  if (!is.character(subject)) {
    stop("subject must be a character string denoting subject ID.")
  }
  if (nchar(trimester) != 1 || !(as.integer(trimester) %in% c(1, 2, 3))) {
    stop("trimester must be either 1, 2, 3 denoting pregnancy trimester.")
  }
  if (!is.logical(nap)) {
    stop("nap must be a logical (TRUE/FALSE) denoting if screening of nap data")
  }

  # Setting file directory based on subject ID and trimester ####
  data_loc <- paste0("./", subject, "/", "Visit ", trimester)
  # Finding file based on if it is a nap ####
  if (nap) {
    files <- list.files(
      path = data_loc,
      pattern = "NAPSONLY.csv$"
    )
  } else {
    files <- list.files(
      path = data_loc,
      pattern = "RESTONLY.csv$"
    )
  }
  # Reading in temporary data set ####
  # to find the true length of the data set by excluding the rows after the
  # EXCLUDED interval types
  temp <- utils::read.csv(
    file = paste0(data_loc, "/", files),
    na.string = c("", "NA", "N/A"),
    skip = 67, header = FALSE, stringsAsFactors = FALSE, ...
  )
  w <- which(temp[, 1] == "EXCLUDED")
  w <- w[length(w)]
  # Importing the data ####
  dat <- utils::read.csv(
    file = paste0(data_loc, "/", files),
    na.string = c("", "NA", "N/A", "NaN"),
    skip = 67, nrow = w, header = FALSE,
    stringsAsFactors = FALSE, ...
  )
  # Subsetting the data to only include REST and SLEEP intervals ####
  dat <- subset(dat, subset = V1 %in% c("REST", "SLEEP"))
  # Providing column names ####
  if (ncol(dat) > 71) {
    dat <- dat[, 1:71]
  }
  names(dat) <- c(
    "interval_type", "interval_num", "start_date", "start_day", "start_time",
    "end_date", "end_day", "end_time", "duration", "off_wrist", "off_wrist_pct",
    "total_ac", "avg_ac_min", "avg_ac_epoch", "std_ac", "max_ac", "inv_time_ac",
    "invalid_ac_pct", "inv_time_sw", "invalid_sw_pct", "onset_latency",
    "snooze_time", "efficiency", "waso", "wake_time", "wake_pct",
    "wake_bouts_num", "avg_wake_b", "sleep_time", "sleep_pct", "sleep_bouts",
    "avg_sleep_b", "immobile_time", "immobile_pct", "imm_bouts_num",
    "avg_imm_bout", "mobile_time", "mobile_pct", "mob_bouts_num",
    "avg_mob_bout", "imm_b_1min_num", "imm_b_1min_pct", "fragmentation",
    "exposure_white", "avg_white", "std_white", "max_white", "talt_white",
    "inv_time_white", "invalid_white_pct", "exposure_red", "avg_red", "std_red",
    "max_red", "talt_red", "inv_time_red", "invalid_red_pct", "exposure_green",
    "avg_green", "std_green", "max_green", "talt_green", "inv_time_green",
    "invalid_green_pct", "exposure_blue", "avg_blue", "std_blue", "max_blue",
    "talt_blue", "inv_time_blue", "invalid_blue_pct"
  )
  # Cleaning up resulting data ####
  dat$subject_id <- subject
  dat$trimester <- trimester
  dat <- dplyr::relocate(dat, subject, .before = interval_type)
  dat <- dplyr::relocate(dat, trimester, .before = interval_type)
  rownames(dat) <- NULL
  # Returning data set
  class(dat) <- c("msleep", "data.frame")
  return(dat)
}