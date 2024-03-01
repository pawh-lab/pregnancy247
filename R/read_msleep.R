#' Import Actiwatch sleep or nap data.
#'
#' Importing the raw Actiwatch sleep or nap for each subject.
#'
#' @inheritParams read_events
#' @param nap Logical denoting if nap data is to be imported.
#' Default is `FALSE`.
#'
#' @details Empty strings (`""`), missing values (`NA`), and not a number
#' (`NaN`) values exported by the Actiwatch device are all considered as
#' missing values for this importing process.
#'
#' As the function is meant for the use of the Pittsburgh team, it
#' assumes all of the subject data is located within a single folder. So, for
#' appropriate use based on `subject` and `trimester` only, the working
#' directory must be the main directory containing the data of each subject
#' within subject-specific folders based on IDs. It is also assumed the names
#' of the folders in subject-specific directories are
#' * `Visit 1`,
#' * `Visit 2`, and
#' * `Visit 3`
#'
#' for each trimester of pregancy.
#'
#' If `file` is provided, then the data is imported based on the `file` name
#' only even though `subject` and `trimester` are still required.
#'
#' The exporting data software by Actiwatch provides extraneous data,
#' so the data is imported twice to find the data actually needed.
#'
#' @return A `msleep` object, which is a `data.frame` containing the imported
#' Actiwatch sleep or nap data. The exporting of data software by
#' Actiwatch does not provide useable variable names. So, the variable names
#' are assigned here by column location. See the `vingettes` for details on the
#' variable names.
#'
#' @seealso `utils::read.csv()`
#'
#' @examples
#' \dontrun{
#' msleep <- read_msleep(subject = "1000-AB", trimester = 1)
#' }
#'
#' @export read_msleep
read_msleep <- function(subject, trimester, nap = FALSE, file = NULL, ...) {
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
  if (is.null(file)) {
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
  } else {
    files <- file
  }

  # Reading in temporary data set ####
  # to find the true length of the data set by excluding the rows after the
  # EXCLUDED interval types
  temp <- utils::read.csv(
    file = paste0(data_loc, "/", files),
    na.string = c("", "NA", "N/A"),
    skip = 67, header = FALSE, stringsAsFactors = FALSE, ...
  )
  tmp_vals <- c(
    "REST", "Rest Summary", "ACTIVE", "Active Summary", "SLEEP",
    "Sleep Summary", "DAILY", "Daily Summary", "EXCLUDED"
  )
  w <- which(temp[, 1] %in% tmp_vals)
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
    # Excluding extraneous columns
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
  dat$trimester <- as.integer(trimester)
  dat <- dplyr::relocate(dat, subject_id, .before = interval_type)
  dat <- dplyr::relocate(dat, trimester, .before = interval_type)
  rownames(dat) <- NULL
  dat$interval_num <- as.numeric(dat$interval_num)
  # Returning data set
  class(dat) <- c("msleep", "data.frame")
  return(dat)
}