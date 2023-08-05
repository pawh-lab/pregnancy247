#' Export screened sleep or nap Actiwatch data
#'
#' Export screened subject-specific sleep or nap Actiwatch data to
#' `subject`-specific directories.
#'
#' @name write_msleep
#'
#' @inheritParams readr::write_csv
#' @inheritParams read_msleep
#' @param ... Any other parameters needed for [readr::write_csv()].
#'
#' @details When `file` is the default value of `NULL` the `subject` and
#' `trimester` parameters along with the current working directory are used to
#' write the files that ends with
#' * `_napsonly_flag.csv` if `nap = TRUE` and
#' * `_restonly_flag.csv` by default.
#'
#' These functions performs more tasks than simply writing a data set. The
#' `write_all_msleep` function looks for data sets that already exists within
#' the project directory that contains a collection of wear period data based on
#' the type of rest (sleep or nap) given by the `nap` parameter. For Pregnancy
#' 24/7, the data sets file names are
#' * `master_dataset_napsonly.csv` if `nap = TRUE` and
#' * `master_dataset_restonly.csv` by default.
#'
#' If these files already exists, then the wear period data for the
#' `subject` of interest during the `trimester` of interest is merged with the
#' the existing data set based on the type of rest for the `subject`.
#'
#' @seealso [readr::write_csv()]
#'
#' @examples
#' # See quality_check() for an example of how these functions are used.
#'
#' @rdname write_msleep
#' @export write_msleep
write_msleep <- function(x, subject, trimester, nap = FALSE, file = NULL, ...) {
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

  # Determining file name ####
  if (is.null(file)) {
    data_loc <- paste0("./", subject, "/", "Visit ", trimester)
    if (nap) {
      ff <- paste0(
        data_loc, "/", paste0(subject, trimester), "_napsonly_flag.csv"
      )
    } else {
      ff <- paste0(
        data_loc, "/", paste0(subject, trimester), "_restonly_flag.csv"
      )
    }
  } else {
    ff <- file
  }

  # Exporting data ####
  readr::write_csv(x = x, file = ff, na = "", ...)
}

#' @rdname write_msleep
#' @export write_all_msleep
write_all_msleep <- function(x, subject, trimester, nap = FALSE, file = NULL, ...) { # nolint

  # Determining file name ####
  if (is.null(file)) {
    if (nap) {
      ff <- "./master_dataset_napsonly.csv"
    } else {
      ff <- "./master_dataset_restonly.csv"
    }
  } else {
    ff <- file
  }

  # Exporting data ####
  if (file.exists(ff)) {
    ## Importing previous data to join with new data
    temp <- utils::read.csv(
      file = ff,
      stringsAsFactors = FALSE,
      na.string = "",
      colClasses = c(
        rep(c("character", "numeric"), times = 2),
        rep("character", 6),
        rep("numeric", 64), "character"
      )
    )
    hold <- paste0(subject, trimester)
    temp <- subset(
      x = temp,
      subset = paste0(temp$subject_id, temp$trimester) != hold
    )
    temp <- dplyr::bind_rows(temp, x)
    temp <- temp[order(temp$subject_id, temp$trimester), ]
    readr::write_csv(
      x = temp,
      file = ff,
      na = "",
      ...
    )
  } else {
    # Exporting data if it is the first collection
    readr::write_csv(
      x = x,
      file = ff,
      na = "",
      ...
    )
  }
}