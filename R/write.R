#' Export processed events data
#'
#' Export events, 1 second EPOCH, daily summary, and wear period averages data
#' sets to `subject` specific folders.
#'
#' @name write
#'
#' @inheritParams read_events
#' @inheritParams readr::write_csv
#' @param ... Any other parameters needed for [readr::write_csv()].
#'
#' @details When `file` is the default value of `NULL` the `subject` and
#' `trimester` parameters along with the current working directory are used to
#' write the files that ends with
#' * `_eventsfile.csv` for events data to be exported with `write_events`,
#' * `_1secepoch.csv` for 1 second EPOCH data to be exported with
#' `write_1s_epoch`,
#' * `_daily_values.csv` for the daily wear data to be exported with
#' `write_daily`, and
#' * `_weekly_avgs.csv` for the averages data to be exported with `write_avgs`
#'
#' by using the [loc()] function. As the purpose of these functions are to work
#' in the background of the [process_data()] function, a filename should only be
#' specified when `write_(datatype)` is used outside of the [process_data()],
#' usually for individual examination of problematic data.
#'
#' In this collection of `write_(datatype)` functions, the `write_all_avgs`
#' function performs more tasks than simply writing a data set. This function
#' looks for data sets that already exists within the project directory that
#' contains a collection of wear period averages based on the site given by the
#' `subject` parameter. For Pregnancy 24/7, the data sets file names are
#' * `weekly_avgs_IOWA.csv` for The University of Iowa,
#' * `weekly_avgs_PITT.csv` for University of Pittsburgh, and
#' * `weekly_avgs_WVU.csv` for West Virginia University.
#'
#' If these files already exists, then the wear period averages for the
#' `subject` of interest during the `trimester` of interest is merged with the
#' the existing data set based on the site the `subject` visited.
#'
#' @seealso [readr::write_csv()]
#'
#' @examples
#' # See process_data() for an example of how these functions are used.
#'
#' @rdname write
#' @export write_events
write_events <- function(x, subject, trimester, file = NULL, ...) {

  if (is.null(file)) {
    subject_id <- paste0(subject, trimester)
    data_loc <- loc(subject = subject)
    readr::write_csv(
      x = x,
      file = paste0(data_loc, subject_id, "_eventfile.csv"),
      na = "",
      ...
    )
  } else {
    readr::write_csv(
      x = x,
      file = file,
      na = "",
      ...
    )
  }
}

#' @rdname write
#' @export write_1s_epoch
write_1s_epoch <- function(x, subject, trimester, file = NULL, ...) {

  if (is.null(file)) {
    subject_id <- paste0(subject, trimester)
    data_loc <- loc(subject = subject)
    readr::write_csv(
      x = x,
      file = paste0(data_loc, subject_id, "_1secepoch.csv"),
      na = "",
      ...
    )
  } else {
    readr::write_csv(
      x = x,
      file = file,
      na = "",
      ...
    )
  }
}

#' @rdname write
#' @export write_daily
write_daily <- function(x, subject, trimester, file = NULL, ...) {

  if (is.null(file)) {
    subject_id <- paste0(subject, trimester)
    data_loc <- loc(subject = subject)
    readr::write_csv(
      x = x,
      file = paste0(data_loc, subject_id, "_daily_values.csv"),
      na = "",
      ...
    )
  } else {
    readr::write_csv(
      x = x,
      file = file,
      na = "",
      ...
    )
  }
}

#' @rdname write
#' @export write_avgs
write_avgs <- function(x, subject, trimester, file = NULL, ...) {

  if (is.null(file)) {
    subject_id <- paste0(subject, trimester)
    data_loc <- loc(subject = subject)
    readr::write_csv(
      x = x,
      file = paste0(data_loc, subject_id, "_weekly_avgs.csv"),
      na = "",
      ...
    )
  } else {
    readr::write_csv(
      x = x,
      file = file,
      na = "",
      ...
    )
  }
}

#' @rdname write
#' @export write_all_avgs
write_all_avgs <- function(x, subject, trimester, file = NULL, ...) {
  data_site <- substr(subject, 1, 1)
  if (is.null(file)) {
    if (data_site == "1") {
      if (file.exists("./DATA_Iowa_activPAL/weekly_avgs_IOWA.csv")) {
        temp <- utils::read.csv(
          file = "./DATA_Iowa_activPAL/weekly_avgs_IOWA.csv",
          stringsAsFactors = FALSE
        )
        hold <- paste0(temp$subject_id, temp$trimester)
        temp <- subset(temp, subset = hold != paste0(subject, trimester))
        temp <- dplyr::bind_rows(temp, x)
        temp <- temp[order(temp$subject_id, temp$trimester), ]
        readr::write_csv(
          x = temp,
          file = "./DATA_Iowa_activPAL/weekly_avgs_IOWA.csv",
          na = "", ...
        )
      } else {
        readr::write_csv(
          x = x,
          file = "./DATA_Iowa_activPAL/weekly_avgs_IOWA.csv",
          na = "", ...
        )
      }
    } else if (data_site == "5") {
      if (file.exists("./DATA_Pitt_activPAL/weekly_avgs_PITT.csv")) {
        temp <- utils::read.csv(
          file = "./DATA_Pitt_activPAL/weekly_avgs_PITT.csv",
          stringsAsFactors = FALSE
        )
        hold <- paste0(temp$subject_id, temp$trimester)
        temp <- subset(temp, subset = hold != paste0(subject, trimester))
        temp <- dplyr::bind_rows(temp, x)
        temp <- temp[order(temp$subject_id, temp$trimester), ]
        readr::write_csv(
          x = temp,
          file = "./DATA_Pitt_activPAL/weekly_avgs_PITT.csv",
          na = "", ...
        )
      } else {
        readr::write_csv(
          x = x,
          file = "./DATA_Pitt_activPAL/weekly_avgs_PITT.csv",
          na = "", ...
        )
      }
    } else if (data_site == "8") {
      if (file.exists("./DATA_WVU_activPAL/weekly_avgs_WVU.csv")) {
        temp <- utils::read.csv(
          file = "./DATA_WVU_activPAL/weekly_avgs_WVU.csv",
          stringsAsFactors = FALSE
        )
        hold <- paste0(temp$subject_id, temp$trimester)
        temp <- subset(temp, subset = hold != paste0(subject, trimester))
        temp <- dplyr::bind_rows(temp, x)
        temp <- temp[order(temp$subject_id, temp$trimester), ]
        readr::write_csv(
          x = temp,
          file = "./DATA_WVU_activPAL/weekly_avgs_WVU.csv",
          na = "", ...
        )
      } else {
        readr::write_csv(
          x = x,
          file = "./DATA_WVU_activPAL/weekly_avgs_WVU.csv",
          na = "", ...
        )
      }
    }
  } else {
    if (file.exists(file)) {
        temp <- utils::read.csv(
          file = file,
          stringsAsFactors = FALSE
        )
        hold <- paste0(temp$subject_id, temp$trimester)
        temp <- subset(temp, subset = hold != paste0(subject, trimester))
        temp <- dplyr::bind_rows(temp, x)
        temp <- temp[order(temp$subject_id, temp$trimester), ]
        readr::write_csv(
          x = temp,
          file = file,
          na = "", ...
        )
      } else {
        readr::write_csv(
          x = x,
          file = file,
          na = "", ...
        )
      }
  }
}