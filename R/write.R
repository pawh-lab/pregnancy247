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
      if (file.exists("./DATA_Iowa_activPAL/weekly_avgs_PITT.csv")) {
        temp <- utils::read.csv(
          file = "./DATA_Iowa_activPAL/weekly_avgs_PITT.csv",
          stringsAsFactors = FALSE
        )
        hold <- paste0(temp$subject_id, temp$trimester)
        temp <- subset(temp, subset = hold != paste0(subject, trimester))
        temp <- dplyr::bind_rows(temp, x)
        temp <- temp[order(temp$subject_id, temp$trimester), ]
        readr::write_csv(
          x = temp,
          file = "./DATA_Iowa_activPAL/weekly_avgs_PITT.csv",
          na = "", ...
        )
      } else {
        readr::write_csv(
          x = x,
          file = "./DATA_Iowa_activPAL/weekly_avgs_PITT.csv",
          na = "", ...
        )
      }
    } else if (data_site == "8") {
      if (file.exists("./DATA_Iowa_activPAL/weekly_avgs_WVU.csv")) {
        temp <- utils::read.csv(
          file = "./DATA_Iowa_activPAL/weekly_avgs_WVU.csv",
          stringsAsFactors = FALSE
        )
        hold <- paste0(temp$subject_id, temp$trimester)
        temp <- subset(temp, subset = hold != paste0(subject, trimester))
        temp <- dplyr::bind_rows(temp, x)
        temp <- temp[order(temp$subject_id, temp$trimester), ]
        readr::write_csv(
          x = temp,
          file = "./DATA_Iowa_activPAL/weekly_avgs_WVU.csv",
          na = "", ...
        )
      } else {
        readr::write_csv(
          x = x,
          file = "./DATA_Iowa_activPAL/weekly_avgs_WVU.csv",
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