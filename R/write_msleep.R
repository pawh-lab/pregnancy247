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

  # Exporting ####
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
    readr::write_csv(x = x, file = ff, na = "", ...)
  } else {
    readr::write_csv(
      x = x,
      file = file,
      na = "",
      ...
    )
  }
}