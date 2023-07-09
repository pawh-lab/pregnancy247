#' Determine the data location
#'
#' A helper function for the `read` and `write` functions that returns the
#' directory path based on the `subject` ID.
#'
#' @inheritParams read_sleep
#'
#' @details This function is meant to work within the [read_events()] and
#' [write_events()] functions based on no errors being produced by
#' [check_dir()].
#'
#' @return A `character` value that is the directory path for the `subject` of
#' interest. If `subject` starts with a
#' * `1` then the start of the directory path will be `./DATA_Iowa_activPAL`,
#' * `5` then the start of the directory path will be `./DATA_Pitt_activPAL`, or
#' * `8` then the start of the directory path will be `./DATA_WVU_activPAL`.
#'
#' @examples
#' loc("1001-AB")
#' loc("5001-AB")
#' loc("8001-AB")
#'
#' @export loc
loc <- function(subject) {
  data_site <- substr(subject, 1, 1)
  if (data_site == "1") {
    data_loc <- paste0(
      "./DATA_Iowa_activPAL/",
      subject, "/"
    )
  } else if (data_site == "5") {
    data_loc <- paste0(
      "./DATA_Pitt_activPAL/",
      subject, "/"
    )
  } else if (data_site == "8") {
    data_loc <- paste0(
      "./DATA_WVU_activPAL/",
      subject, "/"
    )
  }
  return(data_loc)
}