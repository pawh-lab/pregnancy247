#' Imports activity events data
#'
#' Imports the CSV (semi-colon separted value) activPAL EventsEX file
#' for the `subject` and `trimester` of interest that was exported with the
#' [activPAL](https://www.palt.com) software.
#'
#' @inheritParams read_sleep
#' @param file Either a path to a file, a connection, or literal data
#' (either a single string or a raw vector). The default value is `NULL`, which
#' uses the `subject` and `trimester` parameters to find the appropriate
#' filename.
#' @param ... Any other parameters needed for [utils::read.csv()].
#'
#' @details When `file` is the default value of `NULL` the `subject` and
#' `trimester` parameters along with the current working directory are used to
#' find the file that ends in `EventsEx.csv` by using the [loc()] function.
#' As the purpose of this function is to work in the background of the
#' [process_data()] function, a filename should only be specified when
#' `read_events()` is used outside of the [process_data()], usually for
#' individual examination of problematic data.
#'
#' By default, the column names are skipped as the activPAL software tends to
#' produce variable names that do NOT conform to the standard `R` naming
#' convention.
#'
#' @return A `data.events` object is returned, which is a `data.frame` that only
#' includes the raw activPAL EventsEx data for the `subject` of interest during
#' the `trimester` of interest.
#'
#' @seealso [utils::read.csv()], [loc()]
#'
#' @examples
#' \dontrun{
#' # numeric trimester
#' dat <- read_events("0001-AB", 3)
#' }
#'
#' \dontrun{
#' # charactter trimester
#' dat <- read_events("0001-AB", "3")
#' }
#'
#' @export read_events
read_events <- function(subject, trimester, file = NULL, ...) {

  if (is.null(file)){
    subject_id <- paste0(subject, trimester)
    data_loc <- paste0(loc(subject), subject_id)
    files <- list.files(path = data_loc, pattern = "EventsEx.csv$")
    if(grepl("CREA", files)){
      dat <- utils::read.csv(file = paste0(data_loc, "/", files),
                             sep = ";", skip = 1, header = TRUE, row.names = NULL, stringsAsFactors = FALSE, ...)
    } else if(grepl("VANE", files)){
      dat <- utils::read.csv(file = paste0(data_loc, "/", files),
                             sep = ";", skip = 1, header = TRUE, row.names = NULL, stringsAsFactors = FALSE, ...)
        if(length(dat) == 2){
          dat <- utils::read.csv(file = paste0(data_loc, "/", files),
                             sep = ";", skip = 14, header = TRUE, row.names = NULL, stringsAsFactors = FALSE, ...)
          }
      ## Matching the variable names with columns
      names(dat) <- names(dat)[-1] 
      dat <- dat[, -length(dat)] 
    } else{stop("VANE or CREA algorithms are eligible to process")}
  } else {
    if(grepl("CREA", file)){
      dat <- utils::read.csv(
        file = file,
        skip = 1, header = TRUE, sep = ";", stringsAsFactors = FALSE, ...)
    } else if(grepl("VANE", file)){
      dat <- utils::read.csv(file = file,
                             sep = ";", skip = 1, header = TRUE, row.names = NULL, stringsAsFactors = FALSE, ...)
      if(length(dat) == 2){
          dat <- utils::read.csv(file = file,
                             sep = ";", skip = 14, header = TRUE, row.names = NULL, stringsAsFactors = FALSE, ...)
          }
      ## Matching the variable names with columns
      names(dat) <- names(dat)[-1]
      dat <- dat[, -length(dat)]
    } else{stop("VANE or CREA algorithms are eligible to process")}
  }

  class(dat) <- c("data.events", "data.frame")
  return(dat)
}
