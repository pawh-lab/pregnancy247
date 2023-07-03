#' Import master sleep and nap data.
#'
#' Importing the raw Microsoft Excel (.xlsx) master sleep and nap data exported
#' from the ActivWATCH device.
#'
#' @param file Either a path to a file, a connection, or literal data
#' (either a single string or a raw vector).
#' @param ... any other parameters needed for `readxl::read_xlsx()`.
#'
#' @details Empty strings (`""`), missing values (`NA`), and not a number
#' (`NaN`) values exported by the ActivWATCH device are all considered as
#' missing values for this importing process.
#'
#' @return A `msleep` object, which is a `data.frame` containing the imported
#' master sleep or nap data.
#'
#' @seealso `readxl::read_xlsx()`
#'
#' @examples
#' # rest <- read_msleep("msleep.xlsx")
#'
#' @export read_msleep
read_msleep <- function(file, ...) {
  x <- readxl::read_xlsx(path = file, na = c("", NaN, NA), ...)
  class(x) <- c("msleep", "data.frame")
  return(x)
}