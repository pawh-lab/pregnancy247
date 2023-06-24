#' Creates a data dictionary and switches out special characters
#'
#' Creates a data dictionary for a data set that originally had special
#' characters as variable names.
#'
#' @param x A named object.
#' @param out Either a path to a file, a connection, or literal data (either a
#' single string or a raw vector) to export the data dictionary. The default is
#' `NULL`, which does not result in the creation of data dictionary.
#'
#' @details The `x` must be a named object as the main purpose of this function
#' is create a data dictionary for a data set that has special characters in the
#' variable names.
#'
#' Every time this function is executed the named object `x` alters the names to
#' remove special character. When the `out` parameter is specified then a data
#' dictionary is created with the original and new variable names. The current
#' version of this function adjusts for special characters such as spaces, "-",
#' "/", "%", and "#". The naming convention used follows the
#' [Tidyverse](https://style.tidyverse.org) style guide.
#'
#' @return If the `out` parameter is `NULL` then only character vector with the
#' new variable names is returned.
#'
#' @examples
#' y <- 1:4
#' names(y) <- c("a/a", "b-b", "%c", "d#")
#' names(y) <- create_dict(y)
#'
#' @export create_dict
create_dict <- function(x, out = NULL) {
  # Checking object has names ####
  if (is.null(names(x))) {
    stop("x must be named object.")
  }

  # Switching out special characters ####
  nam <- names(x)
  nam <- gsub(" ", "_", nam)
  nam <- gsub("-|/", "_", nam)
  nam <- gsub("^%", "pct_", nam)
  nam <- gsub("%$", "_pct", nam)
  nam <- gsub("^#", "num_", nam)
  nam <- gsub("#$", "_num", nam)

  # Exporting dictionary or variable names ####
  if (!is.null(out)) {
    dict <- data.frame(
      names_origin = names(x),
      names_new = nam
    )
    readr::write_csv(x = dict, file = out)
    return(nam)
  } else {
    return(nam)
  }
}