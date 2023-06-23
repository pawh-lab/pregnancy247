#' Checking current working directory
#' 
#' Checking that current working directory has the sub-directories:
#' `DATA_Iowa_activPAL`, `DATA_Pitt_activPAL`, and `DATA_WVU_activPAL`, 
#' which contains all raw sleep diary and ActivPAL data.
#' 
#' @param path A character vector of full path names; the default corresponds to 
#' the working directory, `getwd()`. Also, `path` sets the working directory
#' with `setwd(path)`.
#' 
#' @return Error message is returned if current working directory does not have 
#' the appropriate sub-directories else nothing is returned.
#' 
#' @seealso `getwd()`, `dir()`
#'
#' @examples
#' # directory_check() # works if current directory has sub-directories
#' # directory_check(path = ".") # works if current directory has sub-directories
#' # directory_check(path = "../") # error by moving out of current directory
#' 
#' @export directory_check
directory_check <- function(path = ".") {
    base::setwd(path)
    dirs <- base::dir()
    dirs_data <- c(
        "DATA_Iowa_activPAL",
        "DATA_Pitt_activPAL",
        "DATA_WVU_activPAL"
    )
    if (!all(dirs_data %in% dirs)) {
        stop(
            paste0(
                "Current directory does not contain the sub-directories:\n",
                "\t", dirs_data[1], "\n\t", dirs_data[2], "\n\t", dirs_data[3]
            )
        )
    }
}