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
#' @examples
#' directory_check()
#' directory_check(path = ".")
#' directory_check(path = "../") # error
#' 
#' @export directory_check
directory_check <- function(path = ".") {
    setwd(path)
    dirs <- dir()
    dirs_data <- c("DATA_Iowa_activPAL",  "DATA_Pitt_activPAL", "DATA_WVU_activPAL")
    if (!all(dirs_data %in% dirs)) {
        stop(
            paste0(
                "Current working direcotry does not contain the sub-directories:\n",
                "\t", dirs_data[1], "\n\t", dirs_data[2], "\n\t", dirs_data[3], "\n",
                "  which contains the raw sleep diary and ActivPal data.\n"
            )
        )
    }
}