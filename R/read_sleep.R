#' Import Actiwatch and sleep diary data.
#'
#' Importing the raw CSV (comma-separated values) Actiwatch and sleep diary
#' data based on the current working directory.
#'
#' @param file Either a path to a file, a connection, or literal data
#' (either a single string or a raw vector).
#' @param subject A character vector denoting the Pregnancy 24/7 subject.
#' @param trimester A character or integer vector denoting trimester of
#' pregnancy.
#' @param ... Any other parameters needed for [readr::read_csv()].
#'
#' @details The `file` argument and any other [readr::read_csv()] arguments are
#' used to read in the CSV Actiwatch and sleep diary data. After the data is
#' imported, the `subject` and `trimester` arguments are used to create an `id`
#' variable for subject and trimester of interest.
#'
#' The Pregnancy 24/7 study includes a rewear protocol for subjects that had
#' trouble with there activPAL device. Before selecting the sleep diary data for
#' subject of interest at the trimester of interest, the raw Actiwatch and sleep
#' diary data for the entire samples needs to be examined for the rewear
#' protocol. To better understand the rewear protocol, suppose subject 0001-AB
#' during the first trimester needed to have their data recorded again. For
#' processing purposes, it was assummed if the rewear protocol was initiated
#' then the original wear for the trimester is invalid. Thus, the data from the
#' second wear period during the trimester is the data to use and the original
#' data is not needed. Original wear is denoted with an "O" and rewear is
#' denoted with a "R" after the initials for the subject.
#'
#' This function does not require that the entire sample of the Pregnancy 24/7
#' study to be included in the data set import with the `file` name. It does
#' require that the subject and trimeseter of interest is listed in the data
#' set.
#'
#' @return A `data.sleep` object is returned, which is a `data.frame` that only
#' includes the raw Actiwatch and sleep diary data for the `subject` of interest
#' during the `trimester` of interest based on the temporarily created
#' `id` variable.
#'
#' @seealso [readr::read_csv()]
#'
#' @examples
#' \dontrun{
#' # numeric trimester
#' sleep <- read_sleep("./sleep.csv", "0001-AB", 3)
#' }
#'
#' \dontrun{
#' # character trimester
#' sleep <- read_sleep("./sleep.csv", "0001-AB", "3")
#' }
#'
#' @export read_sleep
read_sleep <- function(file, subject, trimester, ...) {
  # Reading in the sleep diary data for the entire sample ####
  options(readr.show_progress = FALSE)
  sleep <- utils::read.csv(file = file, na.string = c("", "NA", "N/A"), ...)

  # Creating subject ID variable ####
  ## For the subject of interest ####
  id <- paste0(subject, trimester)
  ## For the entire sample ####
  sleep$subject_id <- paste0(
    sleep$record_id,
    substr(sleep$redcap_event_name, 7, 7)
  )
  ### Moving newly created subject IDs to the first column ####
  sleep <- dplyr::relocate(sleep, subject_id)
  ### Adjusting for rewear protocol ####
  #### Moving the "0"'s and "R"'s to the end of the subject IDs and
  #### placing the trimemester numbers to after the initials
  sleep$subject_id <- paste0(
    substr(sleep$subject_id, 1, 7),
    substr(sleep$subject_id, 9, 9),
    substr(sleep$subject_id, 8, 8)
  )
  #### Sorting the data to account for rewear protocol, where "R"
  #### data is wanted not the "0" data
  sleep <- sleep[order(sleep$subject_id, decreasing = TRUE), ]
  #### Removing the original wear data if the rewear protocol was used.
  sleep <- sleep[!duplicated(substr(sleep$subject_id, 1, 8)), ]
  #### Cleaning up the IDs to exclude the indicator of rewear protocol
  sleep$subject_id <- substr(sleep$subject_id, 1, 8)
  #### Sorting data to the correct order
  sleep <- sleep[order(sleep$subject_id), ]

  # Pulling subject of interest sleep diary data ####
  sleep <- subset(sleep, subset = subject_id == id)
  row.names(sleep) <- NULL
  ## Changing how missing values are labeled
  sleep[which(sleep == "")] <- NA

  # returning subject of interest sleep diary data ####
  class(sleep) <- c("data.sleep", "data.frame")
  return(sleep)
}