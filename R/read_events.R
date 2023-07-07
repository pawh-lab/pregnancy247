read_events <- function(subject, trimester, file = NULL, ...) {

  if (is.null(file)) {
    data_loc <- paste0(loc(subject, trimester), subject, trimester)
    files <- list.files(path = data_loc, pattern = "EventsEx.csv$")
    dat <- utils::read.csv(
      file = paste0(data_loc, "/", files),
      skip = 2, header = FALSE, sep = ";", stringsAsFactors = FALSE, ...
    )
  } else {
    dat <- utils::read.csv(
      file = file,
      skip = 2, header = FALSE, sep = ";", stringsAsFactors = FALSE, ...
    )
  }

  class(dat) <- c("data.events", "data.frame")
  return(dat)
}