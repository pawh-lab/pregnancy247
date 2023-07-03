read_events <- function(subject, trimester) {
  # Finding site location ####
  data_site <- substr(subject, 1, 1)

  if (data_site == "1") {
    data_loc <- paste0(
      "./DATA_Iowa_activPAL/",
      subject, "/", subject, trimester
    )
    files <- list.files(path = data_loc, pattern = "EventsEx.csv$")
    dat <- utils::read.csv(
      file = paste0(data_loc, "/", files),
      skip = 2, header = FALSE, sep = ";", stringsAsFactors = FALSE
    )
  } else if (data_loc == "5") {
    data_loc <- paste0(
      "./DATA_Pitt_activPAL/",
      subject, "/", subject, trimester
    )
    files <- list.files(path = data_loc, pattern = "EventsEx.csv$")
    dat <- utils::read.csv(
      file = paste0(data_loc, "/", files),
      skip = 2, header = FALSE, sep = ";", stringsAsFactors = FALSE
    )
  } else if (data_loc == "8") {
    data_loc <- paste0(
      "./DATA_WVU_activPAL/",
      subject, "/", subject, trimester
    )
    files <- list.files(path = data_loc, pattern = "EventsEx.csv$")
    dat <- utils::read.csv(
      file = paste0(data_loc, "/", files),
      skip = 2, header = FALSE, sep = ";", stringsAsFactors = FALSE
    )
  }
  class(dat) <- c("data.events", "data.frame")
  return(dat)
}