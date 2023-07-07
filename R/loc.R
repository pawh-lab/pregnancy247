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