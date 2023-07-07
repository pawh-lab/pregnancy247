save_plot_1s_epoch <- function(x, subject, trimester, file = NULL, ...) {
  if (is.null(file)) {
    data_site <- substr(subject, 1, 1)
    subject_id <- paste0(subject, trimester)
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
    pdf(file = paste0(data_loc, subject_id, "_graph.pdf"), ...)
    plot(x)
    dev.off()
  } else {
    pdf(file = file, ...)
    plot(x)
    dev.off()
  }
}