save_plot_1s_epoch <- function(x, subject, trimester, file = NULL, ...) {
  if (is.null(file)) {
    subject_id <- paste0(subject, trimester)
    data_loc <- loc(subject)
    pdf(file = paste0(data_loc, subject_id, "_graph.pdf"), ...)
    plot(x)
    dev.off()
  } else {
    pdf(file = file, ...)
    plot(x)
    dev.off()
  }
}