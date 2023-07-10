#' Export 1 second EPOCH plot
#'
#' Save the 1 second EPOCH plot as a PDF document based on the `subject` or
#' `file` parameter.
#'
#' @param x A [graphics::plot()] or [ggplot2::ggplot()] object to be exported.
#' @inheritParams write
#' @param ... Any other parameters needed for [grDevices::pdf()]
#'
#' @details When `file` is the default value of `NULL` the `subject` and
#' `trimester` parameters along with the current working directory are used to
#' write the files that ends with `_grpahic.pdf` by using the [loc()] function.
#' As the purpose of this function is to work in the background of the
#' [process_data()] function, a filename should only be specified when
#' `save_plot_1s_epoch` is used outside of the [process_data()], usually for
#' individual examination of problematic data.
#'
#' @seealso [pregnancy247::plot_1s_epoch()], [grDevices::pdf()],
#' [graphics::plot()]
#'
#' @examples
#' \dontrun{
#' process_dat <- process_events(data = dat)
#' sec_by_sec <- create_1s_epoch(data = process_dat)
#' g <- plot_1s_epoch(sec_by_sec)
#' save_plot_1s_epoch(x = g, subject = "0001-AB", 1, width = 11, height = 8.5)
#' }
#'
#' @export save_plot_1s_epoch
save_plot_1s_epoch <- function(x, subject, trimester, file = NULL, ...) {
  if (is.null(file)) {
    subject_id <- paste0(subject, trimester)
    data_loc <- loc(subject)
    grDevices::pdf(file = paste0(data_loc, subject_id, "_graph.pdf"), ...)
    graphics::plot(x)
    grDevices::dev.off()
  } else {
    grDevices::pdf(file = file, ...)
    graphics::plot(x)
    grDevices::dev.off()
  }
}