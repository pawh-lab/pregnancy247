read_msleep <- function(file, ...) {
  x <- readxl::read_xlsx(path = file, na = c("", NaN, NA), ...)
  class(x) <- c("msleep", "data.frame")
  return(x)
}