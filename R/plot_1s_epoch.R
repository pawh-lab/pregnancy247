plot_1s_epoch <- function(data, sleep_times = NULL, plot = FALSE) {
  # Checking parameter values ####
  if (!("one.epoch.data" %in% class(data))) {
    stop("data must be a one.epoch.data produced by create_1s_epoch")
  }

  if (!is.null(sleep_times)) {
    vars_needed <- c("sleep_start", "sleep_stop", "day", "label")
    if (!all(colnames(sleep_times) %in% vars_needed)) {
      stop("sleep_times does not contain the proper variables to include sleep times") #nolint
    }
    if (!all(unique(sleep_times$day) %in% unique(data$wear_day))) {
      stop("Valid wear days must match between data and sleep_times")
    }
  }

  # Mutating data to plot events and activities ####
  graph_data <- data

  ## Grouping activities based on 50s or 300s intervals ####
  hold1 <- lapply(
    X = table(graph_data$wear_day),
    FUN = function(x) {
      seq(1, x)
    }
  )
  hold2 <- lapply(
    X = seq_along(hold1),
    FUN = function(j) {
      if (max(hold1[[j]]) < 300) {
        return(ceiling(hold1[[j]] / 50))
      } else {
        return(ceiling(hold1[[j]] / 300))
      }
    }
  )
  graph_data$group_column <- unlist(hold2)

  ## Pulling times events occurred ####
  graph_data$o_clock <- strftime(
    x = graph_data$time, format = "%H:%M:%S", tz = "UTC"
  )

  ### Grouping the data and summarizing it
  graph_data <- dplyr::group_by(graph_data, wear_day, group_column) |>
    dplyr::summarise(
      sleep = sum(wake_loop == 0),
      wake = sum(wake_loop == 1),
      non_Wear = sum(wake_loop == 99),
      working = sum(work_loop == 1),
      not_working = sum(work_loop == 0),
      laying = sum(activity == 0),
      standing = sum(activity == 1),
      walking = sum(activity == 2),
      day = max(wear_day),
      o_clock = min(time),
      .groups = "keep"
    ) |>
    tidyr::pivot_longer(
      cols = c(laying, standing, walking),
      names_to = "activity",
      values_to = "interval"
    )

  graph_data <- dplyr::group_by(graph_data, day) |>
    dplyr::mutate(time_of_day = group_column - min(group_column) + 1)

  # Creating plot object ####
  cols <- c("sky blue", "coral", "forest green")
  g <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = tidyr::drop_na(graph_data),
      ggplot2::aes(x = o_clock, y = interval, color = as.factor(activity)),
      linewidth = 1
    ) +
    ggplot2::labs(x = "Time of Day", y = "Duration (s)", color = "Event Type") +
    ggplot2::theme_grey() +
    ggplot2::scale_color_manual(values = cols) +
    ggplot2::facet_wrap(day ~ ., scales = "free_x", ncol = 1) +
    ggplot2::scale_y_continuous(breaks = c(0, 150, 300)) +
    ggplot2::scale_x_datetime(date_breaks = "6 hours")

  ## Adding sleep time windows ####
  if (!is.null(sleep_times)) {
    g <- g + ggplot2::geom_rect(
      data = sleep_times,
      ggplot2::aes(
        xmin = sleep_start,
        xmax = sleep_stop,
        ymin = -Inf,
        ymax = Inf,
        fill = label
      ),
      alpha = .2
    )
  }

  # Plotting and returning plot object ####
  if (plot) {
    plot(g)
  }
  return(g)
}