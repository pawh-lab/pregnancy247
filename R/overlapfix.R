#' Resolve Overlaps Between Sleep and Intervention Time Windows
#'
#' Adjusts intervention time windows (`interwindow`) to avoid overlaps with
#' sleep time windows (`sleepwindow`).
#'
#' @details
#' This function operates by combining all start and end points into a single
#' timeline and sorting them chronologically. It iterates through the timeline
#' and, upon finding abnormal intersections between sleep and other windows
#' to resolve the overlap by swapping event orders or
#' shifting times by 60 seconds. For instance, an intervention window fully
#' contained within a sleep window may be removed.
#'
#' @param sleepwindow A list or data frame containing `start` and `end`
#'   POSIXct vectors. These are the baseline sleep intervals.
#' @param interwindow A list or data frame containing `start` and `end`
#'   POSIXct vectors. These are the work or monitor_off window intervals to be adjusted.
#'
#' @return A list containing the adjusted `start` and `end` POSIXct vectors
#'   for the `interwindow`.
#'
#' @export
#' @examples
#' # Create baseline sleep intervals
#' sleep_intervals <- data.frame(
#'   start = as.POSIXct(c("2025-07-25 22:10:00", "2025-07-26 01:30:00")),
#'   end = as.POSIXct(c("2025-07-25 22:50:00", "2025-07-26 02:00:00"))
#' )
#'
#' # Create intervention intervals to be adjusted
#' intervention_intervals <- data.frame(
#'   start = as.POSIXct(c(
#'     # 1. A non-overlapping interval
#'     "2025-07-25 21:00:00",
#'     # 2. An interval fully contained within a sleep period (target for removal)
#'     "2025-07-25 22:20:00",
#'     # 3. An interval with an overlapping start point (target for adjustment)
#'     "2025-07-26 01:50:00"
#'   )),
#'   end = as.POSIXct(c(
#'     "2025-07-25 21:30:00",
#'     "2025-07-25 22:30:00",
#'     "2025-07-26 02:10:00"
#'   ))
#' )
#'
#' # Run the function
#' adjusted_intervals <- overlapfix(sleep_intervals, intervention_intervals)
#'
#' # Check the results
#' print(adjusted_intervals)
#'
overlapfix <- function(sleepwindow, interwindow){
  
  # Making data frame using sleep window and testing window
  df_all <- do.call(rbind, list(  
    data.frame(seq = seq_along(interwindow$start), datetime = interwindow$start, 
               mode = "inter", index = 1, type = 1),
    data.frame(seq = seq_along(interwindow$end), datetime = interwindow$end, 
               mode = "inter", index = 2, type = 2),
    data.frame(seq = seq_along(sleepwindow$start), datetime = sleepwindow$start, 
               mode = "sleep", index = 1, type = 3),
    data.frame(seq = seq_along(sleepwindow$end), datetime = sleepwindow$end, 
               mode = "sleep", index = 2, type = 4)
  ))
  # Re-order the data based on datetime variable
  df_all <- df_all[order(df_all$datetime),]
  
  # For loop to check the overlapped window
  for(i in 2:length(!is.na(df_all$datetime))){
    
    # If the mode of window is different and the start-end index are the same
    if((df_all$mode[i-1] != df_all$mode[i]) && 
       (df_all$index[i-1] == df_all$index[i])){
      # Save the three lines in the temporary vector
      temp <- df_all[c(i-1, i, i+1), ]
      # If the mode of window is inter (inter, inter, sleep)
      if(names(sort(table(temp$mode), decreasing = TRUE)[1]) == "inter"){
        # And the i is inter, test window is located within sleep window
        if(temp$mode[2] == "inter"){
          # Put NA to the test window
          df_all[c(i, i+1), "datetime"] <- NA
          df_all <- df_all[order(df_all$datetime),] # re-order the data
          i <- 1 # reset the i for re-screening
        # And the i is sleep, test window end after sleep window
        } else if(temp$mode[2] == "sleep"){
          df_all[c(i, i+1),] <- df_all[c(i+1, i), ] # Shift test window end with sleep window start
          df_all$datetime[i] <- df_all$datetime[i+1] - 60 # replace test window end with sleep window start - 1 min
        }
      # If the mode of window is sleep (sleep, inter, sleep)  
      } else if(names(sort(table(temp$mode), decreasing = TRUE)[1]) == "sleep"){
        df_all[c(i, i+1), ] <- df_all[c(i+1, i), ] # Shift test window start with sleep window end
        df_all$datetime[i+1] <- df_all$datetime[i] + 60 # replace test window start with sleep window end + 1 min
      }
    }
  }
  # re-order the data by the type of window
  df_all <- df_all[order(df_all$type, df_all$seq), ]
  
  # return the test window
  return(list(start = df_all[df_all$type == 1, "datetime"],
              end = df_all[df_all$type == 2, "datetime"]))
}