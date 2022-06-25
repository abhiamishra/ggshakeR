#' Plotting passes
#'
#' This function allows you to plot various types of plots
#' that have passes as some sort of input. Data entered must have columns for which you want to plot with.
#' Compatible with StatsBomb and Opta data.
#'
#' @param data The data frame that stores your passing data. Opta data frame must contain at least the following columns: `x`, `y`, `finalX`, `finalY`
#' @param data_type Type of data that is being put in: opta or statsbomb. Default set to "statsbomb"
#' @param type indicates the type of plot to pass. "sep" separates successful and unsuccessful passes. "all" plots all passes on one pitch. Default = "sep"
#' @param progressive_pass indicates whether to map out progressive passes
#' @param cross indicates whether to map out crosses
#' @param shot indicates whether to map out shot assists
#' @param switch indicates whether to map out switches of play
#' @param outcome indicates whether you want successful ("suc"), unsuccessful ("unsuc"), or all ("all")
#' @param theme indicates what theme the map must be shown in: dark (default), white, rose, almond
#' @return returns a ggplot2 object
#'
#' @importFrom magrittr %>%
#' @import dplyr
#' @import ggplot2
#' @import ggsoccer
#' @export
#'
#' @examples
#' \dontrun{
#' plot  <- plot_pass(data, type = "all", progressive_pass = TRUE)
#' plot
#' }

plot_pass <- function(data, data_type = "statsbomb", type = "sep", 
                      progressive_pass = FALSE, cross = FALSE, shot = FALSE, switch = FALSE, 
                      outcome = "all", theme = "dark") {
  
  if (theme == "dark" || theme == "") {
    fill_b <- "#0d1117"
    color_b <- "white"
  } else if (theme == "white") {
    fill_b <- "#F5F5F5"
    color_b <- "black"
  } else if (theme == "rose") {
    fill_b <- "#FFE4E1"
    color_b <- "#696969"
  } else if (theme == "almond") {
    fill_b <- "#FFEBCD"
    color_b <- "#696969"
  }
  
  if (data_type == "opta") { ## OPTA ----
    
    if (nrow(data) > 0 &&
        sum(x = c("x", "y", "finalX", "finalY") %in% names(data)) == 4) {
    } else {
      print(c("x", "y", "finalX", "finalY"))
      stop("The dataset has insufficient columns and/or insufficient data.")
    }
    
    to_sb <- rescale_coordinates(from = pitch_opta, to = pitch_statsbomb)
    data$x <- to_sb$x(data$x)
    data$y <- to_sb$y(data$y)
    
    if (progressive_pass == TRUE) {
      data <- data %>%
        mutate(start = sqrt((120 - x)^2 + (40 - y)^2)) %>%
        mutate(end = sqrt((120 - finalX)^2 + (40 - finalY)^2)) %>%
        mutate(isProg = ifelse(end <= 0.75 * start,
                               1,
                               0))
      data <- data %>% filter(isProg == 1)
    }
    
    ### PLOT OPTA ----
    plot <- ggplot(data = data) +
      annotate_pitch(dimensions = pitch_statsbomb, colour = color_b,
                     fill = fill_b) +
      theme_pitch() +
      theme(panel.background = element_rect(fill = fill_b))
    
    plot <- plot +
      geom_segment(aes(x = x, y = 80 - y,
                       xend = finalX, yend = 80 - finalY, color = "red"),
                   lineend = "round", size = 1.5, arrow = arrow(length = unit(0.10, "inches")), 
                   stat = "identity", position = "identity") +
      labs(color = "Outcome of Pass")
    
    return(plot)
    
  } else if (data_type == "statsbomb") { ## STATSBOMB ----
    
    if (nrow(data) > 0) {
    } else {
      stop("The dataset has insufficient columns and/or insufficient data.")
    }
    
    if (outcome == "suc") {
      data <- data %>%
        filter(is.na(pass.outcome.name))
    } else if (outcome == "unsuc") {
      data <- data %>%
        filter(!is.na(pass.outcome.name))
    }
    
    data$pass.outcome.name <- tidyr::replace_na(data$pass.outcome.name, "Successful")
    data <- data %>% mutate(colorOutcome = ifelse(pass.outcome.name == "Successful",
                                                  "Successful",
                                                  "Unsuccessful"))
    
    if (progressive_pass == TRUE) {
      data <- data %>%
        mutate(start = sqrt((100 - x)^2 + (50 - y)^2)) %>%
        mutate(end = sqrt((100 - finalX)^2 + (50 - finalY)^2)) %>%
        mutate(isProg = ifelse(end <= 0.75 * start,
                               1,
                               0))
      
      data <- data %>% filter(isProg == 1)
    }
    
    if (cross == TRUE) {
      data <- data %>%
        filter(pass.cross == TRUE)
    }
    
    if (shot == TRUE) {
      data <- data %>%
        filter(pass.shot_assist == TRUE)
    }
    
    if (switch == TRUE) {
      data <- data %>%
        mutate(delta_y = abs(
          finalY - y
        )) %>%
        filter(delta_y >= 35)
    }
    
    if (progressive_pass == TRUE) {
      data <- data %>%
        mutate(start = sqrt((120 - x)^2 + (40 - y)^2)) %>%
        mutate(end = sqrt((120 - finalX)^2 + (40 - finalY)^2)) %>%
        mutate(isProg = ifelse(end <= 0.75 * start,
                               1,
                               0))
      data <- data %>% filter(isProg == 1)
      
    }
    
    ### PLOT STATSBOMB ----
    plot <- ggplot(data = data) +
      annotate_pitch(dimensions = pitch_statsbomb, colour = color_b,
                     fill = fill_b) +
      theme_pitch() +
      theme(panel.background = element_rect(fill = fill_b))
    
    if (nrow(data) > 0) {
      if (type == "sep") {
        plot <- plot +
          geom_segment(aes(x = x, y = 80 - y,
                           xend = finalX, yend = 80 - finalY, color = colorOutcome),
                       lineend = "round", size = 1.5, arrow = arrow(length = unit(0.10, "inches")), 
                       stat = "identity", position = "identity") +
          facet_grid(~colorOutcome) +
          labs(
            color = "Outcome of Pass"
          )
      } else if (type == "all") {
        plot <- plot +
          geom_segment(aes(x = x, y = 80 - y,
                           xend = finalX, yend = 80 - finalY, color = colorOutcome),
                       lineend = "round", size = 1.5, arrow = arrow(length = unit(0.10, "inches")), 
                       stat = "identity", position = "identity") +
          labs(
            color = "Outcome of Pass"
          )
      }
    }
    return(plot)
  } else {
    stop("Please input either 'statsbomb' OR 'opta' into the `data_type` argument.")
  }
}
