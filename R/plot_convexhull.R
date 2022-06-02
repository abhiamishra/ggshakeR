#' Function for plotting convex hulls
#'
#' This function allows for data, that can be from Opta or StatsBomb, to be used
#' for plotting convex hulls on top of an outline of a football pitch.
#'
#' @param data Data frame that houses pass data. Opta data frame must contain atleast the following columns: `x`, `y`, `finalX`, `finalY`, `playerId`
#' @param data_type Type of data that is being put in: opta or statsbomb. Default set to "statsbomb"
#' @param color The color of the outline of the convex hull
#' @param title_plot Title of the plot
#' @param theme Indicates what theme the map must be shown in: dark (default), white, rose, almond
#' @return a ggplot2 object
#'
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @import ggsoccer
#' @import purrr
#'
#' @export
#'
#' @examples
#' \dontrun{
#' plot <- plot_convexhull(data = data, data_type = "opta", color = "blue", title_plot = "Team 1")
#' plot
#' }

plot_convexhull <- function(data, data_type = "statsbomb", 
                            color = "#E74C3C", title_plot = "", theme = "dark") {
  
  if (data_type == "opta") {
    if (nrow(data) > 0 &&
        sum(x = c("x", "y", "finalX", "finalY", "playerId") %in% names(data)) == 5) {
    } else {
      print(c("x", "y", "finalX", "finalY", "playerId"))
      stop("The dataset has insufficient columns and/or insufficient data.")
    }
    
    to_sb <- rescale_coordinates(from = pitch_opta, to = pitch_statsbomb)
    data$x <- to_sb$x(data$x)
    data$y <- to_sb$y(data$y)
    
    data <- data %>%
      drop_na(playerId, x, y)
    
  } else if (data_type == "statsbomb") {
    
    if (!"playerId" %in% colnames(data)) {
      data <- data %>%
        mutate(playerId = player.name)
    }
    
    data <- data %>% drop_na(playerId, x, y)
  }
  
  if (theme == "dark") {
    fill_b <- "#0d1117"
    colour_b <- "white"
  } else if (theme == "white") {
    fill_b <- "#F5F5F5"
    colour_b <- "black"
  } else if (theme == "rose") {
    fill_b <- "#FFE4E1"
    colour_b <- "#696969"
  } else if (theme == "almond") {
    fill_b <- "#FFEBCD"
    colour_b <- "#696969"
  }
  
  list_data <- split(data, data$playerId)
  
  hull_data <- list_data %>%
    purrr::map(hull_fun) %>%
    purrr::reduce(full_join)
  
  if (title_plot == "") {
    title_plot <- "Convex Hulls"
  }
  
  convex_hull <- ggplot(hull_data) +
    annotate_pitch(dimensions = pitch_statsbomb, fill = fill_b, colour = colour_b) +
    theme_pitch() +
    geom_point(data = data, aes(x = x, y = y), alpha = 0.5, colour = colour_b) +
    geom_polygon(aes(x = x, y = y), colour = color, alpha = 0.2, fill = color, size = 1) +
    facet_wrap(~playerId) +
    labs(title = title_plot,
         x = "Direction of play faces rightward") +
    theme(plot.background = element_rect(fill = fill_b, colour = NA),
          panel.background = element_rect(fill = fill_b, colour = NA),
          strip.background = element_rect(fill = fill_b, colour = NA),
          strip.text = element_text(colour = colour_b, size = 10),
          plot.title = element_text(colour = colour_b, size = 18, hjust = 0.5, face = "bold"),
          axis.title.x = element_text(colour = colour_b, size = 12, face = "bold"))
  
  if (data_type == "statsbomb") {
    convex_hull <- convex_hull +
      scale_y_reverse()
  } else if (data_type == "opta") {
    convex_hull
  }
  
  return(convex_hull)
}