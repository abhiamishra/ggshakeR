#' Function for plotting voronoi diagrams on the pitch
#'
#' The function allows for creation of voronoi diagrams on a football pitch
#' with either Opta or Statsbomb data.
#'
#' @param data Dataframe that houses pass data. Opta/Statsbomb dataframe must contain atleast the following columns: `x`, `y`
#' @param data_type Type of data that is being put in: opta or statsbomb. Default set to "statsbomb".
#' @param colour The colour of the points in the voronoi plot.
#' @param fill Name of column to add a fill component to the plot
#' @param alpha Alpha value for opacity of fill. Default set to 0.4
#' @param title_plot Title of the plot.
#' @param theme Indicates what theme the map must be shown in: dark (default), white, rose, almond.
#' @return a ggplot2 object
#'
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @import ggsoccer
#' @importFrom ggforce geom_voronoi_tile geom_voronoi_segment
#'
#' @export
#'
#' @examples
#' \dontrun{
#' plot <- plot_voronoi(data = data, colour = "blue", title_plot = "Team 1")
#' plot
#' }
#' 
plot_voronoi <- function(data, data_type = "statsbomb",
                            colour = "#E74C3C", 
                            fill = "",
                            alpha = 0.4,
                            title_plot = "", 
                            theme = "dark") {

  if (data_type == "opta") {
    if (nrow(data) <= 0 ||
        sum(x = c("x", "y") %in% names(data)) < 2) {
        stop("The dataset has insufficient columns and/or insufficient data.")
    } 
    
    to_sb <- rescale_coordinates(from = pitch_opta, to = pitch_statsbomb)
    data$x <- to_sb$x(data$x)
    data$y <- to_sb$y(data$y)
    
    data <- data %>%
      drop_na(x, y)
    
  } else if (data_type == "statsbomb") {
    if (nrow(data) <= 0) {
      stop("The dataset has insufficient data.")
    }
    
    data <- data %>% drop_na(x, y)
  }
  
  if (fill %in% names(data) == TRUE) {
    data[, "fill"] <- data[, fill]
  }
  
  ## Theme ----
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

  ## PLOT ----
  voro_plot <- ggplot(data, aes(x, y)) +
    annotate_pitch(dimensions = pitch_statsbomb, colour = colour_b,
                   fill = fill_b) +
    theme_pitch() +
    geom_point(color = colour, alpha = 0.7, size = 5)
  
  if (fill != "") {
    voro_plot <- voro_plot + 
      geom_voronoi_tile(data = data, aes(fill = fill), alpha = alpha, bound = c(0, 120, 0, 80))
  }
  
  voro_plot <- voro_plot + 
    geom_voronoi_segment(color = "white", bound = c(0, 120, 0, 80)) +
    coord_fixed() +
    labs(title = title_plot,
         x = "Direction of play faces rightward",
         fill = fill) +
    theme(plot.background = element_rect(fill = fill_b, colour = NA),
          panel.background = element_rect(fill = fill_b, colour = NA),
          strip.background = element_rect(fill = fill_b, colour = NA),
          strip.text = element_text(colour = colour_b, size = 10),
          plot.title = element_text(colour = colour_b, size = 20, hjust = 0.5, face = "bold"),
          axis.title.x = element_text(colour = colour_b, size = 12, face = "bold"))
  
  return(voro_plot)
}
