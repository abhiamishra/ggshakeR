#' Function for plotting voronoi diagrams on the pitch
#'
#' The function allows for creation of voronoi diagrams on a football pitch
#' with either Opta or Statsbomb data.
#'
#' @param data Dataframe that houses pass data. Opta/Statsbomb dataframe must contain atleast the following columns: `x`, `y`
#' @param data_type Type of data that is being put in: opta or statsbomb. Default set to "statsbomb".
#' @param colour The colour of the points in the voronoi plot.
#' @param voro_fill Name of column to add a fill component to the plot
#' @param voro_alpha Alpha value for opacity of fill. Default set to 0.4
#' @param title_plot Title of the plot.
#' @param theme Indicates what theme the map must be shown in: dark (default), white, rose, almond.
#' @return a ggplot2 object
#'
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @import ggsoccer
#' @import ggforce
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
                            voro_fill = "",
                            voro_alpha = 0.4,
                            title_plot = "", 
                            theme = "dark") {
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
  
  limit_x = 120
  limit_y = 80
  
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
    
  } 
  else if (data_type == "statsbomb") {
    if (nrow(data) <= 0) {
      stop("The dataset has insufficient data.")
    }
    
    data <- data %>% drop_na(x, y)
  }
  
  if (voro_fill %in% names(data) == TRUE) {
    data[, "voro_fill"] <- data[, voro_fill]
  }

  voro_plot = ggplot(data, aes(x, y)) +
    annotate_pitch(dimensions = pitch_statsbomb, colour = colour_b,
                   fill = fill_b) +
    theme_pitch()+
    geom_point(color=colour)
  
  if(voro_fill != ""){
    voro_plot = voro_plot + 
      geom_voronoi_tile(data=data, aes(fill=voro_fill), alpha=voro_alpha, bound=c(0,120,0,80))
  }
  
  voro_plot =  voro_plot + 
    geom_voronoi_segment(color="white", bound=c(0,120,0,80))+
    coord_fixed()+
    ggtitle(title_plot)
  
  return(voro_plot)
}


