#' Plotting heatmap
#'
#' This function allows you to plot various types of heatmaps of starting x and y coordinates:
#' hex binwidth heatmap,
#' density heatmap, and
#' binwidth heatmap
#'
#' @param data The dataframe that stores your data. Dataframe must contain atleast the following columns: `x`, `y`.
#' @param type indicates the type of heatmap to plot. "hex" indicates hex bins, "density" indicates density (default), 
#' "binwidth" indicates binwidth heatmap pass, and "jdp" indicates a binned heatmap according jdp pitch markings. 
#' @param data_type Type of data that is being put in: opta or statsbomb. Default set to "statsbomb"
#' @param binwidth indicates the size of the bin width to construct heatmap for type "binwidth". The same argument name as the underlying call to `geom_bin2d()`. Default set to 20.
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
#' plot <- plot_heatmap(data = touchData, type = "hex")
#' plot
#' }

plot_heatmap <- function(data, type = "", data_type = "statsbomb", binwidth = 20, theme = "") {

  if (nrow(data) > 0 &&
      sum(x = c("x", "y") %in% names(data)) == 2) {

    if (data_type == "opta") {
      to_sb <- rescale_coordinates(from = pitch_opta, to = pitch_statsbomb)
      data$x <- to_sb$x(data$x)
      data$y <- to_sb$y(data$y)
    }

    plot <- data %>%
      ggplot(aes(x = x, y = y))
    
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
  plot <- plot + annotate_pitch(dimensions = pitch_statsbomb, colour = color_b,
                                  fill = fill_b) +
      theme_pitch() +
      theme(panel.background = element_rect(fill = fill_b))
    
    if (type == "" || type == "density") {
      plot <- plot +
        stat_density_2d(aes(x = x, y = y, fill = ..level..), geom = "polygon")
    } else if (type == "hex") {
      plot <- plot +
        geom_hex(aes(x = x, y = y))
    } else if (type == "binwidth") {
      plot <- plot +
        geom_bin2d(aes(x = x, y = y),
                   binwidth = c(binwidth, binwidth),
                   alpha = 0.9)
    } else if (type == "jdp") {
      
      binX1 <- c(0, 18, 39, 60, 81, 102, 120)
      binY1 <- c(0, 18)
      
      binX2 <- c(0, 18, 39, 60, 81, 102, 120)
      binY2 <- c(62, 80)
      
      binX3 <- c(18, 60, 102)
      binY3 <- c(18, 30, 50, 62)
      
      binX4 <- c(0, 18)
      binY4 <- c(18, 62)
      
      binX5 <- c(102, 120)
      binY5 <- c(18, 62)
      
      plot <- plot +
        geom_bin_2d(breaks = list(binX1, binY1), color = color_b, alpha = 0.9) +
        geom_bin_2d(breaks = list(binX2, binY2), color = color_b, alpha  = 0.9) +
        geom_bin_2d(breaks = list(binX3, binY3), color = color_b, alpha = 0.9) +
        geom_bin_2d(breaks = list(binX4, binY4), color = color_b, alpha = 0.9) +
        geom_bin_2d(breaks = list(binX5, binY5), color = color_b, alpha = 0.9)
    }
  
    plotCaption <- "Created using ggshakeR"
  
    plot <- plot +
      scale_fill_continuous(type = "viridis") +
      labs(
        fill = "Density",
        caption = plotCaption
      )
    
    return(plot)
  } else {
    stop("Please check that your data has the columns: 'x' and 'y'")
  }
}
