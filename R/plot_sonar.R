#' Function for plotting pass sonars
#'
#' This function allows for data, that can be from Opta or Statsbomb, to be used
#' for plotting pass sonars.
#'
#' @param data Dataframe that houses pass data. Dataframe must contain atleast the following columns: `x`, `y`, `finalX`, `finalY`
#' @param data_type Type of data that is being put in: opta or statsbomb. Default set to "statsbomb"
#' @param title Title of the passing sonar plot
#' @return a ggplot2 object
#'
#' @importFrom magrittr %>%
#' @import dplyr
#' @import ggplot2
#' @import ggsoccer
#'
#' @export
#'
#' @examples
#' \dontrun{
#' plot <- plot_sonar(data, data_type = "statsbomb")
#' plot
#' }

plot_sonar <- function(data, data_type = "statsbomb", title = "") {
  #Prerequiste checking: if the data has rows, whether it has the right columns
  #and whether it has the right data_types
  if ((nrow(data) > 0) &&
       sum(c("x", "y", "finalX", "finalY") %in% names(data)) == 4 &&
       (data_type == "statsbomb" || data_type == "opta")) {
    
    #Converting opta data to stasbomb data
    if (data_type == "opta") {
      to_sb <- rescale_coordinates(from = pitch_opta, to = pitch_statsbomb)
      data$x <- to_sb$x(data$x)
      data$y <- to_sb$y(data$y)
      data$finalX <- to_sb$x(data$finalX)
      data$finalY <- to_sb$y(data$finalY)
    }
    
    #Adding columns to calculate the angle and other important variables
    passData <- data %>%
      mutate(xstart = (finalX - x)) %>%
      mutate(ystart = (finalY - y)) %>%
      mutate(slope = ystart / xstart) %>%
      mutate(pass.angle = atan(slope)) %>%
      mutate(starty = ystart * ystart) %>%
      mutate(startx = xstart * xstart) %>%
      mutate(length = startx + starty) %>%
      mutate(pass.length = sqrt(length))
    
    #default angle that we want to bin by
    round.angle <- 30
    
    #Getting the angles of all the passes
    data2 <- passData %>%
      mutate(angle.round = round(pass.angle * 360 / pi / round.angle) * round.angle)
    
    #Summarizing and binning the passes by frequency, distance, and angle
    sonar <- data2 %>%
      mutate(N = n()) %>%
      ungroup() %>%
      group_by(angle.round) %>%
      mutate(n.angle = n() / N) %>%
      ungroup() %>%
      mutate(maxN = max(n.angle),
             angle.norm = n.angle / maxN) %>%
      ungroup() %>%
      group_by(angle.round, N, n.angle) %>%
      summarize(angle.norm = mean(angle.norm),
                frequency = mean(n.angle),
                distance = mean(pass.length),
                distance = ifelse(distance > 30, 30, distance))
    
    #Adding an offset for cleaner aesthetics
    sonar <- sonar %>%
      mutate(angle.round = angle.round + 0.5)
    
    if (title == "") {
      title <- "Pass Sonar"
    }
    
    plotCaption <- paste("Length of passes is in length of arrows + color of dots while frequency is in transparency. ", "Forward is toward's opponent's goal while backwards is towards own goal. ", sep = "\n")
    plotCaption <- paste(plotCaption, "Left and right correspond to left and right of pitch. Made by ggshakeR", sep = "\n")
    #drawing the sonar plot
    plot <- ggplot(data = sonar) +
      geom_segment(aes(x = angle.round, y = 0, xend = angle.round, yend = angle.norm, alpha = frequency),
                   size = 2,
                   color = "white",
                   lineend = "round",
                   arrow = arrow(length = unit(0.03, "npc"))) +
      scale_alpha(guide = 'none') +
      geom_point(aes(x = angle.round, y = 0, color = distance), size = 6) +
      scale_color_gradientn(colours = zissou_pal) +
      scale_x_continuous(breaks = seq(-180, 180, by = 90), limits = c(-180, 180)) +
      coord_polar(start = pi, direction = -1) +
      labs(x = '', y = '', title = title,
           alpha = "Frequency of Passes", color = "Avg. Distance",
           caption = plotCaption) +
      ylim(-0.5, 1.5) +
      theme(plot.title = element_text(hjust = 0.5, color = "white", size = 25),
            plot.caption = element_text(color = "white"),
            plot.background = element_rect(fill = "black", color = NA),
            panel.background = element_rect(fill = "black", color = NA),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank())
    
    return(plot)
  } else {
    stop("Dataframe has insufficient number of rows and/or you don't have the right amount of columns: `x`, `y`, `finalX`, `finalY`")
  }
}
