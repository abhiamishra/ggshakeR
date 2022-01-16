#' Function for plotting pass sonars
#'
#' This function allows for data, that can be from Opta or Statsbomb, to be used
#' for plotting pass sonars.
#'
#' @param sonarData Dataframe that houses pass data. Dataframe must contain atleast the following columns: `x`, `y`, `finalX`, `finalY`
#' @param dataType Type of data that is being put in: opta or statsbomb. Default set to "statsbomb"
#' @param titlePlot Title of the passing sonar plot
#' @return a ggplot2 object
#'
#' @importFrom magrittr %>%
#' @import dplyr
#' @import ggplot2
#' @import ggsoccer
#' @import wesanderson
#'
#' @export
#'
#' @examples
#' \dontrun{
#' plot <- plot_sonar(sonarData, dataType = "statsbomb")
#' plot
#' }

plot_sonar <- function(sonarData, dataType = "statsbomb", titlePlot = "") {
  #Prerequiste checking: if the data has rows, whether it has the right columns
  #and whether it has the right dataTypes
  if ( (nrow(sonarData) > 0) &&
       sum(c("x","y","finalX","finalY") %in% names(sonarData)) == 4 &&
       (dataType == "statsbomb" || dataType == "opta")) {
    
    #Converting opta data to stasbomb data
    if (dataType == "opta") {
      to_sb <- rescale_coordinates(from = pitch_opta, to = pitch_statsbomb)
      sonarData$x <- to_sb$x(sonarData$x)
      sonarData$y <- to_sb$y(sonarData$y)
      sonarData$finalX <- to_sb$x(sonarData$finalX)
      sonarData$finalY <- to_sb$y(sonarData$finalY)
    }
    
    #Adding columns to calculate the angle and other important variables
    passData <- sonarData %>%
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
      mutate(N = n())%>%
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
    
    #Initializing a Wes Anderson palette
    pal <- wes_palette("Zissou1", 10, type = "continuous")
    
    if (titlePlot == "") {
      titlePlot = "Pass Sonar"
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
      scale_color_gradientn(colours = pal) +
      scale_x_continuous(breaks = seq(-180, 180, by = 90), limits = c(-180, 180)) +
      coord_polar(start = pi, direction = -1) +
      labs(x = '', y = '', title = titlePlot,
           alpha = "Frequency of Passes", color = "Avg. Distance",
           caption = plotCaption) +
      ylim(-0.5, 1.5) +
      theme(plot.title = element_text(hjust = 0.5, color = "white", size = 25),
            plot.caption = element_text(color = "white"),
            plot.background = element_rect(fill = "black", colour = NA),
            panel.background = element_rect(fill = "black", colour = NA),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank())
    plot
  }
  plot
}
