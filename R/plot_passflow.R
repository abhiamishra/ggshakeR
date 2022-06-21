#' Passflow plot function
#'
#' This function takes in a dataframe and binsizes
#' to make a passflow map. Compatible, for right now, with StatsBomb data only. Returns a ggplot object
#'
#' @param data Dataframe that must house pass data only and must contain atleast the following columns: `x`, `y`, `finalX`, `finalY`
#' @param data_type Type of data that is being put in: opta or statsbomb. Default set to "statsbomb"
#' @param binwidth Details the bin size the passflow needs to bin to. The same argument name as the underlying call to `geom_bin2d()`. Default is 20.
#' @return returns a ggplot2 object
#'
#' @importFrom magrittr %>%
#' @import dplyr
#' @importFrom ggplot2 ggplot geom_bin2d geom_segment labs arrow aes scale_fill_gradientn 
#' @import ggsoccer
#' @export
#' @examples
#' \dontrun{
#' plot <- plot_passflow(data, binwidth = 30)
#' plot
#' }

plot_passflow <- function(data, data_type = "statsbomb", binwidth = 0) {

  fill_b <- "#0d1117"
  color_b <- "white"
  bin_alpha <- 0.6

  if (binwidth == 0) {
    bin <- 20
  } else {
    bin <- binwidth
  }

  x_bin <- 120 / bin
  y_bin <- 80 / bin

  passfx <- seq(0, 120, by = bin)
  passfy <- seq(0, 80, by = bin)

  if ((nrow(data) > 0) &&
       sum(x = c("x", "y", "finalX", "finalY") %in% names(data)) == 4) {

    # Converting Opta data
    if (data_type == "opta") {
      to_sb <- rescale_coordinates(from = pitch_opta, to = pitch_statsbomb)
      data$x <- to_sb$x(data$x)
      data$y <- to_sb$y(data$y)
      data$finalX <- to_sb$x(data$finalX)
      data$finalY <- to_sb$y(data$finalY)
    }

    PassFlow <- data.frame("x" = 0.0, "y" = 0.0, "finalX" = 0.0, "finalY" = 0.0, countPasses = 0.0)

    for (i in 1:x_bin) {

      filterx <- data %>%
        filter(x >= passfx[i]) %>%
        filter(x < passfx[i + 1])

      for (j in 1:y_bin) {

        minY <- passfy[j]
        maxY <- passfy[j + 1]

        filtery <- filterx %>%
          filter(y >= minY) %>%
          filter(y < maxY)

        if (nrow(filtery) >= 1) {

          me_x <- mean(filtery$x)
          me_y <- mean(filtery$y)
          me_ex <- mean(filtery$finalX)
          me_ey <- mean(filtery$finalY)

          count <- nrow(filtery)

          x <- c(me_x, me_y, me_ex, me_ey, count)
          PassFlow <- rbind(PassFlow, x)

        }

      }

    }

    PassFlow <- PassFlow[2:nrow(PassFlow), ]

    plot <- PassFlow %>%
      ggplot() +
      annotate_pitch(dimensions = pitch_statsbomb, colour = color_b,
                     fill = fill_b) +
      theme_pitch()

    if (nrow(PassFlow) > 0) {
      plot <- plot +
        geom_bin2d(data = data, aes(x = x, y = 80 - y), alpha = bin_alpha,
                   binwidth = c(bin, bin), position = "identity") +
        scale_fill_gradientn(colours = viridis_d_pal) +
        geom_segment(aes(x = x, y = 80 - y, xend = finalX, yend = 80 - finalY, alpha = countPasses),
                     color = "white", lineend = "round", size = 2, arrow = arrow(length = unit(0.08, "inches"))) +
        labs(
          fill = "Count of Passes Started",
          alpha = "Number of Passes Made"
        )
    }
    plot
  }
}
