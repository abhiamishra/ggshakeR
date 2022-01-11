#' Plotting heatmap
#'
#' This function allows you to plot various types of heatmaps of starting x and y coordinates:
#' hex bin heatmap,
#' density heatmap, and
#' bin heatmap
#'
#' @param eventData The dataframe that stores your data. Must contain starting x,y locations
#' @param type indicates the type of heatmap to plot. "hex" indicates hex bins, "density" indicates density (default), and
#' "bin" indicates bin heatmap pass
#' @param theme indicates what theme the map must be shown in: dark (default), white, rose, almond
#' @return returns a ggplot2 object
#'
#' @importFrom magrittr %>%
#' @import dplyr
#' @import ggplot2
#' @import ggsoccer
#' @import viridis
#' @export
#'
#' @examples
#' \dontrun{
#' plot <- plot_heatmap(touchData, type="hex")
#' plot
#' }

plot_heatmap <- function(eventData, type="", theme=""){
  if(nrow(eventData) > 0 &&
  sum(x = c("location.x", "location.y", "pass.end_location.x", "pass.end_location.y") %in% names(eventData))==4){

    plot = eventData %>%
      ggplot(aes(x=location.x,y=location.y))

    if(theme == "dark" || theme == ""){
      fill_b = "#0d1117"
      colour_b = "white"
    }
    else if(theme == "white"){
      fill_b = "#F5F5F5"
      colour_b = "black"
    }
    else if(theme == "rose"){
      fill_b = "#FFE4E1"
      colour_b = "#696969"
    }
    else if(theme == "almond"){
      fill_b = "#FFEBCD"
      colour_b = "#696969"
    }

      plot = plot + annotate_pitch(dimensions = pitch_statsbomb,colour=colour_b,
                    fill = fill_b)+
      theme_pitch()+
      theme(panel.background = element_rect(fill = fill_b))

      if(type == "" || type == "density"){
        plot = plot +
          stat_density_2d(aes(fill = ..level..), geom = "polygon")
      }
      else if(type == "hex"){
        plot = plot +
          geom_hex(aes(x=location.x,y=location.y))
      }
      else if(type == "bin"){
        plot = plot +
          geom_bin2d(aes(x=location.x,y=location.y))
      }

      plot = plot +
        scale_fill_continuous(type = "viridis")+
        labs(
          color = "Density"
        )
        plot

  }
  else{
      plot
  }
}
