#' Plotting scatter plots
#'
#' This function allows you to plot various types of plots 
#' that have passes as some sort of input. Compatible with any data frame of any data type. Returns a ggplot object.
#'
#'
#' @param data the dataframe passed in for plotting.
#' @param x name of column name in data to be used on x-axis
#' @param y name of column name in data to be used on y-axis
#' @param label the name of column name in data to label the scatter plot
#' @param set_size_num sets the size of the points set as a constant. Default size = 5.
#' @param set_size_var Enter name of column name in data to set size based on variable.
#' @param set_color_num sets the color of the points set as a constant. Can enter hexcode or a valid ggplot2 color. Default = "red"
#' @param set_color_var Enter name of column name in data to set color based on variable.
#' @param title pick the title of the scatter plot
#' @param title_size sets the size of the title of the scatter plot. Default size = 25.
#' @param subtitle pick the subtitle of the scatter plot
#' @param subtitle_size sets the size of the subtitle of the scatter plot Default size = 15.
#' @param caption pick the caption of the scatter plot
#' @param caption_size sets the size of the caption of the scatter plot. Default size = 10.
#' @param theme decide the theme of the plot between four choices: classic, minimal, grey, bw. Default = "classic"
#' @return returns a ggplot2 object
#'
#' @importFrom magrittr %>%
#' @import dplyr
#' @import ggplot2
#' @import ggsoccer
#' @import ggrepel
#'
#' @export
#'
#' @examples
#' \dontrun{
#' plot <- plot_scatter(data, x = "player", y = "age", label = "team")
#' plot
#' }

plot_scatter <- function(data, x = "", y = "", label = "",
                         set_size_num = 5, set_size_var = "",
                         set_color_num = "red", set_color_var = "",
                         title = "", title_size = 25,
                         subtitle = "", subtitle_size = 15,
                         caption = "", caption_size = 10,
                         theme = "classic") {
  if (nrow(data) == 0) {
    stop("Please input a data.frame into the 'data' argument.")
  }
  
  ## Pre-processing ----
  total <- 0
  scatter_x <- x
  scatter_y <- y
  scatter_label <- label
  
  if (scatter_x %in% names(data) && scatter_y %in% names(data)) {
    selection <- c(scatter_x, scatter_y)
    
    renaming <- c('scatter_x', 'scatter_y')
    
    total <- 2
    
    col_var <- ""
    size_var <- ""
    
    if (scatter_label != "" && scatter_label %in% names(data)) {
      if (scatter_label %in% selection == TRUE) {
        data[, "scatter_label"] <- data[, scatter_label]
        
        selection <- append(selection, 'scatter_label')
        renaming <- append(renaming, 'scatter_label')
      } else {
        selection <- append(selection, scatter_label)
        renaming <- append(renaming, 'scatter_label')
      }
      total <- total + 1
    }
    
    if (set_size_var != "" && set_size_var %in% names(data)) {
      if (set_size_var %in% selection == TRUE) {
        data[, "set_size_var"] <- data[, set_size_var]
        
        selection <- append(selection, 'set_size_var')
        renaming <- append(renaming, 'set_size_var')
      } else {
        selection <- append(selection, set_size_var)
        renaming <- append(renaming, 'set_size_var')
      }
      total <- total + 1
      
      size_var <- set_size_var
    }
    
    if (set_color_var != "" && set_color_var %in% names(data)) {
      if (set_color_var %in% selection == TRUE) {
        data[, "set_color_var"] <- data[, set_color_var]
        
        selection <- append(selection, 'set_color_var')
        renaming <- append(renaming, 'set_color_var')
      } else {
        selection <- append(selection, set_color_var)
        renaming <- append(renaming, 'set_color_var')
      }
      total <- total + 1
      
      col_var <- set_color_var
    }
    
    data <- data %>%
      select(c(selection))
    
    
    for (i in 1:total) {
      names(data)[i] <- renaming[i]
    }
    #Preprocessing over
    
    # Plotting ----
    plot <- data %>%
      ggplot(aes(x = scatter_x, y = scatter_y))
    
    x_title <- scatter_x
    y_title <- scatter_y
    
    if (set_size_var != "" && set_color_var != "") {
      plot <- plot +
        geom_point(aes(size = set_size_var, color = set_color_var))
    } else if (set_size_var == "" && set_color_var != "") {
      plot <- plot +
        geom_point(aes(color = set_color_var), size = set_size_num)
    } else if (set_size_var != "" && set_color_var == "") {
      plot <- plot +
        geom_point(aes(size = set_size_var), color = set_color_num)
    } else if (set_size_var == "" && set_color_var == "") {
      plot <- plot +
        geom_point(size = set_size_num, color = set_color_num)
    }
    
    if (scatter_label != "") {
      plot <- plot +
        geom_label_repel(aes(x = scatter_x, y = scatter_y, label = scatter_label), max.overlaps = 2)
    }
    
    ## Theme ----
    if (theme == "classic") {
      plot <- plot +
        theme_classic()
    } else if (theme == "minimal") {
      plot <- plot +
        theme_minimal()
    } else if (theme == "grey") {
      plot <- plot +
        theme_grey()
    } else if (theme == "bw") {
      plot <- plot +
        theme_bw()
    }
    
    plotCaption <- "Created using ggshakeR"
    plotCaption <- paste(caption, plotCaption, sep = "\n")
    
    
    plot <- plot +
      labs(
        title = title,
        subtitle = subtitle,
        caption = plotCaption,
        color = col_var,
        size = size_var,
        x = x_title,
        y = y_title
      ) +
      theme(
        plot.title = element_text(color = "black", size = title_size),
        plot.subtitle = element_text(color = "black", size = subtitle_size),
        plot.caption = element_text(color = "black", size = caption_size)
      )
  } else {
    stop("Please input columns for the 'x' and/or 'y' arguments.")
  }
  
  return(plot)
}
