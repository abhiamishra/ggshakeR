#' Function for plotting shots
#'
#' This function allows for data, that has to be scraped from Understat, to be used
#' for plotting shots.
#'
#' @param data Dataframe that houses shot data. Dataframe must contain atleast the following columns: X,Y,xG,result,name
#' @param type Type of showcasing the shotmap: hexbin, density, point (default)
#' @param bins Bin size for creating bins. Use this when using hexbin shotmap. The same argument name as the underlying call to `geom_hex()`. Default = 30.
#' @param highlight_goals to choose to display only the goals in a different color.
#' @param average_location for removing lines denoting average location of shots if need be.
#' @param theme Theme preferences for display: dark (default), white, rose, almond
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
#' plot <- plot_shot(data, type = "hexbin", bins = 20, average_location = TRUE, highlight_goals = FALSE)
#' }

plot_shot <- function(data, type = "", bins = 30, highlight_goals = "", average_location = "", theme = "") {
  
  if ((nrow(data) > 0) &&
       sum(c("X", "Y", "xG", "result", "player") %in% names(data)) == 5) {
    
    last <- sub(".* ", "", data$player[nrow(data)])
    first <- sub(" .*", "", data$player[nrow(data)])
    player_name <- paste(first, last, "Shot Map", sep = "\n")
    
    fill_b <- ""
    color_b <- ""
    colorLine <- ""
    colorText <- ""
    
    ## theme ----
    if (theme == "dark" || theme == "") {
      fill_b <- "#0d1117"
      color_b <- "white"
      
      colorLine <- "white"
      colorText <- "white"
    } else if (theme == "white") {
      fill_b <- "#F5F5F5"
      color_b <- "black"
      
      colorLine <- "black"
      colorText <- "black"
    } else if (theme == "rose") {
      fill_b <- "#FFE4E1"
      color_b <- "#696969"
      
      colorLine <- "#322E2E"
      colorText <- "#322E2E"
    } else if (theme == "almond") {
      fill_b <- "#FFEBCD"
      color_b <- "#696969"
      
      colorLine <- "#322E2E"
      colorText <- "#322E2E"
    }
    
    ## base plot ----
    plot <- data %>%
      ggplot() +
      annotate_pitch(dimensions = pitch_statsbomb, colour = color_b,
                     fill = fill_b) +
      theme_pitch() +
      theme(panel.background = element_rect(fill = fill_b))
    
    
    if (nrow(data) >= 1) {
      data <- data %>%
        mutate(X = 120 * X) %>%
        mutate(Y = 80 * Y)
      
      total_xG <- sum(data$xG)
      total_goal <- sum(data$result == "Goal")
      xg_sot <- total_xG / nrow(data)
      
      if (average_location == TRUE || average_location == "") {
        
        if (type == "point" || type == "") {
          
          if (highlight_goals == FALSE || highlight_goals == "") { ## Don't highlight goals ----
            plot  <- plot +
              geom_point(data = data, aes(x = X, y = (80 - Y), size = xG, color = result), alpha = 0.7) +
              scale_size_continuous(range = c(0.5, 7)) +
              geom_vline(xintercept = mean(data$X), color = colorLine, linetype = 2, size = 1.5) +
              geom_hline(yintercept = mean(data$Y), color = colorLine, linetype = 2, size = 1.5) +
              geom_point(x = 86, y = 10, size = 40, color = colorText, shape = 1) +
              geom_text(x = 86, y = 10, label = format(round(total_xG, 2)), color = colorText, size = 10) +
              geom_text(x = 80, y = 10, label = "xG", color = colorText, size = 10) +
              geom_point(x = 86, y = 70, size = 40, color = colorText, shape = 1) +
              geom_text(x = 86, y = 70, label = format(round(total_goal, 2)), color = colorText, size = 10) +
              geom_text(x = 80, y = 70, label = "Goals", color = colorText, size = 10) +
              geom_point(x = 86, y = 40, size = 40, color = colorText, shape = 1) +
              geom_text(x = 86, y = 40, label = format(round(xg_sot, 2)), color = colorText, size = 10) +
              geom_text(x = 80, y = 40, label = "xG/Shot", color = colorText, size = 10) +
              coord_flip(xlim = c(80, 120),
                         ylim = c(0, 80)) +
              labs(
                color = "Result of Shot",
                size = "xG of Shot"
              )
          } else if (highlight_goals == TRUE) { ## Highlight goals ----
            
            data <- data %>%
              mutate(isGoal = ifelse(result == "Goal", "Goal", "No Goal"))
            
            plot <- plot +
              geom_point(data = data, aes(x = X, y = (80 - Y), size = xG, fill = isGoal), color = color_b, pch = 21, alpha = 0.7) +
              scale_fill_manual(values = c("#2fc22f", fill_b)) +
              scale_size_continuous(range = c(0.5, 7)) +
              geom_vline(xintercept = mean(data$X), color = colorLine, linetype = 2, size = 1.5) +
              geom_hline(yintercept = mean(data$Y), color = colorLine, linetype = 2, size = 1.5) +
              geom_point(x = 86, y = 10, size = 40, color = colorText, shape = 1) +
              geom_text(x = 86, y = 10, label = format(round(total_xG, 2)), color = colorText, size = 10) +
              geom_text(x = 80, y = 10, label = "xG", color = colorText, size = 10) +
              geom_point(x = 86, y = 70, size = 40, color = colorText, shape = 1) +
              geom_text(x = 86, y = 70, label = format(round(total_goal, 2)), color = colorText, size = 10) +
              geom_text(x = 80, y = 70, label = "Goals", color = colorText, size = 10) +
              geom_point(x = 86, y = 40, size  = 40, color = colorText, shape = 1) +
              geom_text(x = 86, y = 40, label = format(round(xg_sot, 2)), color = colorText, size = 10) +
              geom_text(x = 80, y = 40, label = "xG/Shot", color = colorText, size = 10) +
              coord_flip(xlim = c(80, 120),
                         ylim = c(0, 80)) +
              labs(
                color = "Result of Shot",
                size = "xG of Shot"
              )
          } 
        } else if (type == "density") { ## DENSITY ----
          plot <- plot +
            stat_density_2d(data = data, aes(x = X, y = (80 - Y), fill = ..level..), geom = "polygon",
                            alpha = 0.7) +
            scale_fill_gradient(high = "#6BFF84", low = "#01141D") +
            scale_size_continuous(range = c(0.5, 7)) +
            geom_vline(xintercept = mean(data$X), color = colorLine, linetype = 2, size = 1.5) +
            geom_hline(yintercept = mean(data$Y), color = colorLine, linetype = 2, size = 1.5) +
            geom_point(x = 86, y = 10, size = 40, color = colorText, shape = 1) +
            geom_text(x = 86, y = 10, label = format(round(total_xG, 2)), color = colorText, size = 10) +
            geom_text(x = 80, y = 10, label = "xG", color = colorText, size = 10) +
            geom_point(x = 86, y = 70, size = 40, color = colorText, shape = 1) +
            geom_text(x = 86, y = 70, label = format(round(total_goal, 2)), color = colorText, size = 10) +
            geom_text(x = 80, y = 70, label = "Goals", color = colorText, size = 10) +
            geom_point(x = 86, y = 40, size = 40, color = colorText, shape = 1) +
            geom_text(x = 86, y = 40, label = format(round(xg_sot, 2)), color = colorText, size = 10) +
            geom_text(x = 80, y = 40, label = "xG/Shot", color = colorText, size = 10) +
            coord_flip(xlim = c(80, 120),
                       ylim = c(0, 80)) +
            theme(legend.position = "none")
          
        } else if (type == "hexbin") { ## HEXBIN ----
          plot <- plot +
            geom_hex(data = data, aes(x = X, y = (80 - Y)), bins = bins) +
            scale_fill_continuous(type = "viridis") +
            scale_size_continuous(range = c(0.5, 7)) +
            geom_vline(xintercept = mean(data$X), color = colorLine, linetype = 2, size = 1.5) +
            geom_hline(yintercept = mean(data$Y), color = colorLine, linetype = 2, size = 1.5) +
            geom_point(x = 86, y = 10, size = 40, color = colorText, shape = 1) +
            geom_text(x = 86, y = 10, label = format(round(total_xG, 2)), color = colorText, size = 10) +
            geom_text(x = 80, y = 10, label = "xG", color = colorText, size = 10) +
            geom_point(x = 86, y = 70, size = 40, color = colorText, shape = 1) +
            geom_text(x = 86, y = 70, label = format(round(total_goal, 2)), color = colorText, size = 10) +
            geom_text(x = 80, y = 70, label = "Goals", color = colorText, size = 10) +
            geom_point(x = 86, y = 40, size = 40, color = colorText, shape = 1) +
            geom_text(x = 86, y = 40, label = format(round(xg_sot, 2)), color = colorText, size = 10) +
            geom_text(x = 80, y = 40, label = "xG/Shot", color = colorText, size = 10) +
            coord_flip(xlim = c(80, 120),
                       ylim = c(0, 80)) +
            labs(
              fill = "Count of Shots"
            )
        }
      } else if (average_location == FALSE) {
        
        if (type == "point" || type == "") {
          
          if (highlight_goals == FALSE || highlight_goals == "") {
            plot <- plot +
              geom_point(data = data, aes(x = X, y = (80 - Y), size = xG, color = result), alpha = 0.7) +
              scale_size_continuous(range = c(0.5, 7)) +
              geom_point(x = 86, y = 10, size = 40, color = colorText, shape = 1) +
              geom_text(x = 86, y = 10, label = format(round(total_xG, 2)), color = colorText, size = 10) +
              geom_text(x = 80, y = 10, label = "xG", color = colorText, size = 10) +
              geom_point(x = 86, y = 70, size = 40, color = colorText, shape = 1) +
              geom_text(x = 86, y = 70, label = format(round(total_goal, 2)), color = colorText, size = 10) +
              geom_text(x = 80, y = 70, label = "Goals", color = colorText, size = 10) +
              geom_point(x = 86, y = 40, size = 40, color = colorText, shape = 1) +
              geom_text(x = 86, y = 40, label = format(round(xg_sot, 2)), color = colorText, size = 10) +
              geom_text(x = 80, y = 40, label = "xG/Shot", color = colorText, size = 10) +
              coord_flip(xlim = c(80, 120),
                         ylim = c(0, 80)) +
              labs(
                color = "Result of Shot",
                size = "xG of Shot"
              )
          } else if (highlight_goals == TRUE) {
            
            data <- data %>%
              mutate(isGoal = ifelse(result == "Goal", "Goal", "No Goal"))
            
            plot <- plot +
              geom_point(data = data, aes(x = X, y = (80 - Y), size = xG, fill = isGoal), color = color_b, pch = 21, alpha = 0.7) +
              scale_fill_manual(values = c("#2fc22f", fill_b)) +
              scale_size_continuous(range = c(0.5, 7)) +
              geom_point(x = 86, y = 10, size = 40, color = colorText, shape = 1) +
              geom_text(x = 86, y = 10, label = format(round(total_xG, 2)), color = colorText, size = 10) +
              geom_text(x = 80, y = 10, label = "xG", color = colorText, size = 10) +
              geom_point(x = 86, y = 70, size = 40, color = colorText, shape = 1) +
              geom_text(x = 86, y = 70, label = format(round(total_goal, 2)), color = colorText, size = 10) +
              geom_text(x = 80, y = 70, label = "Goals", color = colorText, size = 10) +
              geom_point(x = 86, y = 40, size = 40, color = colorText, shape = 1) +
              geom_text(x = 86, y = 40, label = format(round(xg_sot, 2)), color = colorText, size = 10) +
              geom_text(x = 80, y = 40, label = "xG/Shot", color = colorText, size = 10) +
              coord_flip(xlim = c(80, 120),
                         ylim = c(0, 80)) +
              labs(
                color = "Result of Shot",
                size = "xG of Shot"
              )
          } 
        } else if (type == "density") {
          plot <- plot +
            stat_density_2d(data = data, aes(x = X, y = (80 - Y), fill = ..level..), geom = "polygon",
                            alpha = 0.7) +
            scale_fill_gradient(high = "#6BFF84", low = "#01141D") +
            scale_size_continuous(range = c(0.5, 7)) +
            geom_point(x = 86, y = 10, size = 40, color = colorText, shape = 1) +
            geom_text(x = 86, y = 10, label = format(round(total_xG, 2)), color = colorText, size = 10) +
            geom_text(x = 80, y = 10, label = "xG", color = colorText, size = 10) +
            geom_point(x = 86, y = 70, size = 40, color = colorText, shape = 1) +
            geom_text(x = 86, y = 70, label = format(round(total_goal, 2)), color = colorText, size = 10) +
            geom_text(x = 80, y = 70, label = "Goals", color = colorText, size = 10) +
            geom_point(x = 86, y = 40, size = 40, color = colorText, shape = 1) +
            geom_text(x = 86, y = 40, label = format(round(xg_sot, 2)), color = colorText, size = 10) +
            geom_text(x = 80, y = 40, label = "xG/Shot", color = colorText, size = 10) +
            coord_flip(xlim = c(80, 120),
                       ylim = c(0, 80)) +
            theme(legend.position = "none")
          
        } else if (type == "hexbin") {
          plot <- plot +
            geom_hex(data = data, aes(x = X, y = (80 - Y)), bins = bins) +
            scale_fill_continuous(type = "viridis") +
            scale_size_continuous(range = c(0.5, 7)) +
            geom_point(x = 86, y = 10, size = 40, color = colorText, shape = 1) +
            geom_text(x = 86, y = 10, label = format(round(total_xG, 2)), color = colorText, size = 10) +
            geom_text(x = 80, y = 10, label = "xG", color = colorText, size = 10) +
            geom_point(x = 86, y = 70, size = 40, color = colorText, shape = 1) +
            geom_text(x = 86, y = 70, label = format(round(total_goal, 2)), color = colorText, size = 10) +
            geom_text(x = 80, y = 70, label = "Goals", color = colorText, size = 10) +
            geom_point(x = 86, y = 40, size = 40, color = colorText, shape = 1) +
            geom_text(x = 86, y = 40, label = format(round(xg_sot, 2)), color = colorText, size = 10) +
            geom_text(x = 80, y = 40, label = "xG/Shot", color = colorText, size = 10) +
            coord_flip(xlim = c(80, 120),
                       ylim = c(0, 80)) +
            labs(
              fill = "Count of Shots"
            )
        } 
      }
      
      plot <- plot +
        geom_text(x = 110, y = 10, label = player_name, color = colorText, size = 6)
      
      plot
    } else{
      ## add WARNING that there were no rows in the data frame passed into function??
      plot
    }
  }
}
