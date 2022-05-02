#' Plotting passes
#'
#' This function allows you to plot various types of plots
#' that have passes as some sort of input. Data entered must have columns for which you want to plot with.
#' Compatible, for right now, with StatsBomb data only! Returns a ggplot object.
#'
#' @param pass_data The dataframe that stores your passing data. Must contain starting x,y and ending x,y locations as well as a player name column
#' @param plot_type indicates the type of plot to pass. "sep" separates successful and unsuccessful passes. "all" plots all passes on one pitch. Default = "sep"
#' @param prog indicates whether to map out progressive passes
#' @param cross indicates whether to map out crosses
#' @param shot indicates whether to map out shot assists
#' @param switch indicates whether to map out switches of play
#' @param distance indicates what distance you want to map out
#' @param outcome indicates whether you want successful ("suc"), unsuccessful ("unsuc"), or all ("all")
#' @param team indicates what team's pass map one wants to see
#' @param player_fname player first name
#' @param player_lname player last name
#' @param theme indicates what theme the map must be shown in: dark (default), white, rose, almond
#' @return returns a ggplot2 object
#'
#' @importFrom magrittr %>%
#' @import dplyr
#' @import ggplot2
#' @import ggsoccer
#' @import ggrepel
#' @export
#'
#' @examples
#' \dontrun{
#' plot  <- plot_pass(pass_data, plot_type = "def", prog = TRUE, 
#'                    team = "Barcelona", player_fname = "Lionel")
#' plot
#' }

plot_pass <- function(pass_data, plot_type = "sep", prog = FALSE, cross = FALSE, shot = FALSE, switch = FALSE,
                      distance = "", outcome = "all", team = "", player_fname = "", player_lname = "", theme = "") {
  if ((nrow(pass_data) > 0) &&
      sum(x = c("location.x", "location.y", "pass.end_location.x", "pass.end_location.y", "player.name") %in% names(pass_data)) == 5) {
    
    pass_data <- pass_data %>%
      mutate(lname = sub(".* ", "", player.name)) %>%
      mutate(fname = sub(" .*", "", player.name))
    
    if (team != "") {
      pass_data <- pass_data %>%
        filter(team.name == team)
    }
    
    ## Player name
    if (player_fname != "") {
      pass_data <- pass_data %>%
        filter(fname == player_fname)
    }
    
    if (player_lname != "") {
      pass_data <- pass_data %>%
        filter(lname == player_lname)
    }
    
    ## Outcome
    if (outcome == "suc") {
      pass_data <- pass_data %>%
        filter(is.na(pass.outcome.name))
    } else if (outcome == "unsuc") {
      pass_data <- pass_data %>%
        filter(!is.na(pass.outcome.name))
    }
    
    pass_data$pass.outcome.name <- tidyr::replace_na(pass_data$pass.outcome.name, "Successful")
    pass_data <- pass_data %>% mutate(colorOutcome = ifelse(pass.outcome.name == "Successful",
                                                            "Successful",
                                                            "Unsuccessful"))
    
    if (prog == TRUE) {
      pass_data <- pass_data %>%
        mutate(start = sqrt((100 - location.x)^2 + (50 - location.y)^2)) %>%
        mutate(end = sqrt((100 - pass.end_location.x)^2 + (50 - pass.end_location.y)^2)) %>%
        mutate(isProg = ifelse(end <= 0.75 * start,
                               1,
                               0))
      
      pass_data <- pass_data %>% filter(isProg == 1)
    }
    
    if (cross == TRUE) {
      pass_data <- pass_data %>%
        filter(pass.cross == TRUE)
    }
    
    if (shot == TRUE) {
      pass_data <- pass_data %>%
        filter(pass.shot_assist == TRUE)
    }
    
    if (switch == TRUE) {
      pass_data <- pass_data %>%
        mutate(delta_y = abs(
          pass.end_location.y - location.y
        )) %>%
        filter(delta_y >= 35)
    }
    
    if (theme == "dark" || theme == "") {
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
    
    plot <- ggplot(data = pass_data) +
      annotate_pitch(dimensions = pitch_statsbomb, colour = colour_b,
                     fill = fill_b) +
      theme_pitch() +
      theme(panel.background = element_rect(fill = fill_b))
    
    if (nrow(pass_data) > 0) {
      if (plot_type == "sep") {
        plot <- plot +
          geom_segment(aes(x = location.x, y = 80 - location.y,
                           xend = pass.end_location.x, yend = 80 - (pass.end_location.y), color = colorOutcome),
                       lineend = "round", size = 1.5, arrow = arrow(length = unit(0.10, "inches")), stat = "identity", position = "identity") +
          facet_grid(~colorOutcome) +
          labs(
            color = "Outcome of Pass"
          )
      } else if (plot_type == "all") {
        plot <- plot +
          geom_segment(aes(x = location.x, y = 80 - location.y,
                           xend = pass.end_location.x, yend = 80 - (pass.end_location.y), color = colorOutcome),
                       lineend = "round", size = 1.5, arrow = arrow(length = unit(0.10, "inches")), stat = "identity", position = "identity") +
          labs(
            color = "Outcome of Pass"
          )
      }
    }
    
    plot
  }
}
