#' Create timeline charts (xG) using event data.
#'
#'This function allows to make match timelines using data collected event-by-event.
#'
#' @param data is for the dataset used.
#' @param match_year the year the match was played.
#' @param team_home is for the home team according to data.
#' @param team_away is for the away team according to data.
#' @param color_home is for the color of the line for the home team.
#' @param color_away is for the color of the line for the away team.
#' @param theme to select the colors. Choose from 4 themes -> dark, almond, rose, white.
#' @return a ggplot2 object
#'
#' @import dplyr
#' @import ggtext
#' @import ggplot2
#' @import ggrepel
#'
#' @export
#'
#' @examples
#' \dontrun{
#' plot <- plot_timeline(data = data, match_year = 2021, 
#'                       team_home = "Manchester United", team_away = "Liverpool",
#'                       color_home = "#e31a1c", color_away = "#980043", theme = "dark")
#' plot
#' }

plot_timeline <- function(data, match_year, team_home, team_away, color_home, color_away, theme = "") {
  
  fill_b <- ""
  color_b <- ""
  colorLine <- ""
  colorText <- ""
  gridc <- ""
  
  if (theme == "dark" || theme == "") {
    fill_b <- "#0d1117"
    color_b <- "white"
    
    colorLine <- "white"
    gridc <- "#525252"
    colorText <- "white"
  } else if (theme == "white") {
    fill_b <- "#F5F5F5"
    color_b <- "black"
    
    colorLine <- "black"
    gridc <- "grey"
    colorText <- "black"
  } else if (theme == "rose") {
    fill_b <- "#FFE4E1"
    color_b <- "#696969"
    
    colorLine <- "#322E2E"
    gridc <- "grey"
    colorText <- "#322E2E"
  } else if (theme == "almond") {
    fill_b <- "#FFEBCD"
    color_b <- "#696969"
    
    colorLine <- "#322E2E"
    gridc <- "grey"
    colorText <- "#322E2E"
  }
  
  data$minute <- as.numeric(data$minute)
  
  if (!"season" %in% colnames(data)) {
    data <- data %>%
      mutate(season = year)
  }
  
  data <- data %>%
    dplyr::filter(season == match_year)
  
  if (!"home_away" %in% colnames(data)) {
    data <- data %>%
      mutate(home_away = h_a)
  }
  
  data <- data %>%
    dplyr::filter(home_team == team_home,
           away_team == team_away)
  data1 <- data %>%
    dplyr::filter(home_away == "h") %>%
    mutate(xGsum = cumsum(xG))
  data2 <- data %>%
    dplyr::filter(home_away == "a") %>%
    mutate(xGsum = cumsum(xG))
  
  # data1 <- insertRows(data1, 1, new = 0)
  # data2 <- insertRows(data2, 1, new = 0)
  data1 <- dplyr::add_row(.data = data1, .before = 1, 
                           id = "0", minute = 0, result = "0", X = 0, Y = 0, xG = 0, player = "0", home_away = "0", player_id = "0",
                           situation = "0", season = "0", shotType = "0", match_id = "0", home_team = "0", away_team = "0",
                           home_goals = 0, away_goals = 0, date = "0", player_assisted = "0", lastAction = "0", xGsum = 0)
  
  data2 <- dplyr::add_row(.data = data2, .before = 1, 
                           id = "0", minute = 0, result = "0", X = 0, Y = 0, xG = 0, player = "0", home_away = "0", player_id = "0",
                           situation = "0", season = "0", shotType = "0", match_id = "0", home_team = "0", away_team = "0",
                           home_goals = 0, away_goals = 0, date = "0", player_assisted = "0", lastAction = "0", xGsum = 0)
  
  dat1 <- data1 %>%
    dplyr::filter(result == "Goal")
  d1 <- data1 %>%
    dplyr::filter(result == "OwnGoal")
  dat1 <- rbind(dat1, d1)
  dat2 <- data2 %>%
    dplyr::filter(result == "Goal")
  d2 <- data2 %>%
    dplyr::filter(result == "OwnGoal")
  dat2 <- rbind(dat2, d2)
  
  team1 <- data$home_team
  team2 <- data$away_team
  
  xG1 <- round(sum(data1$xG), 2)
  xG2 <- round(sum(data2$xG), 2)
  
  g1 <- data$home_goals
  g2 <- data$away_goals
  
  if (any(g1 == 1)) {
    gls1 <- "Goal"
  } else {
    gls1 <- "Goals"
  }
  
  if (any(g2 == 1)) {
    gls2 <- "Goal"
  } else {
    gls2 <- "Goals"
  }
  
  # plot_title <- glue("<b style='color:{color_home}'> {team1} : {g1} {gls1} ({xG1} xG) </b> vs. <b style='color:{color_away}'> {team2} : {g2} {gls2} ({xG2} xG)</b>")
  plot_title <- sprintf("<b style='color:%s'> %s : %s %s (%s xG) </b> vs. <b style='color:%s'> %s : %s %s (%s xG)</b>",
                        color_home, team1, g1, gls1, xG1, 
                        color_away, team2, g2, gls2, xG2)
  
  min1 <- dat1$minute
  min2 <- dat2$minute
  p1 <- dat1$player
  p2 <- dat2$player
  
  player_lab1 <- paste0(p1, " : ", min1)
  player_lab2 <- paste0(p2, " : ", min2)
  
  plot_timeline <- ggplot() +
    geom_step(data = data1, aes(x = minute, y = xGsum), color = color_home, size = 3) +
    geom_step(data = data2, aes(x = minute, y = xGsum), color = color_away, size = 3) +
    geom_point(data = dat1, aes(x = minute, y = xGsum), color = color_home, fill = fill_b, shape = 21, stroke = 2, size = 6) +
    geom_point(data = dat2, aes(x = minute, y = xGsum), color = color_away, fill = fill_b, shape = 21, stroke = 2, size = 6) +
    geom_text_repel(data = dat1, aes(x = minute, y = xGsum, label = player_lab1),
                    box.padding   = 2,
                    point.padding = 1.5,
                    segment.color = colorLine,
                    alpha = 0.8,
                    color = colorText,
                    size = 5) +
    geom_text_repel(data = dat2, aes(x = minute, y = xGsum, label = player_lab2),
                    box.padding   = 2,
                    point.padding = 1.5,
                    segment.color = colorLine,
                    alpha = 0.8,
                    color = colorText,
                    size = 5) +
    theme_minimal() +
    labs(title = plot_title) +
    theme(plot.title = element_markdown(lineheight = 1.1, color = colorText, hjust = 0.5, size = 20, face = "bold")) +
    theme(plot.background = element_rect(fill = fill_b, color = color_b)) +
    theme(panel.background = element_rect(fill = fill_b, color = color_b)) +
    labs(x = "Minute", y = "Cumulative xG") +
    theme(axis.title.x = element_text(color = colorLine, size = 15, face = "bold")) +
    theme(axis.title.y = element_text(color = colorLine, size = 15, face = "bold")) +
    theme(axis.text.x = element_text(color = colorLine, size = 12),
          axis.text.y = element_text(color = colorLine, size = 12)) +
    theme(panel.grid.major = element_line(color = gridc, size = 0.5, linetype = "dashed")) +
    theme(panel.grid.minor = element_blank()) +
    theme(axis.line = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90)) +
    geom_vline(xintercept = 45, linetype = "dashed", color = colorLine, size = 1)
  
  return(plot_timeline)
}
