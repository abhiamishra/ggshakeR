#' Create timeline charts (xG) using event data.
#'
#'This function allows to make match timelines using data collected event-by-event.
#'
#' @param data Data frame used in the function
#' @param match_year Year the match was played
#' @param team_home Home team according to data
#' @param team_away Away team according to data
#' @param home_color Color of the line for the home team
#' @param away_color Color of the line for the away team
#' @param theme to select the colors. Choose from 4 themes -> dark, almond, rose, white
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
#'                       home_color = "#e31a1c", away_color = "#980043", theme = "dark")
#' plot
#' }

plot_timeline <- function(data, match_year, team_home, team_away, 
                          home_color, away_color, theme = "") {
  
  if (theme == "dark" || theme == "") {
    fill_b <- "#0d1117"
    colour_b <- "white"
    
    
    colorLine <- "white"
    gridc <- "#525252"
    colorText <- "white"
  } else if (theme == "white") {
    fill_b <- "#F5F5F5"
    colour_b <- "black"
    
    colorLine <- "black"
    gridc <- "grey"
    colorText <- "black"
  } else if (theme == "rose") {
    fill_b <- "#FFE4E1"
    colour_b <- "#696969"
    
    colorLine <- "#322E2E"
    gridc <- "grey"
    colorText <- "#322E2E"
  } else if (theme == "almond") {
    fill_b <- "#FFEBCD"
    colour_b <- "#696969"
    
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
  
  # plot_title <- glue("<b style='color:{home_color}'> {team1} : {g1} {gls1} ({xG1} xG) </b> vs. <b style='color:{away_color}'> {team2} : {g2} {gls2} ({xG2} xG)</b>")
  plot_title <- sprintf("<b style='color:%s'> %s : %s %s (%s xG) </b> vs. <b style='color:%s'> %s : %s %s (%s xG)</b>",
                        home_color, team1, g1, gls1, xG1, 
                        away_color, team2, g2, gls2, xG2)
  
  expG1 <- dat1$xG
  expG2 <- dat2$xG
  p1 <- dat1$player
  p2 <- dat2$player
  
  player_lab1 <- paste0(p1, " : ", expG1)
  player_lab2 <- paste0(p2, " : ", expG2)
  
  plot_timeline <- ggplot() +
    geom_step(data = data1, aes(x = minute, y = xGsum), colour = home_color, size = 3) +
    geom_step(data = data2, aes(x = minute, y = xGsum), colour = away_color, size = 3) +
    geom_point(data = dat1, aes(x = minute, y = xGsum), colour = home_color, fill = fill_b, shape = 21, stroke = 2, size = 6) +
    geom_point(data = dat2, aes(x = minute, y = xGsum), colour = away_color, fill = fill_b, shape = 21, stroke = 2, size = 6) +
    geom_text_repel(data = dat1, aes(x = minute, y = xGsum, label = player_lab1),
                    box.padding   = 2,
                    point.padding = 1.5,
                    segment.color = colorLine,
                    alpha = 0.8,
                    colour = colorText,
                    size = 5) +
    geom_text_repel(data = dat2, aes(x = minute, y = xGsum, label = player_lab2),
                    box.padding   = 2,
                    point.padding = 1.5,
                    segment.color = colorLine,
                    alpha = 0.8,
                    colour = colorText,
                    size = 5) +
    theme_minimal() +
    labs(title = plot_title) +
    theme(plot.title = element_markdown(lineheight = 1.1, colour = colorText, hjust = 0.5, size = 20, face = "bold")) +
    theme(plot.background = element_rect(fill = fill_b, colour = colour_b)) +
    theme(panel.background = element_rect(fill = fill_b, colour = colour_b)) +
    labs(x = "Minute", y = "Cumulative xG") +
    theme(axis.title.x = element_text(colour = colorLine, size = 15, face = "bold")) +
    theme(axis.title.y = element_text(colour = colorLine, size = 15, face = "bold")) +
    theme(axis.text.x = element_text(colour = colorLine, size = 12),
          axis.text.y = element_text(colour = colorLine, size = 12)) +
    theme(panel.grid.major = element_line(colour = gridc, size = 0.5, linetype = "dashed")) +
    theme(panel.grid.minor = element_blank()) +
    theme(axis.line = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90)) +
    geom_vline(xintercept = 45, linetype = "dashed", colour = colorLine, size = 1)
  
  return(plot_timeline)
}
