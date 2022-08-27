#' Create timeline charts (xG) using 'understat' event data.
#'
#'This function allows to make match timelines using data collected event-by-event.
#'
#' @param data Data, for now only compatible with understat data.
#' @param match_year the year the match was played.
#' @param team_home home team according to data.
#' @param team_away away team according to data.
#' @param color_home line colour for the home team.
#' @param color_away line colour for the away team.
#' @param theme Choose from 4 ggplot2 themes -> dark, almond, rose, white.
#' @return ggplot2 object of a xG timeline plot
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

plot_timeline <- function(data, match_year, 
                          team_home, team_away, 
                          color_home, color_away, 
                          theme = "") {

  if (!"season" %in% colnames(data)) {
    data <- data %>%
      rename(season = year)
  }
  
  if (!"home_away" %in% colnames(data)) {
    data <- data %>%
      rename(home_away = h_a)
  }
  
  data <- data %>%
    dplyr::filter(season == match_year) %>% 
    ## Prepare goal labels
    mutate(player_label = dplyr::case_when(
      result == "Goal" & situation != "Penalty" ~ paste0(player, ": ", round(xG, digits = 2), " xG"),
      result == "Goal" & situation == "Penalty" ~ paste0(player, " (Penalty): ", round(xG, digits = 2), " xG"),
      result == "OwnGoal" ~ paste0(player, " (Own Goal): ", round(xG, digits = 2), " xG"),
      TRUE ~ ""),
      minute = as.numeric(minute)) %>%
    dplyr::filter(home_team == team_home,
           away_team == team_away) %>% 
    ## Calculate xG cumulative sum per team
    group_by(home_away) %>% 
    mutate(xGsum = cumsum(xG)) %>% 
    ungroup()

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
  
  ## Set up labels for plot title ----
  team1 <- unique(data$home_team)
  team2 <- unique(data$away_team)
  
  xG1 <- round(sum(data1$xG), 2)
  xG2 <- round(sum(data2$xG), 2)
  
  g1 <- unique(data$home_goals)
  g2 <- unique(data$away_goals)
  
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
  
  ## Plot title ----
  # plot_title <- glue("<b style='color:{color_home}'> {team1} : {g1} {gls1} ({xG1} xG) </b> vs. <b style='color:{color_away}'> {team2} : {g2} {gls2} ({xG2} xG)</b>")
  plot_title <- sprintf("<b style='color:%s'> %s : %s %s (%s xG) </b> vs. <b style='color:%s'> %s : %s %s (%s xG)</b>",
                        color_home, team1, g1, gls1, xG1, 
                        color_away, team2, g2, gls2, xG2)
  
  ## Plot themes ----
  fill_b <- ""
  colour_b <- ""
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
  
  plotCaption <- "Created using ggshakeR"
  
  ## Plot! ----
  plot_timeline <- ggplot() +
    geom_step(data = data1, aes(x = minute, y = xGsum), color = color_home, size = 3) +
    geom_step(data = data2, aes(x = minute, y = xGsum), color = color_away, size = 3) +
    geom_point(data = dat1, aes(x = minute, y = xGsum), color = color_home, fill = fill_b, shape = 21, stroke = 2, size = 6) +
    geom_point(data = dat2, aes(x = minute, y = xGsum), color = color_away, fill = fill_b, shape = 21, stroke = 2, size = 6) +
    geom_text_repel(data = data %>% filter(result %in% c("Goal", "OwnGoal")), 
                    aes(x = minute, y = xGsum, label = player_label),
                    box.padding   = 2,
                    point.padding = 1.5,
                    segment.color = colorLine,
                    alpha = 0.8,
                    color = colorText,
                    size = 5) +
    theme_minimal() +
    labs(title = plot_title, caption = plotCaption) +
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
