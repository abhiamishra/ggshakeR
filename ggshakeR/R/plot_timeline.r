#' Create timeline charts (xG) using event data.
#'
#'This function allows to make match timelines using data collected event-by-event.
#' 
#' @param data is for the dataset used.
#' @param match_year 
#' @param team_home is for the home team according to data.
#' @param team_away is for the away team according to data.
#' @param home_color is for the colour of the line for the home team.
#' @param away_color is for the colour of the line for the away team.
#' @param theme to select the colours. Choose from 4 themes -> dark, almond, rose, white.
#' @return a ggplot2 object
#' 
#' @import dplyr
#' @import ggtext
#' @import ggplot2
#' @import berryFunctions
#' @import ggrepel
#' @import glue
#' 
#' @export 
#' 
#' @examples_plot <- plot_timeline(data = data, match_year = 2021, team_home = "Manchester United", team_away = "Liverpool",
#'                                 home_color = "#e31a1c", away_color = "#980043", theme = "dark")

plot_timeline <- function(data, match_year, team_home, team_away, home_color, away_color, theme = "") {

fill_b = ""
colour_b = ""
colorLine = ""
colorText = ""
gridc = ""
  
if(theme == "dark" || theme == ""){
    fill_b = "#0d1117"
    colour_b = "white"
    
    
    colorLine = "white"
    gridc = "#525252"
    colorText = "white"
  }
else if(theme == "white"){
    fill_b = "#F5F5F5"
    colour_b = "black"
    
    colorLine = "black"
    gridc = "grey"
    colorText = "black"
  }
else if(theme == "rose"){
    fill_b = "#FFE4E1"
    colour_b = "#696969"
    
    colorLine = "#322E2E"
    gridc = "grey"
    colorText = "#322E2E"
  }
else if(theme == "almond"){
    fill_b = "#FFEBCD"
    colour_b = "#696969"
    
    colorLine = "#322E2E"
    gridc = "grey"
    colorText = "#322E2E"
}

data$minute <- as.numeric(data$minute)

if("season" %in% colnames(data)) {
  } else {
    data <- data %>%
    mutate(season = year)
  }

data <- data %>%
filter(season == match_year)

if("home_away" %in% colnames(data)) {
} else {
  data <- data %>%
    mutate(home_away = h_a)
}

data <- data %>%
 filter(home_team == team_home,
        away_team == team_away)
data1 <- data %>%
  filter(home_away == "h") %>%
  mutate(xGsum = cumsum(xG))
data2 <- data %>%
  filter(home_away == "a") %>%
  mutate(xGsum = cumsum(xG))

data1 <- insertRows(data1, 1, new = 0)
data2 <- insertRows(data2, 1, new = 0)

dat1 <- data1 %>%
  filter(result == "Goal")
d1 <- data1 %>%
  filter(result == "OwnGoal")
dat1 <- rbind(dat1, d1)
dat2 <- data2 %>%
  filter(result == "Goal")
d2 <- data2 %>%
  filter(result == "OwnGoal")
dat2 <- rbind(dat2, d2)

team1 <- data$home_team
team2 <- data$away_team

xG1 <- round(sum(data1$xG), 2)
xG2 <- round(sum(data2$xG), 2)

g1 <- data$home_goals
g2 <- data$away_goals

if(g1 == 1) {
  gls1 <- "Goal"
} else {
  gls1 <- "Goals"
}

if(g2 == 1) {
  gls2 <- "Goal"
} else {
  gls2 <- "Goals"
}

plot_title <- glue("<b style='color:{home_color}'> {team1} : {g1} {gls1} ({xG1} xG) </b> vs <b style='color:{away_color}'> {team2} : {g2} {gls2} ({xG2} xG)</b>")

min1 <- dat1$minute
min2 <- dat2$minute
p1 <- dat1$player
p2 <- dat2$player

player_lab1 <- glue("{p1} : {min1}")
player_lab2 <- glue("{p2} : {min2}")

ggplot() +
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
  labs(title= plot_title) + 
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
}