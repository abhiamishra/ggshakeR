#' Function for plotting xG Trendline with FBref/ StatsBomb data.
#'
#' The data can be scraped from FBref.\cr
#' Dataframe passed in must have the following column names: \cr
#' \cr
#' Date (format: year-month-day).yyyy-mm-dd, \cr
#' Home_xG (xG for Home Team), \cr
#' Away_xG (xG for Away Team), \cr
#' Home (Home Team), \cr
#' Away (Away Team)
#'
#' For best clarity, export plot as a 2000x1000 png
#'
#' @param data is for the dataset used. Select the number of matches wanted in the viz beforehand.
#' @param team is to select the specific team for the viz. Team must be accurate as per FBref specifications.
#' @param color_xg is for selecting color for xGoals.
#' @param color_xga is for selecting the color for xGoalsAgainst.
#' @param rolling_average is for setting the rolling average for the data.
#' @param theme to select the theme from 4 options -> dark, almond, rose, white.
#'
#' @import dplyr
#' @import ggplot2
#' @import ggtext
#' @import TTR
#'
#' @export
#'
#' @examples
#' \dontrun{
#' plot <- plot_trendline(data = pl, team = "Tottenham",
#'                        color_xg = "#08519c", color_xga = "#cb181d",
#'                        rolling_average = 10, theme = "dark")
#' plot
#' }

plot_trendline <- function(data, team, color_xg, color_xga, rolling_average, theme = "") {
  
  ## Clean data ----
  data <- data[complete.cases(data[, 'Date']), ]
  data <- data[complete.cases(data[, 'Home_xG']), ]
  data <- data[complete.cases(data[, 'Away_xG']), ]
  
  df1 <- data %>%
    filter(Home == team)
  df1 <- df1[, c("Date", "Home", "Home_xG")]
  df1 <- df1 %>%
    rename(xG = Home_xG) %>%
    rename(Team = Home)
  df2 <- data %>%
    filter(Away == team)
  df2 <- df2[, c("Date", "Away", "Away_xG")]
  df2 <- df2 %>%
    rename(xG = Away_xG) %>%
    rename(Team = Away)
  df <- rbind(df1, df2)
  
  df3 <- data %>%
    filter(Home == team)
  df3 <- df3[, c("Date", "Away", "Away_xG")]
  df3 <- df3 %>%
    rename(xGA = Away_xG) %>%
    rename(Team = Away)
  df4 <- data %>%
    filter(Away == team)
  df4 <- df4[, c("Date", "Home", "Home_xG")]
  df4 <- df4 %>%
    rename(xGA = Home_xG) %>%
    rename(Team = Home)
  dfa <- rbind(df3, df4)
  dfa <- dfa[, 3]
  data <- cbind(df, dfa)
  data <- data %>%
    rename(xGA = dfa) %>%
    mutate(xGSUM = (xG + xGA) / 2)
  
  data <- data[order(as.Date(data$Date), decreasing = FALSE), ]
  
  if (nrow(data) > 0) {
    data <- data %>%
      mutate(xGSM = TTR::SMA(xG, n = rolling_average),
             xGASM = TTR::SMA(xGA, n = rolling_average),
             xGSUM = TTR::SMA(xGSUM, n = rolling_average))
  }
  
  ## Plot titles
  team <- paste(team, "xG Trendline")
  # subtitle <- glue("{rolling_average} Game Rolling Average [<b style='color:{color_xg}'> xG </b> vs <b style='color:{color_xga}'> xGA </b>]")
  subtitle <- sprintf("%s Game Rolling Average [<b style='color:%s'> xG </b> vs <b style='color:%s'> xGA </b>]",
                      rolling_average, color_xg, color_xga)
  
  ## Plot themes
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
  
  ## PLOT! ----
  trendline_plot <- ggplot(data, aes(x = Date)) +
    ## Lines
    geom_line(aes(y = xGSM), color = color_xg, size = 3) +
    geom_line(aes(y = xGASM), color = color_xga, size = 3) +
    geom_line(aes(y = xGSUM), color = fill_b, size = 0.1) +
    ## Points
    geom_point(aes(y = xGSM), color = color_xg, size = 4) +
    geom_point(aes(y = xGASM), color = color_xga, size = 4) +
    scale_color_manual(values = c(color_xg, color_xga)) +
    expand_limits(y = c(0.25, 2.25)) +
    ## labels
    labs(title = team,
         subtitle = subtitle,
         x = "Year", y = "xG") +
    ## theme
    theme(plot.title = element_markdown(lineheight = 1.1, size = 40, color = colorText, face = "bold"),
          plot.subtitle = element_textbox_simple(lineheight = 1.1, size = 30, color = colorText),
          plot.background = element_rect(fill = fill_b, color = color_b),
          # panel.background = element_rect(fill = fill_b, color = color_b),
          axis.title.x = element_text(color = colorText, size = 24, face = "bold"),
          axis.title.y = element_text(color = colorText, size = 24, face = "bold"),
          axis.text.x = element_text(color = colorText, size = 18),
          axis.text.y = element_text(color = colorText, size = 18),
          panel.grid.major = element_line(color = gridc, size = 1, linetype = "dashed"),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(color = gridc, size = 1, linetype = "dashed"),
          panel.background = element_blank(),
          axis.line = element_blank()) + 
    geom_ribbon(aes(ymin = xGASM, ymax = xGSUM, x = Date), fill = color_xga, alpha = 0.4) +
    geom_ribbon(aes(ymin = xGSUM, ymax = xGSM, x = Date), fill = color_xg, alpha = 0.4) +
    stat_smooth(method = 'lm', aes(y = xGSM), color = color_xg, linetype = "dashed", alpha = 0.5, size = 2, se = FALSE) +
    stat_smooth(method = 'lm', aes(y = xGA), color = color_xga, linetype = "dashed", alpha = 0.5, size = 2, se = FALSE)
  
  return(trendline_plot)
}
