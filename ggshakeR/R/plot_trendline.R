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
#' @param colour_xg is for selecting colour for xGoals. 
#' @param colour_xga is for selecting the colour for xGoalsAgainst.   
#' @param roll_avg is for setting the rolling average for the data.
#' @param theme to select the theme from 4 options -> dark, almond, rose, white.
#' 
#' @import dplyr
#' @import ggplot2
#' @import ggtext
#' @import Rcpp
#' @import RcppRoll
#' @import TTR
#' 
#' @export
#' 
#' @examples <- plot_trendline(data = pl, team = "Tottenham",
#'                                 colour_xg = "#08519c", colour_xga = "#cb181d",
#'                                 roll_avg = 10, theme = "dark")

plot_trendline <- function(data, team, colour_xg, colour_xga, roll_avg, theme = "") {
  
  print("Thank you for using ggshakeR!")
  
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
  
  data <- data[complete.cases(data[ ,'Date']), ] 
  data <- data[complete.cases(data[ ,'Home_xG']), ]
  data <- data[complete.cases(data[ ,'Away_xG']), ]
  
  df1 <- data %>%
    filter(Home == team)
  df1 <- df1[, c("Date","Home", "Home_xG")]
  df1 <- df1 %>%
    rename(xG = Home_xG) %>%
    rename(Team = Home)
  df2 <- data %>%
    filter(Away == team)
  df2 <- df2[, c("Date","Away", "Away_xG")]
  df2 <- df2 %>%
    rename(xG = Away_xG) %>%
    rename(Team = Away)
  df <- rbind(df1, df2)
  
  df3 <- data %>%
    filter(Home == team)
  df3 <- df3[, c("Date","Away", "Away_xG")]
  df3 <- df3 %>%
    rename(xGA = Away_xG) %>%
    rename(Team = Away)
  df4 <- data %>%
    filter(Away == team)
  df4 <- df4[, c("Date","Home", "Home_xG")]
  df4 <- df4 %>%
    rename(xGA = Home_xG) %>%
    rename(Team = Home)
  dfa <- rbind(df3, df4)
  dfa <- dfa[, 3]
  data <- cbind(df, dfa)
  data <- data %>%
    rename(xGA = dfa) %>%
    mutate(xGSUM = (xG + xGA)/2)
  
  data <- data[order(as.Date(data$Date),decreasing = FALSE),]
  
  if(nrow(data) > 0) { 
    data <- data %>%
      mutate(xGSM = TTR::SMA(xG, n = roll_avg),
             xGASM = TTR::SMA(xGA, n = roll_avg),
             xGSUM = TTR::SMA(xGSUM, n = roll_avg))
  }
  
  team <- data$Team
  team <- paste(team, "xG Trendline [xG vs. xGA]")
  subtitle <- paste(roll_avg, "Game Rolling Average")
  
  ggplot(data , aes(x = Date)) +
    geom_line(aes(y = xGSM, colour = colour_xg), size = 3) +
    geom_line(aes(y = xGASM, colour = colour_xga), size = 3) +
    geom_line(aes(y = xGSUM), colour = fill_b, size = 0.1) +  
    geom_point(aes(y = xGSM), colour = colour_xg, size = 4) +
    geom_point(aes(y = xGASM), colour = colour_xga, size = 4) +
    expand_limits(y = c(0.25, 2.25)) +
    stat_smooth(method = 'lm', aes(y = xGSM), color = colour_xg, linetype ="dashed",alpha = 0.5, size = 2,se = FALSE)+
    stat_smooth(method = 'lm', aes(y = xGA), color = colour_xga, linetype= "dashed", alpha = 0.5, size = 2,se = FALSE) +
    labs(title= team, 
         subtitle= subtitle) +
    theme(plot.title = element_markdown(lineheight = 1.1, size = 40, colour = colorText, face = "bold"),
          plot.subtitle = element_textbox_simple(lineheight = 1.1, size = 30, colour = colorText)) +
    theme(plot.background = element_rect(fill = fill_b, colour = colour_b)) +
    theme(panel.background = element_rect(fill = fill_b, colour = colour_b)) +
    labs(x = "Year", y = "xG") +
    theme(axis.title.x = element_text(colour = colorText, size = 24, face = "bold")) +
    theme(axis.title.y = element_text(colour = colorText, size = 24, face = "bold")) +
    theme(axis.text.x = element_text(colour = colorText, size = 18),
          axis.text.y = element_text(colour = colorText, size = 18)) +
    theme(panel.grid.major = element_line(colour = gridc, size = 1, linetype = "dashed"),
          panel.grid.minor = element_blank()) +
    theme(panel.grid.major.x = element_line(colour = gridc, size = 1, linetype = "dashed"),
          panel.background = element_blank()) + 
    theme(axis.line = element_line(size = 0.8, colour = colorLine)) + 
    geom_ribbon(aes(ymin=xGASM, ymax=xGSUM, x=Date), fill = colour_xga, alpha = 0.4) +
    geom_ribbon(aes(ymin=xGSUM, ymax=xGSM, x=Date), fill = colour_xg, alpha = 0.4) +
    stat_smooth(method = 'lm', aes(y = xGSM), color = colour_xg, linetype ="dashed",alpha = 0.5, size = 2,se = FALSE)+
    stat_smooth(method = 'lm', aes(y = xGA), color = colour_xga, linetype= "dashed", alpha = 0.5, size = 2,se = FALSE) +
    labs(col = "Key") +
    scale_color_manual(labels = c("xG", "xGA"), values = c(colour_xg, colour_xga)) +
    theme(legend.background = element_rect(colour = colour_b, fill = fill_b),
          legend.title = element_text(colour = colorText, size = 22, face = "bold"),
          legend.text = element_text(colour = colorText, size = 18),
          legend.position = "bottom") +
    theme(legend.key.width = unit(3, "cm"),
          legend.key.size = unit(1, "cm"))
}