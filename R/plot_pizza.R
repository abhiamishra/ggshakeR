#' Function for plotting percentile/pizza plots
#'
#' This function allows for data, that has to be scraped from FBref, to be used
#' for plotting single and comparison percentile plots.
#'
#' For best image quality use:
#' ggsave("image.png", width = 2900, height = 2800, units = "px")
#'
#' @param data that houses data. Dataframe can contain either one player or two depending on the type of plot made.
#' @param type Type of plot -> single and comparison.
#' @param template for selecting a group of pre-selected metrics for each position by position namely:
#'                 forward, winger, midfielder, defender, goalkeeper, full back and custom.
#' @param theme Theme preferences for display: dark (default), white, black.
#' @param colour_poss for selecting the colour for possession group of stats. To be used only for single player plot.
#' @param colour_att for selecting the colour for attacking group of stats. To be used only for single player plot.
#' @param colour_def for selecting the colour for defense group of stats. To be used only for single player plot.
#' @param player_1 for selecting the first player. To be used only for comparison plot.
#' @param player_2 for selecting the second player. To be used only for comparison plot.
#' @param colour_compare for selecting the colour of comparison to be used only for comparison plot.
#' @param season Specify what season to pick for a single player pizza chart. Pick the scouting period from the scouting period column in the data.
#' @param season_player_1 Specify what season to pick for the first player in a pizza chart
#' @param season_player_2 Specify what season to pick for the second player in a pizza chart
#' @param theme Specify the theme of the pizza chart -> dark, black, and white. Default set to black
#' @return a ggplot2 object
#'
#' @import dplyr
#' @import ggplot2
#' @import forcats
#' @import ggtext
#' @import stringr
#' @import stringi
#'
#' @export
#'
#' @examples
#' \dontrun{
#' plot1 <- plot_pizza(data = data, type = "comparison", template = "midfielder",
#'                     player_1 = "Nicolo Barella", player_2 = "Ilkay Gundogan",
#'                     season_player_1 = "Last 365 Days", season_player_2 = "Last 365 Days",
#'                     colour_compare = "lightgreen", theme = "black")
#' plot1
#'
#' plot2 <- plot_pizza(data = data1, type = "single", template = "midfielder",
#'                     colour_poss = "green", colour_att = "lightblue", season = "Last 365 Days",
#'                     colour_def = "red", theme = "dark")
#' plot2
#' }

plot_pizza <- function(data, type = "", template = "", colour_poss, colour_att, colour_def, 
                       player_1, player_2, colour_compare, 
                       season, season_player_1, season_player_2, theme = "") {
  
  colorText <- ""
  gridline <- ""
  fill_b <- ""
  colour_b <- ""
  colorLine <- ""
  
  if (theme == "dark" || theme == "") {
    fill_b <- "#0d1117"
    colour_b <- "#0d1117"
    
    colorText <- "white"
    gridline <- "565656"
    colorLine <- "white"
  } else if (theme == "black") {
    fill_b <- "black"
    colour_b <- "black"
    
    colorText <- "white"
    gridline <- "565656"
    colorLine <- "white"
  } else if(theme == "white") {
    fill_b <- "white"
    colour_b <- "white"
    
    colorText <- "black"
    gridline <- "565656"
    colorLine <- "black"
  }
  
  if (type == "single" || type == "") { ## SINGLE PLOT ----
    
    data <- data %>%
      filter(scouting_period == season)
    
    if (template == "forward" || template == "") {
      
      if (nrow(data) > 148) {
        
        data$no <- 1:nrow(data)
        data_selected <- data[c(3,8,13,24,42,128,45,115,133,107,101,102,26,147),]
        data_selected <- data_selected %>%
          mutate(stat = case_when(Statistic == "Non-Penalty Goals"|
                                    Statistic == "xG"|
                                    Statistic == "Shots Total"|
                                    Statistic == "Non-Penalty Goals - npxG"|
                                    Statistic == "npxG/Shot" ~ "Attacking",
                                  Statistic == "xA"|
                                    Statistic == "Miscontrols"|
                                    Statistic == "Passes into Penalty Area"|
                                    Statistic == "Touches (Att Pen)"|
                                    Statistic == "Progressive Passes Rec" ~ "Possession",
                                  TRUE ~ "Defending"))
        
        data_selected$stat[4] <- "Attacking"
      } else {
        
        data$no <- 1:nrow(data)
        data_selected <- data[c(3,8,13,24,41,127,44,114,132,106,100,101,25,146),]
        data_selected <- data_selected %>%
          mutate(stat = case_when(Statistic == "Non-Penalty Goals"|
                                    Statistic == "xG"|
                                    Statistic == "Shots Total"|
                                    Statistic == "Non-Penalty Goals - npxG"|
                                    Statistic == "npxG/Shot" ~ "Attacking",
                                  Statistic == "xA"|
                                    Statistic == "Miscontrols"|
                                    Statistic == "Passes into Penalty Area"|
                                    Statistic == "Touches (Att Pen)"|
                                    Statistic == "Progressive Passes Rec" ~ "Possession",
                                  TRUE ~ "Defending"))
        
        data_selected$stat[4] <- "Attacking"
      }
    } else if (template == "midfielder") {
      
      if (nrow(data) > 148) {
        
        data$no <- 1:nrow(data)
        data_selected <- data[c(3,9,10,13,53,44,47,116,125,133,146,147,107,96),]
        data_selected <- data_selected %>%
          mutate(stat = case_when(Statistic == "npxG + xA"|
                                    Statistic == "Average Shot Distance"|
                                    Statistic == "Shots Total"|
                                    Statistic == "Non-Penalty Goals" ~ "Attacking",
                                  Statistic == "Progressive Passes"|
                                    Statistic == "Passes Under Pressure"|
                                    Statistic == "Passes into Final Third"|
                                    Statistic == "Touches (Live-Ball)"|
                                    Statistic == "Progressive Carries"|
                                    Statistic == "Progressive Passes Rec" ~ "Possession",
                                  TRUE ~ "Defending"))
        
        data_selected$stat[2] <- "Attacking"
        data_selected$stat[3] <- "Attacking"
      } else {
        
        data$no <- 1:nrow(data)
        data_selected <- data[c(3,9,10,13,52,43,46,115,124,132,145,146,106,97),]
        data_selected <- data_selected %>%
          mutate(stat = case_when(Statistic == "npxG + xA"|
                                    Statistic == "Average Shot Distance"|
                                    Statistic == "Shots Total"|
                                    Statistic == "Non-Penalty Goals" ~ "Attacking",
                                  Statistic == "Progressive Passes"|
                                    Statistic == "Passes Under Pressure"|
                                    Statistic == "Passes into Final Third"|
                                    Statistic == "Touches (Live-Ball)"|
                                    Statistic == "Progressive Carries"|
                                    Statistic == "Progressive Passes Rec" ~ "Possession",
                                  TRUE ~ "Defending"))
        
        data_selected$stat[2] <- "Attacking"
        data_selected$stat[3] <- "Attacking"
      }
    } else if (template == "defender") {
      
      if(nrow(data) > 148) {
        
        data$no <- 1:nrow(data)
        data_selected <- data[c(3,11,13,44,47,129,125,110,88,96,102,106,147,108),]
        data_selected <- data_selected %>%
          mutate(stat = case_when(Statistic == "Non-Penalty Goals"|
                                    Statistic == "npxG + xA"|
                                    Statistic == "Shots Total" ~ "Attacking",
                                  Statistic == "Passes into Final Third"|
                                    Statistic == "Progressive Passes"|
                                    Statistic == "Progressive Carries"|
                                    Statistic == "Touches"|
                                    Statistic == "Dispossessed" ~ "Possession",
                                  TRUE ~ "Defending"))
        
        data_selected$stat[2] <- "Attacking"
      } else {
        
        data$no <- 1:nrow(data)
        data_selected <- data[c(3,11,13,43,46,128,124,109,87,95,101,105,146,107),]
        data_selected <- data_selected %>%
          mutate(stat = case_when(Statistic == "Non-Penalty Goals"|
                                    Statistic == "npxG + xA"|
                                    Statistic == "Shots Total" ~ "Attacking",
                                  Statistic == "Passes into Final Third"|
                                    Statistic == "Progressive Passes"|
                                    Statistic == "Progressive Carries"|
                                    Statistic == "Touches"|
                                    Statistic == "Dispossessed" ~ "Possession",
                                  TRUE ~ "Defending"))
        
        data_selected$stat[2] <- "Attacking"
      }
    } else if (template == "full back") {
      
      if(nrow(data) > 148) {
        
        data$no <- 1:nrow(data)
        data_selected <- data[c(3,9,10,13,114,46,47,125,43,44,147,96,107,102),]
        data_selected <- data_selected %>%
          mutate(stat = case_when(Statistic == "Non-Penalty Goals"|
                                    Statistic == "npxG"|
                                    Statistic == "xA"|
                                    Statistic == "Shots Total" ~ "Attacking",
                                  Statistic == "Passes into Final Third"|
                                    Statistic == "Progressive Passes"|
                                    Statistic == "Progressive Carries"|
                                    Statistic == "Touches (Att 3rd)"|
                                    Statistic == "Crosses into Penalty Area"|
                                    Statistic == "Key Passes" ~ "Possession",
                                  TRUE ~ "Defending"))
        
        data_selected$stat[3] <- "Attacking"
      } else {
        
        data$no <- 1:nrow(data)
        data_selected <- data[c(3,9,10,13,113,45,46,124,42,43,146,95,106,101),]
        data_selected <- data_selected %>%
          mutate(stat = case_when(Statistic == "Non-Penalty Goals"|
                                    Statistic == "npxG"|
                                    Statistic == "xA"|
                                    Statistic == "Shots Total" ~ "Attacking",
                                  Statistic == "Passes into Final Third"|
                                    Statistic == "Progressive Passes"|
                                    Statistic == "Progressive Carries"|
                                    Statistic == "Touches (Att 3rd)"|
                                    Statistic == "Crosses into Penalty Area"|
                                    Statistic == "Key Passes" ~ "Possession",
                                  TRUE ~ "Defending"))
      }
    } else if (template == "winger") {
      
      if(nrow(data) > 148) {
        
        data$no <- 1:nrow(data)
        data_selected <- data[c(3,22,24,42,143,45,119,47,124,133,107,146,101,102),]
        data_selected <- data_selected %>%
          mutate(stat = case_when(Statistic == "Non-Penalty Goals"|
                                    Statistic == "xG"|
                                    Statistic == "xA"|
                                    Statistic == "Penalty Kicks Won"|
                                    Statistic == "npxG/Shot" ~ "Attacking",
                                  Statistic == "Progressive Carrying Distance"|
                                    Statistic == "Successful Dribble %"|
                                    Statistic == "Progressive Passes"|
                                    Statistic == "Passes into Penalty Area"|
                                    Statistic == "Progressive Passes Rec" ~ "Possession",
                                  TRUE ~ "Defending"))
        
        data_selected$stat[3] <- "Attacking"
      } else {
        
        data$no <- 1:nrow(data)
        data_selected <- data[c(3,21,23,41,142,44,118,46,123,132,106,145,100,101),]
        data_selected <- data_selected %>%
          mutate(stat = case_when(Statistic == "Non-Penalty Goals"|
                                    Statistic == "xG"|
                                    Statistic == "xA"|
                                    Statistic == "Penalty Kicks Won"|
                                    Statistic == "npxG/Shot" ~ "Attacking",
                                  Statistic == "Progressive Carrying Distance"|
                                    Statistic == "Successful Dribble %"|
                                    Statistic == "Progressive Passes"|
                                    Statistic == "Passes into Penalty Area"|
                                    Statistic == "Progressive Passes Rec" ~ "Possession",
                                  TRUE ~ "Defending"))
        
        data_selected$stat[3] <- "Attacking"
      }
    } else if (template == "goalkeeper") {
      
      if(nrow(data) > 36) {
        
        data$no <- 1:nrow(data)
        data_selected <- data[c(4,20,22,24,26,29,35,36,37), ]
        data_selected <- data_selected %>%
          mutate(stat = case_when(Statistic == "Save%"|
                                    Statistic == "PSxG"|
                                    Statistic == "PSxG-GA" ~ "Defending",
                                  Statistic == "Passes Attempted (Launched)"|
                                    Statistic == "Passes Attempted"|
                                    Statistic == "Average Pass Length" ~ "Possession",
                                  TRUE ~ "Attacking"))
      } else {
        
        data$no <- 1:nrow(data)
        data_selected <- data[c(4,19,21,23,25,28,34,35,36), ]
        data_selected <- data_selected %>%
          mutate(stat = case_when(Statistic == "Save%"|
                                    Statistic == "PSxG"|
                                    Statistic == "PSxG-GA" ~ "Defending",
                                  Statistic == "Passes Attempted (Launched)"|
                                    Statistic == "Passes Attempted"|
                                    Statistic == "Average Pass Length" ~ "Possession",
                                  TRUE ~ "Attacking"))
      }
    } else if (template == "custom") {
      data_selected <- data %>% 
        mutate(stat = case_when(
          StatGroup == "Standard" ~ "Attacking", 
          StatGroup == "Shooting" ~ "Attacking",
          StatGroup == "Passing" ~ "Possession",
          StatGroup == "Pass Types" ~ "Possession",
          StatGroup == "Goal and Shot Creation" ~ "Possession",
          StatGroup == "Defense" ~ "Defending",
          StatGroup == "Possession" ~ "Possession",
          StatGroup == "Miscellaneous Stats" ~ "Defending",
          TRUE ~ NA
        ))
    }
    
    player_name <- data$Player
    title <- paste(player_name, "Percentile Chart")
    min <- data$BasedOnMinutes
    sub <- data$Versus
    sub1 <- data$scouting_period
    subtitle <- paste("Compared to", sub, "|", sub1, "|", min, "minutes played")
    caption <- "Plot code by @RobinWilhelmus
Data from StatsBomb via FBref. Inspired by @Worville. Created using ggshakeR."
    
    temp <- (360 / (length(data_selected$Player)) / 2)
    myAng <- seq(-temp, -360 + temp, length.out = length(data_selected$Player))
    ang<-ifelse(myAng < -90, myAng + 180, myAng)
    ang<-ifelse(ang < -90, ang + 180, ang)
    
    x <- c(data_selected$Statistic, data_selected$stat)
    
    ggplot(data_selected, aes(fct_reorder(Statistic, stat), Percentile)) +
      geom_bar(aes(y = 100), fill = fill_b, stat = "identity", width = 1, colour = gridline,
               alpha = 0.5, show.legend = FALSE) +
      geom_bar(stat = "identity", width = 1, aes(fill = stat), colour = fill_b, alpha = 1) +
      coord_polar(clip = "off") +
      geom_hline(yintercept = 25, colour = gridline, linetype = "longdash", alpha = 0.5) +
      geom_hline(yintercept = 50, colour = gridline, linetype = "longdash", alpha = 0.5)+
      geom_hline(yintercept = 75, colour = gridline, linetype = "longdash", alpha = 0.5)+
      scale_fill_manual(values = c("Possession" = colour_poss,
                                   "Attacking" = colour_att,
                                   "Defending" = colour_def)) +
      geom_label(aes(label = Per90, fill = stat), size = 3, color = fill_b, show.legend = FALSE)+
      scale_y_continuous(limits = c(-20, 100))+
      labs(fill = "",
           caption = caption,
           title = title,
           subtitle = subtitle)+
      theme_minimal() +
      theme(plot.background = element_rect(fill = fill_b,color = colour_b),
            panel.background = element_rect(fill = fill_b,color = colour_b),
            legend.position = "bottom",
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_text(size = 12,colour = colorText, angle = ang),
            text = element_text(colour = colorText, size = 20),
            plot.title = element_markdown(hjust = 0.5, size = 26, colour = colorText, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, size = 20, colour = colorText),
            plot.caption = element_text(hjust = 0.5, size = 15, colour = colorText),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
    
  } else if (type == "comparison") { ## COMPARISON PLOT ----
    
    data$Player <- stri_trans_general(str = data$Player, id = "Latin-ASCII")
    
    data1 <- data %>%
      filter(Player == player_1) %>%
      filter(scouting_period == season_player_1)
    data2 <- data %>%
      filter(Player == player_2) %>%
      filter(scouting_period == season_player_2)
    
    if (template == "forward" || template == "") {
      
      if (nrow(data1) > 148) {
        
        data1$no <- 1:nrow(data1)
        data2$no <- 1:nrow(data2)
        data1 <- data1[c(3,8,13,24,42,128,45,115,133,107,101,102,26,147),]
        data2 <- data2[c(3,8,13,24,42,128,45,115,133,107,101,102,26,147),]
      } else {
        
        data1$no <- 1:nrow(data1)
        data2$no <- 1:nrow(data2)
        data1 <- data1[c(3,8,13,23,41,127,44,114,132,106,100,101,25,146),]
        data2 <- data2[c(3,8,13,24,42,128,45,115,133,107,101,102,26,147),]
      }
    } else if (template == "midfielder") {
      
      if (nrow(data1) > 148) {
        
        data1$no <- 1:nrow(data1)
        data2$no <- 1:nrow(data2)
        data1 <- data1[c(3,9,10,13,53,44,47,116,125,133,146,147,107,96),]
        data2 <- data2[c(3,9,10,13,53,44,47,116,125,133,146,147,107,96),]
      } else {
        
        data1$no <- 1:nrow(data1)
        data2$no <- 1:nrow(data2)
        data1 <- data1[c(3,9,10,13,52,43,46,115,124,132,145,146,106,95),]
        data2 <- data2[c(3,9,10,13,53,44,47,116,125,133,146,147,107,96),]
      }
    } else if (template == "defender") {
      
      if (nrow(data1) > 148) {
        
        data1$no <- 1:nrow(data1)
        data2$no <- 1:nrow(data2)
        data1 <- data1[c(3,11,13,44,47,129,125,110,88,96,102,106,147,108),]
        data2 <- data2[c(3,11,13,44,47,129,125,110,88,96,102,106,147,108),]
        
      } else {
        
        data1$no <- 1:nrow(data1)
        data2$no <- 1:nrow(data2)
        data1 <- data1[c(3,11,13,43,46,128,124,109,87,95,101,105,146,107),]
        data2 <- data2[c(3,11,13,44,47,129,125,110,88,96,102,106,147,108),]
      }
    } else if (template == "full back") {
      
      if(nrow(data1) > 148) {
        
        data1$no <- 1:nrow(data1)
        data2$no <- 1:nrow(data2)
        data1 <- data1[c(3,9,10,13,114,46,47,125,43,44,147,96,107,102),]
        data2 <- data2[c(3,9,10,13,114,46,47,125,43,44,147,96,107,102),]
      } else {
        
        data1$no <- 1:nrow(data1)
        data2$no <- 1:nrow(data2)
        data1 <- data1[c(3,9,10,13,113,45,46,124,42,43,146,95,106,101),]
        data2 <- data2[c(3,9,10,13,114,46,47,125,43,44,147,96,107,102),]
      }
    } else if(template == "winger") {
      
      if (nrow(data1) > 148) {
        
        data1$no <- 1:nrow(data1)
        data2$no <- 1:nrow(data2)
        data1 <- data1[c(3,22,24,42,143,45,119,47,124,133,107,146,101,102),]
        data2 <- data2[c(3,22,24,42,143,45,119,47,124,133,107,146,101,102),]
      } else {
        
        data1$no <- 1:nrow(data1)
        data2$no <- 1:nrow(data2)
        data1 <- data1[c(3,21,23,41,142,44,118,46,123,132,106,145,100,101),]
        data2 <- data2[c(3,22,24,42,143,45,119,47,124,133,107,146,101,102),]
      }
    } else if (template == "goalkeeper") {
      
      if(nrow(data1) > 36) {
        
        data1$no <- 1:nrow(data1)
        data2$no <- 1:nrow(data2)
        data1 <- data1[c(4,20,22,24,26,29,35,36,37), ]
        data2 <- data2[c(4,20,22,24,26,29,35,36,37), ]
      } else {
        
        data1$no <- 1:nrow(data1)
        data2$no <- 1:nrow(data2)
        data1 <- data1[c(4,19,21,23,25,28,34,35,36), ]
        data2 <- data2[c(4,20,22,24,26,29,35,36,37), ]
      }
    } else if (template == "custom") {
      data1
      data2
    }
    
    data2 <- data2 %>%
      rename(player = Player,
             per90 = Per90,
             percentile = Percentile)
    
    temp <- (360 / (length(data1$Player)) / 2)
    myAng <- seq(-temp, -360 + temp, length.out = length(data1$Player))
    ang<-ifelse(myAng < -90, myAng + 180, myAng)
    ang<-ifelse(ang < -90, ang + 180, ang)
    
    player_name1 <- data1$Player
    player_name2 <- data2$player
    min1 <- data1$BasedOnMinutes
    min2 <- data2$BasedOnMinutes
    sub <- data1$Versus
    lg1 <- data1$scouting_period
    lg2 <- data2$scouting_period
    title <- paste(player_name1, "|", lg1, "|", min1, "minutes")
    subtitle <- paste(player_name2, "|", lg2, "|", min2, "minutes")
    caption <- paste("Compared to", sub, ".
Data from StatsBomb via FBref. Inspired by @FootballSlices. Created using ggshakeR.")
    
    x <- data1$Statistic
    
    ggplot(data1, aes(x = Statistic, y = Percentile)) +
      geom_bar(aes(y = 100), fill = fill_b, stat = "identity", width = 1,colour = colour_b,
               alpha = 0.5, show.legend = FALSE) +
      geom_bar(data = data1, aes(fill = colour_compare), colour = colour_compare, stat = "identity", width = 1, alpha = 1) +
      scale_fill_manual(values = colour_compare) +
      geom_bar(data = data2, aes(y = percentile, fill = NA), stat = "identity", width = 1, alpha = 0, colour = colorLine, size = 3) +
      coord_polar(clip = "off") +
      geom_hline(yintercept = 25, colour = gridline, linetype = "longdash", alpha = 0.5)+
      geom_hline(yintercept = 50, colour = gridline, linetype = "longdash", alpha = 0.5)+
      geom_hline(yintercept = 75, colour = gridline, linetype = "longdash", alpha = 0.5)+
      geom_hline(yintercept = 100, colour = gridline, linetype = "solid", alpha = 0.5)+
      scale_y_continuous(limits = c(-20, 100))+
      labs(caption = caption,
           title = title,
           subtitle = subtitle)+
      theme_minimal() +
      theme(plot.background = element_rect(fill = fill_b,color = colour_b),
            panel.background = element_rect(fill = fill_b,color = colour_b),
            legend.position = "none",
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_text(size = 12,colour = colorText, angle = ang),
            text = element_text(colour = colorText, size = 20),
            plot.title = element_markdown(hjust = 0.5, size = 26, colour = colour_compare, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, size = 26, colour = colorLine, face = "bold"),
            plot.caption = element_text(hjust = 0.5, size = 15, colour = colorText),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
  }
}
