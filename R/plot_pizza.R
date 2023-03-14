#' Function for plotting percentile/pizza plots
#'
#' This function allows for data, that has to be scraped from FBref, to be used
#' for plotting single and comparison percentile plots.
#'
#' For best image quality use:
#' ggsave("image.png", width = 2900, height = 2800, units = "px")
#'
#' @param data Data frame can contain either one player or two depending on the type of plot made
#' @param type Type of plot -> single and comparison
#' @param template Selecting a group of pre-selected metrics for each position by position namely:
#'                 outfielder, goalkeeper and custom
#' @param color_possession Selecting the color for possession group of stats. To be used only for single player plot
#' @param color_attack Selecting the color for attacking group of stats. To be used only for single player plot
#' @param color_defense Selecting the color for defense group of stats. To be used only for single player plot
#' @param player_1 Selecting the first player. To be used only for comparison plot
#' @param player_2 Selecting the second player. To be used only for comparison plot
#' @param color_compare Selecting the color of comparison to be used only for comparison plot
#' @param season Specify what season to pick for a single player pizza chart. Pick the scouting period from the scouting period column in the data
#' @param season_player_1 Specify what season to pick for the first player in a pizza chart
#' @param season_player_2 Specify what season to pick for the second player in a pizza chart
#' @param theme Specify the theme of the pizza chart -> dark, black, and white. Default set to dark
#' @return a ggplot2 object
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom stringi stri_wrap stri_trans_general
#'
#' @export
#'
#' @examples
#' \dontrun{
#' plot1 <- plot_pizza(data = data, type = "comparison", template = "outfielder",
#'                     player_1 = "Nicolo Barella", player_2 = "Ilkay Gundogan",
#'                     season_player_1 = "Last 365 Days", 
#'                     season_player_2 = "Last 365 Days",
#'                     color_compare = "lightgreen", theme = "black")
#' plot1
#'
#' plot2 <- plot_pizza(data = data1, type = "single", template = "outfielder",
#'                     color_possession = "green", color_attack = "lightblue", 
#'                     season = "Last 365 Days",
#'                     color_defense = "red", theme = "dark")
#' plot2
#' }

plot_pizza <- function(data, type = "", template, 
                       color_possession = "#41ab5d", color_attack = "#2171b5", 
                       color_defense = "#fec44f", 
                       player_1, player_2, 
                       color_compare = "#41ab5d", 
                       season = "Last 365 Days Men's Big 5 Leagues, UCL, UEL", 
                       season_player_1 = "Last 365 Days Men's Big 5 Leagues, UCL, UEL", 
                       season_player_2 = "Last 365 Days Men's Big 5 Leagues, UCL, UEL", 
                       theme = "") {
  
  if (theme == "dark" || theme == "") {
    fill_b <- "#0d1117"
    color_b <- "#0d1117"
    colorText <- "white"
    gridline <- "565656"
    colorLine <- "white"
  } else if (theme == "black") {
    fill_b <- "black"
    color_b <- "black"
    colorText <- "white"
    gridline <- "565656"
    colorLine <- "white"
  } else if (theme == "white") {
    fill_b <- "white"
    color_b <- "white"
    colorText <- "black"
    gridline <- "565656"
    colorLine <- "black"
  }
  
  if (type == "single" || type == "") { ## SINGLE PLOT ----
    
    data <-
      data %>%
      as_tibble() %>%
      filter(scouting_period == season) %>%
      select(-any_of("StatGroup")) %>%
      distinct()
    
    if (template == "outfielder") {
      
      data_labelled <-
        data %>%
        mutate(stat = case_when(
          Statistic %in% c(
            "Non-Penalty Goals",
            "Non-Penalty xG",
            "Shots Total",
            "Assists",
            "xAG",
            "npxG + xAG" 
          ) ~ "Attacking",
          Statistic %in% c(
            "Shot-Creating Actions",
            "Passes Attempted",
            "Pass Completion %",
            "Progressive Passes",
            "Carries",
            "Touches (Att Pen)",
            "Progressive Passes Rec"
          ) ~ "Possession",
          Statistic %in% c(
            "Tackles",
            "Interceptions",
            "Blocks",
            "Clearances",
            "Aerials won"
          ) ~ "Defending",
          .default=NA
        ))
      
      data_selected <-
        data_labelled %>%
        filter(stat %in% c("Attacking","Possession","Defending"))
    }
    else if (template == "goalkeeper") {
      
      data_labelled <-
        data %>%
        mutate(stat = case_when(
          Statistic %in% c(
            "Goals Against",
            "PSxG/SoT",
            "Save Percentage",
            "PSxG-GA",
            "Crosses Stopped %"
          ) ~ "Defending",
          Statistic %in% c(
            "Goal Kicks",
            "Launch% (Goal kicks)",
            "Avg. Length of Goal Kicks",
            "Avg. Distance of Def. Actions"
          ) ~ "Possession",
          Statistic %in% c(
            "Def. Actions Outside Pen. Area"
          ) ~ "Attacking",
          .default=NA
        ))
      
      data_selected <-
        data_labelled %>%
        filter(stat %in% c("Attacking","Possession","Defending"))
    }
    else if (template == "custom") {
      data_selected <- data
    }
    else {
      data_selected <- data
    }
    
    player_name <- unique(data$Player)
    title <- paste(player_name, "Percentile Chart")
    min <- unique(data$BasedOnMinutes)
    sub <- unique(data$Versus)
    sub1 <- unique(data$scouting_period)
    # subtitle <- paste("Compared to", sub, "|", sub1, "|", min, "minutes played")
    subtitle <- paste("Compared to", sub, "|", min, "minutes played","\n",sub1)
    caption <- "Plot code by @RobinWilhelmus\nData from Stats Perform via FBref. Inspired by @NathanAClark. Created using ggshakeR."
    
    x <- c(data_selected$Statistic, data_selected$stat)
    
    data_selected <- data_selected %>%
      arrange(desc(stat), desc(Percentile)) %>%
      mutate(Statistic = factor(Statistic, levels = Statistic))
    
    ggplot(data_selected, aes(Statistic, Percentile)) +
      geom_bar(aes(y = 100, fill = stat), stat = "identity", width = 1, color = fill_b,
               alpha = 0.1, show.legend = FALSE) +
      geom_bar(stat = "identity", width = 1, aes(fill = stat), color = fill_b, alpha = 1) +
      coord_polar(clip = "off") +
      geom_hline(yintercept = 25, color = colorLine, linetype = "dashed", alpha = 0.8) +
      geom_hline(yintercept = 50, color = colorLine, linetype = "dashed", alpha = 0.8) +
      geom_hline(yintercept = 75, color = colorLine, linetype = "dashed", alpha = 0.8) +
      scale_fill_manual(values = c("Possession" = color_possession,
                                   "Attacking" = color_attack,
                                   "Defending" = color_defense)) +
      geom_label(aes(y = 90, label = Per90, fill = stat), size = 3, color = fill_b, show.legend = FALSE) +
      scale_y_continuous(limits = c(-20, 100)) +
      labs(fill = "",
           caption = caption,
           title = title,
           subtitle = subtitle) +
      theme_minimal() +
      theme(plot.background = element_rect(fill = fill_b, color = color_b),
            panel.background = element_rect(fill = fill_b, color = color_b),
            legend.position = "bottom",
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_text(size = 12, color = colorText),
            text = element_text(color = colorText, size = 20),
            plot.title = element_text(hjust = 0.5, size = 26, color = colorText, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, size = 20, color = colorText),
            plot.caption = element_text(hjust = 0.5, size = 15, color = colorText),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      scale_x_discrete(labels = function(x) text_wrap(x = x))
    
  } else if (type == "comparison") { ## COMPARISON PLOT ----
    
    data <- as_tibble(data)
    data$Player <- stri_trans_general(str = data$Player, id = "Latin-ASCII")
    
    if (template == "outfielder") {
      
      data_labelled <-
        data %>%
        mutate(stat = case_when(
          Statistic %in% c(
            "Non-Penalty Goals",
            "Non-Penalty xG",
            "Shots Total",
            "Assists",
            "xAG",
            "npxG + xAG" 
          ) ~ "Attacking",
          Statistic %in% c(
            "Shot-Creating Actions",
            "Passes Attempted",
            "Pass Completion %",
            "Progressive Passes",
            "Carries",
            "Touches (Att Pen)",
            "Progressive Passes Rec"
          ) ~ "Possession",
          Statistic %in% c(
            "Tackles",
            "Interceptions",
            "Blocks",
            "Clearances",
            "Aerials won"
          ) ~ "Defending",
          .default=NA
        ))
      
      data_selected <-
        data_labelled %>%
        filter(stat %in% c("Attacking","Possession","Defending"))
      
      data1 <- data_selected %>%
        filter(Player == player_1) %>%
        filter(scouting_period == season_player_1)
      
      data2 <- data_selected %>%
        filter(Player == player_2) %>%
        filter(scouting_period == season_player_2)
      
    }
    else if (template == "goalkeeper") {
      
      data_labelled <-
        data %>%
        mutate(stat = case_when(
          Statistic %in% c(
            "Goals Against",
            "PSxG/SoT",
            "Save Percentage",
            "PSxG-GA",
            "Crosses Stopped %"
          ) ~ "Defending",
          Statistic %in% c(
            "Goal Kicks",
            "Launch% (Goal kicks)",
            "Avg. Length of Goal Kicks",
            "Avg. Distance of Def. Actions"
          ) ~ "Possession",
          Statistic %in% c(
            "Def. Actions Outside Pen. Area"
          ) ~ "Attacking",
          .default=NA
        ))
      
      data_selected <-
        data_labelled %>%
        filter(stat %in% c("Attacking","Possession","Defending"))
      
      data1 <- data_selected %>%
        filter(Player == player_1) %>%
        filter(scouting_period == season_player_1)
      
      data2 <- data_selected %>%
        filter(Player == player_2) %>%
        filter(scouting_period == season_player_2)
      
    }
    else if (template == "custom") {
      data1
      data2
    }
    
    data1 <-
      data1 %>%
      as_tibble() %>%
      select(-any_of("StatGroup")) %>%
      distinct()
    
    data2 <-
      data2 %>%
      as_tibble() %>%
      select(-any_of("StatGroup")) %>%
      distinct()
    
    player_name1 <- unique(data1$Player)
    player_name2 <- unique(data2$Player)
    min1 <- unique(data1$BasedOnMinutes)
    min2 <- unique(data2$BasedOnMinutes)
    sub <- unique(data1$Versus)
    lg1 <- unique(data1$scouting_period)
    lg2 <- unique(data2$scouting_period)
    title <- paste(player_name1, "|", min1, "minutes", "\n", lg1)
    subtitle <- paste(player_name2, "|", min2, "minutes", "\n", lg2)
    caption <- paste("Compared to", sub, ".\nData from Stats Perform via FBref. Inspired by @FootballSlices. Created using ggshakeR.")
    x <- data1$Statistic
    
    data1 <- data1 %>%
      arrange(desc(stat), desc(Percentile)) %>%
      mutate(Statistic = factor(Statistic, levels = Statistic))
    
    data2 <- data2 %>%
      arrange(desc(stat), desc(Percentile)) %>%
      mutate(Statistic = factor(Statistic, levels = Statistic))
    
    ggplot(data1, aes(x = Statistic, y = Percentile)) +
      geom_bar(aes(y = 100), fill = fill_b, stat = "identity", width = 1, color = gridline,
               alpha = 0.5, show.legend = FALSE) +
      geom_bar(data = data1, aes(y = Percentile, fill = color_compare), color = color_compare, stat = "identity", width = 1, alpha = 1) +
      scale_fill_manual(values = color_compare) +
      geom_bar(data = data2, aes(y = Percentile, fill = NA), stat = "identity", width = 1, alpha = 0, color = colorLine, size = 3) +
      coord_polar(clip = "off") +
      geom_hline(yintercept = 25, color = colorLine, linetype = "dashed", alpha = 0.7) +
      geom_hline(yintercept = 50, color = colorLine, linetype = "dashed", alpha = 0.7) +
      geom_hline(yintercept = 75, color = colorLine, linetype = "dashed", alpha = 0.7) +
      scale_y_continuous(limits = c(-20, 100)) +
      labs(caption = caption,
           title = title,
           subtitle = subtitle) +
      theme_minimal() +
      theme(plot.background = element_rect(fill = fill_b, color = color_b),
            panel.background = element_rect(fill = fill_b, color = color_b),
            legend.position = "none",
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_text(size = 12, color = colorText),
            text = element_text(color = colorText, size = 20),
            plot.title = element_text(hjust = 0.5, size = 26, color = color_compare, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, size = 26, color = colorLine, face = "bold"),
            plot.caption = element_text(hjust = 0.5, size = 15, color = colorText),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      scale_x_discrete(labels = function(x) text_wrap(x = x))
  }
}