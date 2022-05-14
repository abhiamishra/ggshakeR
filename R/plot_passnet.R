#' Function for plotting pass networks
#'
#' This function allows for data, that can be from Opta or Statsbomb, to be used
#' for plotting pass networks.
#'
#' @param data Data frame that houses pass data
#' @param data_type Type of data that is being put in: opta or statsbomb. Default set to "statsbomb"
#' @param team_name The name of the team of which you want a pass network
#' @param color_scale Color of higher end of xT color scale. Default set to "#E74C3C"
#' @param subtitle_plot Subtitle of the pass network plot
#' @param theme The background theme -> "light" or "dark"
#' @return a ggplot2 object
#'
#' @import dplyr
#' @import tidyr
#' @import ggsoccer
#' @importFrom gridExtra tableGrob ttheme_minimal
#' @import useful
#'
#' @export
#'
#' @examples
#' \dontrun{
#' plot <- plot_passnet(data, data_type = "opta", team_name = "Arsenal")
#' plot
#' }

plot_passnet <- function(data, data_type = "statsbomb", team_name, 
                         color_scale = "#E74C3C", subtitle_plot = "", theme = "dark") {
  
  if (theme == "dark") {
    fill_b <- "#151515"
    colour_b <- "#151515"
    colorText <- "white"
    colorLine <- "white"
    pitch_line <- "#454545"
  } 
  else if (theme == "light") {
    fill_b <- "#ECF0F1"
    colour_b <- "#ECF0F1"
    colorText <- "black"
    colorLine <- "black"
    pitch_line <- "#95A5A6"
  }
  
  if (data_type == "statsbomb") {
    
    # Checking dataframe
    
    if ((nrow(data) > 0) &&
       sum(c("x", "y", "finalX", "finalY", "team.name", "player.name", "pass.recipient.name", "type.name", "minute", "pass.outcome.name") %in% names(data)) == 10 &&
       (data_type == "statsbomb")) {
    } else {
      print(c("x", "y", "finalX", "finalY", "team.name", "player.name", "pass.recipient.name", "type.name", "minute", "pass.outcome.name"))
      stop("The dataset has insufficient columns and/or insufficient data.")
    }

    df <- data %>%
      ggshakeR::calculate_threat(dataType = "statsbomb")
    
    df <- df %>%
      mutate(xT = xTEnd - xTStart) %>%
      select(xT)
    df[is.na(df)] <- 0
    
    data$xT <- df$xT
    
    # Data Wrangling
    
    data1 <- data %>%
      filter(team.name == team_name)
    
    # Filter to before first substitution
    
    min_events <- data1 %>%
      filter(type.name == "Substitution")
    
    if (nrow(min_events) == 0) {
      min <- 45
    } else {
      min <- min(min_events$minute)
    }
    
    data1 <- data1 %>%
      filter(minute < min)
    
    # Player Average Locations
    
    nodes <- data1 %>% 
      filter(type.name %in% c("Pass", "Ball Receipt*", "Ball Recovery", "Shot", "Dispossessed", "Interception", "Clearance", "Dribble", "Shot", "Goal Keeper", "Miscontrol", "Error")) %>% 
      group_by(player.name) %>% 
      summarise(x = mean(x, na.rm = T), y = mean(y, na.rm = T), events = n(), xT = sum(xT)) %>% 
      na.omit()
    
    # Edges
    
    edgelist <- data1 %>% 
      mutate(pass.outcome.name = fct_explicit_na(pass.outcome.name, "Complete")) %>%
      filter(type.name == "Pass" & pass.outcome.name == "Complete") %>% 
      select(from = player.name, to = pass.recipient.name) %>% 
      group_by(from, to) %>% 
      summarise(n = n()) %>% 
      na.omit()
    
    edges <- left_join(edgelist, 
                       nodes %>% select(player.name, x, y),
                       by = c("from" = "player.name"))
    
    edges <- left_join(edges, 
                       nodes %>% select(player.name, xend = x, yend = y),
                       by = c("to" = "player.name"))
    
    edges <- edges %>% 
      group_by(player1 = pmin(from, to), player2 = pmax(from, to)) %>% 
      summarise(n = sum(n), x = x[1], y = y[1], xend = xend[1], yend = yend[1])
    
    # Minimum Number of Connections
    
    edges <- edges %>% 
      filter(n >= 4)
    
    # Creating Line-up Table
    
    nodes <- nodes %>%
      arrange(events)
    
    nodes$id <- 1:nrow(nodes) 
    nodes$player.name <- sub(".* ", "", nodes$player.name)
    
    table <- nodes %>%
      select(id, player.name) %>%
      rename(Player = player.name) %>%
      rename("#" = id)
    
    # Plot 
    
    if (subtitle_plot == "") {
      subtitle_plot <- paste0("Till First Substitution (", min, "')")
    }
    
    plot_passnet <- ggplot() +
      annotate_pitch(dimensions = pitch_statsbomb, fill = fill_b, colour = pitch_line) +
      theme_pitch() +
      geom_segment(data = edges, aes(x, y, xend = xend, yend = yend, size = n), colour = colorLine, show.legend = FALSE) +
      scale_size_continuous(range = c(0.5, 3)) +
      geom_point(data = nodes, aes(x, y, fill = xT), size = 9, shape = 21, stroke = 1, colour = colorLine) +
      scale_fill_gradient(low = "black", high = color_scale) +
      geom_text(data = nodes, aes(x, y, label = id), colour = "white") +
      labs(title = paste0(team_name, " Pass Network"),
           subtitle = subtitle_plot,
           y = "Only 4+ Pass Connections.\nSize = Number of connections") +
      theme(legend.position = c(0.843, 1.04),
            legend.direction = "horizontal",
            legend.background = element_rect(fill = fill_b),
            legend.title = element_text(colour = colorText),
            legend.text = element_text(colour = colorText),
            plot.background = element_rect(colour = fill_b, fill = fill_b),
            panel.background = element_rect(colour = fill_b, fill = fill_b),
            plot.title = element_text(colour = colorText, hjust = 0, size = 25, face = "bold"),
            axis.title.x = element_text(colour = colorText, size = 8),
            plot.subtitle = element_text(colour = colorText, hjust = 0, size = 14)) +
      coord_flip(ylim = c(0, 120),
                 xlim = c(0, 120)) +
      annotation_custom(tableGrob(table, theme = ttheme_minimal(base_colour = colorText), rows = NULL), xmin = 0, xmax = 120, ymin = 90, ymax = 120) +
      direction_label(colour = colorLine, x_label = 107, y_label = 10)
  }
  
  else if (data_type == "opta") {
    
    # Checking data frame 
    
    if ((nrow(data) > 0) &&
       sum(c("x", "y", "finalX", "finalY", "teamId", "playerId", "type", "minute", "outcome") %in% names(data)) == 9 &&
       (data_type == "opta")) {
    } else {
      print(c("x", "y", "finalX", "finalY", "teamId", "playerId", "type", "minute", "outcome"))
      stop("The dataset has insufficient columns and/or insufficient data.")
    }
    
    # Generate error messages
    
    if (is.character(data$playerId) == FALSE) {
      stop("The playerId column is supposed to contain player names.")
    } else {}
    
    if (is.character(data$teamId) == FALSE) {
      stop("The teamId column is supposed to contain team names.")
    } else {}
    
    # Calculate xT 
    
    df <- data %>%
      ggshakeR::calculate_threat(dataType = "opta")
    
    df <- df %>%
      mutate(xT = xTEnd - xTStart) %>%
      select(xT)
    df[is.na(df)] <- 0
    
    data$xT <- df$xT
    
    # Data Wrangling 
    
    data1 <- data %>%
      mutate(x = x * 1.2) %>%
      mutate(y = y * 0.8) %>%
      mutate(finalX = finalX * 1.2) %>%
      mutate(finalY = finalY * 0.8) %>%
      filter(teamId == team_name)
    
    data1 <- data1[complete.cases(data1[ , "playerId"]), ]
    data1 <- shift.column(data = data1, columns = "playerId", newNames = "receiver", len = 1, up = TRUE)
    
    # Filter to before first substitution
    
    min_events <- data1 %>% 
      filter(type == "SubstitutionOff") %>%
      arrange(minute)
    
    if (nrow(min_events) == 0) {
      min <- 45
    } else {
      min <- min(min_events$minute)
    }
    
    data1 <- data1 %>%
      filter(minute < min)
    
    # Player Average Locations
    
    nodes <- data1 %>% 
      group_by(playerId) %>% 
      summarise(x = mean(x, na.rm = T), y = mean(y, na.rm = T), events = n(), xT = sum(xT)) %>% 
      na.omit()
    
    # Edges
    
    edgelist <- data1 %>% 
      filter(type == "Pass" & outcome == "Successful") %>% 
      select(from = playerId, to = receiver) %>% 
      group_by(from, to) %>% 
      summarise(n = n()) %>% 
      na.omit()
    
    edges <- left_join(edgelist, 
                       nodes %>% select(playerId, x, y),
                       by = c("from" = "playerId"))
    
    edges <- left_join(edges, 
                       nodes %>% select(playerId, xend = x, yend = y),
                       by = c("to" = "playerId"))
    
    edges <- edges %>% 
      group_by(player1 = pmin(from, to), player2 = pmax(from, to)) %>% 
      summarise(n = sum(n), x = x[1], y = y[1], xend = xend[1], yend = yend[1])
    
    # Minimum Number of Connections
    
    edges <- edges %>% 
      filter(n >= 4)
    
    # Create Line-up Table
    
    nodes <- nodes %>%
      arrange(events)
    
    nodes$id <- 1:nrow(nodes) 
    nodes$playerId <- sub(".* ", "", nodes$playerId)
    
    table <- nodes %>%
      select(id, playerId) %>%
      rename(Player = playerId) %>%
      rename("#" = id)
    
    # Plot 
    
    if (subtitle_plot == "") {
      subtitle_plot <- paste0("Till First Substitution (", min, "')")
    }
    
    plot_passnet <- ggplot() +
      annotate_pitch(dimensions = pitch_statsbomb, fill = fill_b, colour = pitch_line) +
      theme_pitch() +
      geom_segment(data = edges, aes(x, y, xend = xend, yend = yend, size = n), colour = colorLine, show.legend = FALSE) +
      scale_size_continuous(range = c(0.5, 3)) +
      geom_point(data = nodes, aes(x, y, fill = xT), size = 9, shape = 21, stroke = 1, colour = colorLine) +
      scale_fill_gradient(low = "black", high = color_scale) +
      geom_text(data = nodes, aes(x, y, label = id), colour = "white") +
      labs(title = paste0(team_name, " Pass Network"),
           subtitle = subtitle_plot,
           y = "Only 4+ Pass Connections.\nSize = Number of connections") +
      theme(legend.position = c(0.843, 1.04),
            legend.direction = "horizontal",
            legend.background = element_rect(fill = fill_b),
            legend.title = element_text(colour = colorText),
            legend.text = element_text(colour = colorText),
            plot.background = element_rect(colour = fill_b, fill = fill_b),
            panel.background = element_rect(colour = fill_b, fill = fill_b),
            plot.title = element_text(colour = colorText, hjust = 0, size = 25, face = "bold"),
            axis.title.x = element_text(colour = colorText, size = 8),
            plot.subtitle = element_text(colour = colorText, hjust = 0, size = 14)) +
      coord_flip(ylim = c(120, 0),
                 xlim = c(0, 120)) +
      annotation_custom(tableGrob(table, theme = ttheme_minimal(base_colour = colorText), rows = NULL), xmin = 0, xmax = 120, ymin = 120, ymax = 90) +
      direction_label(colour = colorLine, x_label = 107, y_label = 10)
  }
  return(plot_passnet)
}