#' Plotting passes
#'
#' This function allows you to plot various types of plots that have
#' that have passes as some sort of input. Returns a ggplot object. 
#' @param pass_data The dataframe that stores your passing data
#' @param standard TRUE if only plotting successful/unsuccessful passes
#' @param background Pick between white or dark background.
#' @param prog indicates whether to map out progressive passes
#' @example
#' plot = plot_pass(passingdata, standard="st", background = "dark")


#' @export
plot_pass <- function(pass_data, standard="", prog=FALSE, cross=FALSE, shot=FALSE, switch=FALSE, 
                      to_zone = "" , from_zone= "", distance= "", outcome="all", team="", player="", background=""){
  if(team != "" && player != ""){
    pass_data = pass_data %>%
      filter(team.name == team) %>%
      filter(player.name == player)
  }
  else if(team != "" && player == ""){
    pass_data = pass_data %>%
      filter(team.name == team)
  }
  else if(player != ""  && team == ""){
    pass_data = pass_data %>%
      filter(player.name == player)
  }
  
  if(outcome == "suc"){
    pass_data = pass_data %>%
      filter(is.na(pass.outcome.name))
  }
  else if(outcome == "unsuc"){
    pass_data = pass_data %>%
      filter(!is.na(pass.outcome.name))
  }
  
  if(standard=="st"){
    pass_data$pass.outcome.name = replace_na(pass_data$pass.outcome.name, "Successful")
    pass_data = pass_data %>% mutate(colorOutcome = ifelse(pass.outcome.name == "Successful",
                                                           "Successful",
                                                         "Unsuccessful"))
  }
  
  
  if(prog == TRUE){
    pass_data = pass_data %>%
      mutate(start= sqrt((100-location.x)^2 + (50-location.y)^2)) %>%
      mutate(end = sqrt((100-pass.end_location.x)^2 + (50-pass.end_location.y)^2)) %>%
      mutate(isProg = ifelse(end <= 0.75*start,
                             1,
                             0))
    
    pass_data = pass_data %>% filter(isProg == 1)
  }
  
  if(cross == TRUE){
    pass_data = pass_data %>%
      filter(pass.cross == TRUE)
  }
  
  if(shot == TRUE){
    pass_data = pass_data %>%
      filter(pass.shot_assist == TRUE)
  }
  
  if(switch==TRUE){
    pass_data = pass_data %>% mutate(delta_y = abs(
      pass.end_location.y - location.y
    )) %>%
      filter(delta_y >= 35)
  }
  
  
  if(nrow(pass_data) > 0){
    
    colour_b = "black"
    fill_b = "white"
    if(background == "dark"){
      colour_b = "white"
      fill_b = "#141622"
    }
    
    
    plot = ggplot(data=pass_data) + 
      annotate_pitch(dimensions = pitch_statsbomb,colour=colour_b,
                     fill = fill_b)+
      theme_pitch()+
      theme(panel.background = element_rect(fill = fill_b))
    
    if(standard=="st"){
      plot = plot +
        geom_segment(aes(x=location.x,y=location.y, 
                                         xend=pass.end_location.x, yend=(pass.end_location.y), color=colorOutcome),
                     lineend = "round", size = 1.5, arrow=arrow(length=unit(0.10, "inches")),stat="identity",position="identity")+
        facet_grid(~colorOutcome) +
        labs(
          color = "Outcome of Pass"
        )
    }
  }
  
  plot
}