#' Plotting passes
#'
#' This function allows you to plot various types of plots that have
#' that have passes as some sort of input. Returns a ggplot object. 
#' @param pass_data The dataframe that stores your passing data
#' @param standard TRUE if only plotting successful/unsuccessful passes
#' @param background Pick between white or dark background.
#' @param prog indicates whether to have progressive passes
#' @example
#' plot = plot_pass(passingdata, standard="st", background = "dark")


#' @export
plot_pass <- function(pass_data, standard="", prog = "", team="", player="", background=""){
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
  
  if(standard=="st"){
    pass_data$pass.outcome.name = replace_na(pass_data$pass.outcome.name, "Successful")
    pass_data = pass_data %>% mutate(colorOutcome = ifelse(pass.outcome.name == "Successful",
                                                           "S",
                                                         "US"))
  }
  if(prog == "prog"){
    pass_data = pass_data %>%
      mutate(start= sqrt((100-location.x)^2 + (50-location.y)^2)) %>%
      mutate(end = sqrt((100-pass.end_location.x)^2 + (50-pass.end_location.y)^2)) %>%
      mutate(isProg = ifelse(end <= 0.75*start,
                             1,
                             0)) %>%
               filter(isProg == 1)
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
    
    plot+
      layer(geom=geom_segment(aes(x=location.x,y=location.y, 
                                                  xend=pass.end_location.x, yend=(pass.end_location.y), color=colorOutcome),
                              lineend = "round", size = 1.5, arrow=arrow(length=unit(0.10, "inches"))),stat = "identity",position=1)+
      facet_grid(~colorOutcome) +
      labs(
        color = "Outcome of Pass"
      )
  }
  
  plot
}
