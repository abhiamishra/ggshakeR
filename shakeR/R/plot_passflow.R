#' Passflow plot function
#' 
#' This function takes in a dataframe and binsizes
#' to make a passflow map. Compatible, for right now, with StatsBomb data only. Returns a ggplot object
#' 
#' @param pass_data Dataframe that must house pass data only and must have atleast 4 columns: starting x,y and ending x,y. 
#' @param bin_size Details the binsize the passflow needs to bin to. Default is 20.
#' @return returns a ggplot2 object
#' 
#' @importFrom magrittr %>%
#' @import dplyr
#' @import ggplot2
#' @import ggsoccer
#' @import StatsBombR
#' @import viridis
#' @export
#' @examples plot = plot_passflow(data, bin_size=30)
plot_passflow <- function(pass_data, bin_size=0){
  
    fill_b = "#0d1117"
    colour_b = "white"
    bin_alpha = 0.6

  if(bin_size == 0){
    bin = 20
  }
  else{
    bin <- bin_size
  }
  
    x_bin <- 120/bin
    y_bin <- 80/bin
    
    passfx <- seq(0,120,by=bin)
    passfy = seq(0,80,by=bin)
    
  if(nrow(pass_data)>0 &&
     sum(x = c("location.x", "location.y", "pass.end_location.x", "pass.end_location.y") %in% names(pass_data))==4){
    pass_data = pass_data %>% rename("x" = "location.x") %>%
      rename("y" = "location.y") %>%
      rename("endX" = "pass.end_location.x") %>%
      rename("endY" = "pass.end_location.y")
    
    
    PassFlow <- data.frame("x"=0.0,"y"=0.0,"endX"=0.0,"endY"=0.0, countPasses=0.0)
    
    for(i in 1:x_bin){
      
      filterx <- pass_data %>% filter(x>=passfx[i]) %>%
        filter(x<passfx[i+1])
      
      for(j in 1:y_bin){
        
        minY = passfy[j]
        maxY = passfy[j+1]
        
        filtery <- filterx %>% filter(y>=minY) %>%
          filter(y<maxY)
        
        if(nrow(filtery)>=1){
          
          me_x = mean(filtery$x)
          me_y = mean(filtery$y)
          me_ex = mean(filtery$endX)
          me_ey = mean(filtery$endY)
          
          count = nrow(filtery)
          
          x <- c(me_x,me_y,me_ex,me_ey,count)
          PassFlow <- rbind(PassFlow, x)
          
        }
        
      }
      
    }
    
    PassFlow <- PassFlow[2:nrow(PassFlow), ]
    
    plot = PassFlow %>%
      ggplot()+
      annotate_pitch(dimensions = pitch_statsbomb, colour = colour_b,
                     fill = fill_b)+
      theme_pitch()
    
    if(nrow(PassFlow)>0){
      plot = plot +
        geom_bin2d(data=pass_data,aes(x=x,y=y),alpha=bin_alpha,
                   binwidth = c(bin, bin), position = "identity")+
        scale_fill_viridis()+
        geom_segment(aes(x=x,y=y,xend=endX,yend=endY,alpha=countPasses),
                     color="white",lineend = "round", size=2, arrow = arrow(length = unit(0.08, "inches")))+
        labs(
          fill = "Count of Passes Started",
          alpha = "Number of Passes Made"
        ) 
    }
    plot  
  }
}