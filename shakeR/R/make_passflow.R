#' @export
make_passflow <- function(pass_data, type="", bin_size=0){
  bin <- bin_size
  
  if(type == "StatsBomb"){
    x_bin <- 120/bin
    y_bin <- 80/bin
    
    passfx <- seq(0,120,by=bin)
    passfy = seq(0,80,by=bin)
    
    pass_data = pass_data %>% rename(location.x = "x") %>%
      rename(location.y = "y") %>%
      rename(pass.end_location.x = "endX") %>%
      rename(pass.end_location.y = "endY")
      
  }
  else if(type == "Opta"){
    x_bin <- 100/bin
    y_bin <- 100/bin
    
    passfx <- seq(0,100,by=bin)
    passfy = seq(0,100,by=bin)
  }
  
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
  
  PassFlow %>%
    ggplot()+
    annotate_pitch(dimensions = pitch_statsbomb, colour = "white",
                   fill = "#141622")+
    theme_pitch()+
    geom_bin2d(data=pass_data,aes(x=x,y=y),alpha=0.6,
               binwidth = c(bin, bin), position = "identity")+
    scale_fill_viridis()+
    geom_segment(aes(x=x,y=y,xend=endX,yend=endY,alpha=countP),
                 color="white",lineend = "round", size=2, arrow = arrow(length = unit(0.08, "inches")))+
    scale_y_reverse()
}