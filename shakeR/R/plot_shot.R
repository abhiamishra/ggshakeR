#' Function for plotting shots
#' 
#' This function allows for data, that has to be scraped from Understat, to be used
#' for plotting shots.
#'
#' @param shotdata Dataframe that houses shot data. Dataframe must contain atleast the following columns: X,Y,xG,result,name
#' @param type Type of showcasing the shotmap: hexbin, density, point (default)
#' @param bin_size Bin size for creating bins. Use this when using hexbin shotmap. Default = 30.
#' @param theme Theme preferences for display: dark (default), white, rose, almond
#' @return a ggplot2 object
#' 
#' @importFrom magrittr %>%
#' @import dplyr
#' @import ggplot2
#' @import ggsoccer
#' @import StatsBombR
#' 
#' @export
#' 
#' @example plot = plot_shot(shotdata, type+"hexbin", bin_size=20)
plot_shot <- function(shotdata, type="point", bin_size=30, theme=""){
  
  if(nrow(shotdata)>0 && 
     sum(c("X","Y","xG","result","name") %in% names(shotdata))==5){
    
    last = sub(".* ", "", shotdata$player[nrow(shotdata)])
    first = sub(" .*", "", shotdata$player[nrow(shotdata)])
    player_name = paste(first,last,"Shot Map",sep="\n")
    
    fill_b = ""
    colour_b = ""
    colorLine = ""
    colorText = ""
    if(theme == "dark" || theme == ""){
      fill_b = "#0d1117"
      colour_b = "white"
      
      
      colorLine = "white"
      colorText = "white"
    }
    else if(theme == "white"){
      fill_b = "#F5F5F5"
      colour_b = "black"
      
      colorLine = "black"
      colorText = "black"
    }
    else if(theme == "rose"){
      fill_b = "#FFE4E1"
      colour_b = "#696969"
      
      colorLine = "#322E2E"
      colorText = "#322E2E"
    }
    else if(theme == "almond"){
      fill_b = "#FFEBCD"
      colour_b = "#696969"
      
      colorLine = "#322E2E"
      colorText = "#322E2E"
    }
    
    plot = shotdata %>% 
      ggplot() + 
      annotate_pitch(dimensions = pitch_statsbomb,colour=colour_b,
                     fill = fill_b)+
      theme_pitch()+
      theme(panel.background = element_rect(fill = fill_b))
    
    
    if(nrow(shotdata)>=1){
      shotdata = shotdata %>%
        mutate(X = 120*X) %>%
        mutate(Y = 80*Y) %>%
        na.omit()
      
      total_xG = sum(shotdata$xG)
      total_goal = sum(shotdata$result == "Goal")
      xg_sot = total_xG/nrow(shotdata)
      
      if(type == ""){
        plot = plot + 
          geom_point(data=shotdata, aes(x=X,y=(80-Y),size=xG,color=result),alpha=0.7)+
          scale_size_continuous(range = c(0.5,7))+
          geom_vline(xintercept = mean(shotdata$X), color=colorLine, linetype=2, size=1.5)+
          geom_hline(yintercept = mean(shotdata$Y), color=colorLine, linetype=2, size=1.5)+
          geom_point(x=86,y=10,size=40, color=colorText, shape=1)+
          geom_text(x=86,y=10,label=format(round(total_xG,2)),color=colorText,size=10)+
          geom_text(x=80,y=10,label="xG",color=colorText,size=10)+
          geom_point(x=86,y=70,size=40, color=colorText, shape=1)+
          geom_text(x=86,y=70,label=format(round(total_goal,2)),color=colorText,size=10)+
          geom_text(x=80,y=70,label="Goals",color=colorText,size=10)+
          geom_point(x=86,y=40,size=40, color=colorText, shape=1)+
          geom_text(x=86,y=40,label=format(round(xg_sot,2)),color=colorText,size=10)+
          geom_text(x=80,y=40,label="xG/Shot",color=colorText,size=10)+
          coord_flip(xlim = c(80, 120),
                     ylim = c(0, 80))+
          labs(
            color = "Result of Shot",
            size = "xG of Shot"
          )
      }
      else if(type == "density"){
        plot = plot + 
          stat_density_2d(data=shotdata, aes(x=X,y=(80-Y),fill = ..level..), geom = "polygon",
                          alpha=0.7)+
          scale_fill_gradient(high="#6BFF84",low="#01141D")+
          scale_size_continuous(range = c(0.5,7))+
          geom_vline(xintercept = mean(shotdata$X), color=colorLine, linetype=2, size=1.5)+
          geom_hline(yintercept = mean(shotdata$Y), color=colorLine, linetype=2, size=1.5)+
          geom_point(x=86,y=10,size=40, color=colorText, shape=1)+
          geom_text(x=86,y=10,label=format(round(total_xG,2)),color=colorText,size=10)+
          geom_text(x=80,y=10,label="xG",color=colorText,size=10)+
          geom_point(x=86,y=70,size=40, color=colorText, shape=1)+
          geom_text(x=86,y=70,label=format(round(total_goal,2)),color=colorText,size=10)+
          geom_text(x=80,y=70,label="Goals",color=colorText,size=10)+
          geom_point(x=86,y=40,size=40, color=colorText, shape=1)+
          geom_text(x=86,y=40,label=format(round(xg_sot,2)),color=colorText,size=10)+
          geom_text(x=80,y=40,label="xG/Shot",color=colorText,size=10)+
          coord_flip(xlim = c(80, 120),
                     ylim = c(0, 80))+
          theme(legend.position = "none")
        
      }
      else if(type == "hexbin"){
        plot = plot + 
          geom_hex(data=shotdata, aes(x=X,y=(80-Y)), bins=bin_size)+
          scale_fill_continuous(type = "viridis")+
          scale_size_continuous(range = c(0.5,7))+
          geom_vline(xintercept = mean(shotdata$X), color=colorLine, linetype=2, size=1.5)+
          geom_hline(yintercept = mean(shotdata$Y), color=colorLine, linetype=2, size=1.5)+
          geom_point(x=86,y=10,size=40, color=colorText, shape=1)+
          geom_text(x=86,y=10,label=format(round(total_xG,2)),color=colorText,size=10)+
          geom_text(x=80,y=10,label="xG",color=colorText,size=10)+
          geom_point(x=86,y=70,size=40, color=colorText, shape=1)+
          geom_text(x=86,y=70,label=format(round(total_goal,2)),color=colorText,size=10)+
          geom_text(x=80,y=70,label="Goals",color=colorText,size=10)+
          geom_point(x=86,y=40,size=40, color=colorText, shape=1)+
          geom_text(x=86,y=40,label=format(round(xg_sot,2)),color=colorText,size=10)+
          geom_text(x=80,y=40,label="xG/Shot",color=colorText,size=10)+
          coord_flip(xlim = c(80, 120),
                     ylim = c(0, 80))+
          labs(
            fill = "Count of Shots"
          )
      }
      
      plot = plot +
        geom_text(x=110,y=10,label=player_name,color=colorText,size=6)
      
      plot
    }
    else{
      plot
    }
  }
}