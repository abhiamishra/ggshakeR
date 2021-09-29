plot_shot <- function(shotdata, type="", bin_size=30){
  plot = shotdata %>%
    ggplot()+
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
        geom_vline(xintercept = mean(shotdata$X), color="white", linetype=2, size=1.5)+
        geom_hline(yintercept = mean(shotdata$Y), color="white", linetype=2, size=1.5)+
        geom_point(x=86,y=10,size=40, color="white", shape=1)+
        geom_text(x=86,y=10,label=format(round(total_xG,2)),color="white",size=10)+
        geom_text(x=80,y=10,label="xG",color="white",size=10)+
        geom_point(x=86,y=70,size=40, color="white", shape=1)+
        geom_text(x=86,y=70,label=format(round(total_goal,2)),color="white",size=10)+
        geom_text(x=80,y=70,label="Goals",color="white",size=10)+
        geom_point(x=86,y=40,size=40, color="white", shape=1)+
        geom_text(x=86,y=40,label=format(round(xg_sot,2)),color="white",size=10)+
        geom_text(x=80,y=40,label="xG/Shot",color="white",size=10)+
        coord_flip(xlim = c(80, 120),
                   ylim = c(0, 80))
    }
    else if(type == "density"){
      plot = plot + 
        stat_density_2d(data=shotdata, aes(x=X,y=(80-Y),fill = ..level..), geom = "polygon",
                        alpha=0.7)+
        scale_fill_gradient(high="#6BFF84",low="#01141D")+
        geom_vline(xintercept = mean(shotdata$X), color="white", linetype=2, size=1.5)+
        geom_hline(yintercept = mean(shotdata$Y), color="white", linetype=2, size=1.5)+
        geom_point(x=86,y=10,size=40, color="white", shape=1)+
        geom_text(x=86,y=10,label=format(round(total_xG,2)),color="white",size=10)+
        geom_text(x=80,y=10,label="xG",color="white",size=10)+
        geom_point(x=86,y=70,size=40, color="white", shape=1)+
        geom_text(x=86,y=70,label=format(round(total_goal,2)),color="white",size=10)+
        geom_text(x=80,y=70,label="Goals",color="white",size=10)+
        geom_point(x=86,y=40,size=40, color="white", shape=1)+
        geom_text(x=86,y=40,label=format(round(xg_sot,2)),color="white",size=10)+
        geom_text(x=80,y=40,label="xG/Shot",color="white",size=10)+
        coord_flip(xlim = c(80, 120),
                   ylim = c(0, 80))
    }
    else if(type == "hexbin"){
      plot = plot + 
        geom_hex(data=shotdata, aes(x=X,y=(80-Y)), bins=bin_size)+
        scale_fill_continuous(type = "viridis")+
        geom_vline(xintercept = mean(shotdata$X), color="white", linetype=2, size=1.5)+
        geom_hline(yintercept = mean(shotdata$Y), color="white", linetype=2, size=1.5)+
        geom_point(x=86,y=10,size=40, color="white", shape=1)+
        geom_text(x=86,y=10,label=format(round(total_xG,2)),color="white",size=10)+
        geom_text(x=80,y=10,label="xG",color="white",size=10)+
        geom_point(x=86,y=70,size=40, color="white", shape=1)+
        geom_text(x=86,y=70,label=format(round(total_goal,2)),color="white",size=10)+
        geom_text(x=80,y=70,label="Goals",color="white",size=10)+
        geom_point(x=86,y=40,size=40, color="white", shape=1)+
        geom_text(x=86,y=40,label=format(round(xg_sot,2)),color="white",size=10)+
        geom_text(x=80,y=40,label="xG/Shot",color="white",size=10)+
        coord_flip(xlim = c(80, 120),
                   ylim = c(0, 80))
    }
    
    plot
  }
  else{
    plot
  }
}