# #' Plotting
# #'
# #' @param data The dataframe that stores your data. Must contain starting x,y locations and ending x,y locations
# #' @param dataType indicator for what type of data the eventData. Currently, options include "opta" (default) and "statsbomb"
# #' @param x_col name of the column that stores the starting x location
# #' @param y_col name of the column that stores the starting y location
# #' @param xend_col name of the column that stores the ending x location
# #' @param yend_col name of the column that stores the ending x location
# #' @return returns a ggplot2 object
# #'
# #' @importFrom magrittr %>%
# #' @import dplyr
# #' @import ggplot2
# #' @import ggsoccer
# #' @import mclust
# #' @import ggforce
# #' @import shadowtext
# #' @export
# #'
# #' @examples endResult = calculate_threat(test, dataType = "statsbomb", x_col = "location.x",
# #'                       y_col = "location.y", xend_col = "pass.end_location.x",
# #'                      yend_col = "pass.end_location.y")
# library(tidyverse)
# library(dplyr)
# library(ggplot2)
# library(mclust)
# library(ggforce)
# library(shadowtext)
# library(ggrepel)
# library(worldfootballR)
# library(viridis)

# plot_comet <- function(data, start_year="", end_year="",
#                        league=""){

#   # Break out team and opponent tables
#   raw_team <- data %>%
#     filter(Team_or_Opponent=="team")
#   raw_opponent <- data %>%
#     filter(Team_or_Opponent=="opponent")
#   # Join team and opponent tables


#   fbref <- left_join(raw_team,raw_opponent,by=c("Squad","Season_End_Year"))
#   # Just the stuff we care about

#     scatter_x = "npxG_Per"
#     scatter_y = "npxG_Per"

#     scatterXFilter = paste(scatter_x, "x", sep=".")
#     scatterYFilter = paste(scatter_y, "y", sep=".")
#     fbref = fbref %>% select(Squad,Season=Season_End_Year,Comp=Comp.x,xPlot=scatterXFilter,yPlot=scatterYFilter)


# ### Set some variables
# # Pick number of clusters to highlight
# #league = "La Liga"
# n_clusters <- 5

# ### Join data
# startYearFilterX = paste("xPlot", start_year, sep="_")
# startYearFilterY = paste("yPlot", start_year, sep="_")

# endYearFilterX = paste("xPlot", end_year, sep="_")
# endYearFilterY = paste("yPlot", end_year, sep="_")

# endYearDiff = paste("diff", end_year, sep="_")
# startYearDiff = paste("diff", start_year, sep="_")

# pivoted <-
#   fbref %>%
#   filter(Comp==league) %>%
#   select(Squad,Season,Comp,xPlot,yPlot) %>%
#   pivot_wider(names_from=Season, values_from=c(xPlot,yPlot))

# for(i in 1:length(names(pivoted))){
#   print(names(pivoted)[i])
#   if(names(pivoted)[i] == startYearFilterX){
#     names(pivoted)[i] = "startYearFilterX"
#   }

#   if(names(pivoted)[i] == startYearFilterY){
#     names(pivoted)[i] = "startYearFilterY"
#   }

#   if(names(pivoted)[i] == endYearFilterX){
#     names(pivoted)[i] = "endYearFilterX"
#   }

#   if(names(pivoted)[i] == endYearFilterY){
#     names(pivoted)[i] = "endYearFilterY"
#   }
# }

# pivoted = pivoted %>%
#   mutate(endYearDiff=endYearFilterX-endYearFilterY,
#          startYearDiff=startYearFilterX-startYearFilterY,
#          diff_change=endYearDiff-startYearDiff) %>%
#   filter(!is.na(endYearDiff))

# ### Use GMM to define number of clusters selected up top
# clusters <- Mclust(pivoted %>%
#                      select(endYearFilterX,endYearFilterY,endYearDiff),
#                    G=n_clusters)

# ### Add clusters to data and we're ready to plot
# plot_data <- bind_cols(pivoted,as.data.frame(clusters$classification)) %>%
#   mutate(cluster=as.character(paste0("cluster_",`clusters$classification`)))


# plot_data$Squad = as.factor(plot_data$Squad)

# c1 = plot_data %>% filter(`clusters$classification` == 1)
# c2 = plot_data %>% filter(`clusters$classification` == 2)
# c3 = plot_data %>% filter(`clusters$classification` == 3)
# c4 = plot_data %>% filter(`clusters$classification` == 4)
# c5 = plot_data %>% filter(`clusters$classification` == 5)


# ### Draw plot
# plot <- plot_data %>%
#     ggplot(aes(y=endYearFilterY,x=endYearFilterX))+
#   # Colored ellipses around each cluster
#   geom_mark_ellipse(aes(fill=cluster),color=NA)+
#   # Dotted lines indicating league average xG
#   geom_vline(xintercept=mean(plot_data$endYearFilterX),color="#4b4e43",linetype="dotted")+
#   geom_hline(yintercept=mean(plot_data$endYearFilterY),color="#4b4e43",linetype="dotted")+
#   # Comet tail
#   geom_link(
#     aes(y=startYearFilterY,x=startYearFilterX,
#         yend=endYearFilterY, xend=endYearFilterX,
#         alpha=stat(index),size=stat(index)))+
#   # Outline circle in team color
#   geom_point(
#     aes(size=14, color=cluster, fill=cluster), shape=21, colour="black")+
#   # This sets team colors
#   scale_color_identity() +
#   # Define axes
#   scale_y_reverse(breaks=seq(0.5, 3.5, by=0.5))+
#   scale_x_continuous(breaks=seq(0.5, 3.5, by=0.5))+
#   #Labels
#   labs(title=paste0("How are ",league ," clubs compared to last season?"),
#        subtitle="Comet tails lead from each team's location in ", start_year, "to its labeled point in ", end_year, ".",
#        y= "Non-Penalty Expected Goals Against",
#        x= "Non-Penalty Expected Goals For",
#        caption="@abhiamishra | Data: StatsBomb via FBref | Inspired by: @johnspacemuller ")+
#   geom_label_repel(aes(label = Squad), force_pull = 0.3, force=2)+
#   # Theme aesthetics
#   theme(plot.title=element_text(face = "bold", margin=margin(0,0,10,0),size=26),
#         plot.subtitle = element_text(margin=margin(0,0,10,0), size=16),
#         text = element_text(color="#4b4e43", margin=margin(0,0,0,0), size=14),
#         plot.caption = element_text(color="#4b4e43", margin=margin(10,0,0,0), size=10),
#         legend.position = "none",
#         legend.direction = "horizontal",
#         legend.margin = margin(0,0,0,0),
#         legend.title = element_text(margin=margin(0,0,10,0), size=14),
#         legend.text = element_blank(),
#         legend.background = element_rect(fill="#fffbef", color="#fffbef"),
#         strip.text = element_text(color="#4b4e43", size=14, margin=margin(10,0,3,0)),
#         strip.background = element_blank(),
#         axis.title.x = element_text(color="#4b4e43", size=18, margin=margin(10,0,0,0)),
#         axis.title.y = element_text(color="#4b4e43", size=18, margin=margin(0,10,0,0)),
#         axis.text.x = element_text(color="#4b4e43", margin=margin(10,0,5,0), size=14, angle=45, hjust=1),
#         axis.text.y = element_text(color="#4b4e43", margin=margin(0,5,0,0), size=14),
#         panel.spacing =  unit(0.1,'in'),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_rect(fill="#fffbef", color="#fffbef"),
#         plot.background = element_rect(fill="#fffbef", color="#fffbef"),
#         panel.border = element_rect(colour = "#4b4e43", fill=NA, size=0.5),
#         plot.margin = margin(0.3,0.3,0.3,0.3,"in"))
# plot

# }


# plot = plot_comet(start_year = 2021, end_year = 2022,
#                   league = "Premier League")
# plot
