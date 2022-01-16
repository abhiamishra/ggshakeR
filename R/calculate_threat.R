#' Calculating xT for passes, carries, etc
#'
#' @param eventData The dataframe that stores your data. Must contain starting x,y locations and ending x,y locations: x,y,finalX,finalY
#' @param dataType indicator for what type of data the eventData. Currently, options include "opta" (default) and "statsbomb"
#' @return returns a dataframe object
#'
#' @importFrom magrittr %>%
#' @import dplyr
#' @export
#'
#' @examples
#' \dontrun{
#' endResult <- calculate_threat(test, dataType = "statsbomb", x_col = "location.x",
#'                               y_col = "location.y", xend_col = "pass.end_location.x",
#'                               yend_col = "pass.end_location.y")
#' endResult
#' }

calculate_threat <- function(eventData, dataType = "opta") {
  if (nrow(eventData) > 0) {
    copydata <- eventData
    
    copydata <- copydata %>% mutate(uniqueID = 1:nrow(copydata))
    
    parsing <- copydata
    
    for (i in 1:length(names(parsing))) {
      if (names(parsing)[i] == "x") {
        names(parsing)[i] <- "x_col"
        #print(names(parsing)[i])
      }
      
      if (names(parsing)[i] == "y") {
        names(parsing)[i] <- "y_col"
      }
      
      if (names(parsing)[i] == "finalX") {
        names(parsing)[i] <- "xend_col"
      }
      
      if (names(parsing)[i] == "finalY") {
        names(parsing)[i] <- "yend_col"
      }
    }
    
    parsing <- parsing %>% tidyr::drop_na(y_col)
    parsing <- parsing %>% tidyr::drop_na(x_col)
    parsing <- parsing %>% tidyr::drop_na(xend_col)
    parsing <-  parsing %>% tidyr::drop_na(yend_col)
    
    if(dataType != "opta") {
      to_opta <- rescale_coordinates(from = pitch_statsbomb, to = pitch_opta)
      parsing$x <- to_opta$x(parsing$x_col)
      parsing$y <- to_opta$y(parsing$y_col)
      parsing$endX <- to_opta$x(parsing$xend_col)
      parsing$endY <- to_opta$y(parsing$yend_col)
    } else {
      parsing$x <- (parsing$x_col)
      parsing$y <- (parsing$y_col)
      parsing$endX <- (parsing$xend_col)
      parsing$endY <- (parsing$yend_col)
    }
    
    assign_threat <- function(a, b) {
      row <- 0
      col <- 0
      if (a %% 8.33 == 0) {
        if (b %% 12.5 == 0) {
          col <- as.integer(a / 8.33)
          row <- as.integer(b / 12.5)
          
          if (maindata$x[i+j] == 0) {
            col <- 1
          }
          
          if (maindata$y[i+j] == 0) {
            row <- 1
          }
          
        } else {
          col <- as.integer(a / 8.33)
          if (as.integer(b / 12.5) + 1 > 8) {
            row <- as.integer(b / 12.5)
          } else{
            row <- as.integer(b / 12.5) + 1
          }
        }
      } else {
        if (b %% 12.5 == 0) {
          if (as.integer(a / 8.33) + 1 > 12) {
            col <- as.integer(a / 8.33)
          } else {
            col <- as.integer(a / 8.33) + 1
          }
          row <- as.integer(b / 12.5)
        } else {
          if (as.integer(a / 8.33) + 1 > 12) {
            col <- as.integer(a / 8.33)
          } else {
            col <- as.integer(a / 8.33) + 1
          }
          
          if (as.integer(b / 12.5) + 1 > 8) {
            row <- as.integer(b / 12.5)
          } else {
            row = as.integer(b / 12.5) + 1
          }
        }
      }
      xTGrid[row, col]
    }
    
    parsing$xTStart <- mapply(assign_threat, parsing$x, parsing$y)
    parsing$xTEnd <- mapply(assign_threat, parsing$endX, parsing$endY)
    
    parsing <- parsing %>%
      select(-c(x,y,endX,endY))
    
    for (i in 1:length(names(parsing))) {
      if (names(parsing)[i] == "x_col") {
        names(parsing)[i] <- "x"
        #print(names(parsing)[i])
      }
      
      if (names(parsing)[i] == "y_col") {
        names(parsing)[i] <- "y"
      }
      
      if (names(parsing)[i] == "xend_col") {
        names(parsing)[i] <- "finalX"
      }
      
      if (names(parsing)[i] == "yend_col") {
        names(parsing)[i] <- "finalY"
      }
    }
    
    joined <- copydata %>%
      full_join(parsing, by = "uniqueID", suffix = c("", ".joined")) %>%
      select(-ends_with(".joined"))
    
    joined <- joined %>%
      select(-c("uniqueID"))
    
    joined$xTStart[joined$xTStart=="NULL"] <- -1
    joined$xTEnd[joined$xTEnd=="NULL"] <- -1
    
    joined$xTStart <- as.numeric(joined$xTStart)
    joined$xTEnd <- as.numeric(joined$xTEnd)
    
    joined
  } else{
    eventData
  }
}
