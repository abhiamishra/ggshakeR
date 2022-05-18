#' Calculating EPV for passes, carries, etc
#'
#' @param eventData The dataframe that stores your data. Must contain starting x,y locations and ending x,y locations: `x`, `y`, `finalX`, `finalY`
#' @param dataType indicator for what type of data the eventData. Currently, options include "opta" (default) and "statsbomb"
#' @return returns a dataframe object
#'
#' @importFrom magrittr %>%
#' @import dplyr
#' @export
#'
#' @examples
#' \dontrun{
#' endResult <- calculate_epv(test, dataType = "statsbomb")
#' endResult
#' }

calculate_epv <- function(eventData, dataType = "opta") {
  if (nrow(eventData) > 0 &&
        sum(x = c("x", "y", "finalX", "finalY") %in% names(eventData)) == 4) {
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
    
    if (dataType != "opta") {
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
    
    assign_epv <- function(a, b) {
      row <- 0
      col <- 0
      if (a %% 2 == 0) {
        if (b %% 3.225 == 0) {
          col <- as.integer(a / 2)
          row <- as.integer(b / 3.225)
          
        } else {
          col <- as.integer(a / 2)
          if (as.integer(b / 3.225) + 1 > 31) {
            row <- as.integer(b / 3.225)
          } else{
            row <- as.integer(b / 3.225) + 1
          }
        }
      } else {
        if (b %% 3.225 == 0) {
          if (as.integer(a / 2) + 1 > 50) {
            col <- as.integer(a / 2)
          } else {
            col <- as.integer(a / 2) + 1
          }
          row <- as.integer(b / 3.225)
        } else {
          if (as.integer(a / 2) + 1 > 50) {
            col <- as.integer(a / 2)
          } else {
            col <- as.integer(a / 2) + 1
          }
          
          if (as.integer(b / 3.225) + 1 > 31) {
            row <- as.integer(b / 3.225)
          } else {
            row <- as.integer(b / 3.225) + 1
          }
        }
      }
      return(EPVGrid[row, col])
    }
    
    parsing$EPVStart <- mapply(assign_epv, parsing$x, parsing$y)
    parsing$EPVEnd <- mapply(assign_epv, parsing$endX, parsing$endY)
    
    parsing <- parsing %>%
      select(-c(x, y, endX, endY))
    
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
    
    joined$EPVStart[joined$EPVStart == "NULL"] <- -1
    joined$EPVEnd[joined$EPVEnd == "NULL"] <- -1
    
    joined$EPVStart <- as.numeric(joined$EPVStart)
    joined$EPVEnd <- as.numeric(joined$EPVEnd)
    
    return(joined)
  } else {
    stop("Dataframe has insufficient number of rows and/or you don't have the right amount of columns: x,y,finalX, finalY")
  }
}
