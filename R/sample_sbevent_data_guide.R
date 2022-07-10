#' @title Sample StatsBomb event data frame
#'
#' @description StatsBomb event \code{dataframe} to play with and understand usage of functions.
#' The following code was used with the use of {StatsBombR}:
#' 
#' \code{ Comp <- FreeCompetitions() %>% }
#' \code{ filter(competition_id == 11 & season_name == "2014/2015") }
#' \code{ Matches <- FreeMatches(Comp) }
#' \code{ StatsBombData <- StatsBombFreeEvents(MatchesDF = Matches, Parallel = TRUE) }
#' \code{ plotting_data  <- allclean(StatsBombData) }
#' \code{ plotting_data <- plotting_data %>% }
#'  \code{ rename("x" = "location.x", }
#'         \code{ "y" = "location.y", }
#'         \code{ "finalX" = "pass.end_location.x", }
#'       \code{ "finalY" = "pass.end_location.y") }

#' @format A \code{dataframe} with the columns `x`, `y`, `finalX`, `finalY`, and the rest of StatsBomb columns
#'
"SampleSBData"
