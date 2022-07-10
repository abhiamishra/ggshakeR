#' @title Sample StatsBomb event data frame
#'
#' @description StatsBombe event \code{dataframe} to play with and understand usage of functions.
#' The following code was used with the use of {StatsBombR}:
#' Comp <- FreeCompetitions() %>%
#' filter(competition_id == 11 & season_name == "2014/2015")
#' Matches <- FreeMatches(Comp)
#' StatsBombData <- StatsBombFreeEvents(MatchesDF = Matches, Parallel = TRUE)
#' plotting_data  <- allclean(StatsBombData)
#' plotting_data <- plotting_data %>%
#'  rename("x" = "location.x",
#'         "y" = "location.y",
#'         "finalX" = "pass.end_location.x",
#'       "finalY" = "pass.end_location.y")
#'
#' @format A \code{dataframe} with the columns `x`, `y`, `finalX`, `finalY`, and the rest of StatsBomb columns
#'
"SampleSBData"
