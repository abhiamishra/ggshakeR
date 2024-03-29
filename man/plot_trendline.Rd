% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_trendline.R
\name{plot_trendline}
\alias{plot_trendline}
\title{Function for plotting xG Trendline with FBref/ StatsBomb data.}
\usage{
plot_trendline(data, team, color_xg, color_xga, rolling_average, theme = "")
}
\arguments{
\item{data}{is for the dataset used. Select the number of matches wanted in the viz beforehand.}

\item{team}{is to select the specific team for the viz. Team must be accurate as per FBref specifications.}

\item{color_xg}{is for selecting color for xGoals.}

\item{color_xga}{is for selecting the color for xGoalsAgainst.}

\item{rolling_average}{is for setting the rolling average for the data.}

\item{theme}{to select the theme from 4 options -> dark, almond, rose, white.}
}
\description{
The data can be scraped from FBref.\cr
Dataframe passed in must have the following column names: \cr
\cr
Date (format: year-month-day).yyyy-mm-dd, \cr
Home_xG (xG for Home Team), \cr
Away_xG (xG for Away Team), \cr
Home (Home Team), \cr
Away (Away Team)
}
\details{
For best clarity, export plot as a 2000x1000 png
}
\examples{
\dontrun{
plot <- plot_trendline(data = pl, team = "Tottenham",
                       color_xg = "#08519c", color_xga = "#cb181d",
                       rolling_average = 10, theme = "dark")
plot
}
}
