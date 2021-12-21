############# TESTING PLOT_SCATTER ################
#Creating a dataframe and testing plot_scatter
df = data.frame(
  xA = seq(1,100,by=1),
  yA = seq(1,100, by=1),
  col_scat = seq(1,100, by=1),
  size_scat = seq(5,500, by=5)
)

#Creating an empty dataframe
df_empty = data.frame(matrix(ncol=3,nrow=0))
x = c("xA", "yA", "size_scat")
colnames(df_empty) <- x

testthat::test_that("Plotting scatterplots: ",{
  #Testing for normal plotting
  p = plot_scatter(data=df,
                   scatter_x="xA",
                   scatter_y="yA",
                   set_color_var="col_scat",
                   set_size_var = "size_scat")
  testthat::expect_true(is.ggplot(p))
  testthat::expect_identical(p$labels$x, "xA")
  testthat::expect_identical(p$labels$y, "yA")
  testthat::expect_identical(p$labels$colour, "col_scat")
  testthat::expect_identical(p$labels$size, "size_scat")

  #Testting for plotting with an empty dataframe
  p = plot_scatter(data=df_empty,
                   scatter_x="xA",
                   scatter_y="yA",
                   set_size_var = "size")
  testthat::expect_true(!is.ggplot(p))

  #Testing for using wrong/nonexistent column names
  p = plot_scatter(data=df,
                   scatter_x="xA",
                   scatter_y="yA",
                   set_color_var="col_scat",
                   set_size_var = "size")
  testthat::expect_true(is.ggplot(p))
  testthat::expect_identical(p$labels$size, "")

  #Testing for using wrong x,y column names
  p = plot_scatter(data=df,
                   scatter_x="xA",
                   scatter_y="y",
                   set_color_var="col_scat",
                   set_size_var = "size_scat")
  testthat::expect_true(!is.ggplot(p))
})
############# TESTING PLOT_SCATTER ################







############# TESTING PLOT_SHOTS ################
#Creating simple dataframe for testing basic plots
df = data.frame(
  X = seq(81,100,by=1),
  Y = seq(81,100,by=1),
  xG = seq(1, 20, by=1),
  result = rep(c("A","B"),times=10),
  player = rep(c("Abhishek"),times=20)
)

#Creating an empty dataframe
df_empty = data.frame(matrix(ncol=5,nrow=0))
x = c("X", "Y", "xG", "result", "player")
colnames(df_empty) <- x

#Creating simple dataframe for testing basic plots
df_absent = data.frame(
  X = seq(81,100,by=1),
  Y = seq(81,100,by=1),
  result = rep(c("A","B"),times=10),
  player = rep(c("Abhishek"),times=20)
)


testthat::test_that("Testing plotting pass flow maps: ", {
  p = plot_shot(df)
  testthat::expect_true(is.ggplot(p))

  #testing for plotting on an empty dataframe
  p = plot_shot(df_empty)
  testthat::expect_true(!is.ggplot(p))

  #testing using a dataframe that does not have the required columns
  p = plot_shot(df_absent)
  testthat::expect_true(!is.ggplot(p))
})
############# TESTING PLOT_SHOTS ################







############# TESTING PLOT_PASSFLOW ################
#Creating simple dataframe for testing basic plots
df = data.frame(
  location.x = seq(81,100,by=1),
  location.y = seq(81,100,by=1),
  pass.end_location.x = seq(51, 70, by=1),
  pass.end_location.y = rep(61, 80, by=1)
)

#Creating an empty dataframe
df_empty = data.frame(matrix(ncol=5,nrow=0))
x = c("location.x", "location.y", "pass.end_location.x", "pass.end_location.y")
colnames(df_empty) <- x

#Creating simple dataframe for testing basic plots
df_absent = data.frame(
  location.x = seq(81,100,by=1),
  location.y = seq(81,100,by=1),
  pass.end_location.x = seq(51, 70, by=1)
)


testthat::test_that("Testing plotting shot maps: ", {
  p = plot_passflow(df)
  testthat::expect_true(is.ggplot(p))

  #testing for plotting on an empty dataframe
  p = plot_passflow(df_empty)
  testthat::expect_true(!is.ggplot(p))

  #testing using a dataframe that does not have the required columns
  p = plot_passflow(df_absent)
  testthat::expect_true(!is.ggplot(p))
})
############# TESTING PLOT_PASSFLOW ################





############# TESTING PLOT_PASS ################
#Creating simple dataframe for testing basic plots
df = data.frame(
  location.x = seq(81,100,by=1),
  location.y = seq(81,100,by=1),
  pass.end_location.x = seq(51, 70, by=1),
  pass.end_location.y = seq(61, 80, by=1),
  pass.outcome.name = rep(c("Unsuccessful", NA),times=10),
  player.name = rep(c("Katsu"),times=20)
)

#Creating an empty dataframe
df_empty = data.frame(matrix(ncol=5,nrow=0))
x = c("location.x", "location.y", "pass.end_location.x", "pass.end_location.y", "player.name")
colnames(df_empty) <- x

#Creating simple dataframe for testing basic plots
df_absent = data.frame(
  location.x = seq(81,100,by=1),
  location.y = seq(81,100,by=1),
  pass.end_location.x = seq(51, 70, by=1),
  pass.outcome.name = rep(c("Unsuccessful", NA),times=10)
)


testthat::test_that("Testing plotting shot maps: ", {
  p = plot_pass(df, plotType = "def", outcome = "suc")
  testthat::expect_true(is.ggplot(p))

  #testing for plotting on an empty dataframe
  p = plot_pass(df_empty)
  testthat::expect_true(!is.ggplot(p))

  #testing using a dataframe that does not have the required columns
  p = plot_pass(df_absent)
  testthat::expect_true(!is.ggplot(p))
})
############# TESTING PLOT_PASS ################





############# TESTING PLOT_TRENDLINE ################
# Scraping data and selecting only 200 rows
#install.packages(worldfootballR) if not downloaded
library(worldfootballR)
laliga_2022 <- get_match_results(country = "ESP", gender = "M", season_end_year = c(2020, 2021, 2022), tier = "1st")
data <- laliga_2022
data1 <- data[c(1:200), ]

# Creating a dataset with a row of NA's (Beginning, middle and end of dataset)
dat1 <- laliga_2022[c(1:200), ]
dat2 <- laliga_2022[257, ]
dat2 <- NA
dat3 <- laliga_2022[c(400:600), ]

#Beginning
data2 <- rbind(dat1, dat2, dat3)
#Middle
data3 <- rbind(dat2, dat1, dat3)
#End
data4 <- rbind(dat1, dat3, dat2)

# Normal dataframe
data5 <- laliga_2022

testthat::test_that("Testing plotting trendlines: ", {
  p <- plot_trendline(data = laliga_2022, team = "Barcelona",
                      colour_xg = "blue", colour_xga = "red",
                      roll_avg = 10, theme = "dark")
  testthat::expect_true(is.ggplot(p))
  
  #testing for plotting on a dataframe with NA's 
  p <- plot_trendline(data = data3, team = "Barcelona",
                      colour_xg = "blue", colour_xga = "red",
                      roll_avg = 10, theme = "dark")
  testthat::expect_true(is.ggplot(p))
  
  #testing using a dataframe that has limited rows
  p <- plot_trendline(data = data1, team = "Barcelona",
                      colour_xg = "blue", colour_xga = "red",
                      roll_avg = 10, theme = "dark")
  testthat::expect_true(is.ggplot(p))
})





############# TESTING PLOT_PIZZA ################
# Scraping data 
data1 <- fb_player_scouting_report("https://fbref.com/en/players/6928979a/Nicolo-Barella", pos_versus = "primary")
data2 <- fb_player_scouting_report("https://fbref.com/en/players/819b3158/Ilkay-Gundogan", pos_versus = "primary")

# Dataset for single player plot 
View(data1)
View(data2)

# Dataset for comparison plot
data <- rbind(data1, data2)

testthat::test_that("Testing plotting pizzas: ", {
  p <- plot_pizza(data = data, type = "comparison", template = "midfielder",
                  player_1 = "Nicolo Barella", player_2 = "Ilkay Gundogan", 
                  season_player_1 = "Last 365 Days", season_player_2 = "Last 365 Days",
                  colour_compare = "#90ee90", theme = "black")
  testthat::expect_true(is.ggplot(p))
  
  #testing for single player plot
  p <- plot_pizza(data = data1, type = "single", template = "midfielder", 
                  colour_poss = "green", colour_att = "lightblue", season = "Last 365 Days", 
                  colour_def = "#fec44f", theme = "dark")
  testthat::expect_true(is.ggplot(p))
  
  #testing for comparison plot
  p <- plot_pizza(data = data, type = "comparison", template = "midfielder",
                  player_1 = "Nicolo Barella", player_2 = "Ilkay Gundogan",
                  season_player_1 = "Last 365 Days", season_player_2 = "Last 365 Days",
                  colour_compare = "#90ee90", theme = "black")
  testthat::expect_true(is.ggplot(p))
})
############# TESTING PLOT_PIZZA ################





############# TESTING PLOT_HEATMAP ################
#Creating simple dataframe for testing basic plots
df = data.frame(
  location.x = seq(81,100,by=1),
  location.y = seq(81,100,by=1),
  pass.end_location.x = seq(51, 70, by=1),
  pass.end_location.y = seq(61, 80, by=1)
)

#Creating an empty dataframe
df_empty = data.frame(matrix(ncol=4,nrow=0))
x = c("location.x", "location.y", "pass.end_location.x", "pass.end_location.y")
colnames(df_empty) <- x

#Creating simple dataframe for testing basic plots
df_absent = data.frame(
  location.x = seq(81,100,by=1),
  location.y = seq(81,100,by=1),
  pass.end_location.x = seq(51, 70, by=1)
)


testthat::test_that("Testing plotting heatmaps: ", {
  p = plot_heatmap(df, type="hex")
  testthat::expect_true(is.ggplot(p))

  #testing for plotting on an empty dataframe
  p = plot_heatmap(df_empty)
  testthat::expect_true(!is.ggplot(p))

  #testing using a dataframe that does not have the required columns
  p = plot_heatmap(df_absent)
  testthat::expect_true(!is.ggplot(p))
})
############# TESTING PLOT_HEATMAP ################










############# TESTING CALCULATE_THREAT ################
df = data.frame(
  location.x = seq(81,100,by=1),
  location.y = seq(81,100,by=1),
  pass.end_location.x = seq(51, 70, by=1),
  pass.end_location.y = seq(61, 80, by=1)
)

#Creating an empty dataframe
df_empty = data.frame(matrix(ncol=4,nrow=0))
x = c("location.x", "location.y", "pass.end_location.x", "pass.end_location.y")
colnames(df_empty) <- x

testthat::test_that("Testing calculation of expected threat: ", {
  p = calculate_threat(df, x_col="location.x", y_col="location.y",
                          xend_col = "pass.end_location.x", yend_col = "pass.end_location.y")
  testthat::expect_equal((ncol(df)+2), ncol(p))


  #testing for plotting on an empty dataframe
  p = plot_heatmap(df_empty)
  testthat::expect_equal(NULL, ncol(p))
})
############# TESTING CALCULATE_THREAT ################
