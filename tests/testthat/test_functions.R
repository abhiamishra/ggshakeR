#############  |- PLOT_SCATTER ################
# Creating a dataframe and testing plot_scatter
df <- data.frame(
  xA = seq(1, 100, by = 1),
  yA = seq(1, 100, by = 1),
  col_scat = seq(1, 100, by = 1),
  size_scat = seq(5, 500, by = 5)
)

# Creating an empty dataframe
df_empty <- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("xA", "yA", "size_scat")
colnames(df_empty) <- x

test_that("Plotting scatterplots: ", {
  # Testing for normal plotting
  p <- plot_scatter(
    data = df,
    x = "xA",
    y = "yA",
    set_color_var = "col_scat",
    set_size_var = "size_scat"
  )
  expect_true(is.ggplot(p))
  expect_identical(p$labels$x, "xA")
  expect_identical(p$labels$y, "yA")
  expect_identical(p$labels$colour, "col_scat") ## in internals it's 'colour' here
  expect_identical(p$labels$size, "size_scat")

  # Testting for plotting with an empty dataframe
  expect_error(plot_scatter(
    data = df_empty,
    x = "xA",
    y = "yA",
    set_size_var = "size_scat"
  ), "Please input a data.frame into the 'data' argument.")
  
  # Testing for using wrong/nonexistent column names
  p <- plot_scatter(
    data = df,
    x = "xA",
    y = "yA",
    set_color_var = "col_scat",
    set_size_var = "size"
  )
  expect_true(is.ggplot(p))
  expect_identical(p$labels$size, "")
  
  # Testing for using wrong x,y column names
  expect_error(plot_scatter(
    data = df,
    x = "xA",
    y = "y",
    set_color_var = "col_scat",
    set_size_var = "size_scat"
  ), "Please input columns for the 'x' and/or 'y' arguments.")
  
  # testing for themes
  p <- plot_scatter(
    data = df,
    x = "xA",
    y = "yA",
    set_color_var = "col_scat",
    set_size_var = "size_scat"
  )
  expect_true(is.ggplot(p)) # theme = dark (classic)
  
  p <- plot_scatter(
    data = df,
    x = "xA",
    y = "yA",
    set_color_var = "col_scat",
    set_size_var = "size_scat",
    theme = "minimal"
  )
  expect_true(is.ggplot(p)) # theme = minimal
  
  p <- plot_scatter(
    data = df,
    x = "xA",
    y = "yA",
    set_color_var = "col_scat",
    set_size_var = "size_scat",
    theme = "grey"
  )
  expect_true(is.ggplot(p)) # theme = grey
  
  p <- plot_scatter(
    data = df,
    x = "xA",
    y = "yA",
    set_color_var = "col_scat",
    set_size_var = "size_scat",
    theme = "bw"
  )
  expect_true(is.ggplot(p)) # theme = bw
  
  # Testing captions
  p <- plot_scatter(
    data = df,
    x = "xA",
    y = "yA"
  )
  expect_true(is.ggplot(p)) # caption = "" (default)
  
  p <- plot_scatter(
    data = df,
    x = "xA",
    y = "yA",
    caption = "This is testing"
  )
  expect_true(is.ggplot(p)) # caption = "This is testing"
  plotCaption <- paste("This is testing", "Created using ggshakeR", sep = "\n")
  expect_identical(p$labels$caption, plotCaption)
  
  # Testing caption size
  p <- plot_scatter(
    data = df,
    x = "xA",
    y = "yA",
    caption = "This is testing"
  )
  expect_true(is.ggplot(p)) # caption_size = 10 (default)
  
  p <- plot_scatter(
    data = df,
    x = "xA",
    y = "yA",
    caption = "This is testing",
    caption_size = 40
  )
  expect_true(is.ggplot(p)) # caption_size = 40
  
  # Testing subtitles
  p <- plot_scatter(
    data = df,
    x = "xA",
    y = "yA"
  )
  expect_true(is.ggplot(p)) # subtitle = "" (default)
  
  p <- plot_scatter(
    data = df,
    x = "xA",
    y = "yA",
    subtitle = "Made by ggshakeR"
  )
  expect_true(is.ggplot(p)) # subtitle = "Made by ggshakeR"
  expect_identical(p$labels$subtitle, "Made by ggshakeR")
  
  # testing subtitle size
  p <- plot_scatter(
    data = df,
    x = "xA",
    y = "yA",
    subtitle = "Made by ggshakeR"
  )
  expect_true(is.ggplot(p)) # subtitle_size = 15 (default)
  
  p <- plot_scatter(
    data = df,
    x = "xA",
    y = "yA",
    subtitle = "Made by ggshakeR",
    subtitle_size = 25
  )
  expect_true(is.ggplot(p)) # subtitle_size = 25
  
  
  # Testing titles
  p <- plot_scatter(
    data = df,
    x = "xA",
    y = "yA"
  )
  expect_true(is.ggplot(p)) # title = "" (default)
  
  p <- plot_scatter(
    data = df,
    x = "xA",
    y = "yA",
    title = "Plot One"
  )
  expect_true(is.ggplot(p)) # subtitle = "Plot One"
  expect_identical(p$labels$title, "Plot One")
  
  # testing title size
  p <- plot_scatter(
    data = df,
    x = "xA",
    y = "yA",
    title = "Made by ggshakeR"
  )
  expect_true(is.ggplot(p)) # title_size = 25 (default)
  
  p <- plot_scatter(
    data = df,
    x = "xA",
    y = "yA",
    title = "Made by ggshakeR",
    title_size = 45
  )
  expect_true(is.ggplot(p)) # subtitle_size = 45
  
  # testing coloring in scatter plot with constant color
  p <- plot_scatter(
    data = df,
    x = "xA",
    y = "yA"
  )
  expect_true(is.ggplot(p)) # set_color_num = "red" (default)
  
  p <- plot_scatter(
    data = df,
    x = "xA",
    y = "yA",
    set_color_num = "blue"
  )
  expect_true(is.ggplot(p)) # set_color_num = "blue" (default)
  
  # testing coloring in scatter plot with different color
  p <- plot_scatter(
    data = df,
    x = "xA",
    y = "yA"
  )
  expect_true(is.ggplot(p)) # set_color_var = "" (default)
  
  p <- plot_scatter(
    data = df,
    x = "xA",
    y = "yA",
    set_color_var = "xA"
  )
  expect_true(is.ggplot(p)) # set_color_var = "xA"
  expect_identical(p$labels$colour, "xA")
  
  # testing sizing in scatter plot with constant size
  p <- plot_scatter(
    data = df,
    x = "xA",
    y = "yA"
  )
  expect_true(is.ggplot(p)) # set_size_num = 5 (default)
  
  p <- plot_scatter(
    data = df,
    x = "xA",
    y = "yA",
    set_size_num = 10
  )
  expect_true(is.ggplot(p)) # set_color_num = 10
  
  # testing sizing in scatter plot with different size
  p <- plot_scatter(
    data = df,
    x = "xA",
    y = "yA"
  )
  expect_true(is.ggplot(p)) # set_size_var = "" (default)
  
  p <- plot_scatter(
    data = df,
    x = "xA",
    y = "yA",
    set_size_var = "col_scat"
  )
  expect_true(is.ggplot(p)) # set_color_var = "col_scat"
  expect_identical(p$labels$size, "col_scat")
  
  # testing labels
  p <- plot_scatter(
    data = df,
    x = "xA",
    y = "yA"
  )
  expect_true(is.ggplot(p)) # label = "" (default)
  
  p <- plot_scatter(
    data = df %>% filter(xA <= 10),
    x = "xA",
    y = "yA",
    label = "xA"
  )
  expect_true(is.ggplot(p)) # label = "xA"
  
})



#############  |- PLOT_SHOTS ################
# Creating simple dataframe for testing basic plots
shotData <- system.file("testdata", "shot_data.rds", package = "ggshakeR")
shotData_df <- readRDS(shotData)

# Creating an empty dataframe
df_empty <- data.frame(matrix(ncol = 5, nrow = 0))
x <- c("X", "Y", "xG", "result", "player")
colnames(df_empty) <- x

# Creating simple dataframe for testing basic plots
df_absent <- data.frame(
  X = seq(81, 100, by = 1),
  Y = seq(81, 100, by = 1),
  result = rep(c("A", "B"), times = 10),
  player = rep(c("Abhishek"), times = 20)
)


test_that("Testing plotting shot maps: ", {
  # testing normal shot plotting
  p <- plot_shot(shotData_df, highlight_goals = TRUE, average_location = FALSE)
  expect_true(is.ggplot(p))
  
  # testing for plotting on an empty dataframe
  p <- plot_shot(df_empty)
  expect_true(!is.ggplot(p))
  
  # testing using a dataframe that does not have the required columns
  p <- plot_shot(df_absent)
  expect_true(!is.ggplot(p))
  
  # testing the different types that exist
  p <- plot_shot(shotData_df) # type: point (default)
  expect_true(is.ggplot(p))
  
  p <- plot_shot(shotData_df, type = "hexbin") # type: hexbin
  expect_true(is.ggplot(p))
  
  p <- plot_shot(shotData_df, type = "density") # type: density
  expect_true(is.ggplot(p))
  
  # testing different bin sizes
  p <- plot_shot(shotData_df, type = "hexbin") # bins: 30 (default)
  expect_true(is.ggplot(p))
  
  p <- plot_shot(shotData_df, type = "hexbin", bins = 50) # bins: 50
  expect_true(is.ggplot(p))
  
  # testing highlight_goals
  p <- plot_shot(shotData_df, highlight_goals = FALSE) # default choice of FALSE for highlight_goals 
  expect_true(is.ggplot(p))
  
  p <- plot_shot(shotData_df, highlight_goals = TRUE) # highlight_goals = TRUE
  expect_true(is.ggplot(p))
  
  # testing average_location
  p <- plot_shot(shotData_df, average_location = TRUE) # average_location = TRUE (default)
  expect_true(is.ggplot(p))
  
  p <- plot_shot(shotData_df, average_location = FALSE) # average_location = FALSE
  expect_true(is.ggplot(p))
  
  # testing themes on shot maps
  p <- plot_shot(shotData_df, theme = "dark") # theme = dark (default)
  expect_true(is.ggplot(p))
  
  p <- plot_shot(shotData_df, theme = "white") # theme = dark (default)
  expect_true(is.ggplot(p))
  
  p <- plot_shot(shotData_df, theme = "rose") # theme = dark (default)
  expect_true(is.ggplot(p))
  
  p <- plot_shot(shotData_df, theme = "almond") # theme = dark (default)
  expect_true(is.ggplot(p))
  
  #testing captions
  p <- plot_shot(shotData_df, theme = "almond") # theme = dark (default)
  expect_identical(p$labels$caption, "Created using ggshakeR")
})


#############  |- PLOT_PASSFLOW ################
# Creating simple dataframe for testing basic plots
df <- SampleEventData %>%
  rename("finalX" = "endX") %>%
  rename("finalY" = "endY")

#Statsbomb dataset
sb_df <- SampleSBData

# Creating an empty dataframe
df_empty <- data.frame(matrix(ncol = 5, nrow = 0))
x <- c("x", "y", "finalX", "finalY")
colnames(df_empty) <- x

# Creating simple dataframe for testing basic plots
df_absent <- data.frame(
  location.x = seq(81, 100, by = 1),
  location.y = seq(81, 100, by = 1),
  pass.end_location.x = seq(51, 70, by = 1)
)


test_that("Testing plotting passflow maps: ", {
  # testing opta dataset
  p <- plot_passflow(data = df)
  expect_true(is.ggplot(p))

  # testing statsbomb dataset
  p <- plot_passflow(data = sb_df)
  expect_true(is.ggplot(p))
  
  # testing for plotting on an empty dataframe
  p <- plot_passflow(data = df_empty)
  expect_true(!is.ggplot(p))
  
  # testing using a dataframe that does not have the required columns
  p <- plot_passflow(data = df_absent)
  expect_true(!is.ggplot(p))
  
  # testing binsizes
  p <- plot_passflow(data = df) # binsize = 20 (default)
  expect_true(is.ggplot(p))
  
  p <- plot_passflow(data = df, binwidth = 50) #binsize = 50
  expect_true(is.ggplot(p))
  
  #testing captions
  p <- plot_passflow(data = df, binwidth = 50)
  expect_identical(p$labels$caption, "Created using ggshakeR")
})



#############  |- PLOT_PASS ################

#StatsBomb dataset 
sbevents <- system.file("testdata", "sbevents.RDS", package = "ggshakeR")
sb_df <- readRDS(sbevents)

# Creating simple dataframe for testing basic plots
df <- data.frame(
  x = seq(81, 100, by = 1),
  y = seq(81, 100, by = 1),
  finalX = seq(51, 70, by = 1),
  finalY = seq(61, 80, by = 1),
  pass.outcome.name = rep(c("Unsuccessful", NA), times = 10),
  player.name = rep(c("Katsu"), times = 20)
)

# Creating an empty dataframe
df_empty <- data.frame(matrix(ncol = 4, nrow = 0))
x <- c("x", "y", "finalX", "finalY")
colnames(df_empty) <- x

# Creating simple dataframe with missing columns
df_absent <- data.frame(
  x = seq(81, 100, by = 1),
  y = seq(81, 100, by = 1),
  finalX = seq(51, 70, by = 1)
)


test_that("Testing plotting passes: ", {
  p <- plot_pass(sb_df, data_type = "statsbomb", type = "sep", 
                 outcome = "suc", progressive_pass = TRUE, switch = TRUE,
                 theme = "dark")
  expect_true(is.ggplot(p))
  
  p <- plot_pass(sb_df, data_type = "statsbomb", type = "all", 
                 outcome = "unsuc", progressive_pass = TRUE,
                 theme = "rose")
  expect_true(is.ggplot(p))
  
  p <- plot_pass(df, data_type = "opta", progressive_pass = TRUE)
  expect_true(is.ggplot(p))
  
  # testing for plotting on an empty dataframe
  expect_error(plot_pass(df_empty),
               "The dataset has insufficient columns and/or insufficient data.",
               fixed = TRUE)
  # testing on a dataframe with insufficient columns
  expect_error(plot_pass(df_absent, data_type = "opta"),
               "The dataset has insufficient columns and/or insufficient data.",
               fixed = TRUE)
  
  #testing captions
  p <- plot_pass(df, data_type = "opta", progressive_pass = TRUE)
  expect_identical(p$labels$caption, "Created using ggshakeR")
  
})


#############  |- PLOT_TRENDLINE ################

# Scraping data and selecting only 200 rows
# install.packages(worldfootballR) if not downloaded
# laliga_2022 <- worldfootballR::get_match_results(country = "ESP", gender = "M", season_end_year = c(2020, 2021, 2022), tier = "1st")

dliga <- system.file("testdata", "laliga2022.RDS", package = "ggshakeR")
data <- readRDS(dliga)
laliga_2022 <- data

# Creating a dataset with a row of NA's (Beginning, middle and end of dataset)
data1 <- data[c(1:200), ]
dat1 <- laliga_2022[c(1:200), ]
dat2 <- laliga_2022[257, ]
dat2 <- NA
dat3 <- laliga_2022[c(400:600), ]

# Beginning
data2 <- rbind(dat1, dat2, dat3)
# Middle
data3 <- rbind(dat2, dat1, dat3)
# End
data4 <- rbind(dat1, dat3, dat2)

# Normal dataframe
data5 <- laliga_2022

test_that("Testing plotting trendlines: ", {
  p <- plot_trendline(
    data = laliga_2022, team = "Barcelona",
    color_xg = "blue", color_xga = "red",
    rolling_average = 10, theme = "dark"
  )
  expect_true(is.ggplot(p))
  
  # testing for plotting on a dataframe with NA's
  p <- plot_trendline(
    data = data3, team = "Barcelona",
    color_xg = "blue", color_xga = "red",
    rolling_average = 10, theme = "dark"
  )
  expect_true(is.ggplot(p))
  
  # testing using a dataframe that has limited rows
  p <- plot_trendline(
    data = data1, team = "Barcelona",
    color_xg = "blue", color_xga = "red",
    rolling_average = 10, theme = "dark"
  )
  expect_true(is.ggplot(p))
  
  #testing captions
  p <- plot_trendline(
    data = data1, team = "Barcelona",
    color_xg = "blue", color_xga = "red",
    rolling_average = 10, theme = "dark"
  )
  expect_identical(p$labels$caption, "Created using ggshakeR")
})


#############  |- PLOT_PIZZA ################
# Scraping data
# data1 <- worldfootballR::fb_player_scouting_report("https://fbref.com/en/players/6928979a/Nicolo-Barella", pos_versus = "primary")
# data2 <- worldfootballR::fb_player_scouting_report("https://fbref.com/en/players/819b3158/Ilkay-Gundogan", pos_versus = "primary")

#d1 <- system.file("testdata", "nicb.RDS", package = "ggshakeR")
#data1 <- readRDS(d1)
#d2 <- system.file("testdata", "ilg.RDS", package = "ggshakeR")
#data2 <- readRDS(d2)

# Dataset for single player plot (Do NOT keep `View()` uncommented unless manually checking things!)
# View(data1)
# View(data2)

# Dataset for comparison plot
#data <- rbind(data1, data2)

#Dataset for custom pizza plots 
#data1_cus <- data1[c(1,2,3,4,5,6,7,8,9,10,11,12), ]
#data2_cus <- data2[c(1,2,3,4,5,6,7,8,9,10,11,12), ]

#data_cus <- rbind(data1_cus, data2_cus)

#test_that("Testing plotting pizzas: ", {
  # testing for single player plot
#  p <- suppressWarnings(plot_pizza(
#    data = data1, type = "single", template = "outfielder",
#    color_possession = "green", color_attack = "lightblue", season = "Last 365 Days",
#    color_defense = "#fec44f", theme = "dark"
#  ))
#  
#  expect_true(is.ggplot(p))
#  
#  p <- suppressWarnings(plot_pizza(
#    data = data1_cus, type = "single", template = "custom",
#    color_possession = "green", color_attack = "lightblue", season = "Last 365 Days",
#    color_defense = "#fec44f", theme = "dark"
#  ))
#  
#  expect_true(is.ggplot(p))
#  
#  # testing for comparison plot
#  p <- suppressWarnings(plot_pizza(
#    data = data, type = "comparison", template = "outfielder",
#    player_1 = "Nicolo Barella", player_2 = "Ilkay Gundogan",
#    season_player_1 = "Last 365 Days", season_player_2 = "Last 365 Days",
#    color_compare = "#90ee90", theme = "white"
#  ))
#  expect_true(is.ggplot(p))
#  
#  p <- suppressWarnings(plot_pizza(
#    data = data_cus, type = "comparison", template = "outfielder",
#    player_1 = "Nicolo Barella", player_2 = "Ilkay Gundogan",
#    season_player_1 = "Last 365 Days", season_player_2 = "Last 365 Days",
#    color_compare = "#90ee90", theme = "black"
#  ))
#  expect_true(is.ggplot(p))
#  
#  #testing captions
#  p <- suppressWarnings(plot_pizza(
#  data = data_cus, type = "comparison", template = "custom",
#  player_1 = "Nicolo Barella", player_2 = "Ilkay Gundogan",
#  season_player_1 = "Last 365 Days", season_player_2 = "Last 365 Days",
#  color_compare = "#90ee90", theme = "black"))
#
#  expect_match(p$labels$caption, "Created using ggshakeR")
#})
#


#############  |- PLOT_HEATMAP ################

#StatsBomb dataset 
sbevents <- system.file("testdata", "sbevents.RDS", package = "ggshakeR")
sb_df <- readRDS(sbevents)

# Creating simple dataframe for testing basic plots
df <- data.frame(
  x = seq(81, 100, by = 1),
  y = seq(81, 100, by = 1)
)

# Creating an empty dataframe
df_empty <- data.frame(matrix(ncol = 4, nrow = 0))
x <- c("x", "y", "finalX", "finalY")
colnames(df_empty) <- x

# Creating simple dataframe for testing basic plots
df_absent <- data.frame(
  x = seq(81, 100, by = 1)
)


test_that("Testing plotting heatmaps: ", {
  p <- plot_heatmap(data = sb_df, data_type = "statsbomb", type = "hex")
  expect_true(is.ggplot(p))
  
  p <- plot_heatmap(data = df, data_type = "opta", type = "jdp", theme = "white")
  expect_true(is.ggplot(p))
  
  p <- plot_heatmap(data = df, data_type = "opta", type = "density", theme = "rose")
  expect_true(is.ggplot(p))
  
  p <- plot_heatmap(data = df, data_type = "opta", type = "binwidth", theme = "almond")
  expect_true(is.ggplot(p))
  
  # testing for plotting on an empty dataframe
  empty_heatmap <- expect_error(plot_heatmap(data = df_empty))
  expect_equal(empty_heatmap$message, "Please check that your data has the columns: 'x' and 'y'")
  
  # testing using a dataframe that does not have the required columns
  expect_error(plot_heatmap(data = df_absent))
  
  #testing captions
  p <- plot_heatmap(data = df, data_type = "opta", type = "binwidth", theme = "almond")
  expect_identical(p$labels$caption, "Created using ggshakeR")
})


#############  |- CALCULATE_THREAT ################
#Opta Dataset
opta_df <- data.frame(
  x = seq(81, 100, by = 1),
  y = seq(81, 100, by = 1),
  finalX = seq(81, 100, by = 1),
  finalY = seq(81, 100, by = 1)
)

#Statsbomb Dataset
sb_df <- SampleSBData

# Creating an empty dataframes
df_empty <- data.frame(matrix(ncol = 4, nrow = 0))
x <- c("x", "y", "finalX", "finalY")
colnames(df_empty) <- x

#Creating a dataframe with one less column
df_absent <- data.frame(
  x = seq(81, 100, by = 1),
  y = seq(81, 100, by = 1),
  finalY = seq(61, 80, by = 1)
)

test_that("Testing calculation of expected threat: ", {
  p <- calculate_threat(opta_df, type = "opta")
  expect_equal((ncol(opta_df) + 2), ncol(p))
  
  p <- calculate_threat(sb_df, type = "statsbomb")
  expect_equal((ncol(sb_df) + 2), ncol(p))
  
  # testing for plotting on an empty dataframe
  expect_error(calculate_threat(df_empty),
               "Dataframe has insufficient number of rows and/or you don't have the right amount of columns: `x`, `y`, `finalX`, `finalY`")
  
  expect_error(calculate_threat(df_absent),
               "Dataframe has insufficient number of rows and/or you don't have the right amount of columns: `x`, `y`, `finalX`, `finalY`")
  ## 'Can't subset columns that don't exist.'
})





#############  |- PLOT_SONAR ################
# Creating simple dataframe for testing basic plots
df <- data.frame(
  x = seq(81, 100, by = 1),
  y = seq(81, 100, by = 1),
  finalX = seq(51, 70, by = 1),
  finalY = seq(61, 80, by = 1)
)

# Creating an empty dataframe
df_empty <- data.frame(matrix(ncol = 4, nrow = 0))
x <- c("x", "y", "finalX", "finalY")
colnames(df_empty) <- x

# Creating simple dataframe for testing basic plots
df_absent <- data.frame(
  x = seq(81, 100, by = 1),
  y = seq(81, 100, by = 1),
  finalX = seq(51, 70, by = 1)
)


test_that("Testing plotting sonars: ", {
  p <- plot_sonar(data = df, title = "Test 1")
  expect_true(is.ggplot(p))
  expect_equal(p$labels$title, "Test 1")
  
  # testing for plotting on an empty dataframe
  expect_error(plot_sonar(data = df_empty))
  
  # testing using a dataframe that does not have the required columns
  expect_error(plot_sonar(data = df_absent))
  
  #testing captions
  p <- plot_sonar(data = df, title = "Test 1")
  expect_match(p$labels$caption, "Created using ggshakeR")
})






#############  |- PLOT_TIMELINE ################
# loading dataset

# data <- worldfootballR::understat_team_season_shots(team_url = "https://understat.com/team/Manchester_City/2021")

data <- data.frame(
  stringsAsFactors = FALSE,
  id = c(
    "440822", "440825", "440826",
    "440827", "440835", "440839", "440821", "440823", "440824",
    "440828", "440829", "440830", "440831", "440832",
    "440833", "440834", "440836", "440837", "440838", "440840",
    "440841", "440842"
  ),
  minute = c(
    5, 6, 25, 25, 58, 78, 2, 6, 6,
    28, 28, 28, 29, 33, 34, 44, 65, 75, 75, 80, 85, 85
  ),
  result = c(
    "MissedShots", "OwnGoal",
    "SavedShot", "MissedShots", "MissedShots", "BlockedShot",
    "MissedShots", "BlockedShot", "MissedShots", "BlockedShot",
    "SavedShot", "SavedShot", "MissedShots", "SavedShot",
    "SavedShot", "Goal", "ShotOnPost", "BlockedShot",
    "MissedShots", "ShotOnPost", "BlockedShot", "MissedShots"
  ),
  X = c(
    0.911999969482422,
    0.0340000009536743, 0.88, 0.931999969482422, 0.724000015258789, 0.785,
    0.864000015258789, 0.775, 0.966999969482422,
    0.886999969482422, 0.905999984741211, 0.781999969482422,
    0.803000030517578, 0.903000030517578, 0.850999984741211,
    0.984000015258789, 0.945, 0.750999984741211, 0.878000030517578,
    0.868000030517578, 0.905, 0.919000015258789
  ),
  Y = c(
    0.512999992370605,
    0.442000007629395, 0.540999984741211, 0.485999984741211,
    0.385999984741211, 0.355999984741211, 0.337000007629395, 0.38,
    0.560999984741211, 0.535999984741211, 0.472000007629395,
    0.625999984741211, 0.605999984741211, 0.327999992370605,
    0.297000007629395, 0.427000007629395, 0.714000015258789,
    0.673000030517578, 0.370999984741211, 0.661999969482422,
    0.477999992370605, 0.562999992370605
  ),
  xG = c(
    0.114495478570461, 0,
    0.0799927860498428, 0.52828985452652, 0.0130347907543182,
    0.0237483829259872, 0.0723646283149719, 0.0232037547975779,
    0.49623042345047, 0.405555367469788, 0.147768631577492,
    0.0301809180527925, 0.0122211733832955, 0.0871875286102295,
    0.0246190950274467, 0.395720958709717, 0.082965686917305,
    0.0222787037491798, 0.0478545501828194, 0.110088758170605,
    0.0250045731663704, 0.121743828058243
  ),
  player = c(
    "Harry Maguire", "Eric Bailly",
    "Cristiano Ronaldo", "Mason Greenwood",
    "Mason Greenwood", "Bruno Fernandes", "Bernardo Silva", "Kevin De Bruyne",
    "Ilkay Gündogan", "Kevin De Bruyne", "Gabriel Jesus",
    "João Cancelo", "João Cancelo", "Kevin De Bruyne",
    "João Cancelo", "Bernardo Silva", "Ilkay Gündogan",
    "João Cancelo", "Kevin De Bruyne", "Phil Foden", "Rodri",
    "John Stones"
  ),
  home_away = c(
    "h", "h", "h", "h", "h", "h",
    "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a",
    "a", "a", "a", "a"
  ),
  player_id = c(
    "1687", "1739", "2371", "7490",
    "7490", "1228", "3635", "447", "314", "447", "5543",
    "2379", "2379", "447", "2379", "3635", "314", "2379", "447",
    "6055", "2496", "586"
  ),
  situation = c(
    "SetPiece", "OpenPlay",
    "OpenPlay", "OpenPlay", "OpenPlay", "OpenPlay", "OpenPlay",
    "OpenPlay", "OpenPlay", "OpenPlay", "OpenPlay", "FromCorner",
    "FromCorner", "OpenPlay", "FromCorner", "OpenPlay",
    "OpenPlay", "OpenPlay", "OpenPlay", "OpenPlay", "FromCorner",
    "FromCorner"
  ),
  season = c(
    "2021", "2021", "2021", "2021",
    "2021", "2021", "2021", "2021", "2021", "2021", "2021",
    "2021", "2021", "2021", "2021", "2021", "2021", "2021",
    "2021", "2021", "2021", "2021"
  ),
  shotType = c(
    "Head", "RightFoot", "LeftFoot",
    "RightFoot", "LeftFoot", "RightFoot", "LeftFoot",
    "LeftFoot", "LeftFoot", "LeftFoot", "RightFoot", "LeftFoot",
    "RightFoot", "RightFoot", "RightFoot", "LeftFoot",
    "LeftFoot", "RightFoot", "RightFoot", "LeftFoot", "Head",
    "LeftFoot"
  ),
  match_id = c(
    "16483", "16483", "16483",
    "16483", "16483", "16483", "16483", "16483", "16483", "16483",
    "16483", "16483", "16483", "16483", "16483", "16483",
    "16483", "16483", "16483", "16483", "16483", "16483"
  ),
  home_team = c(
    "Manchester United",
    "Manchester United", "Manchester United", "Manchester United",
    "Manchester United", "Manchester United", "Manchester United",
    "Manchester United", "Manchester United",
    "Manchester United", "Manchester United", "Manchester United",
    "Manchester United", "Manchester United", "Manchester United",
    "Manchester United", "Manchester United",
    "Manchester United", "Manchester United", "Manchester United",
    "Manchester United", "Manchester United"
  ),
  away_team = c(
    "Manchester City",
    "Manchester City", "Manchester City", "Manchester City",
    "Manchester City", "Manchester City", "Manchester City",
    "Manchester City", "Manchester City", "Manchester City",
    "Manchester City", "Manchester City", "Manchester City",
    "Manchester City", "Manchester City", "Manchester City",
    "Manchester City", "Manchester City", "Manchester City",
    "Manchester City", "Manchester City", "Manchester City"
  ),
  home_goals = c(
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  ),
  away_goals = c(
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2
  ),
  date = c(
    "2021-11-06 12:30:00",
    "2021-11-06 12:30:00", "2021-11-06 12:30:00",
    "2021-11-06 12:30:00", "2021-11-06 12:30:00", "2021-11-06 12:30:00",
    "2021-11-06 12:30:00", "2021-11-06 12:30:00",
    "2021-11-06 12:30:00", "2021-11-06 12:30:00", "2021-11-06 12:30:00",
    "2021-11-06 12:30:00", "2021-11-06 12:30:00",
    "2021-11-06 12:30:00", "2021-11-06 12:30:00", "2021-11-06 12:30:00",
    "2021-11-06 12:30:00", "2021-11-06 12:30:00",
    "2021-11-06 12:30:00", "2021-11-06 12:30:00", "2021-11-06 12:30:00",
    "2021-11-06 12:30:00"
  ),
  player_assisted = c(
    "Luke Shaw", NA, "Luke Shaw",
    NA, "Fred", NA, "Kevin De Bruyne", "Gabriel Jesus",
    "Kyle Walker", "Phil Foden", NA, "Kevin De Bruyne",
    "Kevin De Bruyne", "Gabriel Jesus", "Gabriel Jesus", "João Cancelo",
    "Rodri", "Bernardo Silva", NA, "Kevin De Bruyne",
    "Phil Foden", NA
  ),
  lastAction = c(
    "Cross", "Clearance", "Cross",
    "None", "Pass", "BallRecovery", "TakeOn", "Pass", "Cross",
    "Pass", "Rebound", "Pass", "Chipped", "Pass", "Pass",
    "Cross", "Chipped", "Pass", "Aerial", "Pass", "Aerial",
    "Rebound"
  )
)

test_that("Testing plotting timelines: ", {
  p <- plot_timeline(
    data = data, match_year = 2021, team_home = "Manchester United", team_away = "Manchester City",
    color_home = "#e31a1c", color_away = "#980043", theme = "dark"
  )
  expect_true(is.ggplot(p))
  
  #testing captions
  p <- plot_timeline(
    data = data, match_year = 2021, team_home = "Manchester United", team_away = "Manchester City",
    color_home = "#e31a1c", color_away = "#980043", theme = "dark"
  )
  expect_identical(p$labels$caption, "Created using ggshakeR")
})


############# |- PLOT_CONVEXHULL ################
# Creating dataset 

sbevents <- system.file("testdata", "sbevents.RDS", package = "ggshakeR")
sb_data <- readRDS(sbevents)

names <- c("Jesse", "Jasmine", "Cruz", "Muneeb", "Robert", "Shyanne", 
           "Angela", "Jennifer", "Ariel", "Austin", "Anaisha")
x <- sample(100)
y <- sample(100)
finalX <- sample(100)
finalY <- sample(100)

names_rep <- rep_len(names, length.out = 700)
x <- rep_len(x, length.out = 700)
y <- rep_len(y, length.out = 700)
finalX <- rep_len(finalX, length.out = 700)
finalY <- rep_len(finalY, length.out = 700)

data <- data.frame(playerId = names_rep,
                   x = x, 
                   y = y, 
                   finalX = finalX, 
                   finalY = finalY)

# Dataset without the required columns 

data_absent <- data[, c("x", "y", "finalX", "finalY")]

# Empty dataset

data_empty <- data.frame(matrix(ncol = 5, nrow = 0))
x <- c("x", "y", "finalX", "finalY", "playerId")
colnames(data_empty) <- x

# Test

test_that("Testing plotting convex hulls: ", {
  p <- plot_convexhull(data, data_type = "opta", title = "Test 1")
  expect_true(is.ggplot(p))
  
  p <- plot_convexhull(sb_data, data_type = "statsbomb", color = "red", 
                       theme = "rose")
  expect_true(is.ggplot(p))
  
  # testing for plotting on an empty dataframe
  expect_error(plot_convexhull(data_empty, data_type = "opta"),
               "The dataset has insufficient columns and/or insufficient data.",
               fixed = TRUE)
  
  # testing using a dataframe that does not have the required columns
  expect_error(plot_convexhull(data_absent, data_type = "opta"),
               "The dataset has insufficient columns and/or insufficient data.",
               fixed = TRUE)
  
  #testing captions
  p <- plot_convexhull(sb_data, data_type = "statsbomb", color = "red", 
                       theme = "rose")
  expect_identical(p$labels$caption, "Created using ggshakeR")
})


############# |- PLOT_VORONOI ################
# Creating simple dataframes for testing basic plots
opta_df <- SampleEventData
sb_df <- SampleSBData

# Creating an empty dataframe
df_empty <- data.frame(matrix(ncol = 2, nrow = 0))
x <- c("x", "y")
colnames(df_empty) <- x

# Creating dataframe with missing columns
x <- sample.int(100, 10)
df_absent <- data.frame(x)

# Test
test_that("Testing plotting voronoi plots: ", {
  # testing on opta dataset
  p <- plot_voronoi(opta_df, data_type = "opta", fill = "y", alpha = 0.3)
  expect_true(is.ggplot(p))
  
  # testing on statsbomb dataset
  p <- plot_voronoi(sb_df, data_type = "statsbomb", fill = "y", alpha = 0.3)
  expect_true(is.ggplot(p))
  
  # testing for plotting on an empty dataframe
  expect_error(plot_voronoi(df_empty, data_type = "opta"),
               "The dataset has insufficient columns and/or insufficient data.",
               fixed = TRUE)
  
  # testing using a dataframe that does not have the required columns
  expect_error(plot_voronoi(df_absent, data_type = "opta"),
               "The dataset has insufficient columns and/or insufficient data.",
               fixed = TRUE)
  
  # testing fill argument
  p <- plot_voronoi(opta_df, data_type = "opta") # fill = "" (default)
  expect_true(is.ggplot(p))
  
  p <- plot_voronoi(opta_df, data_type = "opta", fill = "x") # fill = "x"
  expect_true(is.ggplot(p))
  
  # testing alpha values
  p <- plot_voronoi(opta_df, data_type = "opta") # alpha = 0.4 (default)
  expect_true(is.ggplot(p))
  
  p <- plot_voronoi(opta_df, data_type = "opta", alpha = 0.2) # alpha = 0.2
  expect_true(is.ggplot(p))
  
  # testing title of the plot values
  p <- plot_voronoi(opta_df, data_type = "opta") # title_plot = "" (default)
  expect_true(is.ggplot(p))
  
  p <- plot_voronoi(opta_df, data_type = "opta", title = "Testing Plot One") # alpha = 0.2
  expect_true(is.ggplot(p))
  
  # testing the colour values
  p <- plot_voronoi(opta_df, data_type = "opta") # colour = "#E74C3C" (default)
  expect_true(is.ggplot(p))
  
  p <- plot_voronoi(opta_df, data_type = "opta", colour = "red") # colour = red
  expect_true(is.ggplot(p))
  
  # testing the theme values
  p <- plot_voronoi(opta_df, data_type = "opta") # theme = "dark" (default)
  expect_true(is.ggplot(p))
  
  p <- plot_voronoi(opta_df, data_type = "opta", theme = "white") # theme = white
  expect_true(is.ggplot(p))
  
  p <- plot_voronoi(opta_df, data_type = "opta", theme = "rose") # theme = rose
  expect_true(is.ggplot(p))
  
  p <- plot_voronoi(opta_df, data_type = "opta", theme = "almond") # theme = almond
  expect_true(is.ggplot(p))
  
  #testing captions
  p <- plot_voronoi(opta_df, data_type = "opta", theme = "almond") # theme = almond
  expect_identical(p$labels$caption, "Created using ggshakeR")
})


############# |- PLOT_PASSNET ################
# Create Dataset

sbevents <- system.file("testdata", "sbevents.RDS", package = "ggshakeR")
sb_data <- readRDS(sbevents)

names <- c("Adam", "Tony", "Avery", "Darrick", "Zachary", "Zachary", "Daisha", 
           "Maliha", "Candace", "Jeffrey", "Abdul Khaliq", "Andrew", "Raafi",
           "Jalyn", "Rochelle", "Safwaan", "Angel", "Tawasa", "Olympia", "Nikko",
           "Mikayla", "Kimberly")
teams <- c("Team 1", "Team 2")
outcome <- c("Successful", "Unsuccessful")
type <- "Pass"
minute <- c(1:90)
x <- sample(100)
y <- sample(100)
finalX <- sample(100)
finalY <- sample(100)

names_rep <- rep_len(names, length.out = 1000)
teams_rep <- rep_len(teams, length.out = 1000)
outcome_rep <- rep_len(outcome, length.out = 1000)
type_rep <- rep_len(type, length.out = 1000)
minute <- rep_len(minute, length.out = 1000)
x <- rep_len(x, length.out = 1000)
y <- rep_len(y, length.out = 1000)
finalX <- rep_len(finalX, length.out = 1000)
finalY <- rep_len(finalY, length.out = 1000)

data <- data.frame(teamId = teams_rep,
                   playerId = names_rep,
                   x = x, 
                   y = y, 
                   finalX = finalX, 
                   finalY = finalY,
                   minute = minute,
                   type = type_rep,
                   outcome = outcome_rep)

data$type[c(500, 600, 700)] <- "SubstitutionOff"

# Dataset without the required columns 

data_absent <- data[, c("x", "y", "finalX", "finalY")]

# Empty dataset

data_empty <- data.frame(matrix(ncol = 9, nrow = 0))
x <- c("x", "y", "finalX", "finalY", "playerId", "teamId", "type", "minute", "outcome")
colnames(data_empty) <- x

# Test 

test_that("Testing plotting pass networks: ", {
  #testing
  p <- plot_passnet(data, data_type = "opta", scale_stat = "xT", scale_color = "blue", 
                    team_name = "Team 1", subtitle = "Test 1", theme = "dark")
  expect_true(is.ggplot(p))
  
  p <- plot_passnet(sb_data, data_type = "statsbomb", scale_stat = "EPV", scale_color = "red", 
                    team_name = "Barcelona", theme = "light")
  expect_true(is.ggplot(p))
  
  # testing for plotting on an empty dataframe
  expect_error(plot_passnet(data_empty, data_type = "opta", team_name = "Team 1"),
               "The dataset has insufficient columns and/or insufficient data.",
               fixed = TRUE)
  
  # testing using a dataframe that does not have the required columns
  expect_error(plot_passnet(data_absent, data_type = "opta", team_name = "Team 1"),
               "The dataset has insufficient columns and/or insufficient data.",
               fixed = TRUE)
  
  #testing captions
  p <- plot_passnet(sb_data, data_type = "statsbomb", scale_stat = "EPV", scale_color = "red", 
                    team_name = "Barcelona", theme = "light")
  expect_match(p$labels$caption, "Created using ggshakeR")
})


############# |- CALCULATE_EPV ################
#Opta Dataset
opta_df <- data.frame(
  x = seq(81, 100, by = 1),
  y = seq(81, 100, by = 1),
  finalX = seq(81, 100, by = 1),
  finalY = seq(81, 100, by = 1)
)

#Statsbomb Dataset
sb_df <- SampleSBData

# Creating an empty dataframes
df_empty <- data.frame(matrix(ncol = 4, nrow = 0))
x <- c("x", "y", "finalX", "finalY")
colnames(df_empty) <- x

#Creating a dataframe with one less column
df_absent <- data.frame(
  x = seq(81, 100, by = 1),
  y = seq(81, 100, by = 1),
  finalY = seq(61, 80, by = 1)
)


test_that("Testing calculation of expected threat: ", {
  p <- calculate_epv(opta_df, type = "statsbomb")
  expect_equal((ncol(opta_df) + 2), ncol(p))
  
  p <- calculate_epv(sb_df)
  expect_equal((ncol(sb_df) + 2), ncol(p))
  
  
  # testing for plotting on an empty dataframe
  expect_error(calculate_epv(df_empty),
               "Dataframe has insufficient number of rows and/or you don't have the right amount of columns: x,y,finalX, finalY")
  
  expect_error(calculate_epv(df_absent),
               "Dataframe has insufficient number of rows and/or you don't have the right amount of columns: x,y,finalX, finalY")
})
