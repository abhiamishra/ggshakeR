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
  name = rep(c("Abhishek"),times=20)
)

#Creating an empty dataframe
df_empty = data.frame(matrix(ncol=5,nrow=0))
x = c("X", "Y", "xG", "result", "name")
colnames(df_empty) <- x

#Creating simple dataframe for testing basic plots
df_absent = data.frame(
  X = seq(81,100,by=1),
  Y = seq(81,100,by=1),
  result = rep(c("A","B"),times=10),
  name = rep(c("Abhishek"),times=20)
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


