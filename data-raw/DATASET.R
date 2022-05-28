## code to prepare `DATASET` dataset goes here
xTGrid <- read.csv("./inst/extdata/xT.csv")
usethis::use_data(xTGrid, overwrite = TRUE, compress = "xz")

EPVGrid <- read.csv("./inst/extdata/EPV.csv")
usethis::use_data(EPVGrid, overwrite = TRUE, compress = "xz")

TestEventData <- read.csv("./inst/extdata/TestEventData.csv")
usethis::use_data(TestEventData, overwrite = TRUE, compress = "xz")