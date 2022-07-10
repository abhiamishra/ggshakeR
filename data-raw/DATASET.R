## code to prepare `DATASET` dataset goes here
xTGrid <- read.csv("./inst/extdata/xT.csv")
usethis::use_data(xTGrid, overwrite = TRUE, compress="xz", header=FALSE)

EPVGrid <- read.csv("./inst/extdata/EPV.csv", header=FALSE)
usethis::use_data(EPVGrid, overwrite = TRUE, compress="xz")

SampleEventData <- read.csv("./inst/extdata/SampleEventData.csv")
usethis::use_data(SampleEventData, overwrite = TRUE, compress = "xz")

SampleSBData <- base::readRDS("./inst/testdata/sbevents.RDS")
usethis::use_data(SampleSBData, overwrite = TRUE, compress = "xz")
