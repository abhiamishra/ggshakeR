## code to prepare `DATASET` dataset goes here
xTGrid <- read.csv("./inst/extdata/xT.csv")
usethis::use_data(xTGrid, overwrite = TRUE, compress="xz")

EPVGrid <- read.csv("./inst/extdata/EPV.csv")
usethis::use_data(EPVGrid, overwrite = TRUE, compress="xz")


