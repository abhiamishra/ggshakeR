


plot_test <- function(pass_data, bin_size = 0, dataType = "statsbomb") {
  
  fill_b = "#0d1117"
  colour_b <- "white"
  bin_alpha <- 0.6
  
  if(bin_size == 0) {
    bin <- 20
  } else{
    bin =  bin_size
  }
  
  x_bin <- 120 / bin
  y_bin <- 80 / bin
  
  passfx <- seq(0,120, by = bin)
  passfy <- seq(0, 80, by =bin)
  
  return(x_bin)
}
