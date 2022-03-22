
plot_test2 <- function(pass_data, bin_size = 0, dataType = "statsbomb") {
  
  fill_b = "#0d1117"
  colour_b <- "white"
  bin_alpha <- 0.6
  
  if (bin_size == 0) {
    bin <- 20
  } else{
    bin =  bin_size
  }
  
  ## blah blah this is a comment
  
  x_bin <- 120 / bin
  y_bin <- 80 / bin
  
  passfx <- seq( 0,120, by = bin)
  passfy <- seq(0, 80, by =bin)
  
  ggplot2(pass_data)+
    geom_line()+
    geom_step()
  
  return(x_bin)
}
