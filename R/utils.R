#' @title text_wrap
#' @description Wrap text using stringi to mimic stringr
#' @param x text vector
#' @return text vector
#' @rdname text_wrap
#' @keywords internal 
#' @importFrom stringi stri_wrap stri_c

text_wrap <- function(x) {
  wrapped_text <- stri_wrap(x, width = 10, whitespace_only = TRUE, simplify = FALSE)
  final_text <- vapply(wrapped_text, stri_c, collapse = "\n", character(1))
  
  return(final_text) 
}


#' @title shift_column
#' @description Port of `shift.column()` from Jared Lander's {useful} package 
#' with a few minor changes.
#' @param data data
#' @param columns columns to shift over
#' @param new_names New name of shifted columns, Default: sprintf("%s.Shifted", columns)
#' @param len length of rows to shift, Default: 1
#' @param up shift rows up (TRUE) or down (FALSE), Default: TRUE
#' @return data.frame with shifted columns
#' @keywords internal
#' @rdname shift_column

shift_column <- function(data, columns, new_names = sprintf("%s.Shifted", columns), len = 1L, up = TRUE) {
  if (length(columns) != length(new_names)) {
    stop("columns and new_names must be the same length")
  }
  
  # get the rows to keep based on how much to shift it by and weather to shift up or down
  rowsToKeep <- seq(from = 1 + len * up, length.out = NROW(data) - len)
  
  # for the original data -- it needs to be shifted the other way
  dataRowsToKeep <- seq(from = 1 + len * !up, length.out = NROW(data) - len)
  
  #create a df of the shifted rows
  shiftedDF <- data[rowsToKeep, columns]
  
  # give the right names to these new columns
  names(shiftedDF) <- new_names
  
  # data names
  dataNames <- names(data)
  
  # get rid of excess rows in data
  data <- data[dataRowsToKeep, ]
  
  # tack shifted data onto the end of the original (and cutoff) data
  data <- cbind(data, shiftedDF)
  names(data) <- c(dataNames, new_names)
  
  return(data)
}


#' @title hull_fun
#' @description Create boundaries for convex hull
#' @param x data frame
#' @return data frame
#' @rdname hull_fun
#' @keywords internal 
#' @importFrom dplyr filter slice
#' @importFrom grDevices chull
#' @importFrom stats quantile

hull_fun <- function(data) {
  
  x_low <- quantile(data$x, 0.05)
  x_high <- quantile(data$x, 0.95)
  
  y_low <- quantile(data$y, 0.05)
  y_high <- quantile(data$y, 0.95)
  
  hull_data <- data %>%  
    filter((x > x_low) & (x < x_high)) %>%
    filter((y > y_low) & (y < y_high)) %>%
    slice(chull(x, y))
  
  return(hull_data)
}


#' title zissou_pal
#' @description 'Zissou' palette hex codes. Useed in `plot_sonar()` function.
#' Created by Karthik Ram in the {wesanderson} R package.
#' @keywords internal

zissou_pal <- c("#3B9AB2", "#56A6BA", "#71B3C2", "#9EBE91", "#D1C74C", 
                "#E8C520", "#E4B80E", "#E29E00", "#EA5C00", "#F21A00")


#' @title viridis_d_pal
#' @description Viridis palette hex codes. Used in `plot_passflow()` function.
#' Created by Stefan van der Walt & Nathaniel Smith for matplotlib.
#' @keywords internal

viridis_d_pal <- c("#440154FF", "#440256FF", "#450457FF", "#450559FF", "#46075AFF", 
                   "#46085CFF", "#460A5DFF", "#460B5EFF", "#470D60FF", "#470E61FF", 
                   "#471063FF", "#471164FF", "#471365FF", "#481467FF", "#481668FF", 
                   "#481769FF", "#48186AFF", "#481A6CFF", "#481B6DFF", "#481C6EFF", 
                   "#481D6FFF", "#481F70FF", "#482071FF", "#482173FF", "#482374FF", 
                   "#482475FF", "#482576FF", "#482677FF", "#482878FF", "#482979FF", 
                   "#472A7AFF", "#472C7AFF", "#472D7BFF", "#472E7CFF", "#472F7DFF", 
                   "#46307EFF", "#46327EFF", "#46337FFF", "#463480FF", "#453581FF", 
                   "#453781FF", "#453882FF", "#443983FF", "#443A83FF", "#443B84FF", 
                   "#433D84FF", "#433E85FF", "#423F85FF", "#424086FF", "#424186FF", 
                   "#414287FF", "#414487FF", "#404588FF", "#404688FF", "#3F4788FF", 
                   "#3F4889FF", "#3E4989FF", "#3E4A89FF", "#3E4C8AFF", "#3D4D8AFF", 
                   "#3D4E8AFF", "#3C4F8AFF", "#3C508BFF", "#3B518BFF", "#3B528BFF", 
                   "#3A538BFF", "#3A548CFF", "#39558CFF", "#39568CFF", "#38588CFF", 
                   "#38598CFF", "#375A8CFF", "#375B8DFF", "#365C8DFF", "#365D8DFF", 
                   "#355E8DFF", "#355F8DFF", "#34608DFF", "#34618DFF", "#33628DFF", 
                   "#33638DFF", "#32648EFF", "#32658EFF", "#31668EFF", "#31678EFF", 
                   "#31688EFF", "#30698EFF", "#306A8EFF", "#2F6B8EFF", "#2F6C8EFF", 
                   "#2E6D8EFF", "#2E6E8EFF", "#2E6F8EFF", "#2D708EFF", "#2D718EFF", 
                   "#2C718EFF", "#2C728EFF", "#2C738EFF", "#2B748EFF", "#2B758EFF", 
                   "#2A768EFF", "#2A778EFF", "#2A788EFF", "#29798EFF", "#297A8EFF", 
                   "#297B8EFF", "#287C8EFF", "#287D8EFF", "#277E8EFF", "#277F8EFF", 
                   "#27808EFF", "#26818EFF", "#26828EFF", "#26828EFF", "#25838EFF", 
                   "#25848EFF", "#25858EFF", "#24868EFF", "#24878EFF", "#23888EFF", 
                   "#23898EFF", "#238A8DFF", "#228B8DFF", "#228C8DFF", "#228D8DFF", 
                   "#218E8DFF", "#218F8DFF", "#21908DFF", "#21918CFF", "#20928CFF", 
                   "#20928CFF", "#20938CFF", "#1F948CFF", "#1F958BFF", "#1F968BFF", 
                   "#1F978BFF", "#1F988BFF", "#1F998AFF", "#1F9A8AFF", "#1E9B8AFF", 
                   "#1E9C89FF", "#1E9D89FF", "#1F9E89FF", "#1F9F88FF", "#1FA088FF", 
                   "#1FA188FF", "#1FA187FF", "#1FA287FF", "#20A386FF", "#20A486FF", 
                   "#21A585FF", "#21A685FF", "#22A785FF", "#22A884FF", "#23A983FF", 
                   "#24AA83FF", "#25AB82FF", "#25AC82FF", "#26AD81FF", "#27AD81FF", 
                   "#28AE80FF", "#29AF7FFF", "#2AB07FFF", "#2CB17EFF", "#2DB27DFF", 
                   "#2EB37CFF", "#2FB47CFF", "#31B57BFF", "#32B67AFF", "#34B679FF", 
                   "#35B779FF", "#37B878FF", "#38B977FF", "#3ABA76FF", "#3BBB75FF", 
                   "#3DBC74FF", "#3FBC73FF", "#40BD72FF", "#42BE71FF", "#44BF70FF", 
                   "#46C06FFF", "#48C16EFF", "#4AC16DFF", "#4CC26CFF", "#4EC36BFF", 
                   "#50C46AFF", "#52C569FF", "#54C568FF", "#56C667FF", "#58C765FF", 
                   "#5AC864FF", "#5CC863FF", "#5EC962FF", "#60CA60FF", "#63CB5FFF", 
                   "#65CB5EFF", "#67CC5CFF", "#69CD5BFF", "#6CCD5AFF", "#6ECE58FF", 
                   "#70CF57FF", "#73D056FF", "#75D054FF", "#77D153FF", "#7AD151FF", 
                   "#7CD250FF", "#7FD34EFF", "#81D34DFF", "#84D44BFF", "#86D549FF", 
                   "#89D548FF", "#8BD646FF", "#8ED645FF", "#90D743FF", "#93D741FF", 
                   "#95D840FF", "#98D83EFF", "#9BD93CFF", "#9DD93BFF", "#A0DA39FF", 
                   "#A2DA37FF", "#A5DB36FF", "#A8DB34FF", "#AADC32FF", "#ADDC30FF", 
                   "#B0DD2FFF", "#B2DD2DFF", "#B5DE2BFF", "#B8DE29FF", "#BADE28FF", 
                   "#BDDF26FF", "#C0DF25FF", "#C2DF23FF", "#C5E021FF", "#C8E020FF", 
                   "#CAE11FFF", "#CDE11DFF", "#D0E11CFF", "#D2E21BFF", "#D5E21AFF", 
                   "#D8E219FF", "#DAE319FF", "#DDE318FF", "#DFE318FF", "#E2E418FF", 
                   "#E5E419FF", "#E7E419FF", "#EAE51AFF", "#ECE51BFF", "#EFE51CFF", 
                   "#F1E51DFF", "#F4E61EFF", "#F6E620FF", "#F8E621FF", "#FBE723FF", 
                   "#FDE725FF")
