#' @title text_wrap
#' @description Wrap text using stringi to mimic stringr
#' @param x text vector
#' @return text vector
#' @rdname text_wrap
#' @keywords internal 
#' @importFrom stringi stri_wrap stri_c

text_wrap <- function(x) {
  wrapped_text <- stringi::stri_wrap(x, width = 10, whitespace_only = TRUE, simplify = FALSE)
  final_text <- vapply(wrapped_text, stringi::stri_c, collapse = "\n", character(1))
  
  return(final_text) 
}

#' @title hull_fun
#' @description Create boundaries for convex hull
#' @param x data frame
#' @return data frame
#' @rdname hull_fun
#' @keywords internal 
#' @importFrom dplyr filter slice
  
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