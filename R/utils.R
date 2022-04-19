#' @title text_wrap
#' @description Wrap text using stringi to mimic stringr
#' @param x text vector
#' @return text vector
#' @rdname text_wrap
#' @export 
#' @importFrom stringi stri_wrap stri_c

text_wrap <- function(x) {
  wrapped_text <- stringi::stri_wrap(x, width = 10, whitespace_only = TRUE, simplify = FALSE)
  final_text <- vapply(wrapped_text, stringi::stri_c, collapse = "\n", character(1))
  
  return(final_text) 
}
