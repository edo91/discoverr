
# missing_method ----------------------------------------------------------

#' @title You better call Saul
#'
#' @param f   function with missing method
#' @param ... missing class method
#'
#' @importFrom purrr map
#'
missing_method <- function(f, ...){

  args <- paste0(map(list(...), multi_class, collapse = "/"), collapse = ".")
  warning("Sorry, method ", paste(f, args, sep = "."), " not available.", call. = FALSE)

  invisible()
}
