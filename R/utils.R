
# missing_method ----------------------------------------------------------

#' @title You better call Saul
#'
#' @param f   function with missing method
#' @param ... missing class method
#'
#' @importFrom purrr map
#'
missing_method <- function(f, ...){

  stopifnot(length(f) == 1)

  args <- list(...)
  args <- map(args, class)
  args <- map(args, paste, collapse = "/")
  args <- paste0(args, collapse = ".")
  res <- paste(f, args, sep = ".")
  warning("Sorry, method ", res, " not available. Suggest a chart on https://github.com/edo91/discoverr/issues", call. = FALSE)

  invisible()
}

