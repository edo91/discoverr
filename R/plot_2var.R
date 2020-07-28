# plot_2var ---------------------------------------------------------------

#' @title Bivariate Plots
#'
#' @description Bivariate plots accordig to variables type
#'
#' @param data   A data frame
#' @param x      Strings. Explicatory variables. If NULL: names(data)
#' @param y      Strings. Target variable. If NULL: last x.
#' @param remove Strings. Variables to exclude.
#' @param ...    Extra parameters for methods
#'
#' @importFrom purrr pmap walk compact is_empty
#'
#' @return Returns invisibly a bivariate plot or a named list of bivariate plots, ggplot2 objects
#'
#' @examples
#'
#' test <- data.frame(num_var  = rnorm(100),
#'                    num_var2 = rnorm(100),
#'                    chr_var  = sample(letters, 100, TRUE),
#'                    fct_var  = factor(sample(LETTERS, 100, TRUE)),
#'                    dat_var  = seq.Date(Sys.Date(), by = "day", length.out = 100),
#'                    pos_var  = as.POSIXct(as.character(seq.Date(Sys.Date(), by = "day", length.out = 100))),
#'                    stringsAsFactors = FALSE)
#'
#' plot_2var(test, y = "num_var")
#' plot_2var(test, y = "chr_var")
#' plot_2var(iris)
#' # automatically sets x to names of data and y to species
#'
#' @export
#'
plot_2var <- function(data, x = NULL, y = NULL, remove = NULL, ...){

  stopifnot(is.data.frame(data))
  stopifnot(is.null(x)|is.character(x))
  stopifnot(is.null(y)|is.character(y))
  stopifnot(is.null(remove)|is.character(remove))

  if(is.null(x)) x <- names(data)

  x <- setdiff(x, remove)

  if(is.null(y)) y <- x[length(x)]

  x <- setdiff(x, y)

  stopifnot(all(y %in% names(data)))
  stopifnot(all(x %in% names(data)))
  stopifnot(length(x) > 0)
  stopifnot(length(y) > 0)

  xy <- expand.grid(x, y, stringsAsFactors = FALSE)

  out <- pmap(xy, ~.plot_2var(data, .x, .y, ...))

  names(out) <- paste0(xy$Var2, "_vs_", xy$Var1)

  out <- compact(out)

  if(!is_empty(out)) walk(out, plot)
  else warning("Nothing to plot")

  invisible(out)

}


#' @title Bivariate Plots
#' @export
.plot_2var <- function(data, x, y, ...){

  UseMethod(".plot_2var", object = data[[y]])

}

#' @title Bivariate Plots
#' @export
.plot_2var.default   <- function(data, x, y, ...){

  missing_method(".plot_2var", data[[y]])

}

#' @title Bivariate Plots
#' @export
.plot_2var.numeric   <- function(data, x, y, ...){
  UseMethod(".plot_2var.numeric", object = data[[x]])

}

#' @title Bivariate Plots
#' @export
.plot_2var.factor    <- function(data, x, y, ...){

  .plot_2var.character(data, x, y, ...)

}

#' @title Bivariate Plots
#' @export
.plot_2var.character <- function(data, x, y, ...){

  UseMethod(".plot_2var.character", object = data[[x]])

}

#' @title Bivariate Plots
#' @export
.plot_2var.numeric.default   <- function(data, x, y, ...){

  missing_method(".plot_2var.numeric", data[[x]])

}

#' @title Bivariate Plots
#' @export
.plot_2var.numeric.numeric   <- function(data, x, y, ...){

  plot_scatter(data, x, y, ...)

}


#' @title Bivariate Plots
#' @export
.plot_2var.numeric.factor    <- function(data, x, y, ...){

  .plot_2var.numeric.character(data, x, y, ...)

}


#' @title Bivariate Plots
#' @export
.plot_2var.numeric.character <- function(data, x, y, ...){

  plot_boxplot(data, y, x, ...)

}

#' @title Bivariate Plots
#' @export
.plot_2var.numeric.POSIXct    <- function(data, x, y, ...){

  .plot_2var.numeric.Date(data, x, y, ...)

}

#' @title Bivariate Plots
#' @export
.plot_2var.numeric.POSIXlt    <- function(data, x, y, ...){

  .plot_2var.numeric.Date(data, x, y, ...)

}

#' @title Bivariate Plots
#' @export
.plot_2var.numeric.Date      <- function(data, x, y, ...){

  plot_line(data, x, y, ...)

}


#' @title Bivariate Plots
#' @export
.plot_2var.character.default   <- function(data, x, y, ...){

  missing_method(".plot_2var.character", data[[x]])

}


#' @title Bivariate Plots
#' @export
.plot_2var.character.numeric   <- function(data, x, y, ...){

  plot_boxplot(data, x, y, ...)

}


#' @title Bivariate Plots
#' @export
.plot_2var.character.factor    <- function(data, x, y, ...){

  .plot_2var.character.character(data, x, y, ...)

}

#' @title Bivariate Plots
#' @export
.plot_2var.character.character <- function(data, x, y, ...){

  plot_bar(data, x, y, ...)

}


