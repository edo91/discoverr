# plot_2var ---------------------------------------------------------------

#' @title Bivariate Plots
#'
#' @description Bivariate plots accordig to variables type
#'
#' @param data a data frame or tibble
#' @param x an explicative variable
#' @param y target variable
#' @param ... extra parameters for methods
#' @param remove a character vector of columns to exclude from plotting
#'
#' @importFrom purrr pmap
#'
#' @return Returns invisibly a bivariate plot or a named list of bivariate plots, ggplot2 objects
#'
#' @examples
#'
#' test <- data.frame(num_var = rnorm(100),
#'                    chr_var = sample(letters, 100, TRUE),
#'                    fct_var = factor(sample(LETTERS, 100, TRUE)),
#'                    dat_var = seq.Date(Sys.Date(), by = "day", length.out = 100),
#'                    pos_var = as.POSIXct(as.character(seq.Date(Sys.Date(), by = "day", length.out = 100))),
#'                    stringsAsFactors = FALSE)
#'
#'
#'
#' lapply(combn(names(test), 2, simplify = FALSE),
#'        function(x, test) plot_2var(test, x[1], x[2]), test)
#' lapply(combn(names(test), 2, simplify = FALSE),
#'        function(x, test) plot_2var(test, x[2], x[1]), test)
#'
#'
#' plot_2var(iris, setdiff(names(iris), "Species"), "Species")
#' plot_2var(iris, setdiff(names(iris), "Species"), "Species",
#'  remove = names(iris)[tidyselect::starts_with("Petal", vars = names(iris))])
#'
#' @export
plot_2var <- function(data, x, y, ..., remove = NULL){

  check_2var(data, x, y)
  x <- setdiff(x, remove)

  xy <- expand.grid(x, y, stringsAsFactors = FALSE)

  out <- pmap(xy, ~.plot_2var(data, .x, .y, ...), data, ...)

  names(out) <- paste0(xy$Var2, "_vs_", xy$Var1)

  return(out)
}

#' @title Bivariate Plots
#' @export
.plot_2var <- function(data, x, y, ...) UseMethod(".plot_2var", object = data[[y]])

#' @title Bivariate Plots
#' @export
.plot_2var.default   <- function(data, x, y, ...) missing_method(".plot_2var", data[[y]])

#' @title Bivariate Plots
#' @export
.plot_2var.numeric   <- function(data, x, y, ...) UseMethod(".plot_2var.numeric", object = data[[x]])

#' @title Bivariate Plots
#' @export
.plot_2var.factor    <- function(data, x, y, ...) .plot_2var.character                    (data, x, y, ...)

#' @title Bivariate Plots
#' @export
.plot_2var.character <- function(data, x, y, ...) UseMethod(".plot_2var.character", object = data[[x]])

#' @title Bivariate Plots
#' @export
.plot_2var.numeric.default   <- function(data, x, y, ...) missing_method(".plot_2var.numeric", data[[x]])

#' @title Bivariate Plots
#' @export
.plot_2var.numeric.numeric   <- function(data, x, y, ...) plot_scatter                   (data, x, y, ...)

#' @title Bivariate Plots
#' @export
.plot_2var.numeric.factor    <- function(data, x, y, ...) .plot_2var.numeric.character    (data, x, y, ...)

#' @title Bivariate Plots
#' @export
.plot_2var.numeric.character <- function(data, x, y, ...) plot_boxplot                   (data, y, x, ...)

#' @title Bivariate Plots
#' @export
.plot_2var.numeric.POSIXct    <- function(data, x, y, ...) .plot_2var.numeric.Date         (data, x, y, ...)

#' @title Bivariate Plots
#' @export
.plot_2var.numeric.POSIXlt    <- function(data, x, y, ...) .plot_2var.numeric.Date         (data, x, y, ...)

#' @title Bivariate Plots
#' @export
.plot_2var.numeric.Date      <- function(data, x, y, ...) plot_line                      (data, x, y, ...)

#' @title Bivariate Plots
#' @export
.plot_2var.character.default   <- function(data, x, y, ...) missing_method(".plot_2var.character", data[[x]])

#' @title Bivariate Plots
#' @export
.plot_2var.character.numeric   <- function(data, x, y, ...) plot_boxplot                 (data, x, y, ...)

#' @title Bivariate Plots
#' @export
.plot_2var.character.factor    <- function(data, x, y, ...) .plot_2var.character.character(data, x, y, ...)

#' @title Bivariate Plots
#' @export
.plot_2var.character.character <- function(data, x, y, ...) plot_bar                     (data, x, y, ...)


# check_2var --------------------------------------------------------------

#' @title Check plot_2var
#'
#' @param data See plot_2var
#' @param x    See plot_2var
#' @param y    See plot_2var
#'
#' @export
#'
check_2var <- function(data, x, y){

  if(!(is.data.frame(data))) stop("data must be a dataframe")
  if(!(is.character(x) & all(x %in% names(data)))) stop("x must be a char vector of variable names in data")
  if(!(is.character(y) & all(y %in% names(data)))) stop("y must be a char vector of variable names in data")

  invisible()
}

