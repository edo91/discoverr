# plot_var ----------------------------------------------------------------

#' @title Histogram or Bar Chart for each variable
#' @description Plot a vector, each variable of a list, or each variable of a dataframe
#'
#' @param x    Vector, List or Dataframe.
#' @param ...  Additional parameters (see details)
#'
#' @details
#'
#' lab: name to write
#'
#' @return Returns a plot or a list of plots, ggplot2 objects
#'
#' @examples
#'
#' # vector
#' plot_var(iris$Species, "Species")
#' plot_var(iris$Petal.Length, "Petal.Length")
#'
#' # dataframe
#' plot_var(iris)
#'
#' # tibble
#' plot_var(tibble::as_tibble(iris))
#'
#' # list
#' plot_var(list(rnorm(1000),
#'        rcauchy(1000),
#'        rpois(1000, 100)))
#'
#' # named list
#' plot_var(list(norm = rnorm(1000),
#'        cauchy = rcauchy(1000),
#'        poisson = rpois(1000, 100)))
#'
#' # list of lists and dataframes
#' plot_var(list(iris,
#'        list(rnorm(1000),
#'             rcauchy(1000),
#'             rpois(1000, 100))))
#'
#' @export
#'
plot_var <- function(x, ...){

  UseMethod("plot_var")

}


#' @title Plot_var for tibbles
#' @export
plot_var.default <- function(x, ...){

  missing_method("plot_var", x)

}


#' @title Plot_var for dataframes
#' @export
plot_var.data.frame <- function(x, ...){

  plot_var(as.list(x))

}

#' @title Plot_var for lists
#' @importFrom purrr walk walk2
#' @export
plot_var.list <- function(x, ...){

  if(is.null(names(x))) walk(x, plot_var, ...)
  else walk2(x, names(x), plot_var, ...)

}

#' @title Plot_var for factor vactors
#' @export
plot_var.factor <- function(x, lab = NULL, ...) plot_var(as.character(x), lab = lab)

#' @title Plot_var for numeric vectors
#' @importFrom ggplot2 ggplot geom_histogram aes xlab theme_light theme element_blank
#' @export
plot_var.numeric <- function(x, lab = NULL, ...){

  ggplot() +
    geom_histogram(aes(x = x), bins = 30, colour = "coral4", fill = "coral3", alpha = 0.6) +
    xlab(lab) +
    theme_light() +
    theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())

}

#' @title Plot_var for character vectors
#' @importFrom ggplot2 ggplot geom_bar aes xlab theme_light coord_flip theme element_blank
#' @importFrom dplyr count
#' @importFrom tibble tibble
#'
#' @importFrom ggfancy plot_pareto
#'
#' @export
plot_var.character <- function(x, lab = NULL, ...){

  if(require(ggfancy)){

    gg <- plot_pareto(count(tibble(x = x), x),
                      x,
                      n,
                      xlab = lab,
                      group_over = 10,
                      title = NULL, ...)

  } else {

    message("install ggfancy to plot a pareto chart instead of a bar chart for your character variables")

    # in case I change my mind
    gg <- ggplot() +
      geom_bar(aes(x), colour = "steelblue", fill = "steelblue", alpha = 0.6) +
      coord_flip() +
      xlab(lab) +
      theme_light() +
      theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())

  }

  gg

}

#' @title Plot_var for date vectors
#' @importFrom ggplot2 ggplot geom_bar aes xlab theme_light theme element_blank
#' @importFrom dplyr count
#' @importFrom tibble tibble
#' @export
plot_var.Date <- function(x, lab = NULL, ...){

  ggplot() +
    geom_bar(aes(x = x), colour = NA, fill = "springgreen3", alpha = 0.6) +
    xlab(lab) +
    theme_light() +
    theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())

}

#' @title Plot_var for POSIXct vectors
#' @importFrom lubridate as_date
#' @export
plot_var.POSIXct    <- function(x, lab = NULL, ...){

  plot_var.Date(as_date(x), lab = lab, ...)

}


#' @title Plot_var for POSIXlt vectors
#' @importFrom lubridate as_date
#' @export
plot_var.POSIXlt    <- function(x, lab = NULL, ...){

  plot_var.Date(as_date(x), lab = lab, ...)

}



