# Funs for Graphs

# plot_boxplot ------------------------------------------------------------

#' @title Boxplot
#'
#' @description Plots a standard boxplot using ggplot2
#'
#' @param data  a data frame or tibble
#' @param x     string. numeric or integer variable
#' @param y     string. factor variable
#'
#' @return a boxplot, ggplot2 object
#'
#' @importFrom ggplot2 ggplot labs labs theme_light aes_string geom_boxplot theme element_blank
#' @importFrom forcats fct_lump
#'
#' @examples
#'
#' plot_boxplot(iris, "Petal.Length", "Species")
#'
#' @export
#'
plot_boxplot <- function(data, x, y, ...) {

  data[[y]] <- fct_lump(data[[y]], n = 10)

  ggplot(data = data, aes_string(x = y, y = x, fill = y)) +
    geom_boxplot(colour = "gray40", alpha = 0.6)  +
    labs(title = paste("Boxplot -", x, "vs", y),
         x     = y,
         y     = x,
         fill  = y) +
    theme_light() +
    theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())

}



# plot_bar ----------------------------------------------------------------

#' @title Barplot
#'
#' @description Plots a standard barplot using ggplot2
#'
#' @param data a data frame or tibble
#' @param x    string. factor variable to colour the graph
#' @param y    string. factor variable to show the count
#'
#' @importFrom ggplot2 ggplot labs labs theme_light aes_string guides geom_bar position_dodge2 theme element_blank
#' @importFrom forcats fct_lump
#'
#' @return a barplot, ggplot2 object
#'
#' @examples
#'
#' df <- mtcars
#' df$cyl <- factor(df$cyl)
#' df$gear <- factor(df$gear)
#'
#' plot_bar(df, "cyl", "gear")
#'
#' @export
#'
plot_bar <- function(data, x, y, ...) {

  data[[x]] <- fct_lump(data[[x]], n = 10)
  data[[y]] <- fct_lump(data[[y]], n = 10)

  ggplot(data = data, mapping = aes_string(x = y, fill = x, color = x)) +
    geom_bar(position = position_dodge2(preserve = "single"), alpha = 0.6) +

    labs(title = paste("Bar Plot -", x, "vs", y),
         x = y,
         y = "Count",
         fill = x) +
    theme_light()  +
    theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
    guides(colour = FALSE)

}




# plot_scatter ------------------------------------------------------------

#' @title Scatter plot
#'
#' @description Plots a scatter plot between two variables using ggplot2
#'
#' @param data a data frame or tibble
#' @param x    string. a numeric variable
#' @param y    string. a numeric variable
#'
#' @importFrom ggplot2 ggplot labs theme_light aes_string geom_point geom_smooth
#'
#' @return a barplot, ggplot2 object
#'
#' @examples
#'
#' plot_scatter(iris, "Petal.Length", "Petal.Width")
#'
#' @export
#'
plot_scatter <- function(data, x, y, ...) {

  ggplot(data = data, aes_string(x = x, y = y)) +
    geom_point(colour = "royalblue2", alpha = 0.4, size = 3) +

    labs(title = paste("Scatter Plot -", x, "vs", y),
         x = x,
         y = y) +
    theme_light() +
    geom_smooth(method = lm, color = "coral2", fill = NA)

}



# plot_line ---------------------------------------------------------------

#' @title Line Chart for Date data
#'
#' @description Plots a line chart with Date data
#'
#' @param data a data frame or tibble
#' @param x    string. a Date variable
#' @param y    string. a numeric variable
#'
#' @importFrom ggplot2 ggplot labs theme_light aes_string geom_line theme element_blank
#'
#' @return a barplot, ggplot2 object
#'
#' @examples
#'
#' dd <- data.frame(time = seq.Date(Sys.Date(), by = "day", length.out = 10),
#'                  rand = rnorm(10))
#' plot_line(dd, "time", "rand")
#'
#' @export

plot_line <- function(data, x, y, ...) {

  ggplot(data = data, aes_string(x = x, y = y)) +
    geom_line(color = "royalblue2", size = 1) +
    labs(title = paste("Line Plot -", y, "vs", x),
         x = x,
         y = y) +
    theme_light() +
    theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())

}
