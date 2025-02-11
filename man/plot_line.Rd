% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{plot_line}
\alias{plot_line}
\title{Line Chart for Date data}
\usage{
plot_line(data, x, y, ...)
}
\arguments{
\item{data}{a data frame or tibble}

\item{x}{string. a Date variable}

\item{y}{string. a numeric variable}
}
\value{
a barplot, ggplot2 object
}
\description{
Plots a line chart with Date data
}
\examples{

dd <- data.frame(time = seq.Date(Sys.Date(), by = "day", length.out = 10),
                 rand = rnorm(10))
plot_line(dd, "time", "rand")

}
