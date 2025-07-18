% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/all_functions.R
\name{independent_qq}
\alias{independent_qq}
\title{independent_qq}
\usage{
independent_qq(data, continuous, grouping)
}
\arguments{
\item{data}{A data frame in long format.}

\item{variable}{A string specifying the column name of the outcome variable.}

\item{group}{A string specifying the column name of the grouping variable (should have exactly 2 levels).}
}
\value{
A ggpubr::ggarrange object with two QQ plots side-by-side.
}
\description{
Constructs QQ plots for two independent samples based on a grouping variable.
}
\examples{
independent_qq_plot(penguins, "bill_length_mm", "sex")
}
