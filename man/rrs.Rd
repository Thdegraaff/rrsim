% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rrs.R
\name{rrs}
\alias{rrs}
\title{A Robust Regression Simulation}
\usage{
rrs(formula, fixed = NA, fe = NA, data, k = 4)
}
\arguments{
\item{formula}{An R formula}

\item{fixed}{A vector of strings for variables that do not need to vary (default = NA)}

\item{fe}{A vector of strings for variables that are considered fixed effects (default = NA)}

\item{data}{The dataframe to be used}

\item{k}{Number of variables in each regression that vary (default = 4)}
}
\value{
A list of two dataframes, one with the coefficients and one with t-values of the variables
}
\description{
A Robust Regression Simulation
}
\examples{
result <- rrs(mpg ~., data = mtcars, k = 4)
}
