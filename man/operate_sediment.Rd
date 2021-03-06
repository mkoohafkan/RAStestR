% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.r
\name{operate_sediment}
\alias{operate_sediment}
\title{Table Operations (Sediment)}
\usage{
operate_sediment(..., fun, partial = FALSE, time.col = "Time",
  grain.col = "GrainClass")
}
\arguments{
\item{...}{Arbitrary number of wide-format data tables to combine.}

\item{fun}{A function to apply. If multiple tables are supplied in \code{...},
\code{fun} must either be one of the strings "+", "-", "*" and "/" or be 
a function that accepts exactly two arguments. If only one table
is supplied in \code{...}, \code{fun} must accept exactly one 
argument.}

\item{partial}{If TRUE, only the overlapping times and columns will 
be processed.}

\item{time.col}{The time column name.}

\item{grain.col}{the grain class column name.}
}
\value{
A single sediment table.
}
\description{
Combine sediment tables via an operation, e.g. addition or multiplication.
}
\examples{
simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
  package = "RAStestR")
quasi.voloutcum = read_sediment(simple.quasi, "Vol Out Cum")

operate_sediment(quasi.voloutcum, quasi.voloutcum, fun = "+")
operate_sediment(quasi.voloutcum, quasi.voloutcum, fun = "-")
operate_sediment(quasi.voloutcum, fun = function(x) 2*x)
operate_sediment(quasi.voloutcum, fun = function(x) x*x)

}
