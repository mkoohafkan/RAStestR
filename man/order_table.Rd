% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.r
\name{order_table}
\alias{order_table}
\title{Order Table}
\usage{
order_table(d, time.col = "Time")
}
\arguments{
\item{d}{A wide-format table.}

\item{time.col}{The time column name.}
}
\value{
the data frame \code{d}, ordered by time and cross section.
}
\description{
Reorder a table by time and cross section.
}
\examples{
simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
  package = "RAStestR")
quasi.flow = read_standard(simple.quasi, "Flow")

quasi.disordered = quasi.flow[sample(1:nrow(quasi.flow), nrow(quasi.flow)),]
order_table(quasi.disordered)

}
