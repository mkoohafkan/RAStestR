% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.r
\name{drop_interpolated_xs}
\alias{drop_interpolated_xs}
\title{Drop Interpolated Cross Section Data}
\usage{
drop_interpolated_xs(d)
}
\arguments{
\item{d}{A data table to drop interpolated cross section data from.}
}
\value{
The data frame \code{d} without rows or columns corresponding to
  interpolated cross sections.
}
\description{
Drop data from interpolated cross-sections.
}
\details{
Interpolated cross sections are identified by the presence of a
  '*' in the column name or value of the "Station" column (for long-format
  data).
}
\examples{
simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
  package = "RAStestR")
quasi.flow = read_standard(simple.quasi, "Flow")
drop_interpolated_xs(quasi.flow)

}
