% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xs.r
\name{read_bank_stations}
\alias{read_bank_stations}
\title{Read Bank Stations}
\usage{
read_bank_stations(f)
}
\arguments{
\item{f}{The HDF5 file to read.}
}
\value{
A table with columns "Station", "LOB" and "ROB", where
  columns "LOB" and "ROB" list the left and right bank stations,
  respectively.
}
\description{
Read the bank stations.
}
\examples{
simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
  package = "RAStestR")
 
read_bank_stations(simple.quasi)

}
