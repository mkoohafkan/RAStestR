% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/export.r
\name{reformat_fields}
\alias{reformat_fields}
\title{Reformat Standard Fields}
\usage{
reformat_fields(ld, time.col = "Time", station.col = "Station")
}
\arguments{
\item{ld}{A data table.}

\item{time.col}{The time column name.
To skip formatting the times, set to \code{NULL}.}

\item{station.col}{(For long tables) The station column name. 
To skip formatting the stations, set to \code{NULL}.}
}
\value{
The data table with reformatted fields.
}
\description{
Reformat standard RAStestR fields as standard R data types.
}
\details{
Values in the Time column are formatted as R timestamps 
  (POSIXct). No adjustments for daylight savings or leap years are 
  made. Values in the Station column of long-format tables are 
  converted to numeric values.
}
\examples{
simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
  package = "RAStestR")
quasi.flow = read_standard(simple.quasi, "Flow")
reformat_fields(quasi.flow)

long.flow = to_longtable(quasi.flow, "Flow")
reformat_fields(long.flow)

}
