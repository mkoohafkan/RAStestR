% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/viewer.r
\name{read_survey}
\alias{read_survey}
\title{Read Cross Section Viewer Survey}
\usage{
read_survey(f, viewer.version = "1.1.56.0")
}
\arguments{
\item{f}{The cross section viewer file to read. The function expects that
the file was exported in standard csv format, NOT as HEC format.}

\item{viewer.version}{The version of the cross viewer used.}
}
\value{
A data frame.
}
\description{
Read survey data exported from the Cross Section Viewer.
}
