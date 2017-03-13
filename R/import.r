#' Read Cross Section Viewer Outputs
#'
#' Read data files exported from the cross section viewer.
#'
#' @param f The cross section viewer file to read. Typically a \code{.txt} file.
#' @param col.spec Define column types. If \code{NULL}, RAStestR will try to
#'   determine the column types automatically.
#' @param viewer.version The version of the cross viewer used.
#'
#' @import readr
#' @export
read_xsviewer = function(f, col.spec = NULL, viewer.version = "1.1.56.0") {
  if (viewer.version == "1.1.56.0")
    header.row = 10L
  read_csv(f, col_names = TRUE, col_types = col.spec, skip = header.row - 1L)
}

#' Read Cross Section Viewer Longitudinal Change
#'
#' Read the longitudinal change output exported from the Cross Section Viewer.
#'
#' @inheritParams read_xsviewer
#' @param cumulative If \code{TRUE}, return the cumulative volume change.
#'   Otherwise, return the incremental control volume change.
#' @return a dataframe.
#'
#' @import dplyr
#' @export
read_xsviewer_long_change = function(f, cumulative = TRUE,
  viewer.version = "1.1.56.0"){
  if (viewer.version == "1.1.56.0"){
    col.spec = cols(
      "downstream river mile" = col_character(),
      "upstream river mile" = col_character(),
      "control reach sailing line distance" = col_character(),
      "new area" = col_number(),
      "old area" = col_number(),
      "area change at downstream RM" = col_number(),
      "New top width at downstream RM" = col_number(),
      "Old top width at downstream RM" = col_number(),
      "control volume change" = col_number(),
      "cumulative volume change" = col_number(),
      "bed change" = col_number(),
      "avg bed change over increment" = col_number(),
      "weighted bed change over increment" = col_number(),
      "cumulative bed change" = col_number()
    )
  }
  d = read_xsviewer(f, col.spec, viewer.version)
  if (cumulative)
    d %>% select_(downstream.station = "`downstream river mile`",
      upstream.station = "`upstream river mile`",
      cumulative.volume.change = "`cumulative volume change`")
  else
    d %>% select_(downstream.station = "`downstream river mile`",
      upstream.station = "`upstream river mile`",
      control.volume.change = "`control volume change`")
}

#' Fix Cross Section Viewer Stations
#'
#' Replace the cross section viewer stations with the actual stations.
#'
#' @param d The cross section viewer data.
#' @param station.data A two-column dataframe containing the actual (column 1)
#'   and offset (column 2) river stations.
#' @param station.cols The names of columns containing station data to be
#'   adjusted.
#' @return The data frame \code{d} with adjusted station values.
#'
#' @details Some versions of the cross section viewer did not allow station
#'   IDs above a certain value, requiring some users to offset their actual
#'   river stations to use the software. This function reverses the offset or
#'   scaling.
#'
#' @export
fix_xsviewer_stations = function(d, station.data,
  station.cols = c("downstream.station", "upstream.station")){
  fix.cols = which(names(d) %in% station.cols)
  for (fc in fix.cols)
    d[fc] = station.data[[1]][match(d[[fc]], station.data[[2]])]
  d
}

#' Offset Cross Section Viewer Longitudinal Change
#'
#' Extract a segment of a Cross Section Viewer longitudinal change curve,
#' offsetting data values where appropriate.
#'
#' @inheritParams fix_xsviewer_stations
#' @param offset.station The station to offset the data from.
#' @param upstream.station.col The name of the column containing the station at
#'   the upstream face of the control volume.
#' @param downstream.station.col The name of the column containing the station
#'   at the downstream face of the control volume.
#' @param data.col The name of the column containing the longitudinal change
#'   values.
#' @return A subset of the data frame \code{d} containing only stations below
#'   \code{offset.station} and with adjusted longitudinal change values.
#'
#' @import dplyr
#' @export
offset_xsviewer_long_change = function(d, offset.station,
  upstream.station.col = "upstream.station",
  downstream.station.col = "downstream.station",
  data.col = "cumulative.volume.change") {
  stop("NOT TESTED")
  # get upstream data
  select.upstream = sprintf("%s >= %f", downstream.station.col, offset.station)
  d.upstream = d %>% filter_(.dots = select.upstream)
  # identify offset value
  offset.value = d.upstream[[data.col]][which.min(d.upstream[[downstream.station.col]])]
  select.downstream = sprintf("%s <= %f", upstream.station.col, offset.station)
  # apply offset to downstream data
  mutate.offset = sprintf("%s = %s - %f", data.col, data.col, offset.value)
  names(mutate.offset) = data.col
  d %>% filter_(.dots = select.downstream) %>%
    mutate_(.dots = mutate.offset)
}
