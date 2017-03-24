#' Read Cross Section Viewer Survey
#'
#' Read survey data exported from the Cross Section Viewer.
#'
#' @param f The cross section viewer file to read. The function expects that
#' the file was exported in standard csv format, NOT as HEC format.
#' @param viewer.version The version of the cross viewer used.
#' @return A data frame.
#'
#' @import readr
#' @export
read_survey = function(f, viewer.version = "1.1.56.0") {
  if (viewer.version == "1.1.56.0") {
    header.row = 1L
    col.spec = cols(
      "SurveyName" = col_character(),
      "CrossSectionID" = col_character(),
      "RiverMile" = col_character(),
      "SailingLineDistance" = col_character(),
      "Easting" = col_number(),
      "Northing" = col_number(),
      "Elevation" = col_number(),
      "Date" = col_date(format = "%m%d%Y"),
      "Station" = col_number(),
      "Offset" = col_number()
    )
  }
  read_csv(f, col_names = TRUE, col_types = col.spec, skip = header.row - 1L)
}

#' Read Cross Section Viewer Longitudinal Change
#'
#' Read the longitudinal change output exported from the Cross Section Viewer.
#'
#' @param f The cross section viewer file to read. Typically a \code{.txt} file.
#' @param cumulative If \code{TRUE}, return the cumulative volume change.
#'   Otherwise, return the incremental control volume change.
#' @inheritParams read_survey
#' @return a dataframe.
#'
#' @import dplyr
#' @import readr
#' @export
read_xsviewer_long_change = function(f, cumulative = TRUE,
  viewer.version = "1.1.56.0"){
  if (viewer.version == "1.1.56.0") {
    header.row = 10L
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
  d = read_csv(f, col_names = TRUE, col_types = col.spec, skip = header.row - 1L)
  if (cumulative)
    d %>% select_(downstream.station = "`downstream river mile`",
      upstream.station = "`upstream river mile`",
      cumulative.volume.change = "`cumulative volume change`")
  else
    d %>% select_(downstream.station = "`downstream river mile`",
      upstream.station = "`upstream river mile`",
      control.volume.change = "`control volume change`")
}

#' Cross Sections To Survey Template
#'
#' Format cross section data according to the Cross Section Viewer survey import
#' template and write to the clipboard.
#'
#' @param d The cross section data to format.
#' @param survey.name The survey name. If missing, the function will search for
#'   a column in d that matches the Cross Section Viewer template.
#' @inheritParams read_survey
#' @return A dataframe formatted according to the template.
#'
#' @import stringr
#' @import readr
#' @export
to_survey_template = function(d, survey.name, viewer.version = "1.1.56.0") {
  if (viewer.version == "1.1.56.0") {
    col.names = c("SurveyDate", "RiverDistance", "Easting",
      "Northing", "Elevation", "Station", "Offset")
    survey.col = "SurveyName"
    date.col = "SurveyDate"
  }
  if (!all(col.names  %in% names(d)))
    stop('Argument "d" must be a data frame with the following columns: ',
      str_c(col.names, collapse = ", "))
  if (missing(survey.name))
    if (!(survey.col %in% names(d)))
      stop('Argument "survey.name" is missing')
  else
    message('Using "', survey.col, '" column in "d" for survey name')
  else
    d[survey.col] = survey.name
  if (any(is.na(as.Date(d[[date.col]], format = "%m%d%Y"))))
    stop('Format of column "', date.col, '" not recognized')
  d[date.col] = strftime(d[[date.col]], format = "%m/%d/%Y")
  d[c(survey.col, col.names)]
}

#' Cross Section Viewer To RAS Geometry
#'
#' Convert a csv file exported from the Cross Section Viewer to a RAS geometry
#' file.
#'
#' @inheritParams read_survey
#' @param out.format The output format to use. If \code{out.format = "XYZ"}, the
#'    geometry is output in X, Y, Z format (requires that \code{f} contains data
#'    in the "Northing" and "Easting" columns). If \code{out.format = "SE"}
#'    the geometry is output in Station-Elevation format.
#' @param river The river label to assign to the survey data.
#' @param reach The reach label to assign to the survey data.
#' @param which.survey The name of the survey to extract. Only one survey can
#'   be converted at a time. If \code{NULL}, the function assumes that only
#'   one survey is listed in \code{f}.
#' @return The survey in a format ready for import into HEC RAS.
#'
#' @import dplyr
#' @export
survey_to_geometry = function(f, river, reach, out.format = c("XYZ", "SE"),
  which.survey = NULL, viewer.version = "1.1.56.0") {
  out.format = match.arg(out.format, c("XYZ", "SE"))
  if (viewer.version == "1.1.56.0") {
    survey.col = "SurveyName"
    rivermile.col = "RiverMile"
    station.col = "Station"
    elevation.col = "Elevation"
    northing.col = "Northing"
    easting.col = "Easting"
  }
  d = read_survey(f, viewer.version)
  if (!is.null(which.survey))
    d = d %>% filter_(.dots = sprintf('%s == "%s"', survey.col, which.survey))
  d["River"] = river
  d["Reach"] = reach
  if (out.format == "SE")
    d %>% transmute_("River", "Reach", RS = rivermile.col,
      Station = station.col, Elevation = elevation.col)
  else
    d %>% transmute_("River", "Reach", RS = rivermile.col,
      X = easting.col, Y = northing.col, Z = elevation.col)
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

#' Survey Longitudinal Change
#'
#' Compute longitudinal cumulative change from survey data.
#'
#' @inheritParams area_to_volume
#'
#' @details This function performs similarly to \code{cumulative_table} but is
#'   designed to work with survey data rather than RAS outputs. The primary
#'   difference is that this function allows the computation of longitudinal
#'   cumulative change when some surveys are incomplete. The function recomputes
#'   the station lengths for each survey based on what cross sections are
#'   present in the survey. The longitudinal cumulative volume is then computed
#'   with missing stations omitted from the data. The longitudinal volume curves
#'   are then linearly interpolated across the missing stations prior to
#'   computing longitudinal change.
#'
survey_change = function(d, station.lengths){

}
