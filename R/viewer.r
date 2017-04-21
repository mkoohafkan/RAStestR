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

#' Survey To Cross Section Data
#'
#' Reformat survey data to match the cross section data format of RAStestR,
#' e.g. the output of \code{read_xs}.
#'
#' @param d The survey data to reformat, i.e. output of \code{read_survey}.
#' @inheritParams read_survey
#'
#' @import stringr
#' @import dplyr
#' @export
survey_to_xs = function(d, viewer.version = "1.1.56.0"){
  # nse workaround
  Elevation = NULL; Station = NULL; RiverMile = NULL; Date = NULL; Time = NULL
  Distance = NULL
  if (viewer.version == "1.1.56.0") {
    d %>% transmute(Elevation, Distance = Station,
        Station = str_c("XS_", RiverMile),
        Time = str_c(str_to_upper(strftime(Date, "%d%b%Y")),
          " 00:00:00")) %>%
      select(Time, Station, Distance, Elevation)
  } else
    stop("Viewer version ", viewer.version, " is not supported")
}

#' Survey Longitudinal Change
#'
#' Compute (longitudinal) (cumulative) change from survey data.
#'
#' @inheritParams xs_cumulative_change
#' @return A wide-format table of (accumulated) change. The output data
#'   will contain NA values where stations are missing from surveys, but the
#'   change
#'
#' @details This function performs similarly to \code{xs_cumulative_change} but
#' is designed to work with survey data rather than RAS outputs. The primary
#'   difference is that this function allows the computation of longitudinal
#'   cumulative change when some surveys are incomplete. The function recomputes
#'   the station lengths for each survey based on what cross sections are
#'   present in the survey. The longitudinal cumulative volume is then computed
#'   with missing stations omitted from the data.
#'
#' @import dplyr
#' @importFrom stats na.omit
#' @export
survey_change = function(d, time.col = "Time", station.col = "Station",
  distance.col = "Distance", elevation.col = "Elevation", bank.stations = NULL,
  reference.elevation = NULL, station.lengths = NULL, over.time = TRUE,
  longitudinal = TRUE, direction = "downstream"){
  direction = match.arg(direction, c("upstream", "downstream"))
  # station lengths
  if (is.null(station.lengths))
    station.lengths = data_frame(Station = unique(d$Station), LOB = 1,
      Channel = 1, ROB = 1)
  # compute area
  d.area = d %>% select_(time.col, station.col, distance.col, elevation.col) %>%
    xs_area(time.col, station.col, distance.col, elevation.col,
      bank.stations, reference.elevation)
  # interpolate over time
  d = survey_interp(d)

  station.cols = names(d.area)[str_detect(names(d.area), "XS_")]
  drop.stations = NULL
  for (sc in station.cols) {
    if (all(is.na(d.area[[sc]]))) {
      drop.stations = c(drop.stations, sc)
      next
    }
    area.dat = d.area[[sc]]
    naidx = is.na(area.dat)
    interpdat = approx(which(!naidx), area.dat[!naidx], which(naidx))
    d.area[interpdat$x, sc] = interpdat$y
    # interpolation not acheived
    if (any(is.na(d.area[[sc]])))
      drop.stations = c(drop.stations, sc)
  }
  # drop missing stations
  if (!is.null(drop.stations))
    warning("Could not interpolate some stations. The following stations will ",
      "be dropped: ", str_c(drop.stations, collapse = ", "))
  # compute volume
  keep.cols = c(time.col, setdiff(station.cols, drop.stations))
  new.lengths = xs_lengths(setdiff(station.cols, drop.stations),
    station.lengths)
  d.area[keep.cols] %>%
    area_to_volume(time.col, new.lengths) %>%
    change_table(time.col) %>%
    cumulative_table(time.col, over.time, longitudinal, direction)
}


#' Interpolate Survey Stations
#'
#' Interpolate data over stations. Useful for working with surveys that have
#' partial overlap.
#'
#' @param d A wide-format table of processed data, e.g. output of
#'   \code{xs_area}.
#' @param drop.stations Logical: Drop stations that cannot be interpolated.
#' @return The data frame \code{d} with some or all \code{NA} values filled in.
#'
#' @import dplyr
#' @import stringr
#' @importFrom stats approx spline
#' @export
survey_interp = function(d, drop.stations = FALSE) {
  station.cols = names(d)[str_detect(names(d), "XS_")]
  missing.stations = NULL
  incomplete.stations = NULL
  for (sc in station.cols) {
    this.dat = d[[sc]]
    naidx = is.na(this.dat)
    if (all(naidx)) {
      missing.stations = c(missing.stations, sc)
      next
    }
    interpdat = approx(which(!naidx), this.dat[!naidx], which(naidx))
    d[interpdat$x, sc] = interpdat$y
    # interpolation not acheived
    if (any(is.na(d[[sc]])))
      incomplete.stations = c(incomplete.stations, sc)
  }
  # drop missing stations
  if (!is.null(missing.stations))
    warning("No data available at stations ",
      str_c(missing.stations, collapse = ", "))
  if (!is.null(incomplete.stations))
    warning("Some missing values remain at stations ",
      str_c(incomplete.stations, collapse = ", "))
  if (drop.stations) {
    keep.cols = setdiff(names(d), c(drop.stations, incomplete.stations))
    d[keep.cols]
  } else
    d
}

#' Extend Survey Cross Section
#'
#' Extend survey cross sections to the specified extents.
#'
#' @param d The survey data in cross section format.
#' @inheritParams survey_change
#' @param mode Extend cross sections by using data from prior cross section
#' (lag) or future cross section (lead).
#' @return The extended cross section data.
#'
#' @import dplyr
#' @export
survey_extend = function(d, station.col = "Station", time.col = "Time",
  distance.col = "Distance", mode = c("lag", "lead")) {
  # nse workaround
  Station = NULL; Time = NULL; Distance = NULL
  d = d %>% rename_(Station = station.col, Time = time.col,
    Distance = distance.col)
  station.order = sort(unique(d$Station))
  time.order = d %>% mutate(time.char = Time) %>%
    reformat_fields("Time") %>% arrange(Time) %>%
    `[[`("time.char") %>% unique()
  if (mode == "lead")
    time.order = rev(time.order)
  # loop through cross sections. For each cross section, loop through time
  # and successively append data from next cross section time
  d.list = d %>% split(d$Station) %>% lapply(function(x) split(x, x$Time))
  for (i in seq_along(station.order)) {
    this.station = station.order[i]
    for (j in 2:length(time.order)) {
      # get current time
      this.time = time.order[j]
      this.data = d.list[[this.station]][[this.time]]
      # get last available time
      for (jj in (j - 1):1) {
        prior.time = time.order[jj]
        prior.data = d.list[[this.station]][[prior.time]]
        if (!is.null(prior.data))
          break
      }
      # skip if current or prior time is not available
      if (is.null(this.data) || is.null(prior.data))
        next
      # extract data from prior time that is outside of current extents
      this.re = max(this.data$Distance)
      this.le = min(this.data$Distance)
      new.xs.data =  prior.data %>% filter(
          (Distance > this.re) | (Distance < this.le)
        ) %>%
        mutate(Time = this.time)
      # merge prior and current data
      d.list[[this.station]][[this.time]] = bind_rows(this.data, new.xs.data)
    }
  }
  # return data
  select.cols = list("Station", "Time", "Distance")
  names(select.cols) = c(station.col, time.col, distance.col)
  lapply(d.list, bind_rows) %>% bind_rows %>% rename_(.dots = select.cols)
}

#' Clip Survey Cross Section
#'
#' Clip survey cross sections to a set of specified extent stations.
#'
#' @inheritParams survey_extend
#' @inheritParams xs_regions
#' @return The clipped cross section data.
#'
#' @import dplyr
survey_clip = function(d, extent.stations, station.col = "Station", time.col = "Time",
  distance.col = "Distance"){
  stop("not finished")
  # nse workaround
  Station = NULL; Time = NULL; Distance = NULL; LE = NULL; RE= NULL;
  LE.old = NULL; RE.old = NULL; LE.new = NULL; RE.new = NULL
  # check edge stations
  if (is.numeric(extent.stations)) {
    extent.stations = data_frame(Station = d[[station.col]],
      LE = extent.stations[[1]], RE = extent.stations[[2]])
  } else if (ncol(extent.stations) == 3L) {
    if (!all(names(extent.stations) %in% c("Station", "LE", "RE"))) {
      warning('Names of argument "extent.stations" not recognized. ',
        "Default column order is 'Station', 'LE', 'RE'")
      names(extent.stations) = c("Station", "LE", "RE")
    }
  } else {
    stop('Format of argument "extent.stations" not recognized')
  }
  extent.stations = xs_extents(d) %>% left_join(extent.stations, by = "Station",
    suffix = c(".old", ".new")) %>% mutate(
      LE.new = ifelse(is.na(LE.new), LE.old, LE.new),
      RE.new = ifelse(is.na(RE.new), RE.old, RE.new)
    ) %>% select(Station, LE = LE.new, RE = RE.new)
  d = d %>% rename_(Station = station.col, Time = time.col,
    Distance = distance.col) %>% left_join(extent.stations, by = "Station") %>%
    filter(Distance >= LE | Distance <= RE)
}
