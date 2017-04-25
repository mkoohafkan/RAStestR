#' Cross Section Table
#'
#' Read the cross section data output.
#'
#' @inheritParams read_standard
#' @return A dataframe with a column "Time" containing the Date Time
#'   Stamp data; column "Station" containing the station ID in format "XS_####"
#'   where ### is the cross-section ID; column "Distance" containing the lateral
#'   distance location; and column "Elevation" containing the elevation at the
#'   distance location.
#'
#' @import h5
#' @import dplyr
#' @import stringr
#' @export
read_xs = function(f, run.type, which.times = NULL, which.stations = NULL) {
  Station = NULL # workaround for nse
  tblblock = get_xsection_block(run.type)
  xsoutputs = list_xs(f, tblblock)
  if (is.null(which.times))
    which.times = str_split_fixed(xsoutputs, "[(.+)]", 3)[,2]
  else if (any(nchar(which.times) != 18L))
    stop('Format of argument "which.times" not recognized')
  else {
    xsoutputs = str_subset(xsoutputs, str_c(which.times, collapse = "|"))
    timeoutputs = xsoutputs %>% str_sub(20, 37)
    times.found = which.times %in% timeoutputs
    if (!all(times.found)) {
      warning("The following times were not found in ", f, ": ",
        str_c(which.times[!times.found], collapse = ", "))
      which.times = which.times[times.found]
    }
  }
  stations = list_stations(f)
  x = h5file(f)
  on.exit(h5close(x))
  dlist = vector("list", length(which.times))
  for (i in seq_along(which.times)) {
    this.time = which.times[i]
    xs = xsoutputs %>% str_subset(this.time)
    if (length(xs) != 1L)
      stop("multiple tables named ", xs)
    index.table = file.path(tblblock, str_c(xs, " info"))
    values.table = file.path(tblblock, str_c(xs, " values"))
    xs.indices = rep(stations, get_dataset(x, index.table, "integer")[,2])
    xs.table = as_data_frame(get_dataset(x, values.table, "double"))
    names(xs.table) = c("Distance", "Elevation")
    xs.table["Station"] = str_c("XS_", xs.indices)
    xs.table["Time"] = this.time
    if (!is.null(which.stations))
      dlist[[i]] = filter(xs.table, Station %in% str_c("XS_", which.stations))
    else
      dlist[[i]] = xs.table
  }
  do.call(bind_rows, dlist)[c("Time", "Station", "Distance", "Elevation")]
}

#' Read Bed Limits
#'
#' Read the moveable bed limit stations.
#'
#' @inheritParams read_standard
#' @return a dataframe with columns "Time", "Station", "LOB" and "ROB", where
#'   columns "LOB" and "ROB" list the left and right bank stations,
#'   respectively.
#'
#' @details The moveable bed limits will change over time in models that use
#'   BSTEM. In general, it is recommended but not required that the BSTEM toe
#'   stations are coincident with the moveable bed limits; otherwise, cross
#'   sections may adjust in unexpected ways.
#'
#' @import stringr
#' @import dplyr
#' @export
read_bed_limits = function(f, run.type, which.stations = NULL,
  which.times = NULL) {
  # get bank station data
  lob = read_standard(f, "Moveable Sta L", run.type, which.times = which.times,
    which.stations = which.stations)
  rob = read_standard(f, "Moveable Sta R", run.type, which.times = which.times,
    which.stations = which.stations)
  # insert starting bank station data
  station.cols = names(lob) %>% str_detect("XS_")
  lob[1, station.cols] = lob[2, station.cols]
  rob[1, station.cols] = rob[2, station.cols]

  list(LOB = lob, ROB = rob) %>%
    lapply(to_longtable, data.col = "Distance") %>%
    bind_rows(.id = "section") %>%
    to_widetable("section", "Distance") %>%
    arrange_("Time", "desc(Station)")
}

#' Read River Station Lengths
#'
#' Read the river station lengths.
#'
#' @inheritParams read_standard
#' @return a dataframe with columns "Station", "LOB", "Channel" and "ROB", where
#'   column "Channel" list the channel length and columns "LOB" and "ROB" list
#'   the left and right bank stations lengths, respectively.
#'
#' @import stringr
#' @import dplyr
#' @export
read_station_lengths = function(f) {
  station.lengths = list_lengths(f)
  river.stations = list_stations(f)
  data_frame(Station = str_c("XS_", river.stations), LOB = station.lengths[,1],
    Channel = station.lengths[,2], ROB = station.lengths[,3])
}

#' Shift Cross Sections
#'
#' Apply a horizontal and/or vertical shift to cross section data.
#'
#' @param d The cross section data.
#' @param what Apply a \code{"horizontal"} and/or \code{"vertical"} shift to
#'   data.
#' @param shift.table A table of cross section stations and shift values.
#'   If \code{what} is \code{"horizontal"}  (\code{"vertical"}),
#'   a two-column table is expected with the first column listing the
#'   cross section stations and the second column listing the horizontal
#'   (vertical) shift to apply to that cross section. If
#'   \code{what = c("horizontal", "vertical")}, a three-column table is expected
#'   with the second and third columns containing the horizontal and vertical
#'   shifts, respectively.
#' @param station.col The column in \code{d} containing the cross section
#'   stations.
#' @param distance.col The column in \code{d} containing the distance
#'   (horizontal) values.
#' @param elevation.col The column in \code{d} containing the elevation
#'   (vertical) values.
#' @param subtract If \code{TRUE}, values in \code{shift.table} will be
#'   subtracted from \code{d}; if \code{FALSE}, values will be added.
#' @return The data frame \code{d} with cross sections shifted according to
#'   \code{shift.table}
#'
#' @import dplyr
#' @importFrom stats setNames
#' @export
xs_shift = function(d, what = c("horizontal", "vertical"), shift.table,
  station.col = "Station", distance.col = "Distance",
  elevation.col = "Elevation", subtract = TRUE) {
  d.names = names(d)
  if (!all(what %in% c("horizontal", "vertical")))
    stop('Value of argument "what" not recognized')
  if (ncol(shift.table) == 2L) {
    if (what == "horizontal")
      names(shift.table) = c("Station", "horizontal")
    else
      names(shift.table) = c("Station", "vertical")
  } else {
      names(shift.table) = c("Station", "horizontal", "vertical")
  }
  if (subtract)
    op = "-"
  else
    op = "+"
  d.join = d %>% left_join(shift.table, by = setNames("Station", station.col))
  mutate.strings = list()
  if ("horizontal" %in% what) {
    mutate.strings[[distance.col]] = sprintf("%s %s horizontal", distance.col,
      op)
    d.join["horizontal"] = ifelse(is.na(d.join$horizontal), 0,
      d.join$horizontal)
  }
  if ("vertical" %in% what) {
    mutate.strings[[elevation.col]] = sprintf("%s %s vertical", elevation.col,
      op)
    d.join["vertical"] = ifelse(is.na(d.join$vertical), 0,
      d.join$vertical)
  }
  # apply shift
  d.join %>% mutate_(.dots = mutate.strings) %>% select_(.dots = d.names)
}

#' Compute Cross Section Lengths
#'
#' Compute cross section lengths from the river station lengths table.
#'
#' @param stations A list of stations to compute lengths for.
#' @param station.lengths A data frame containing station lengths, i.e. output
#'   of \code{read_station_lengths}. Must include columns "Station" and
#'   "Channel".
#' @param station.col The column in \code{d} containing the cross section
#'   stations.
#' @return a dataframe with columns "Station" and "Length", where column
#' "Length" lists the cross section length.
#'
#' @details The cross section length is computed as the average of of the
#'   current river station channel length and the upstream river station channel
#'   length; in other words, the cross section control volume is assumed to
#'   extend half the channel distance between the next upstream and downstream
#'   cross sections.
#'
#' @import dplyr
#' @import stringr
#' @export
xs_lengths = function(stations, station.lengths, station.col = "Station") {
  # nse workaround
  Station = NULL; Channel = NULL; ds.dist = NULL; us.dist = NULL
  us.station = max(stations)
  ds.station = min(stations)
  if (ds.station != min(station.lengths$Station))
    message("Ignoring stations downstream of ", ds.station)
  if (us.station != max(station.lengths$Station))
    message("Ignoring stations upstream of ", us.station)
  dropped.stations = station.lengths %>% filter(!(Station %in% stations),
    Station >= ds.station, Station <= us.station) %>%
    `[[`("Station") %>% unique()
  if (length(dropped.stations) > 0L)
    message("The following stations will be dropped: ", str_c(dropped.stations,
      collapse = ", "))
  station.lengths %>%
    filter(Station >= ds.station, Station <= us.station) %>%
    arrange(Station) %>%
    transmute(Station, ds.dist = cumsum(Channel)) %>%
    filter(Station %in% stations) %>%
    transmute(Station, ds.dist = ds.dist - lag(ds.dist, default = 0)) %>%
    mutate(us.dist = lead(ds.dist, default = 0)) %>%
    transmute(Station, Length = 0.5*(ds.dist + us.dist))
}

#' Read Bank Stations
#'
#' Read the bank stations.
#'
#' @inheritParams read_standard
#' @return A table with columns "Station", "LOB" and "ROB", where
#'   columns "LOB" and "ROB" list the left and right bank stations,
#'   respectively.
#'
#' @import dplyr
#' @import stringr
#' @export
read_bank_stations = function(f){
  bank.stations = list_bank_stations(f)
  river.stations = list_stations(f)
  data_frame(Station = str_c("XS_", river.stations),
    LOB = bank.stations[,1], ROB = bank.stations[,2])
}

#' Cross Section Extents
#'
#' Compute the coincident extents of a cross section
#'
#' @inheritParams xs_area
#' @return A dataframe with columns "Station", "LE" and "RE", where "LE" and
#'   "RE" are the left and right extents, respectively.
#'
#' @import dplyr
#' @export
xs_extents = function(d, station.col = "Station", time.col = "Time",
  distance.col = "Distance") {
  # nse workaround
  Station = NULL; Time = NULL; LE = NULL; RE = NULL; Distance = NULL;
  d %>% select_(Station = station.col, Time = time.col,
    Distance = distance.col) %>%
    group_by(Station, Time) %>%
    summarize(LE = min(Distance), RE = max(Distance)) %>%
    summarize(LE = max(LE), RE = min(RE))
}

# area of a cross section
#
# Compute cross section volumes from cross section data.
#
# @param distance The lateral cross section stations.
# @param elevation The cross section elevations at each station.
# @param left.bank The left bank station.
# @param right.bank The right bank station.
# @param reference.elevation The reference elevation to use for computing
#   volume. If \code{NULL}, the maximum elevation of the cross section will be
#   used.
# @return The flow area of the cross section. The maximum elevation is used as
#   the upper boundary.
#
#' @importFrom utils head tail
#' @importFrom stats approx
calc_area = function(dist, elev, left.bank = NA, right.bank = NA,
  reference.elev = NA) {
  # add bank stations if they're not in the data
  if (length(dist) != length(elev))
    stop('Arguments "dist" and "elev" must be the same length')
  if (is.na(left.bank)) {
    left.bank = min(dist)
  } else if (left.bank < min(dist)) {
    warning("Left bank is outside of cross section extent. Returning NA",
      call. = FALSE)
  } else if (!any(abs(left.bank - dist) < 0.1)) {
    elev = c(elev, approx(dist, elev, xout = left.bank)$y)
    dist = c(dist, left.bank)
  }
  if (is.na(right.bank)) {
    right.bank = max(dist)
  } else if (right.bank > max(dist)) {
    warning("Right bank is outside of cross section extent. Returning NA",
      call. = FALSE)
  } else if (!any(abs(right.bank - dist) < 0.1)) {
    elev = c(elev, approx(dist, elev, xout = right.bank)$y)
    dist = c(dist, right.bank)
  }
  if ((right.bank > max(dist)) || (left.bank < min(dist)))
    return(NA)
  if (is.na(reference.elev))
    reference.elev = max(elev)
  # rescale
  new.right.bank = right.bank - min(dist)
  new.left.bank = left.bank - min(dist)
  new.reference.elev = reference.elev - min(elev)
  new.elev = elev - min(elev)
  dist.order = order(dist)
  new.dist = dist[dist.order] - min(dist)
  new.elev = new.elev[dist.order]
  idx = head(seq.int(
    from = which.min(abs(new.dist - new.left.bank)),
    to = which.min(abs(new.dist - new.right.bank)),
    by = 1L
  ), -1)
  if (length(idx) < 2L) {
    warning("Not enough points in cross section. Area is zero.")
    return(0)
  }
  traps = 0.5 * (tail(new.dist, -1) - head(new.dist, -1)) *
    (head(new.elev, -1) + tail(new.elev, -1))
  area.under = sum(traps[idx])
  new.reference.elev * (new.right.bank - new.left.bank) - area.under
}

#' Cross Section Area
#'
#' Compute cross section flow area directly from cross section gometry.
#'
#' @param d The cross section data, i.e. output of \code{read_xs}.
#' @param time.col The column containing time stamps.
#' @param station.col The column containing cross section station IDs.
#' @param distance.col The column containing cross section distances.
#' @param elevation.col The column containing cross section elevations.
#' @param bank.stations A table defining the bank station distances to use when
#'   computing cross section area. If \code{NULL}, the full extent of each cross
#'   section will be used. See details for more information.
#' @param reference.elevation Use predefined reference elevations when computing
#' cross section area. Can be a constant or a two column station of cross
#'   section labels and associated reference elevation. If \code{NULL}, the
#'   maximum elevation of each cross section will be used.
#' @return A wide-format table of cross section areas.
#'
#' @details The \code{bank.stations} argument can be formatted in multiple ways
#'   to accomodate different bank station definitions. If bank stations are
#'   fixed at the same distances in each cross section and do not change over
#'   time, \code{bank.stations} can be a two-element numeric vector specifying
#'   constant left and right bank stations, respectively. If bank stations vary
#'   by station but are fixed in time, \code{bank.stations} can be formatted as
#'   a three-column table with the first column containing station IDs, the
#'   second column containing the left bank stations and the third column
#'   containing the right bank stations. This can be used if e.g. the bank
#'   stations are read from a RAS output file using \code{read_bank_stations}.
#'   If bank stations vary both by station and across time, \code{bank.stations}
#'   can be formatted as a four column table with columns "Time", "Station",
#'   "LOB", "ROB". This can be used if e.g. the bank stations are read from a
#'   RAS output file using \code{read_bed_limits}.
#'
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @export
xs_area = function(d, time.col = "Time", station.col = "Station",
  distance.col = "Distance", elevation.col = "Elevation", bank.stations = NULL,
  reference.elevation = NULL) {
  # nse workaround
  Time = NULL; Station = NULL; Distance = NULL; Elevation = NULL
  LOB = NULL; ROB = NULL; RefElev = NULL; LE = NULL; RE = NULL
  # identify stations present at all times
  d = d %>% transmute_(Station = station.col, Time = time.col,
    Distance = distance.col, Elevation = elevation.col)
  # check station data
  xs.limits = xs_extents(d, station.col, time.col) %>%
    select(Station, LOB = LE, ROB = RE)
  if (any(is.na(xs.limits)))
    stop('Cross section data contains NA values. Check contents of "d"')
  # check bank stations
  if (is.null(bank.stations)) {
    d = d %>% left_join(xs.limits, by = "Station")
  } else if (is.numeric(bank.stations)) {
    d["LOB"] = bank.stations[[1]]
    d["ROB"] = bank.stations[[2]]
  } else if (ncol(bank.stations) == 3L) {
      # not time-dependent
      if (!all(names(bank.stations) %in% c("Station", "LOB", "ROB"))) {
        warning('Names of argument "bank.stations" not recognized. ',
          "Default column order is 'Station', 'LOB', 'ROB'")
        names(bank.stations) = c("Station", "LOB", "ROB")
      }
      d = d %>% left_join(bank.stations, by = "Station")
  } else if (ncol(bank.stations) == 4L) {
    # time-dependent
    if (!all(names(bank.stations) %in% c("Time", "Station", "LOB", "ROB"))) {
      warning('Names of argument "bank.stations" not recognized. ',
        "Default column order is 'Time', 'Station', 'LOB', 'ROB'")
      names(bank.stations) = c("Time", "Station", "LOB", "ROB")
    }
    d = d %>% left_join(bank.stations, by = c("Time", "Station"))
  } else {
    stop('Format of argument "bank.stations" not recognized')
  }
  # check if bank stations are defined for all cross sections
  if (any(is.na(d$LOB)))
    warning("LOB not defined for stations ",
      str_c(unique(d$Station[is.na(d$LOB)]), collapse = ", "))
  if (any(is.na(d$ROB)))
    warning("ROB not defined for stations ",
      str_c(unique(d$Station[is.na(d$ROB)]), collapse = ", "))
  # check reference elevation
  if (is.null(reference.elevation)) {
    reference.elevation = d %>% group_by(Station) %>%
      summarize(RefElev = max(Elevation))
    d = d %>% left_join(reference.elevation, by = "Station")
  }  else if (is.numeric(reference.elevation)) {
    d["RefElev"] = reference.elevation
  } else if (ncol(reference.elevation) == 2L) {
    if (!all(names(reference.elevation) %in% c("Station", "RefElev"))) {
      warning('Names of argument "reference.elevation" not recognized. ',
        "Default column order is 'Station', 'RefElev'")
      names(bank.stations) = c("Station", "RefElev")
    }
    d = d %>% left_join(reference.elevation, by = "Station")
  } else {
    stop('Format of argument "reference.elevation" not recognized')
  }
  # check if reference elevation defined for all cross sections
  if (any(is.na(d$RefElev)))
    warning('Argument "reference.elevation" does not contain information ',
      "for all cross sections")
  # compute area
  select.cols = list("Area", "Station", "Time")
  names(select.cols) = c("Area", station.col, time.col)
  d %>% group_by(Station, Time) %>%
    summarize(Area = calc_area(Distance, Elevation, unique(LOB),
      unique(ROB), unique(RefElev))) %>%
    select_(.dots = select.cols) %>%
    spread_(station.col, "Area", fill = NA)
}

#' Area to Volume
#'
#' Compute cross section volumes given cross section areas and lengths.
#'
#' @param d The area data, e.g. output of \code{xs_area}.
#' @param time.col The time column name.
#' @param station.lengths The cross section length values.
#' @return A wide-format table of cross section volumes.
#'
#' @import dplyr
#' @import tidyr
#' @export
area_to_volume = function(d, time.col = "Time", station.lengths = NULL) {
  # nse workaround
  Length = NULL; Area = NULL
  # check if all stations are present
  d = to_longtable(d, "Area", station.col = "Station")
  # Check station lengths
  if (is.null(station.lengths)) {
    warning("No station lengths provided. Default is unit length")
    d["Length"] = 1
  } else if (is.numeric(station.lengths)) {
    d["Length"] = station.lengths
  } else if (ncol(station.lengths == 2L)) {
    # not time-dependent
    if (!all(names(station.lengths) %in% c("Station", "Length"))) {
      warning('Names of argument "station.lengths" not recognized. ',
        "Default column order is 'Station', 'Length'")
      names(station.lengths) = c("Station", "length")
    }
    d = d %>% left_join(station.lengths, by = "Station")
  } else if (ncol(station.lengths) == 3L) {
    if (all(names(station.lengths) %in% c("Station", "LOB", "Channel", "ROB")))
      stop('Argument "station.lengths" has not been processed. ',
        'Process with function xs_lengths()')
    # time-dependent
    if (!all(names(station.lengths) %in% c("Time", "Station", "Length"))) {
      warning('Names of argument "station.lengths" not recognized. ',
        "Default column order is 'Time', 'Station', 'Length'")
      names(station.lengths) = c("Time", "Station", "Length")
    }
    d = d %>% left_join(station.lengths, by = c("Time", "Station"))
  } else {
    stop('Format of argument "station.lengths" not recognized')
  }
  if (any(is.na(d$Length)))
    warning("Some station lengths not defined")
  # compute volume and convert to wide table
  d %>% mutate(Volume = Length * Area) %>%
    select_(time.col, "Station", "Volume") %>%
    spread_("Station", "Volume", fill = NA)
}

#' Cross Section Cumulative Change
#'
#' Compute cumulative cross section change directy from cross section data.
#'
#' @inheritParams xs_area
#' @inheritParams area_to_volume
#' @inheritParams change_table
#' @inheritParams cumulative_table
#'
#' @details This is essentially a wrapper for processing cross section data
#'   through the sequence \code{xs_area} --> \code{area_to_volume} -->
#'   \code{change_table} --> \code{cumulative_table.}
#'
#' @import dplyr
#' @export
xs_cumulative_change = function(d, time.col = "Time", station.col = "Station",
  distance.col = "Distance", elevation.col = "Elevation", bank.stations = NULL,
  reference.elevation = NULL, station.lengths = NULL, over.time = TRUE,
  longitudinal = TRUE, direction = "downstream"){
  d %>% xs_area(time.col, station.col, distance.col, elevation.col,
      bank.stations, reference.elevation) %>%
    area_to_volume(time.col, station.lengths) %>%
    change_table(time.col) %>%
    cumulative_table(time.col, over.time, longitudinal, direction)
}

#' Cross Section Region Area
#'
#' Compute cross section area by region.
#'
#' @inheritParams xs_area
#' @param extent.stations The maximum extents of each station. Expects a
#'   three-column table with column names "Station", "LE" and "RE, where "LE"
#'   and "RE" define the left extent and right extent of each station,
#'   respectively. Can also accept a two-element numeric vector specifying
#'   constant left and right edges. If NULL, the minimum and maximum coincident
#'   distances of each cross section will be used.
#' @param region The region(s) to compute area for. Can be any combination of
#'   "channel", "LOB", "ROB". See details for more information.
#' @return A wide-format table with additional column "Region" specifying the
#'   region for which area was computed.
#'
#' @details The \code{extent.stations} argument is similar to the
#'   \code{bank.stations} argument, but instead defines the coincident extent
#'   of each cross section. Unlike \code{bank.stations}, cross section
#'   extents are not allowed to vary through time.
#'
#'   The \code{region} argument is used to separate cross section area
#'   computations based on bank and extent definitions. The "LOB" region is
#'   bounded by the left extent and the left bank station; the "Channel" is
#'   bounded by the left and right bank stations; and the "ROB" region is
#'   bounded by the right bank station and the right extent.
#'
#' @import dplyr
#' @export
xs_regions = function(d, time.col = "Time", station.col = "Station",
  distance.col = "Distance", elevation.col = "Elevation", bank.stations,
  extent.stations = NULL, reference.elevation = NULL,
  region = c("LOB", "Channel", "ROB")) {
  # nse workaround
  Station = NULL; Time = NULL; Distance = NULL; LE = NULL; RE = NULL
  ROB = NULL; LOB = NULL
  # check regions
  if (!all(region %in% c("LOB", "Channel", "ROB")))
    stop('Value of argument "region" not recognized' )
  # check bank stations
  if (missing(bank.stations)) {
    stop('Missing argument "bank.stations"')
  } else if (is.numeric(bank.stations)) {
    bank.stations = data_frame(Station = d[[station.col]],
      LOB = bank.stations[[1]], ROB = bank.stations[[2]])
  } else if (ncol(bank.stations) == 3L) {
    # not time-dependent
    if (!all(names(bank.stations) %in% c("Station", "LOB", "ROB"))) {
      warning('Names of argument "bank.stations" not recognized. ',
        "Default column order is 'Station', 'LOB', 'ROB'")
      names(bank.stations) = c("Station", "LOB", "ROB")
    }
  } else if (ncol(bank.stations) == 4L) {
    # time-dependent
    if (!all(names(bank.stations) %in% c("Time", "Station", "LOB", "ROB"))) {
      warning('Names of argument "bank.stations" not recognized. ',
        "Default column order is 'Time', 'Station', 'LOB', 'ROB'")
      names(bank.stations) = c("Time", "Station", "LOB", "ROB")
    }
  } else {
    stop('Format of argument "bank.stations" not recognized')
  }
  # check edge stations
  if (any(region %in% c("LOB", "ROB") && is.null(extent.stations))) {
    warning('Argument "extent.stations" not defined. ',
      'Default is coincident cross section extents')
    extent.stations = xs_extents(d, station.col, time.col, distance.col)
  } else if (is.numeric(extent.stations)) {
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
  all.stations = bank.stations %>%
    left_join(extent.stations, by = "Station")
  region.list = vector("list", length(region))
  names(region.list) = region
  if ("Channel" %in% region) {
    if ("Time" %in% names(all.stations))
      channel.stations = all.stations %>% select(Time, Station, LOB, ROB)
    else
      channel.stations = all.stations %>% select(Station, LOB, ROB)
    region.list[["Channel"]] = xs_area(d, time.col, station.col,
      distance.col, elevation.col, channel.stations, reference.elevation)
  }
  if ("LOB" %in% region) {
    if ("Time" %in% names(all.stations))
      lob.stations = all.stations %>%
        select(Time, Station, LOB = LE, ROB = LOB)
    else
      lob.stations = all.stations %>%
        select(Station, LOB = LE, ROB = LOB)
    region.list[["LOB"]] = xs_area(d, time.col, station.col,
      distance.col, elevation.col, lob.stations, reference.elevation)
  }
  if ("ROB" %in% region) {
    if ("Time" %in% names(all.stations))
      rob.stations = all.stations %>%
        select(Time, Station, LOB = ROB, ROB = RE)
    else
      rob.stations = all.stations %>%
        select(Station, LOB = ROB, ROB = RE)
    region.list[["ROB"]] = xs_area(d, time.col, station.col,
      distance.col, elevation.col, rob.stations, reference.elevation)
  }
  bind_rows(region.list, .id = "Region")
}

#' Region Area to Volume
#'
#' Compute cross section volumes by region.
#'
#' @inheritParams area_to_volume
#' @param region.col The name of the column containing the Region identifier.
#' @return A wide-format table of cross section region volumes.
#'
#' @import dplyr
#' @export
region_to_volume = function(d, time.col = "Time", region.col = "Region",
  station.lengths = NULL){
  # nse workaround
  . = NULL
  d %>% group_by_(region.col) %>% do(area_to_volume(., time.col,
    station.lengths))
}

#' Cross Section Region Cumulative Change
#'
#' Compute cumulative cross section change by region directy from cross section
#' data.
#'
#' @inheritParams xs_cumulative_change
#' @inheritParams xs_regions
#'
#' @details This is essentially a wrapper for processing cross section data
#'   through the sequence \code{xs_regions} --> \code{region_to_volume} -->
#'   \code{change_sediment} --> \code{cumulative_sediment}.
#'
#' @import dplyr
#' @export
xs_region_cumulative_change = function(d, time.col = "Time",
  station.col = "Station", distance.col = "Distance",
  elevation.col = "Elevation", bank.stations, extent.stations = NULL,
  reference.elevation = NULL, region = c("LOB", "Channel", "ROB"),
  station.lengths = NULL, over.time = TRUE, longitudinal = TRUE,
  direction = "downstream"){
  d %>% xs_regions(time.col, station.col, distance.col, elevation.col,
    bank.stations, extent.stations, reference.elevation, region) %>%
    region_to_volume(time.col, "Region", station.lengths) %>%
    change_sediment(time.col, "Region") %>%
    cumulative_sediment(time.col, "Region", over.time, longitudinal, direction)
}
