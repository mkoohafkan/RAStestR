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
  xsoutputs = str_subset(xsoutputs, str_c(which.times, collapse = "|"))
  times.found = unlist(lapply(which.times, function(x)
    any(str_detect(xsoutputs, x))))
  if (!all(times.found)) {
    warning("The following times were not found in ", f, ": ",
      str_c(which.times[!times.found], collapse = ", "))
    which.times = which.times[times.found]
  }
  stations = list_stations(f)
  x = h5file(f)
  on.exit(h5close(x))
  dlist = vector("list", length(which.times))
  for (i in seq_along(which.times)) {
    this.time = which.times[i]
    xs = xsoutputs %>% str_subset(this.time)
    index.table = file.path(tblblock, str_c(xs, " info"))
    values.table = file.path(tblblock, str_c(xs, " values"))
    xs.indices = rep(stations, x[index.table][][,2])
    xs.table = as_data_frame(x[values.table][])
    names(xs.table) = c("Distance", "Elevation")
    xs.table["Station"] = str_c("XS_", xs.indices)
    xs.table["Time"] = this.time
    dlist[[i]] = xs.table
  }
  res = do.call(bind_rows, dlist)[c("Time", "Station", "Distance", "Elevation")]
  if (!is.null(which.stations))
    res = filter(res, Station %in% str_c("XS_", which.stations))
  res
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
#' @import h5
#' @import stringr
#' @import tidyr
#' @import dplyr
#' @export
read_bed_limits = function(f, run.type) {
  # get bank station data
  lob = read_standard(f, "Moveable Sta L", run.type)
  rob = read_standard(f, "Moveable Sta R", run.type)
  # get starting bank stations
  starting.banks = list_bank_stations(f)
  # insert starting bank station data
  station.cols = names(lob) %>% str_detect("XS_")
  lob[1, station.cols] = starting.banks[,1]
  rob[1, station.cols] = starting.banks[,2]

  list(LOB = lob, ROB = rob) %>%
    lapply(to_longtable, data.col = "Distance") %>%
    bind_rows(.id = "section") %>%
    to_widetable("section", "Distance") %>%
    arrange_("Time", "desc(Station)")
}

# Cross Section Area
#
# Compute cross section volumes from cross section data.
#
# @param distance The lateral cross section stations.
# @param elevation The cross section elevations at each station.
# @param left.bank The left bank station.
# @param right.bank The right bank station.
# @return The flow area of the cross section. The maximum elevation is used as
#   the upper boundary.
#
# @importFrom utils head tail
#
xs_area = function(distance, elevation, left.bank = NULL, right.bank = NULL) {
  if (is.null(left.bank))
    left.bank = min(distance)
  if (is.null(right.bank))
    right.bank = max(distance)
  # rescale
  new.right.bank = right.bank - min(distance)
  new.left.bank = left.bank - min(distance)
  new.distance = distance - min(distance)
  new.elevation = elevation - min(elevation)
  extract = head(seq.int(
    from = which.min(abs(new.distance - new.left.bank)),
    to = which.min(abs(new.distance - new.right.bank)),
    by = 1L
  ), -1)
  if (length(extract) < 2L)
    stop("Not enough points in cross section")
  traps = 0.5 * (tail(new.distance, -1) - head(new.distance, -1)) *
    (head(new.elevation, -1) + tail(new.elevation, -1))
  area.under = sum(traps[extract])
  max(new.elevation) * (new.right.bank - new.left.bank) - sum(area.under)
}

#' Cross Section Volume
#'
#' Compute cross section volumes from cross section data.
#'
#' @inheritParams read_xs
#' @param which.regions List of sections of the cross section to compute volume for.
#'   See details section for more information.
#' @param fix.bank.stations If \code{TRUE}, the separation between "bed" and
#'   "bank" regions will be defined by the starting moveable bed limits. If
#'   \code{FALSE}, the separation between regions vary through time according to
#'    values in the Moveable Bed Station tables.
#' @return A data frame with columns "Time", "Station", "Volume" and "Region".
#'
#' @details The boundaries of a cross section volume element are defined as
#'   halfway between the cross section and the next upstream and downstream
#'   cross sections. For cross sections at upstream or downstream boundaries of
#'   a reach a "half-volume" is computed, i.e. the cross section volume boundary
#'   is not extended beyond the reach boundaries.
#'
#'   Volume change can be computed for separate sections of the cross section
#'   through the argument \code{which.regions}. The following choices are available:
#'   \describe{
#'     \item{"total"}{Compute the volume of the entire cross section.}
#'     \item{"bed"}{Compute bed change volume only. The bed is defined as the region spanned by the moveable bed limits.}
#'     \item{"banks"}{Compute the bank change volume only. The banks are defined by the region outside of the moveable bed limits.}
#'     \item{"left bank"}{Compute the left bank change volume only.}
#'     \item{"right bank"}{Compute the right bank change volume only.}
#'   }
#'   Note that the section "banks" is equal to the sum of the "left bank" and
#'   "right bank" sections, and "total" is equal to the sum of "bed" and
#'   "banks".
#'
#' @import dplyr
#' @import stringr
#' @export
xs_volume = function(f, run.type, which.times = NULL, which.stations = NULL,
  which.regions = c("total", "bed", "banks", "left bank", "right bank"),
  fix.bank.stations = FALSE) {
  # nse workaround
  Station = NULL; Time = NULL; Elevation = NULL; Distance = NULL; Length = NULL
  upstream = NULL; downstream = NULL; Width = NULL; Area = NULL
  LOB = NULL; ROB = NULL; left.bank = NULL; right.bank = NULL
  Volume.x = NULL; Volume.y = NULL;
  # check which.regions
  which.regions = str_to_lower(which.regions)
  if (!all(which.regions  %in% c("total", "bed", "banks", "left bank", "right bank")))
    stop('Some values of argument "which.regions" not recognized')
  # get cross section distances
  length.table = list_lengths(f)
  station.table = list_stations(f)
  widths = data_frame(Station = station.table, Length = length.table[,2]) %>%
    mutate(downstream = 0.5*Length, upstream = 0.5*lag(Length, default = 0)) %>%
    transmute(Station = str_c("XS_", Station), Width = upstream + downstream)
  if (fix.bank.stations) {
    bank.stations = list_bank_stations(f)
    bed.limits = data_frame(Station = str_c("XS_", station.table),
      LOB = bank.stations[,1], ROB = bank.stations[,2])
    xs.data = read_xs(f, run.type, which.times, which.stations) %>%
      left_join(bed.limits, by = "Station")
  } else {
    bed.limits = read_bed_limits(f, run.type)
    xs.data = read_xs(f, run.type, which.times, which.stations) %>%
      left_join(bed.limits, by = c("Station", "Time"))

  }
  volumes.list = vector("list", length(which.regions))
  names(volumes.list) = which.regions
  if ("total" %in% which.regions) {
    volumes.list[["total"]] = xs.data %>%
      group_by(Station, Time) %>%
      summarise(Area = xs_area(Distance, Elevation)) %>% ungroup %>%
      left_join(widths, by = "Station") %>%
      transmute(Time, Station, Volume = Width * Area)
  }
  if ("bed" %in% which.regions) {
    volumes.list[["bed"]] = xs.data %>%
      group_by(Station, Time) %>%
      summarise(left.bank = unique(LOB), right.bank = unique(ROB),
        Area = xs_area(Distance, Elevation, left.bank, right.bank)) %>%
      ungroup %>%
      left_join(widths, by = "Station") %>%
      transmute(Time, Station, Volume = Width * Area)
  }
  if (any(c("banks", "left bank") %in% which.regions)) {
    volumes.list[["left bank"]] = xs.data %>%
      group_by(Station, Time) %>%
      summarise(left.bank = unique(LOB),
        Area = xs_area(Distance, Elevation, right.bank = left.bank)) %>%
      ungroup %>%
      left_join(widths, by = "Station") %>%
      transmute(Time, Station, Volume = Width * Area)
  }
  if (any(c("banks", "right bank") %in% which.regions)) {
    volumes.list[["right bank"]] = xs.data %>%
      group_by(Station, Time) %>%
      summarise(right.bank = unique(ROB),
        Area = xs_area(Distance, Elevation, left.bank = right.bank)) %>%
      ungroup %>%
      left_join(widths, by = "Station") %>%
      transmute(Time, Station, Volume = Width * Area)
    }
  if ("banks" %in% which.regions) {
    volumes.list[["banks"]] = volumes.list[["left bank"]] %>%
      full_join(volumes.list[["right bank"]], by = c("Time", "Station")) %>%
      transmute(Time, Station, Volume = Volume.x + Volume.y)
  }
  volumes.list[which.regions] %>% bind_rows(.id = "Region")
}

#' Cross Section Volume Change
#'
#' Compute cross-section volume change from volume data.
#'
#' @param d The volume data.
#' @param data.col The column containing volume data.
#' @param station.col The column containing Station IDs.
#' @param time.col The column containing times.
#' @param region.col The column containing region IDs. Ignored if
#'   \code{region} is \code{NULL}.
#' @param region The region to compute volume change for. If multiple
#'   values are provided, the function will add the volumes of the specified
#'   regions before computing change. Results will be erroneous if overlapping
#'   regions are specified. If NULL, the function assumes the supplied
#'   data is for a single region only. If \code{region} is  not named, the
#'   function will assume the region column is named "Region". If \code{region}
#'   is named, the name will be used to identify the column containing region
#'   information.
#' @param cumulative If \code{TRUE}, accumulate volume change over time.
#' @param longitudinal If \code{TRUE}, accumulate volume change in the
#'   downstream direction. Only valid when all cross sections are included in
#'   \code{d}.
#'  @return A wide data table containing volume change. If \code{over.time} or
#'    \code{longitudinal} are \code{TRUE}, data may be reordered.
#'
#' @details The \code{cumulative} and \code{longitudinal} arguments are simply
#'   flags to call \code{accumulate_data} on the output data using the same
#'   conventions as standard RAS output. If \code{cumulative} is \code{TRUE} and
#'   \code{longitudinal} is \code{FALSE}, the output data will essentially match
#'   Vol Bed Change Cum RAS output. If both \code{cumulative} and
#'   \code{longitudinal} are \code{TRUE}, the output data will essentially match
#'   the Long. Cum Vol Change RAS output.
#'
#'  There may be some significant differences between the volume change computed
#'  by this function and the equivalent RAS output, depending on the cross
#'  section update tolerances specified in RAS and whether bank failure
#'  mechanisms (i.e. BSTEM) are incorporated into the model.
#'
#' @import dplyr
#' @import tidyr
#' @export
xs_volume_to_change = function(d, data.col = "Volume", station.col = "Station",
  time.col = "Time", region.col = "Region", region = NULL, cumulative = TRUE,
  longitudinal = TRUE) {
  # nse workaround
  Volume = NULL
  if (!is.null(region)) {
      d = filter_(d, str_c(sprintf("(%s == '%s')", region.col, region),
        collapse = "|"))
  }
  vc = d %>% group_by_(station.col, time.col) %>%
    summarize_(.dots = list(Volume = sprintf("sum(%s)", data.col))) %>%
    ungroup %>%
    mutate_(.dots = list(old.time = time.col)) %>%
    reformat_fields(list("Time" = time.col)) %>%
    arrange_(.dots = list(time.col)) %>%
    group_by_(station.col) %>%
    mutate(Change = lag(Volume) - Volume) %>%
    mutate_(.dots = setNames(list("old.time"), time.col)) %>%
    select_(time.col, station.col, "Change") %>%
    spread_(station.col, "Change", fill = 0)
  if (cumulative || longitudinal)
    accumulate_data(vc, time.col, cumulative, longitudinal, "downstream")
  else
    vc
}

#' Accumulate Data Over Time and/or Space
#'
#' Accumulate data over time and/or longitudinally.
#'
#' @param d The data frame containing values to accumulate.
#' @inheritParams xs_volume_to_change
#' @param over.time If \code{TRUE}, accumulate data across time steps. Unless
#'   working with cross-section volume change, this is only valid for data
#'   output at the computation time step.
#' @param longitudinal If \code{TRUE}, accumulate data along the reach. Only
#'   valid when all cross sections are included in \code{d}.
#' @param direction Accumulate data in the downstream (descending order of cross
#'   section IDs) or upstream (ascending order) direction. Ignored if
#'   \code{longitudinal} is \code{FALSE}.
#' @return A data frame containing the accumulated data from \code{d}. Note
#' that \code{d} may be reordered in time and by cross-section.
#'
#' @import dplyr
#' @import stringr
#' @export
accumulate_data = function(d, time.col = "Time", over.time = TRUE,
  longitudinal = TRUE, direction = c("upstream", "downstream")){
  # nse workaround
  . = NULL
  time.order = d %>% reformat_fields(time.col) %>% `[[`(time.col) %>% order()
  xs.order = names(d) %>% str_subset("XS_") %>% data_frame(Station = .) %>%
    reformat_fields("Station") %>% `[[`("Station") %>% order()
  ordered.xs = names(d) %>% str_subset("XS_") %>% `[`(xs.order)
  ordered.d = d[time.order, c("Time", ordered.xs)]
  if (over.time)
    for (oxs in ordered.xs)
      ordered.d[oxs] = cumsum(ordered.d[[oxs]])
  if (longitudinal) {
    direction = match.arg(direction, c("upstream", "downstream"))
    if (direction == "downstream")
      lon.fun = rev
    else
      lon.fun = identity
    xs.d = as.matrix(ordered.d[ordered.xs])
    for (i in seq(nrow(xs.d)))
      xs.d[i,] = lon.fun(cumsum(lon.fun(xs.d[i,])))
    bind_cols(ordered.d["Time"], as_data_frame(xs.d))
  } else
    ordered.d
}
