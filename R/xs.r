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
  xsoutputs = list_xsections(f, tblblock)
  if (is.null(which.times))
    which.times = str_split_fixed(xsoutputs, "[(.+)]", 3)[,2]
  xsoutputs = str_subset(xsoutputs, str_c(which.times, collapse = "|"))
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

#' Cross Section Volume
#'
#' Compute cross section volumes from cross section data.
#'
#' @inheritParams read_xs
#'
#' @details The boundaries of a cross section volume element are defined as
#'   halfway between the cross section and the next upstream and downstream
#'   cross sections. For cross sections at upstream or downstream boundaries of
#'   a reach a "half-volume" is computed, i.e. the cross section volume boundary
#'   is not extended beyond the reach boundaries.
#'
#' @import dplyr
#' @import stringr
#' @export
xs_volume = function(f, run.type, which.times = NULL, which.stations = NULL){
  # nse workaround
  Station = NULL; Time = NULL; Elevation = NULL; Distance = NULL; Length = NULL
  upstream = NULL; downstream = NULL; Width = NULL; Area = NULL; Volume = NULL
  # get cross section distances
  length.table = list_lengths(f)
  station.table = list_stations(f)
  widths = data_frame(Station = station.table, Length = length.table[,2]) %>%
    mutate(downstream = 0.5*Length, upstream = 0.5*lag(Length, default = 0)) %>%
    transmute(Station = str_c("XS_", Station), Width = upstream + downstream)
  areafun = function(el, ds){
    el = el - min(el)
    ds = ds - min(ds)
    max(el) * max(ds) - 0.5 * sum(diff(ds) * (head(el, -1) + tail(el, -1)))
  }
  read_xs(f, run.type, which.times, which.stations) %>%
    group_by(Station, Time) %>%
    summarise(Area = areafun(Elevation, Distance)) %>% ungroup %>%
    left_join(widths, by = "Station") %>% mutate(Volume = Width * Area) %>%
    select(Time, Station, Volume)
}

#' Cross Section Volume Change
#'
#' Compute cross-section volume change from volume data.
#'
#' @param d The volume data.
#' @param station.col The column containing Station IDs.
#' @param time.col The column containing times.
#' @param cumulative If \code{TRUE}, accumulate volume change over time.
#' @param longitudinal If \code{TRUE}, accumulate volume change in the
#'   downstream direction. Only valid when all cross sections are included in
#'   \code{d}.
#'  @return A wide data table containing volume change. If \code{over.time} or
#'    \code{longitudinal} are \code{TRUE}, data may be reordered.
#'
#' @import dplyr
#' @import tidyr
#' @export
xs_volume_to_change = function(d, station.col = "Station", time.col = "Time",
  cumulative = TRUE, longitudinal = TRUE) {
  # nse workaround
  Volume = NULL
  vc = d %>% mutate_(.dots = list(old.time = time.col)) %>%
    reformat_fields(list("Time" = time.col)) %>%
    arrange_(.dots = list(time.col)) %>%
    group_by_(station.col) %>% mutate(Change = lag(Volume) - Volume) %>%
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

#' Cross Section Longitudinal Cumulative Volume Change
#'
#' Build a longitudinal cumulative volume change curve from cross section data.
#'
#' @param d The data table of cross section volumes, e.g. output from \code{xs_volume}
#' @inheritParams xs_volume_to_change
#' @inheritParams accumulate_data
#' @return A data frame of longitudinal cumulative volume change in wide-table
#'   format.
#'
#' @details This function is simply a wrapper that applies \code{xs_volume},
#'   \code{xs_volume_to_change}, and \code{accumulate_data} to generate a
#'   longitudinal cumulative volume change curve.
#'
#' @import dplyr
#' @export
xs_cumulative_change = function(d, station.col = "Station", time.col = "Time",
  direction = "downstream"){
  d %>% xs_volume_to_change(station.col, time.col) %>%
    accumulate_data(time.col, over.time = TRUE, longitudinal = TRUE, direction)
}

