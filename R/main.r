#' Automated Testing of HEC-RAS
#'
#' This package is designed to provide an automated testing environment
#' of HEC-RAS sediment transport model outputs. Functions are provided
#' for reading, analyzing and visualizing HDF5 output data and
#' generating test reports. See the vignette to get started.
#' @name RAStestR-package
#' @aliases RAStestR
#' @docType package
NULL

#' Drop Interpolated Cross Section Data
#'
#' Drop data from interpolated cross-sections.
#'
#' @param d A data table to drop interpolated cross section data from.
#' @return The data frame \code{d} without rows or columns corresponding to
#'   interpolated cross sections.
#'
#' @details Interpolated cross sections are identified by the presence of a
#'   '*' in the column name or value of the "Station" column (for long-format
#'   data).
#' @import stringr
#' @export
drop_interpolated_xs = function(d) {
  if (any(str_detect(names(d), "XS_")))
    d[, !str_detect(names(d), "[*]")]
  else if ("Station" %in% names(d))
    d[!str_detect(d$Station, "[*]"),]
  else
    stop("Format of 'd' not recognized. Could not find 'XS_' columns or ",
      "column 'Station'")
}

#' Rename Interpolated Cross Sections
#'
#' Rename the identifiers of interpolated cross sections.
#'
#' @param d A data table containing interpolated cross section data.
#' @return The data frame \code{d} with reformatted identifiers for interpolated
#'   cross sections.
#'
#' @details Interpolated cross sections are identified by the presence of a
#'   '*' in the column name or value of the "Station" column (for long-format
#'   data). The '*' symbol can interfere with certain selections or data
#'   manipulations. This function removes the '*' symbol from the Station
#'   identifiers.
#'
#' @import stringr
#' @export
rename_interpolated_xs = function(d){
  if (any(str_detect(names(d), "XS_")))
    names(d) = names(d) %>% str_replace("[*]", "")
  else if ("Station" %in% names(d))
    d["Station"] = d$Station %>% str_replace("[*]", "")
  else
    stop("Format of 'd' not recognized. Could not find 'XS_' columns or ",
      "column 'Station'")
  d
}
#' Read Standard Table
#'
#' Read a standard (not grain class-specific) table.
#'
#' @param f The h5 file to read.
#' @param table.name The table to read.
#' @param run.type The model run type, e.g. "quasi" or "unsteady".
#' @param which.times Character vector of timestamps to extract. If
#'   NULL, all timestamps will be returned.
#' @param which.stations Character vector of station numbers to extract. If
#'   NULL, all stations will be returned.
#' @return A dataframe with a column "Time" containing the Date Time
#'   Stamp data and columns "XS_####" where ### is the cross-section ID.
#'
#' @import stringr
#' @export
read_standard = function(f, table.name, run.type, which.times = NULL,
  which.stations = NULL) {
  geompath = get_station_table(run.type)
  tblpath = file.path(get_output_block(run.type), table.name)
  tspath = get_timestep_table(run.type)
  if (is.numeric(which.stations))
    which.stations = list_stations(f)[which.stations]
  res = read_hdtable(f, tblpath, tspath, geompath, run.type, "Time", "XS_")
  if (!is.null(which.times))
    res = res[res$Time %in% which.times,]
  if (nrow(res) < 1)
    stop("No data matching 'which.times' was found")
  if (!is.null(which.stations)) {
    othercols = !str_detect(names(res), "XS_")
    stationcols = names(res) %in% str_c("XS_", which.stations)
    if (length(stationcols) < 1)
      stop("No data matching 'which.stations' was found")
    res = res[, which(othercols | stationcols)]
  }
  res
}

#' Sediment By Grain Class Table
#'
#' Read the sediment data output for all grain classes.
#'
#' @inheritParams read_standard
#' @param which.grains Grain class tables to extract. Can accept either numeric
#'   grain class IDs or grain class labels. Label "ALL" or "" corresponds to
#'   the totals. If NULL, all grain
#'   classes will be returned.
#' @return A dataframe with a column "Time" containing the Date Time
#'   Stamp data; columns "XS_####" where ### is the cross-section ID;
#'   and column "GrainClass".
#'
#' @import h5
#' @import dplyr
#' @import stringr
#' @export
read_sediment = function(f, table.name, run.type, which.times = NULL,
  which.stations = NULL, which.grains = NULL) {
  grain.levels = c("", paste(1:20))
  grain.labels = list_grains(f)
  if (!is.null(which.grains)) {
    which.grains = as.character(which.grains)
    if (any(which.grains %in% grain.labels))
      which.grains[which.grains %in% grain.labels] = grain.levels[
        match(which.grains[which.grains %in% grain.labels], grain.labels)]
  }
  sedimentpath = file.path(get_sediment_block(run.type), table.name)
  alltables = list_sediment(f, sedimentpath)
  if (length(alltables) < 1)
    stop('Table "', sedimentpath, '" could not be found.', call. = FALSE)
  if (!is.null(which.grains)) {
    whichtables = str_c(table.name, which.grains, sep = " ") %>% str_trim()
    table.paths = alltables[basename(alltables) %in% whichtables]
  } else {
    table.paths = alltables
    which.grains = basename(table.paths) %>% str_replace(table.name, "") %>%
      str_trim()
  }
  table.names = basename(table.paths)
  res = vector("list", length(table.names))
  for (i in seq_along(table.names)) {
    res[[i]] = read_standard(f, table.names[[i]], run.type, which.times,
      which.stations)
    res[[i]]["GrainClass"] = factor(which.grains[i],
      levels = grain.levels, labels = grain.labels)
  }
  do.call(bind_rows, res)
}

#' Generic Table Read Function
#'
#' Read RAS sediment data output.
#'
#' @param f The h5 file to read.
#' @param table.path The table to read in.
#' @param row.table.path The table containing the row identifiers.
#' @param col.table.path The table containing the column identifiers.
#' @param run.type The model run type, e.g. "quasi" or "unsteady".
#' @param rowcolname The name to assign to the new row id column.
#' @param colprefix A prefix to apply to the column IDs.
#' @return A dataframe.
#'
#' @importFrom utils head
#' @importFrom utils tail
#' @import h5
#' @import dplyr
#' @import stringr
read_hdtable = function(f, table.path, row.table.path, col.table.path,
  run.type, rowcolname, colprefix) {
  if (!file.exists(f))
    stop("Could not find ", suppressWarnings(normalizePath(f)))
  x = h5file(f)
  on.exit(h5close(x))
  for (pth in c(table.path, row.table.path, col.table.path))
    if (!existsDataSet(x, pth))
      stop('Table "', pth, '" could not be found. ',
        'Check that argument "run.type" is correct.',
        call. = FALSE)
  clabs = x[col.table.path][] %>% str_trim()
  rlabs = x[row.table.path][] %>% str_trim()
  this = x[table.path][] %>% as_data_frame()
  if (run.type == "unsteady") {
    this = this %>% head(-1) #%>% tail(-1)
#    rlabs[2] = rlabs[1]
    rlabs = rlabs %>% head(-1) #%>% tail(-1)
  }
#  else if (run.type == "quasi") {
#    this = this %>% head(-1)
#    rlabs = rlabs %>% head(-1)
#  }
  clabs = str_c(colprefix, clabs)
  names(this) = clabs
  this[rowcolname] = rlabs
  this[c(rowcolname, clabs)]
}

#' Difference Table
#'
#' Compute a difference table.
#'
#' @param d1 The first dataframe, considered the "base" result.
#' @param d2 The second dataframe, considered the "new" result.
#' @param time.col The time column name.
#' @param difference.col The name of the difference column to be created.
#' @param relative Logical: report differences as relative difference.
#' @return A dataframe, with difference defined as \code{d2- d1}.
#'   if \code{relative = TRUE}, the difference is defined as
#'   \code{(d2 - d1)/(0.5*(d2 + d1))}.
#'
#' @import dplyr
#' @export
diff_table = function(d1, d2, difference.col, time.col = "Time",
  relative = FALSE) {
  ####
  # MODIFY TO OUTPUT WIDE TABLE
  ####
  if (missing(difference.col))
    stop('argument "difference.col" is missing, with no default')
  datime.cols = intersect(names(d1), names(d2))
  datime.cols = datime.cols[datime.cols != time.col]
  d1 = d1 %>% arrange_(time.col)
  d2 = d2 %>% arrange_(time.col)
  if (relative)
    fun = function(x1, x2)
      2 * (x2 - x1) / (x2 + x1)
    else
      fun = function(x1, x2)
        x2 - x1
  as_data_frame(cbind(d1[time.col], fun(d1[, datime.cols], d2[, datime.cols]))) %>%
    to_longtable(difference.col)
}

#' Difference Table (Sediment)
#'
#' Compute a difference table from sediment data.
#'
#' @inheritParams diff_table
#' @param grain.col the grain class column name.
#' @return A dataframe in long table format, with difference defined as
#'  \code{d2- d1}. If \code{relative = TRUE}, the difference is defined
#'  as \code{(d2 - d1)/(0.5*(d2 + d1))}
#'
#' @import dplyr
#' @export
diff_sediment = function(d1, d2, time.col, grain.col, difference.col,
  relative = FALSE) {
  ####
  # MODIFY TO OUTPUT WIDE TABLE
  ####
  datime.cols = intersect(names(d1), names(d2))
  datime.cols = datime.cols[datime.cols != time.col & datime.cols != grain.col]
  gvals = intersect(unique(d1[[grain.col]]), unique(d2[[grain.col]]))
  d1 = d1[d1[[grain.col]] %in% gvals,] %>% arrange_(time.col, grain.col)
  d2 = d2[d2[[grain.col]] %in% gvals,] %>% arrange_(time.col, grain.col)
  if (relative)
    fun = function(x1, x2)
      2 * (x2 - x1) / (x2 + x1)
  else
    fun = function(x1, x2)
      x2 - x1
  as_data_frame(cbind(d1[time.col], d1[grain.col], fun(d1[, datime.cols],
    d2[, datime.cols]))) %>% to_longtable(difference.col)
}

#' Root Mean Square Error Table
#'
#' Compute RMSE from a difference table.
#'
#' @param d The difference table.
#' @param group.col the column(s) to group differences by. For standard
#'   tables, \code{group.col} will typically be either \code{"Station"}
#'    or \code{"Time"}. For sediment tables, \code{group.col} will
#'   typically be either \code{c("GrainClass", "Station")} or
#'   \code{c("GrainClass", "Time")}.
#' @param difference.col the column containing difference values.
#' @param rmse.col The output column containing RMSE values
#' @return A dataframe.
#'
#' @importFrom stats setNames
#' @import dplyr
#' @import stringr
#' @export
rmse_table = function(d, group.col, difference.col, rmse.col) {
  ####
  # MODIFY TO TAKE WIDE TABLE AS INPUT
  # SPLIT INTO RMSE_TABLE AND RMSE_SEDIMENT
  ####
  d %>% group_by_(.dots = group.col) %>% summarize_(
    .dots = setNames(str_c("sqrt(mean(", difference.col, "^2))"), rmse.col))
}

#' Accumulate Data Over Time and/or Space
#'
#' Accumulate data from a table over time and/or longitudinally.
#'
#' @param d A wide-format table containing values to accumulate.
#' @inheritParams diff_table
#' @param over.time If \code{TRUE}, accumulate data across time steps. This
#'   is generally valid only for data output at the computation time step.
#' @param longitudinal If \code{TRUE}, accumulate data along the reach. This
#'   is generally only valid when all cross sections are included in \code{d}.
#' @param direction Accumulate data in the downstream (descending order of cross
#'   section IDs) or upstream (ascending order) direction. Ignored if
#'   \code{longitudinal} is \code{FALSE}.
#' @return A data frame containing the accumulated data from \code{d}. Note
#'   that \code{d} may be reordered in time and by cross-section.
#'
#' @import dplyr
#' @import stringr
#' @export
cumulative_table = function(d, time.col = "Time", over.time = TRUE,
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
    cum.d = as.matrix(ordered.d[ordered.xs])
    for (i in seq(nrow(cum.d)))
      cum.d[i,] = lon.fun(cumsum(lon.fun(cum.d[i,])))
    bind_cols(ordered.d[time.col], as_data_frame(cum.d))
  } else
    ordered.d
}

#' Accumulate Data Over Time and/or Space (Sediment)
#'
#' Accumulate data from a sediment table over time and/or longitudinally.
#'
#' @param d A wide-format table containing values to accumulate.
#' @inheritParams diff_sediment
#' @inheritParams cumulative_table
#' @return A data frame containing the accumulated data from \code{d}. Note
#'   that \code{d} may be reordered in time and by cross-section and grain
#'   class.
#'
#' @import dplyr
#' @import stringr
#' @export
cumulative_sediment = function(d, time.col = "Time", grain.col = "GrainClass",
  over.time = TRUE, longitudinal = TRUE,
  direction = c("upstream", "downstream")){
  # nse workaround
  . = NULL
  d %>% group_by_(grain.col) %>% do(cumulative_table(., time.col, over.time,
    longitudinal, direction))
}

#' Change Over Time
#'
#' Compute change over time from a table.
#'
#' @param d A wide format data table containing values to compute change
#'   over time.
#' @inheritParams cumulative_table
#' @return A wide-format table of change over time. A value of 0 is assigned to
#'   the first time step.
#'
#' @import dplyr
#' @import tidyr
#' @export
change_table = function(d, time.col = "Time") {
  # nse workaround
  Value = NULL; ftime = NULL; Station = NULL
  vol.d = d %>% to_longtable("Value", station.col = "Station") %>%
    mutate_(.dots = list(ftime = time.col)) %>%
    reformat_fields(list("Time" = "ftime")) %>%
    arrange(ftime) %>%
    group_by(Station) %>%
    mutate(Change = lag(Value) - Value)
  vol.d[vol.d$ftime == min(vol.d$ftime), "Change"] = 0
  vol.d %>% select_(time.col, "Station", "Change") %>%
    spread_("Station", "Change", fill = NA)
}

#' Change Over Time (Sediment)
#'
#' Compute change over time from a sediment data table.
#'
#' @param d A wide format data table containing values to compute change
#'   over time.
#' @inheritParams cumulative_sediment
#' @return A wide-format table of change over time. A value of 0 is assigned to
#'   the first time step.
#'
#' @import dplyr
#' @import tidyr
#' @export
change_sediment = function(d, time.col = "Time", grain.col = "GrainClass") {
  # nse workaround
  . = NULL
  d %>% group_by_(grain.col) %>% do(change_table(., time.col = time.col))
}

