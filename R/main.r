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

# Get RAS Plan Meta Data
#
# Get meta data of RAS plan.
#
# @param f The HDF5 file to read.
# @return A named list of plan meta data.
#
# @examples
# simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#   package = "RAStestR")
# RAStestR:::get_meta(simple.quasi)
#
#' @import hdf5r
get_meta = function(f, mode = 'r') {
  run.type = get_run_type(f)
  x = H5File$new(f, mode = 'r')
  on.exit(x$close_all())
  tryCatch({
  plan.attr = c(
      get_group_attr(x),
      get_group_attr(x, "Plan Data/Plan Information"),
      get_group_attr(x, "Plan Data/Plan Parameters")
    )
  }, error = function(e) {
       warning(e)
       stop("Could not find Plan Data", call. = FALSE)
     }
  )
  plan.attr[["Type of Run"]] = run.type
  return(plan.attr)
}

# Get RAS Plan Run Type
#
# Identify a RAS plan as Unsteady, Steady or QuasiUnsteady.
#
# @inheritParams get_meta
#
# @examples
# simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#   package = "RAStestR")
# RAStestR:::get_run_type(simple.quasi)
#
# simple.unsteady= system.file("sample-data/SampleUnsteady.hdf",
#   package = "RAStestR")
# RAStestR:::get_run_type(simple.unsteady)
#
#' @import hdf5r
get_run_type = function(f) {
  x = H5File$new(f, mode = 'r')
  on.exit(x$close_all())
  event.x = tryCatch(x$open("Event Conditions"),
    error = function(e) {
      warning(e)
      stop("Could not find Event Conditions", call. = FALSE)
    }
  )
  on.exit(event.x$close(), add = TRUE)
  event.groups = basename(event.x$ls(recursive = FALSE)$name)
  if ("QuasiUnsteady" %in% event.groups)
    "QuasiUnsteady"
  else if ("Unsteady" %in% event.groups)
    if ("Sediment" %in% event.groups)
      "Unsteady+Sediment"
    else
      "Unsteady"
  else if ("Steady" %in% event.groups)
    "Steady"
  else
    stop("Could not find run type specification 'Steady', ",
      "'Unsteady' or 'QuasiUnsteady'")
}

# Read HDF Table
#
# Safely read in a specific HDF table. This function is used internally and
# should not be called directly by the user.
#
# @param x an HDF object, e.g. output of \code{H5File$new()}.
# @param path The table path.
# @param type The type of data contained in the table. Can be one of "double",
# "integer", or "character".
#
# @examples
# simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#   package = "RAStestR")
# tryCatch({
#     test.table = "Geometry/Cross Sections/Bank Stations"
#     quasi.h5 = hdf5r::H5File$new(simple.quasi)
#     RAStestR:::get_dataset(quasi.h5, test.table, "double")
#   }, finally = {
#     invisible(try(quasi.h5$close_all()))
#   }) 
#
#' @import hdf5r
get_dataset = function(x, path) {
  g = tryCatch(
        x$open(path),
        error = function(e) {
          warning(e)
          stop("Could not find ", x, call. = FALSE)
    }
  )
  on.exit(g$close())
  if (length(g$dims) == 2L)
    t(g$read())
  else
    g$read()
}

# Read HDF Attributes
#
# Safely read attributes of an HDF Group
#
# @inheritParams get_dataset
# @param attrs The attributes to read. If \code{NULL}, all 
#   attributes will be read.
# @return a named list of attributes.
#
# @examples
# simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#   package = "RAStestR")
#
# tryCatch({
#     quasi.h5 = hdf5r::H5File$new(simple.quasi)
#     RAStestR:::get_group_attr(quasi.h5)
#   }, finally = {
#     invisible(try(quasi.h5$close_all()))
#   }) 
#
# tryCatch({
#     test.group = "Plan Data/Plan Information"
#     quasi.h5 = hdf5r::H5File$new(simple.quasi)
#     RAStestR:::get_group_attr(quasi.h5, test.group)
#   }, finally = {
#     invisible(try(quasi.h5$close_all()))
#   }) 
#
#' @import hdf5r
get_group_attr = function(x, path = NULL, attrs = NULL) {
  if (!is.null(path)) {
    group.x = x$open(path)
    on.exit(group.x$close())
  } else {
    group.x = x
  }
  if (is.null(attrs))
    attrs = h5attr_names(group.x)
  group.attr = lapply(attrs, function(a)
    group.x$attr_open_by_name(a, ".")$read())
  names(group.attr) = attrs
  group.attr
}

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
#'
#' @examples
#' simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#'   package = "RAStestR")
#' quasi.flow = read_standard(simple.quasi, "Flow")
#' drop_interpolated_xs(quasi.flow)
#'
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
#' @examples
#' simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#'   package = "RAStestR")
#' quasi.flow = read_standard(simple.quasi, "Flow")
#' rename_interpolated_xs(quasi.flow)
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
#' @param f The HDF5 file to read.
#' @param table.name The table to read.
#' @param which.times Character vector of timestamps to extract. If
#'   NULL, all timestamps will be returned.
#' @param which.stations Character vector of stations to extract. If
#'   NULL, all stations will be returned.
#' @param override.sediment (For Unsteady+Sediment models only) If True,
#'   extract data from the hydraulic rather than sediment output.
#' @return A dataframe with a column "Time" containing the Date Time
#'   Stamp data and columns "XS_####" where ### is the cross-section ID.
#'
#' @examples
#' simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#'   package = "RAStestR")
#' read_standard(simple.quasi, "Flow")
#'
#' simple.unsteady = system.file("sample-data/SampleUnsteady.hdf",
#'   package = "RAStestR")
#' read_standard(simple.unsteady, "Flow")
#'
#' read_standard(simple.quasi, "Flow", which.times = "11DEC1990 01:00:00",
#'   which.stations = c("XS_800", "XS_796.00*"))
#'
#' read_standard(simple.quasi, "Flow", which.times = 2:3,
#'   which.stations = 1:4)
#'
#' @import stringr
#' @export
read_standard = function(f, table.name, which.times = NULL,
  which.stations = NULL, override.sediment = FALSE) {
  # get run type
  run.type = get_run_type(f)
  ras.version = get_RAS_version(f)
  if (run.type == "Unsteady+Sediment" && override.sediment)
    run.type == "Unsteady"
  # argument checks
  output.times = list_output_times(f)
  if (is.null(which.times))
    which.times = seq_along(output.times)
  else if (!is.numeric(which.times))
    which.times = which(output.times %in% which.times)
  else
    which.times = which(seq_along(output.times) %in% which.times)
  if (length(which.times) < 1L)
    stop("No data matching 'which.times' was found")
  stations = list_stations(f)
  if (is.null(which.stations))
    which.stations = seq_along(stations)
  else if (!is.numeric(which.stations))
    which.stations = which(stations %in% str_replace(which.stations, 
      "XS_", ""))
  else
    which.stations = which(seq_along(stations) %in% which.stations)
  if (length(which.stations) < 1L)
    stop("No data matching 'which.stations' was found")
  # specify tables
  tblpath = file.path(get_output_block(run.type, ras.version), table.name)
  # read data
  res = read_hdtable(f, tblpath, "Time", output.times,
    str_c("XS_", stations))[[1]]
  # filter by time/station
  othercols = which(!str_detect(names(res), "XS_"))
  stationcols = which(str_detect(names(res), "XS_"))[which.stations]
  res[which.times, c(othercols, stationcols)]
}

#' Sediment By Grain Class Table
#'
#' Read the sediment data output for all grain classes.
#'
#' @inheritParams read_standard
#' @param which.grains Grain class tables to extract. Can accept either numeric
#'   grain class IDs or grain class labels. Label "ALL" or "" corresponds to
#'   the totals. If NULL, all grain classes will be returned.
#' @return A dataframe with a column "Time" containing the Date Time
#'   Stamp data; columns "XS_####" where ### is the cross-section ID;
#'   and column "GrainClass".
#'
#' @examples
#' simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#'   package = "RAStestR")
#' read_sediment(simple.quasi, "Vol Out Cum")
#'
#' read_sediment(simple.quasi, "Vol Out Cum", 
#'   which.grains = c("6", "7"))
#' read_sediment(simple.quasi, "Vol Out Cum", 
#'   which.grains = c("VFS", "FS"))
#'
#' @import hdf5r
#' @import dplyr
#' @import stringr
#' @export
read_sediment = function(f, table.name, which.times = NULL,
  which.stations = NULL, which.grains = NULL) {
  # nse workaround
  GrainClass = NULL
  # get run type
  run.type = get_run_type(f)
  ras.version = get_RAS_version(f)
  # argument checks
  grain.labels = list_grain_classes(f)
  grain.levels = c("", paste(1:20))
  if (!is.null(which.grains)) {
    given.grains = which.grains
    if (any(which.grains %in% grain.labels)) {
      error.grains = setdiff(which.grains, grain.labels)
      if (length(error.grains) > 0L)
        stop("Grain class ID not recognized: ",
          paste(error.grains, collapse =", "))
      which.grains = grain.levels[grain.labels %in% which.grains]
    }
    else if (any(which.grains %in% grain.levels)) {
      error.grains = setdiff(which.grains, grain.levels)
      if (length(error.grains) > 0L)
        stop("Grain class ID not recognized: ",
          paste(error.grains, collapse =", "))
      which.grains = grain.levels[grain.levels %in% which.grains]
    }
    else
      stop("No data matching 'which.grains' was found")
    }
  else
    which.grains = grain.levels
  output.times = list_output_times(f)
  if (is.null(which.times))
    which.times = seq_along(output.times)
  else if (!is.numeric(which.times))
    which.times = which(output.times %in% which.times)
  else
    which.times = which(seq_along(output.times) %in% which.times)
  if (length(which.times) < 1L)
    stop("No data matching 'which.times' was found")
  stations = list_stations(f)
  if (is.null(which.stations))
    which.stations = seq_along(stations)
  else if (!is.numeric(which.stations))
    which.stations = which(stations %in% str_replace(which.stations, 
      "XS_", ""))
  else
    which.stations = which(seq_along(stations) %in% which.stations)
  if (length(which.stations) < 1L)
    stop("No data matching 'which.stations' was found")
  # get sediment tables
  sedimentpath = file.path(get_sediment_block(run.type, ras.version), table.name)
  included.grains = str_trim(str_replace(list_sediment(f, sedimentpath),
    sedimentpath, ""))
  if (length(included.grains) < 1)
    stop('Table "', sedimentpath, '" could not be found.', call. = FALSE)
  missing.grains = setdiff(which.grains, included.grains)
  selected.grains = intersect(which.grains, included.grains)  
#  if (length(missing.grains) > 0L)
#    warning("Some grain classes could not be found: ",
#      paste(grain.labels[grain.levels %in% missing.grains],
#        collapse = ", "))
  table.paths = str_trim(str_c(sedimentpath, selected.grains, sep = " "))
  table.labels = grain.labels[grain.levels %in% selected.grains]
  # read in data
  res.list = read_hdtable(f, table.paths, "Time", output.times,
    str_c("XS_", stations))
  res.list = lapply(res.list, function(tbl) tbl[which.times,])
  names(res.list) = table.labels
  res = bind_rows(res.list, .id = "GrainClass") %>%
    mutate(GrainClass = factor(GrainClass, levels = grain.labels))
  othercols = which(!str_detect(names(res), "XS_"))
  stationcols = which(str_detect(names(res), "XS_"))[which.stations]
  res[c(othercols, stationcols)]  
}

# Read RAS Table
#
# Read RAS sediment data output. This function is used internally and should
# not be called directly by the user.
#
# @param f The HDF55 file to read.
# @param table.path The table to read in.
# @param rowcolname The name to assign to the new row id column.
# @param rlabs A vector of row identifiers.
# @param clabs A vector of column identifiers.
# @return A dataframe.
#
# @examples
# simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#   package = "RAStestR")
# RAStestR:::read_hdtable(simple.quasi, 
#   file.path("Results", "Sediment", "Output Blocks", "Sediment", 
#     "Sediment Time Series", "Cross Sections", "Flow"),
#   file.path("Results", "Sediment", "Output Blocks", "Sediment", 
#     "Sediment Time Series", "Time Date Stamp"),
#   file.path("Geometry", "Cross Sections", "River Stations"),
#   "Time", "XS_")
#
#' @importFrom utils head
#' @importFrom utils tail
#' @import hdf5r
#' @import dplyr
#' @import stringr
read_hdtable = function(f, table.paths, rowcolname, rlabs, clabs) {
  if (!file.exists(f))
    stop("Could not find ", suppressWarnings(normalizePath(f)))
  # open file
  x = H5File$new(f, mode = 'r')
  on.exit(x$close_all())
  all.tables = list.datasets(x)
  missing.tables = setdiff(table.paths, all.tables)
  if (length(missing.tables) > 0L)
    stop("Could not find tables: ",
      paste(basename(missing.tables), collapse = ", "),
      call. = FALSE)
  process_table = function(pth) {
    this = as_data_frame(get_dataset(x, pth))
    names(this) = clabs
    this[rowcolname] = rlabs
    this[c(rowcolname, clabs)]
#    run.type = get_run_type(f)
#    if (run.type == "Unsteady") {
#      this = this %>% head(-1) #%>% tail(-1)
#      rlabs[2] = rlabs[1]
#      rlabs = rlabs %>% head(-1) #%>% tail(-1)
#    }
#    else if (run.type == "QuasiUnsteady") {
#      this = this %>% head(-1)
#      rlabs = rlabs %>% head(-1)
#    }
  }
  lapply(table.paths, process_table)
}

#' Difference Table
#'
#' Compute a difference table.
#'
#' @inheritParams operate_table
#' @param d1 The first dataframe, considered the "base" result.
#' @param d2 The second dataframe, considered the "new" result.
#' @inheritParams order_table
#' @param relative Logical: report differences as relative difference.
#' @param difference.col The name of the difference column to be created.
#' @return A dataframe, with difference defined as \code{d2- d1}.
#'   if \code{relative = TRUE}, the difference is defined as
#'   \code{(d2 - d1)/(0.5*(d2 + d1))}.
#'
#' @examples
#' simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#'   package = "RAStestR")
#' quasi.flow = read_standard(simple.quasi, "Flow")
#'
#' difference_table(quasi.flow, quasi.flow)
#' 
#' quasi.double = operate_table(quasi.flow, fun = function(x) 2*x)
#' difference_table(quasi.flow, quasi.double)
#' difference_table(quasi.flow, quasi.double, relative = TRUE)
#'
#' @import dplyr
#' @export
difference_table = function(d1, d2, relative = FALSE, partial = FALSE,
  difference.col = "Difference", time.col = "Time") {
  if (relative)
    fun = function(x1, x2)
      2 * (x2 - x1) / (x2 + x1)
    else
      fun = function(x1, x2)
        x2 - x1
  operate_table(d1, d2, fun = fun, partial = partial, time.col = time.col) %>%
    to_longtable(difference.col)  
}

#' Difference Table (Sediment)
#'
#' Compute a difference table from sediment data.
#'
#' @inheritParams difference_table
#' @param grain.col the grain class column name.
#' @return A dataframe in long table format, with difference defined as
#'  \code{d2- d1}. If \code{relative = TRUE}, the difference is defined
#'  as \code{(d2 - d1)/(0.5*(d2 + d1))}
#'
#' @examples
#' simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#'   package = "RAStestR")
#' quasi.volincum = read_sediment(simple.quasi, "Vol In Cum")
#'
#' difference_sediment(quasi.volincum, quasi.volincum)
#' 
#' quasi.double = operate_sediment(quasi.volincum , fun = function(x) 2*x)
#' difference_sediment(quasi.volincum, quasi.double)
#' difference_sediment(quasi.volincum, quasi.double, relative = TRUE)
#'
#' @import dplyr
#' @export
difference_sediment = function(d1, d2, relative = FALSE, partial = FALSE,
  difference.col = "Difference", time.col = "Time", grain.col = "GrainClass") {
  if (relative)
    fun = function(x1, x2)
      2 * (x2 - x1) / (x2 + x1)
  else
    fun = function(x1, x2)
      x2 - x1
  operate_sediment(d1, d2, fun = fun, partial = partial, time.col = time.col, 
    grain.col = grain.col) %>% to_longtable(difference.col)
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
#' @examples
#' simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#'   package = "RAStestR")
#' quasi.flow = read_standard(simple.quasi, "Flow")
#' quasi.double = operate_table(quasi.flow, fun = function(x) 2*x)
#' quasi.difference = difference_table(quasi.flow, quasi.double)
#'
#' rmse_table(quasi.difference, "Time", "Difference", "RMSE")
#' rmse_table(quasi.difference, "Station", "Difference", "RMSE")
#'
#' quasi.volincum = read_sediment(simple.quasi, "Vol In Cum")
#' quasi.double = operate_sediment(quasi.volincum, fun = function(x) 2 * x)
#' quasi.difference = difference_sediment(quasi.volincum, quasi.double)
#' rmse_table(quasi.difference, c("Time", "GrainClass"), "Difference", "RMSE")
#' rmse_table(quasi.difference, c("Station", "GrainClass"), "Difference", "RMSE")
#'
#' @importFrom stats setNames
#' @import dplyr
#' @import stringr
#' @export
rmse_table = function(d, group.col, difference.col = "Difference", 
  rmse.col = "RMSE") {
  d %>% group_by_(.dots = group.col) %>% 
    summarize_(
      .dots = setNames(str_c("sqrt(mean(", difference.col, "^2))"), rmse.col)
    ) %>%
    ungroup()
}

#' Accumulate Data Over Time and/or Space
#'
#' Accumulate data from a table over time and/or longitudinally.
#'
#' @param d A wide-format table containing values to accumulate.
#' @inheritParams difference_table
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
#' @examples
#' simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#'   package = "RAStestR")
#' quasi.flow = read_standard(simple.quasi, "Flow")
#'
#' cumulative_table(quasi.flow, over.time = TRUE, longitudinal = FALSE)
#' cumulative_table(quasi.flow, over.time = TRUE, longitudinal = TRUE)
#' cumulative_table(quasi.flow, over.time = TRUE, longitudinal = TRUE,
#'   direction = "downstream")
#'
#' @import dplyr
#' @import stringr
#' @export
cumulative_table = function(d, time.col = "Time", over.time = TRUE,
  longitudinal = TRUE, direction = c("upstream", "downstream")){
  # nse workaround
  . = NULL
  time.order = d %>%
    reformat_fields(time.col = time.col, station.col = NULL) %>%
    `[[`(time.col) %>% order()
  xs.order = names(d) %>% str_subset("XS_") %>% data_frame(Station = .) %>%
    reformat_fields(station.col = "Station", time.col = NULL) %>%
    `[[`("Station") %>% order()
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
#' @inheritParams difference_sediment
#' @inheritParams cumulative_table
#' @return A data frame containing the accumulated data from \code{d}. Note
#'   that \code{d} may be reordered in time and by cross-section and grain
#'   class.
#'
#' @examples
#' simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#'   package = "RAStestR")
#' quasi.voloutcum = read_sediment(simple.quasi, "Vol Out Cum")
#'
#' cumulative_sediment(quasi.voloutcum, over.time = FALSE)
#' cumulative_sediment(quasi.voloutcum, direction = "downstream")
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
    longitudinal, direction)) %>% ungroup()
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
#' @examples
#' simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#'   package = "RAStestR")
#' quasi.flow = read_standard(simple.quasi, "Flow")
#'
#' change_table(quasi.flow)
#'
#' @import dplyr
#' @import tidyr
#' @export
change_table = function(d, time.col = "Time") {
  # nse workaround
  Value = NULL; ftime = NULL; Station = NULL
  vol.d = d %>% to_longtable("Value", station.col = "Station") %>%
    mutate_(.dots = list(ftime = time.col)) %>%
    reformat_fields(time.col = "ftime", station.col = NULL) %>%
    arrange(ftime) %>%
    group_by(Station) %>%
    mutate(Change = lag(Value) - Value) %>%
    ungroup()
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
#' @examples
#' simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#'   package = "RAStestR")
#' quasi.voloutcum = read_sediment(simple.quasi, "Vol Out Cum")
#'
#' change_sediment(quasi.voloutcum)
#'
#' @import dplyr
#' @export
change_sediment = function(d, time.col = "Time", grain.col = "GrainClass") {
  # nse workaround
  . = NULL
  d %>% group_by_(grain.col) %>% 
    do(change_table(., time.col = time.col)) %>%
    ungroup()
}

#' Order Table
#'
#' Reorder a table by time and cross section.
#'
#' @param d A wide-format table.
#' @param time.col The time column name.
#' @return the data frame \code{d}, ordered by time and cross section.
#'
#' @examples
#' simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#'   package = "RAStestR")
#' quasi.flow = read_standard(simple.quasi, "Flow")
#' 
#' quasi.disordered = quasi.flow[sample(1:nrow(quasi.flow), nrow(quasi.flow)),]
#' order_table(quasi.disordered)
#'
#' @import stringr
#' @import dplyr
#' @export
order_table = function(d, time.col = "Time") {
  # nse workaround
  . = NULL; time.order = NULL; Station = NULL; station.num = NULL
  col.names = names(d)
  station.cols = col.names %>% `[`(str_detect(., "XS_")) %>%
    data_frame(Station = .) %>% mutate(station.num = Station) %>%
    reformat_fields(station.col = "station.num", time.col = NULL) %>%
    arrange(station.num) %>% `[[`("Station")
  other.cols = setdiff(col.names, station.cols)
  d["time.order"] = d[[time.col]]
  d %>% reformat_fields(time.col = "time.order", station.col = NULL) %>%
    arrange(time.order) %>% `[`(c(other.cols, station.cols))
}

#' Order Table (Sediment)
#'
#' Reorder a sediment data table by time and cross section.
#'
#' @inheritParams order_table
#' @inheritParams change_sediment
#' @return the data frame \code{d}, ordered by time, cross section and grain class.
#'
#' @examples
#' simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#'   package = "RAStestR")
#' quasi.voloutcum = read_sediment(simple.quasi, "Vol Out Cum")
#'
#' quasi.disordered = quasi.voloutcum[sample(1:nrow(quasi.voloutcum), 
#'   nrow(quasi.voloutcum)),]
#' order_sediment(quasi.voloutcum)
#'
#' @import stringr
#' @import dplyr
#' @export
order_sediment = function(d, time.col = "Time", grain.col = "GrainClass") {
  # nse workaround
  . = NULL
  d %>% arrange_(grain.col) %>% group_by_(grain.col) %>%
    do(order_table(., time.col = time.col)) %>% ungroup()
}

#' Table Operations
#'
#' Combine tables via an operation, e.g. addition or multiplication.
#'
#' @param ... Arbitrary number of wide-format data tables to combine.
#' @param fun A function to apply. If multiple tables are supplied in \code{...},
#'   \code{fun} must either be one of the strings "+", "-", "*" and "/" or be 
#'   a function that accepts exactly two arguments. If only one table
#'   is supplied in \code{...}, \code{fun} must accept exactly one 
#'   argument.
#' @param partial If TRUE, only the overlapping times and columns will 
#'   be processed.
#' @inheritParams order_table
#' @return A single table.
#'
#' @examples
#' simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#'   package = "RAStestR")
#' quasi.flow = read_standard(simple.quasi, "Flow")
#'
#' operate_table(quasi.flow, quasi.flow, fun = "+")
#' operate_table(quasi.flow, quasi.flow, fun = "-")
#' operate_table(quasi.flow, fun = function(x) 0.5*x)
#' operate_table(quasi.flow, fun = function(x) x/x)
#'
#' @import dplyr
#' @export
operate_table = function(..., fun, partial = FALSE, time.col = "Time") {
  dots = list(...)
  # single table
  if(length(dots) == 1L) {
    dots = dots[[1]]
    datime.cols = setdiff(names(dots),time.col)
    return(as_data_frame(cbind(dots[time.col],
      fun(dots[, datime.cols]))))
  }
  # check column names
  union.cols = Reduce(function(...) union(...), 
    lapply(dots, names))
  intersect.cols = Reduce(function(...) intersect(...), 
    lapply(dots, names))
  diff.cols = setdiff(union.cols, intersect.cols)
  if (length(diff.cols) > 0L)
    if (partial)
      message("Excluding columns: ", paste(diff.cols, sep = ", "))
    else
      stop('Tables in "..." do not have matching columns')
  # check time stamps
  union.times = Reduce(function(...) union(...), 
    lapply(dots, function(x) x[[time.col]]))
  intersect.times = Reduce(function(...) intersect(...), 
    lapply(dots, function(x) x[[time.col]]))
  diff.times = setdiff(union.times, intersect.times)
  if (length(diff.times) > 0L)
    if (partial)
      message("Excluding timestamps: ", paste(diff.times, sep = ", "))
    else
      stop('Tables in "..." do not have matching time stamps')
  # arrange and filter data
  dots = lapply(dots, function(x) 
    order_table(x[x[[time.col]] %in% intersect.times, intersect.cols],
      time.col))
  # get data columns
  datime.cols = setdiff(intersect.cols, time.col)
  # apply function
  as_data_frame(cbind.data.frame(dots[[1]][time.col],
    Reduce(fun, lapply(dots, function(x) x[datime.cols]))))
}

#' Table Operations (Sediment)
#'
#' Combine sediment tables via an operation, e.g. addition or multiplication.
#'
#' @inheritParams operate_table
#' @inheritParams order_sediment
#' @return A single sediment table.
#'
#' @examples
#' simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#'   package = "RAStestR")
#' quasi.voloutcum = read_sediment(simple.quasi, "Vol Out Cum")
#'
#' operate_sediment(quasi.voloutcum, quasi.voloutcum, fun = "+")
#' operate_sediment(quasi.voloutcum, quasi.voloutcum, fun = "-")
#' operate_sediment(quasi.voloutcum, fun = function(x) 2*x)
#' operate_sediment(quasi.voloutcum, fun = function(x) x*x)
#'
#' @import dplyr
#' @export
operate_sediment = function(..., fun = "+", partial = FALSE, 
  time.col = "Time", grain.col = "GrainClass") {
  dots = list(...)
  if (length(dots) == 1L) {
    dots = dots[[1]]
    datime.cols = setdiff(names(dots), c(time.col, grain.col))
    return(as_data_frame(cbind(dots[c(time.col, grain.col)],
        fun(dots[, datime.cols]))))
  }
  # check grain classes
  union.grains = Reduce(function(...) union(...), 
    lapply(dots, function(x) unique(x[[grain.col]])))
  union.levels = Reduce(function(...) union(...), 
    lapply(dots, function(x) levels(x[[grain.col]])))
  intersect.grains = Reduce(function(...) intersect(...), 
    lapply(dots, function(x) unique(x[[grain.col]])))
  diff.grains = setdiff(union.grains, intersect.grains)
  if (length(diff.grains) > 0L)
    if (partial)
      message("Excluding grain classes: ", paste(diff.grains, sep = ", "))
    else
      stop('Tables in "..." do not have matching grain classes')
  # extract data by grain class
  grain.tables = vector("list", length(intersect.grains))
  names(grain.tables) = intersect.grains
  for(g in intersect.grains) {
    grain.tables[[g]] = lapply(dots, function(x) 
      x[x[[grain.col]] == g, setdiff(names(x), grain.col)])
  }
  bind_rows(
    lapply(grain.tables, function(x) 
      do.call(operate_table, args = c(x, list(fun = fun, 
        partial = partial, time.col = time.col)))),
    .id = grain.col
  )
}
