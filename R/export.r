#'Reformat As Long Table
#'
#' Reformat RAS data from wide table format (stations are individual
#' columns) to long table format (stations and data are collapsed to
#' key-value column format.
#'
#' @param d The data table, i.e. output from \code{read_table} or
#'   \code{read_sediment}.
#' @param data.col The name of the new column holding data values.
#' @param gather.cols The column names to gather data from. If not specified,
#'   all columns with the prefix "XS_" will be used.
#' @param station.col The name of the new column holding station IDs.
#' @param gather.attr The attributes to include in the resulting 
#'   long-format table. By default the "River" and "Reach" attributes are
#'   added as additional columns to the long-format table.
#' @return The original data table in long format.
#'
#' @examples
#' simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#'   package = "RAStestR")
#' quasi.flow = read_standard(simple.quasi, "Flow")
#' to_longtable(quasi.flow, "Flow")
#'
#' quasi.volincum = read_sediment(simple.quasi, "Vol In Cum")
#' to_longtable(quasi.volincum, "VolumeIn")
#'
#' @import tidyr
#' @import dplyr
#' @export
to_longtable = function(d, data.col, gather.cols,
  station.col = "Station", gather.attr = c("River", "Reach")) {
  if (missing(gather.cols)) {
    gather.idx = grepl("XS_", names(d))
    gather.cols = names(d)[gather.idx]
  } else if(is.numeric(gather.cols)) {
    gather.idx = gather.cols
    gather.cols = names(d)[gather.idx]
  } else {
    gather.idx = names(d) %in% gather.cols
  }
  attr.names = intersect(names(attributes(d)), gather.attr)
  if (length(attr.names) > 0L) {
    missing.attr = setdiff(gather.attr, attr.names)
    if (length(missing.attr) > 0L)
      warning("Could not find attributes: ", str_c(missing.attr,
        collapse = ", "))
    namepref = do.call(str_c, c(attributes(d)[attr.names],
      list(sep = "\t")))
    newnames = str_c(namepref, names(d), sep = "\t")
    names(d)[gather.idx] = newnames[gather.idx]
    gather.cols = names(d)[gather.idx]
  }
  res = gather_(d, station.col, data.col, gather.cols,
    convert = FALSE, factor_key = FALSE)
  if (length(attr.names) > 0L) {
    split.names = str_split(res[[station.col]], "\t")
    for (i in seq_along(attr.names))
      res[attr.names[i]] = sapply(split.names, function(x) x[i])
    res[station.col] = sapply(split.names, function(x) x[i + 1])
  }
  res
}

#'Reformat As Wide Table
#'
#' Reformat RAS data from long table format (i.e. output of
#' \code{to_longtable}) to wide table format (key values are mapped to
#' individual columns).
#'
#' @param d The data table, i.e. output from \code{to_longtable}.
#' @param key.col The name of the column holding data keys.
#' @param value.col The name of the column holding data values.
#' @param key.prefix Text to prepend to the new columns created from
#'   keys in \code{key.col}.
#' @param key.attr Columns to maintain as attributes in the resulting 
#'   wide-format table. By default, columns "River" and "Reach" will be
#'   searched for and maintained as attributes in the output wide-format
#    table (if they exist).
#' @return The original data table in wide format.
#'
#' @examples
#' simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#'   package = "RAStestR")
#' quasi.volincum = read_sediment(simple.quasi, "Vol In Cum")
#' quasi.long = to_longtable(quasi.volincum, "VolIn")
#' to_widetable(quasi.long, "Station", "VolIn")
#' to_widetable(quasi.long, "GrainClass", "VolIn")
#'
#' @import tidyr
#' @export
to_widetable = function(d, key.col, value.col, key.prefix,
  key.attr = c("River", "Reach")) {
  if (!missing(key.prefix))
    d[key.col] = paste0(key.prefix, d[[key.col]])
  # add attributes to key
  attr.names = intersect(names(d), key.attr)
  if (length(attr.names) != length(key.attr)) {
    missing.attr = setdiff(key.attr, attr.names)
    warning("Could not find columns: ",
    str_c(missing.attr, collapse = ", "))
  }
  if (length(attr.names) > 0L) {
    namepref = do.call(str_c, c(as.list(d[attr.names]), sep = "\t"))
    d[key.col] = str_c(namepref, d[[key.col]], sep = "\t")
    spreadcols = setdiff(names(d), attr.names)
  } else {
  spreadcols = names(d)
  }
  res = spread(d[spreadcols], key.col, value.col)
  if (length(attr.names) > 0L) {
    valcols = tail(names(res), - (length(spreadcols) - length(attr.names)))
    othercols = head(names(res), - length(valcols))
    split.attr = str_split(valcols, "\t")
    new.attr = vector("list", length(attr.names))
    names(new.attr) = attr.names
    for (i in seq_along(attr.names))
      new.attr[[attr.names[i]]] = sapply(split.attr, function(x) x[i])
    newnames = sapply(split.attr, function(x) tail(x, 1))
    names(res)[length(othercols) + 1:length(valcols)] = newnames
    # generate column attributes
    for (n in names(new.attr))
      attr(res, n) = c(rep("", length(othercols)), new.attr[[n]])
  }
  # return result
  res
}

#' Combine Data Tables
#'
#' Combine data tables into a single table. The tables must have
#'   identical column names. For data sourced from plans with different
#'   geometry or stations, use \code{to_longtable} to format the tables
#'   consistently before combining. If combining tables of different
#'   data, provide explicit names to the tables to easily differentiate
#'   between variables.
#'
#' @param ... Series of named tables to combine.
#' @param data.list Alternative input to \code{...}. A single list of
#'  data tables to combine. If not named, the elements will be named
#'  sequentially using \code{id.col}, e.g. "table 1", "table 2",
#'  etc.
#' @param id.col The name of the new column containing data source IDs.
#' @return A single data table with an additional column specifying
#'   the data source.
#'
#' @examples
#' simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#'   package = "RAStestR")
#' quasi.flow = read_standard(simple.quasi, "Flow")
#' quasi.wse = read_standard(simple.quasi, "Water Surface")
#' long.flow = to_longtable(quasi.flow, "Value")
#' long.wse= to_longtable(quasi.flow, "Value")
#' combine_data(flow = long.flow, wse = long.wse, id.col = "Variable")
#'
#' @import dplyr
#' @export
combine_data = function(..., data.list, id.col = "table") {
  if (missing(data.list))
    data.list = list(...)
  if (length(data.list) < 1L)
    stop("No data tables provided.")
  else if (length(data.list) == 1L)
    warning("Only one data table provided.")
  if (is.null(names(data.list)))
    names(data.list) == paste(id.col, seq(length(data.list)))
  bind_rows(data.list, .id = id.col)
}

#' Write Data To Clipboard
#'
#' Write RAS data to clipboard in comma-separated format for pasting
#' into e.g. Microsoft Excel. Note that the clipboard is only supported
#' Windows platforms.
#'
#' @param d The data table.
#' @param header If \code{TRUE}, write the column names to the first row.
#' @return Writes the data table to the clipboard in tab-separated format.
#'
#' @examples
#' \dontrun{
#' simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#'   package = "RAStestR")
#' quasi.flow = read_standard(simple.quasi, "Flow")
#' data_to_clipboard()
#' }
#'
#' @import readr
#' @export
data_to_clipboard = function(d, header = TRUE) {
  if (Sys.info()['sysname'] == "Windows") {
    cat(format_tsv(d, col_names = header), file = "clipboard")
    message("Data copied to clipboard.")
  } else {
    message("Clipboard not supported on OSX/Linux systems.")
  } 
  invisible(NULL)
}

#' Convert Stations To Distance
#'
#' Convert station values to distance upstream.
#'
#' @param d The long-format data, i.e. output from
#'   \code{to_longtable}.
#' @param distance.col The new column to hold distance values.
#' @param station.col The column containing station values.
#' @param direction Compute distance \code{upstream} or \code{downstream}.
#' @param metric If \code{TRUE}, stations are assumed to be in
#'   kilometers and distances are returned in meters. If \code{FALSE},
#'   stations are assumed to be in miles and distances are returned in
#'   feet.
#' @return The data table with an additional column of distances.
#'
#' @examples
#' simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#'   package = "RAStestR")
#' quasi.flow = read_standard(simple.quasi, "Flow")
#' long.flow = to_longtable(quasi.flow, "Flow")
#' station_to_distance(long.flow)
#' station_to_distance(long.flow, metric = TRUE)
#' station_to_distance(long.flow, "Distance Upstream",
#'   direction = "upstream")
#'
#' @export
station_to_distance = function(d, distance.col = "Distance", 
  station.col = "Station", direction = c("downstream", "upstream"), 
  metric = FALSE) {
  direction = match.arg(direction, c("downstream", "upstream"))
  ld = reformat_fields(d, station.col = station.col)
  if (metric)
    ld[distance.col] = 1000 * (as.numeric(ld[[station.col]]) -
      min(as.numeric(ld[[station.col]])))
  else
    ld[distance.col] = 5280 * (as.numeric(ld[[station.col]]) -
      min(as.numeric(ld[[station.col]])))
  if (direction == "upstream")
    d[distance.col] = ld[[distance.col]]
  else
    d[distance.col] = rev(ld[[distance.col]])
  d
}

#' Reformat Standard Fields
#'
#' Reformat standard RAStestR fields as standard R data types.
#'
#' @param ld A data table.
#' @param time.col The time column name.
#'   To skip formatting the times, set to \code{NULL}.
#' @param station.col (For long tables) The station column name. 
#'   To skip formatting the stations, set to \code{NULL}.
#' @return The data table with reformatted fields.
#'
#' @details Values in the Time column are formatted as R timestamps 
#'   (POSIXct). No adjustments for daylight savings or leap years are 
#'   made. Values in the Station column of long-format tables are 
#'   converted to numeric values.
#'
#' @examples
#' simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#'   package = "RAStestR")
#' quasi.flow = read_standard(simple.quasi, "Flow")
#' reformat_fields(quasi.flow)
#'
#' long.flow = to_longtable(quasi.flow, "Flow")
#' reformat_fields(long.flow)
#'
#' @import stringr
#' @export
reformat_fields = function(ld, time.col = "Time", station.col = "Station") {
  if (!is.null(time.col)) {
    if (is.numeric(time.col))
      time.col = names(ld)[time.col]
    if (time.col %in% names(ld)) {
      if (!("POSIXt" %in% class(ld[[time.col]])))
        ld[time.col] = as.POSIXct(ld[[time.col]], tz = "UTC",
          format = "%d%b%Y %H:%M:%S")
    } else {
      warning("Could not find column ", sprintf('"%s"', time.col))
    }
  }
  if (!is.null(station.col)) {
    if (is.numeric(station.col))
      station.col = names(ld)[station.col]
    if (station.col %in% names(ld)) { 
      if (is.character(ld[[station.col]])) {
        ld[station.col] = as.numeric(str_replace_all(ld[[station.col]], 
          c("XS_" = "", "[*]" = "")))
      }
    } else {
      warning("Could not find column ", sprintf('"%s"', station.col))
    }
  }
  ld
}
