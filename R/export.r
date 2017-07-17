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
#' @return The original data table in long format.
#'
#' @examples
#' simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#'   package = "RAStestR")
#' quasi.flow = read_standard(simple.quasi, "Flow")
#' to_longtable(quasi.flow, "Flow")
#'
#' quasi.volincum = read_sediment(simple.quasi, "Vol In Cum")
#' to_longtable(quasi.volincum, "Vol In Cum")
#'
#' @import tidyr
#' @import dplyr
#' @export
to_longtable = function(d, data.col, gather.cols, station.col = "Station") {
  if (missing(gather.cols))
    gather.cols = names(d)[grepl("XS_", names(d))]
  gather_(d, station.col, data.col, gather.cols, convert = FALSE,
    factor_key = FALSE)
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
#' @return The original data table in wide format.
#'
#' @examples
#' simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#'   package = "RAStestR")
#' quasi.volincum = read_sediment(simple.quasi, "Vol In Cum")
#' quasi.long = to_longtable(quasi.volincum, "Vol In Cum")
#' to_widetable(quasi.long, "Station", "Vol In Cum")
#' to_widetable(quasi.long, "GrainClass", "Vol In Cum")
#'
#' @import tidyr
#' @export
to_widetable = function(d, key.col, value.col, key.prefix) {
  if (!missing(key.prefix))
    d[key.col] = paste0(key.prefix, d[[key.col]])
  d %>% spread_(key.col, value.col)
}

#' Combine Data Tables
#'
#' Combine data tables into a single table. The tables must have
#'   identical column names. For Data sourced from plans with different
#'   geometry or stations, use \code{to_longtable} to format the tables
#'   consistently before combining. If combining tables of different
#'   data, provide explicit names to the tables to easily differentiate
#'   between variables.
#'
#' @param ... Series of named data sets to combine.
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
#' into e.g. Microsoft Excel.
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
#' @importFrom utils writeClipboard
#' @export
data_to_clipboard = function(d, header = TRUE) {
  writeClipboard(format_tsv(d, col_names = header))
  message("Data copied to clipboard.")
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
  ld = reformat_fields(d, list(Station = station.col))
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
#' @param fields The fields to reformat. For non-default Station and Time
#'   columns, \code{fields} must be a named list identifying the actual names or
#'   positions of the "Time" and "Station" columns.
#' @return The data table with reformatted fields.
#'
#' @details If the "Time" field is specified, values in that column
#'   will be formatted as R timestamps (POSIXct). If the "Station"
#'   field is specified, stations will be converted to numeric values
#'   (only applicable to long format tables).
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
reformat_fields = function(ld, fields = list("Time", "Station")) {
  fields = as.list(fields)
  if (is.null(names(fields)))
    names(fields) = fields
  if (!all(names(fields) %in% c("Time", "Station")))
    stop("Argument 'fields' not recognized")
  time.col = fields[["Time"]]
  station.col = fields[["Station"]]
  if ("Time" %in% names(fields))
    ld[time.col] = as.POSIXct(ld[[time.col]], tz = "UTC",
      format = "%d%b%Y %H:%M:%S")
  if ("Station" %in% names(fields))
    ld[station.col] = ld[[station.col]] %>% as.character() %>%
      str_replace("XS_", "") %>% str_replace("[*]", "") %>% as.numeric()
  ld
}
