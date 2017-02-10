#'Reformat As Long Table
#'
#' Reformat RAS data from wide table format (stations are individual 
#' columns) to long table format (stations and data are collapsed to 
#' key-value column format.
#'
#' @param d The data table, i.e. output from \code{read_table} or
#'   \code{read_sediment}.
#' @param data.col The name of the new column holding data values.
#' @param station.col The name of the new column holding station IDs. 
#'   The prefix "XS_" will be stripped from the station IDs.
#' @return The original data table in long format.
#'
#' @import tidyr
#' @import dplyr
#' @export
to_longtable = function(d, data.col, station.col = "Station") {
  gather.cols = names(d)[grepl("XS_", names(d))]
  gather_(d, station.col, data.col, gather.cols, convert = FALSE,
    factor_key = TRUE)
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
#'   the data souce.
#'
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
#' Write RAS data to clipboard in comma-seperated format for pasting 
#' into e.g. Microsoft Excel.
#'
#' @param d The data table.
#' @return Writes the data table into the clipboard in tab-seperated 
#'   format.
#'
#' @importFrom utils write.table
#' @importFrom utils writeClipboard
#' @export
data_to_clipboard = function(d) {
  tf = tempfile(fileext = ".txt")
  write.table(d, tf, sep = "\t")
  writeClipboard(paste(readLines(tf), collapse = "\n"))
  message("Data copied to clipboard.")
  invisible(NULL)
}

#' Convert Stations To Distance
#'
#' Convert station values to distance upstream.
#'
#' @param ld The long-format data, i.e. output from 
#'   \code{to_longtable}.
#' @param distance.col The new column to hold distance values.
#' @param station.col The column containing station values.
#' @param metric If \code{TRUE}, stations are assumed to be in 
#'   kilometers and distances are returned in meters. If \code{FALSE}, 
#'   stations are assumed to be in miles and distances are returned in 
#'   feet.
#' @return The data table with an additional column of distances.
#'
#' @export
station_to_distance = function(ld, distance.col,
  station.col = "Station", metric = FALSE) {
  if (metric)
    ld[distance.col] = 1000 * (as.numeric(ld[[station.col]]) -
      min(as.numeric(ld[[station.col]])))
  else
    ld[distance.col] = 5280 * (as.numeric(ld[[station.col]]) -
      min(as.numeric(ld[[station.col]])))
  ld
}

#' Reformat Standard Fields
#'
#' Reformat standard RAStestR fields as standard R data types.
#'
#' @param ld A data table.
#' @param fields A list of fields to reformat.
#' @return The data table with reformatted fields.
#'
#' @details If the "Time" field is specified, values in that column
#'   will be formatted as R timestamps (POSIXct). If the "Station"
#'   field is specified, stations will be converted to numeric values
#'   (only applicable to long format tables).
#' 
#' @export
reformat_fields = function(ld, fields = c("Time", "Station")) {
  if ("Time" %in% fields)
    ld["Time"] = as.POSIXct(ld$Time, tz = "UTC", format = "%d%b%Y %H:%M:%S")
  if ("Station" %in% fields)
    ld["Station"] = as.numeric(gsub("XS_", "", as.character(ld$Station)))
  ld
}
