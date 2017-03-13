# station table path
get_station_table = function(RAS.version = "5.0.3") {
  file.path("Geometry", "Cross Sections", "River Stations")
}
get_lengths_table = function(RAS.version = "5.0.3") {
  file.path("Geometry", "Cross Sections", "Lengths")
}
# grain class table path
get_grain_class_table = function(RAS.version = "5.0.3") {
  file.path("Event Conditions", "Sediment", "Grain Class Names")
}
# output interval table path
get_timestep_table = function(run.type, RAS.version = "5.0.3"){
  run.type = match.arg(run.type, c("unsteady", "quasi"))
  if (run.type == "unsteady")
    file.path("Results", "Unsteady", "Output", "Output Blocks",
      "Sediment", "Sediment Time Series", "Time Date Stamp")
  else
    file.path("Results", "Sediment", "Output Blocks",
      "Sediment", "Sediment Time Series", "Time Date Stamp")
}
# parent path of output data
get_output_block = function(run.type, RAS.version = "5.0.3") {
  run.type = match.arg(run.type, c("unsteady", "quasi"))
  if (run.type == "unsteady")
    file.path("Results", "Unsteady", "Output", "Output Blocks",
      "Sediment", "Sediment Time Series", "Cross Sections")
  else
    file.path("Results", "Sediment", "Output Blocks",
      "Sediment", "Sediment Time Series", "Cross Sections")
}
# parent path of sediment output data
get_sediment_block = function(run.type, RAS.version = "5.0.3") {
  run.type = match.arg(run.type, c("unsteady", "quasi"))
  if (run.type == "unsteady")
    file.path("Results", "Unsteady", "Output", "Output Blocks",
      "Sediment", "Sediment Time Series", "Cross Sections")
  else
    file.path("Results", "Sediment", "Output Blocks",
      "Sediment", "Sediment Time Series", "Cross Sections")
}
# parent path of cross section output data
get_xsection_block = function(run.type, RAS.version = "5.0.3"){
  run.type = match.arg(run.type, c("unsteady", "quasi"))
  if (run.type == "unsteady")
    file.path("Results", "Unsteady", "Output", "Output Blocks",
      "Sediment SE", "Sediment Time Series", "Cross Section SE")
  else
    file.path("Results", "Sediment", "Output Blocks",
      "Sediment SE", "Sediment Time Series", "Cross Section SE")
}

#' List Grain Classes
#'
#' Get list of RAS sediment grain class labels.
#'
#' @inheritParams read_standard
#' @return a vector of grain glass labels.
#'
#' @import h5
list_grains = function(f) {
  if (!file.exists(f))
    stop("Could not find ", suppressWarnings(normalizePath(f)))
  x = h5file(f)
  on.exit(h5close(x))
  grainpath = get_grain_class_table()
  if (!existsDataSet(x, grainpath))
    stop('Table "', grainpath, '" could not be found.', call. = FALSE)
  c("ALL", x[grainpath][])
}

#' List Cross Section Lengths
#'
#' Get list cross section lengths.
#'
#' @inheritParams read_standard
#' @return a vector of cross section lengths.
#'
#' @import h5
list_lengths = function(f) {
  if (!file.exists(f))
    stop("Could not find ", suppressWarnings(normalizePath(f)))
  x = h5file(f)
  on.exit(h5close(x))
  lengthpath = get_lengths_table()
  if (!existsDataSet(x, lengthpath))
    stop('Table "', lengthpath, '" could not be found.', call. = FALSE)
  x[lengthpath][]
}

#' Read River Station Table
#'
#' Read RAS river station labels.
#'
#' @inheritParams read_standard
#' @return a vector of river station labels.
#'
#' @import h5
#' @import stringr
list_stations = function(f) {
  if (!file.exists(f))
    stop("Could not find ", suppressWarnings(normalizePath(f)))
  x = h5file(f)
  on.exit(h5close(x))
  stationpath = get_station_table()
  if (!existsDataSet(x, stationpath))
    stop('Table "', stationpath, '" could not be found.', call. = FALSE)
  x[stationpath][] %>% str_trim()
}

#' List Sediment Tables
#'
#' List grain class-specific tables of the specified type.
#'
#' @inheritParams read_standard
#' @return a vector of grain glass labels.
#'
#' @import h5
#' @import stringr
list_sediment = function(f, table.name) {
  if (!file.exists(f))
    stop("Could not find ", suppressWarnings(normalizePath(f)))
  x = h5file(f)
  on.exit(h5close(x))
  str_subset(list.datasets(x, path = dirname(table.name), recursive = FALSE),
    table.name)
}

#' List Cross Section Tables
#'
#' List cross section tables.
#'
#' @inheritParams read_standard
#' @param xs.block The HDF folder containing the cross section output tables.
#' @return a vector of cross section output labels.
#'
#' @import h5
#' @import stringr
list_xs = function(f, xs.block) {
  if (!file.exists(f))
    stop("Could not find ", suppressWarnings(normalizePath(f)))
  x = h5file(f)
  on.exit(h5close(x))
  #tblpath = file.path(dirname(xs.block), "Time Date Stamp")
  #x[tblpath][] %>% str_trim() %>% sprintf("Station Elevation (%s)", .)
  list.datasets(x, path = xs.block, recursive = FALSE) %>% basename() %>%
    str_subset("info") %>% str_replace("info", "") %>% str_trim()
}
