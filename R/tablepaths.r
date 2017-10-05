# station table path
get_station_table = function(RAS.version = "5.0.3") {
  file.path("Geometry", "Cross Sections", "River Stations")
}

# river table path
get_river_table = function(RAS.version = "5.0.3") {
  file.path("Geometry", "Cross Sections", "River Names")
}

# reach table path
get_reach_table = function(RAS.version = "5.0.3") {
  file.path("Geometry", "Cross Sections", "Reach Names")
}

get_lengths_table = function(RAS.version = "5.0.3") {
  file.path("Geometry", "Cross Sections", "Lengths")
}
# grain class table path
get_grain_class_table = function(RAS.version = "5.0.3") {
  file.path("Event Conditions", "Sediment", "Grain Class Names")
}
# bank station table path
get_bank_stations_table = function(RAS.version = "5.0.3") {
  file.path("Geometry", "Cross Sections", "Bank Stations")
}

# output interval table path
get_timestep_table = function(run.type, RAS.version = "5.0.3"){
  run.type = match.arg(run.type, c("Unsteady", "QuasiUnsteady"))
  if (run.type == "Unsteady")
    file.path("Results", "Unsteady", "Output", "Output Blocks",
      "Sediment", "Sediment Time Series", "Time Date Stamp")
  else
    file.path("Results", "Sediment", "Output Blocks",
      "Sediment", "Sediment Time Series", "Time Date Stamp")
}
# parent path of output data
get_output_block = function(run.type, RAS.version = "5.0.3") {
  run.type = match.arg(run.type, c("Unsteady", "QuasiUnsteady"))
  if (run.type == "Unsteady")
    file.path("Results", "Unsteady", "Output", "Output Blocks",
      "Sediment", "Sediment Time Series", "Cross Sections")
  else
    file.path("Results", "Sediment", "Output Blocks",
      "Sediment", "Sediment Time Series", "Cross Sections")
}
# parent path of sediment output data
get_sediment_block = function(run.type, RAS.version = "5.0.3") {
  run.type = match.arg(run.type, c("Unsteady", "QuasiUnsteady"))
  if (run.type == "Unsteady")
    file.path("Results", "Unsteady", "Output", "Output Blocks",
      "Sediment", "Sediment Time Series", "Cross Sections")
  else
    file.path("Results", "Sediment", "Output Blocks",
      "Sediment", "Sediment Time Series", "Cross Sections")
}
# parent path of cross section output data
get_xsection_block = function(run.type, RAS.version = "5.0.3"){
  run.type = match.arg(run.type, c("Unsteady", "QuasiUnsteady"))
  if (run.type == "Unsteady")
    file.path("Results", "Unsteady", "Output", "Output Blocks",
      "Sediment SE", "Sediment Time Series", "Cross Section SE")
  else
    file.path("Results", "Sediment", "Output Blocks",
      "Sediment SE", "Sediment Time Series", "Cross Section SE")
}

# Plan Information Path
get_plan_info_table = function(RAS.version = "5.0.3") {
  file.path("Plan Data", "Plan Information")
}


#' List Plan Information
#'
#' List Basic HEC-RAS plan information.
#'
#' @inheritParams read_standard
#' @param print Print the plan information to the console in a nice format.
#' @return A list of plan information.
#'
#' @export
list_plan_info = function(f, print = TRUE) {
  which.attr = c("Plan Name", "Plan ShortID", "Type of Run",
    "Time Window", "Computation Time Step", "Output Interval")
  metadata = get_meta(f)[which.attr]
  if (print) {
    message(paste(names(metadata), unlist(metadata),
      sep = ": ", collapse = "\n"), "\n")
  }
  invisible(metadata)
}

#' List Grain Classes
#'
#' Get list of RAS sediment grain class labels.
#'
#' @inheritParams read_standard
#' @return a vector of grain glass labels.
#'
#' @examples
#' simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#'   package = "RAStestR")
#' RAStestR:::list_grain_classes(simple.quasi)
#'
#' @import h5
#' @import stringr
#' @export
list_grain_classes = function(f) {
  if (!file.exists(f))
    stop("Could not find ", suppressWarnings(normalizePath(f)))
  x = h5file(f)
  on.exit(h5close(x))
  grainpath = get_grain_class_table()
  if (!existsDataSet(x, grainpath))
    stop('Table "', grainpath, '" could not be found.', call. = FALSE)
  c("ALL", get_dataset(x, grainpath, "character")) %>% str_trim()
}

#' List Time Steps
#'
#' Get list of output times.
#'
#' @inheritParams read_standard
#' @return a vector of cross section lengths.
#'
#' @examples
#' simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#'   package = "RAStestR")
#' RAStestR:::list_output_times(simple.quasi)
#'
#' @import h5
#' @import stringr
#' @export
list_output_times = function(f) {
  if (!file.exists(f))
    stop("Could not find ", suppressWarnings(normalizePath(f)))
  run.type = get_run_type(f)
  timespath = get_timestep_table(run.type)
  x = h5file(f)
  on.exit(h5close(x))
  if (!existsDataSet(x, timespath))
    stop('Table "', timespath, '" could not be found.', call. = FALSE)
  get_dataset(x, timespath, "character") %>% str_trim()
}


#' List Cross Section Lengths
#'
#' Get list cross section lengths.
#'
#' @inheritParams read_standard
#' @return a vector of cross section lengths.
#'
#' @examples
#' simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#'   package = "RAStestR")
#' RAStestR:::list_lengths(simple.quasi)
#'
#' @import h5
#' @export
list_lengths = function(f) {
  if (!file.exists(f))
    stop("Could not find ", suppressWarnings(normalizePath(f)))
  x = h5file(f)
  on.exit(h5close(x))
  lengthpath = get_lengths_table()
  if (!existsDataSet(x, lengthpath))
    stop('Table "', lengthpath, '" could not be found.', call. = FALSE)
  get_dataset(x, lengthpath, "double")
}

#' List Bank Stations
#'
#' Get list of bank stations
#'
#' @inheritParams read_standard
#' @return a vector of bank stations
#'
#' @examples
#' simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#'   package = "RAStestR")
#' RAStestR:::list_bank_stations(simple.quasi)
#'
#' @import h5
#' @export
list_bank_stations = function(f){
  if (!file.exists(f))
    stop("Could not find ", suppressWarnings(normalizePath(f)))
  x = h5file(f)
  on.exit(h5close(x))
  bankpath = get_bank_stations_table()
  if (!existsDataSet(x, bankpath))
    stop('Table "', bankpath, '" could not be found.', call. = FALSE)
  get_dataset(x, bankpath, "double")
}

#' List River Stations
#'
#' List RAS river station labels.
#'
#' @inheritParams read_standard
#' @return a vector of river station labels.
#'
#' @examples
#' simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#'   package = "RAStestR")
#' RAStestR:::list_stations(simple.quasi)
#'
#' @import h5
#' @import stringr
#' @export
list_stations = function(f) {
  if (!file.exists(f))
    stop("Could not find ", suppressWarnings(normalizePath(f)))
  x = h5file(f)
  on.exit(h5close(x))
  stationpath = get_station_table()
  if (!existsDataSet(x, stationpath))
    stop('Table "', stationpath, '" could not be found.', call. = FALSE)
  get_dataset(x, stationpath, "character") %>% str_trim()
}

#' List River Names
#'
#' List RAS river names.
#'
#' @inheritParams read_standard
#' @return a vector of river names.
#'
#' @examples
#' simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#'   package = "RAStestR")
#' RAStestR:::list_rivers(simple.quasi)
#'
#' @import h5
#' @import stringr
#' @export
list_rivers = function(f) {
  if (!file.exists(f))
    stop("Could not find ", suppressWarnings(normalizePath(f)))
  x = h5file(f)
  on.exit(h5close(x))
  riverpath = get_river_table()
  if (!existsDataSet(x, riverpath))
    stop('Table "', riverpath, '" could not be found.', call. = FALSE)
  get_dataset(x, riverpath, "character") %>% str_trim()
}

#' List Reach Names
#'
#' List RAS reach names.
#'
#' @inheritParams read_standard
#' @return a vector of reach names.
#'
#' @examples
#' simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#'   package = "RAStestR")
#' RAStestR:::list_reaches(simple.quasi)
#'
#' @import h5
#' @import stringr
#' @export
list_reaches = function(f) {
  if (!file.exists(f))
    stop("Could not find ", suppressWarnings(normalizePath(f)))
  x = h5file(f)
  on.exit(h5close(x))
  reachpath = get_reach_table()
  if (!existsDataSet(x, reachpath))
    stop('Table "', reachpath, '" could not be found.', call. = FALSE)
  get_dataset(x, reachpath, "character") %>% str_trim()
}



#' List Sediment Tables
#'
#' List grain class-specific tables of the specified type.
#'
#' @inheritParams read_standard
#' @return a vector of grain glass labels.
#'
#' @examples
#' simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#'   package = "RAStestR")
#' RAStestR:::list_sediment(simple.quasi, file.path("Results", "Sediment", 
#'     "Output Blocks", "Sediment", "Sediment Time Series", "Cross Sections", 
#'     "Vol In"))
#' RAStestR:::list_sediment(simple.quasi, file.path("Results", "Sediment", 
#'     "Output Blocks", "Sediment", "Sediment Time Series", "Cross Sections", 
#'     "Vol Inactive"))
#' RAStestR:::list_sediment(simple.quasi, file.path("Results", "Sediment", 
#'     "Output Blocks", "Sediment", "Sediment Time Series", "Cross Sections", 
#'     "Vol In Cum"))
#'
#' @import h5
#' @import stringr
list_sediment = function(f, table.name) {
  if (!file.exists(f))
    stop("Could not find ", suppressWarnings(normalizePath(f)))
  x = h5file(f)
  on.exit(h5close(x))
  str_subset(list.datasets(x, path = dirname(table.name), recursive = FALSE),
    sprintf("%s\\b(\\s\\d)*$", table.name))
}

#' List Cross Section Tables
#'
#' List cross section tables.
#'
#' @inheritParams read_standard
#' @param xs.block The HDF folder containing the cross section output tables.
#' @return a vector of cross section output labels.
#'
#' @examples
#' simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#'   package = "RAStestR")
#' RAStestR:::list_xs(simple.quasi, file.path("Results", "Sediment",
#'     "Output Blocks", "Sediment SE", "Sediment Time Series", 
#'     "Cross Section SE"))
#'
#' @import h5
#' @import stringr
list_xs = function(f, xs.block) {
  # nse workaround
  . = NULL
  if (!file.exists(f))
    stop("Could not find ", suppressWarnings(normalizePath(f)))
  x = h5file(f)
  on.exit(h5close(x))
  tblpath = file.path(dirname(xs.block), "Time Date Stamp")
  get_dataset(x, tblpath, "character") %>% str_trim() %>%
    sprintf("Station Elevation (%s)", .) %>% unique()
}


