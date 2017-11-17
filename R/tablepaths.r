# station table path
get_station_table = function(RAS.version) {
  if (is.null(RAS.version))
    RAS.version = options()[["RAStestR.RASversion"]]
  switch(RAS.version,
    "5.0.3" = file.path("Geometry", "Cross Sections", "River Stations")
  )
}

# river table path
get_river_table = function(RAS.version) {
  switch(RAS.version,
    "5.0.3" = file.path("Geometry", "Cross Sections", "River Names"),
    "5.0.4" = file.path("Geometry", "Cross Sections", "Attributes")
  )
}

# reach table path
get_reach_table = function(RAS.version) {
  switch(RAS.version,
    "5.0.3" = file.path("Geometry", "Cross Sections", "Reach Names"),
    "5.0.4" = file.path("Geometry", "Cross Sections", "Attributes")
  )
}

get_lengths_table = function(RAS.version) {
  switch(RAS.version,
    "5.0.3" = file.path("Geometry", "Cross Sections", "Lengths"),
    "5.0.4" = file.path("Geometry", "Cross Sections", "Attributes")
  )
}
# grain class table path
get_grain_class_table = function(RAS.version) {
  switch(RAS.version,
    "5.0.3" = file.path("Event Conditions", "Sediment", "Grain Class Names")
  )
}
# bank station table path
get_bank_stations_table = function(RAS.version) {
  switch(RAS.version,
    "5.0.3" = file.path("Geometry", "Cross Sections", "Bank Stations"),
    "5.0.4" = file.path("Geometry", "Cross Sections", "Attributes")
  )
}

# output interval table path
get_timestep_table = function(run.type, RAS.version) {
  run.type = match.arg(run.type, c("Steady", "Unsteady", 
    "Unsteady+Sediment", "QuasiUnsteady"))
  if (run.type == "Unsteady+Sediment")
    file.path("Results", "Unsteady", "Output", "Output Blocks",
      "Sediment", "Sediment Time Series", "Time Date Stamp")
  else if (run.type == "Unsteady")
    file.path("Results", "Unsteady", "Output", "Output Blocks",
      "Base Output", "Unsteady Time Series", "Time Date Stamp")
  else if (run.type == "Steady")
    file.path("Results", "Steady", "Output", "Output Blocks", 
      "Base Output", "Steady Profiles", "Profile Names")
  else
    file.path("Results", "Sediment", "Output Blocks",
      "Sediment", "Sediment Time Series", "Time Date Stamp")
}
# parent path of output data
get_output_block = function(run.type, RAS.version) {
  run.type = match.arg(run.type, c("Steady", "Unsteady", 
    "Unsteady+Sediment", "QuasiUnsteady"))
  if (run.type == "Unsteady+Sediment")
    file.path("Results", "Unsteady", "Output", "Output Blocks",
      "Sediment", "Sediment Time Series", "Cross Sections")
  else if (run.type == "Unsteady")
    file.path("Results", "Unsteady", "Output", "Output Blocks",
      "Base Output", "Unsteady Time Series", "Cross Sections")
  else if (run.type == "Steady")
    file.path("Results", "Steady", "Output", "Output Blocks", 
      "Base Output", "Steady Profiles", "Cross Sections")
  else
    file.path("Results", "Sediment", "Output Blocks",
      "Sediment", "Sediment Time Series", "Cross Sections")
}
# parent path of sediment output data
get_sediment_block = function(run.type, RAS.version) {
  run.type = match.arg(run.type, c("Steady", "Unsteady", 
    "Unsteady+Sediment", "QuasiUnsteady"))
  if (run.type == "Unsteady+Sediment")
    file.path("Results", "Unsteady", "Output", "Output Blocks",
      "Sediment", "Sediment Time Series", "Cross Sections")
  else if (run.type == "QuasiUnsteady")
    file.path("Results", "Sediment", "Output Blocks",
      "Sediment", "Sediment Time Series", "Cross Sections")
  else
    stop(sprintf('No sediment data available for run type "%s"', run.type))
}
# parent path of cross section output data
get_xsection_block = function(run.type, RAS.version) {
  run.type = match.arg(run.type, c("Steady", "Unsteady", 
    "Unsteady+Sediment", "QuasiUnsteady"))
  if (run.type == "Unsteady+Sediment")
    file.path("Results", "Unsteady", "Output", "Output Blocks",
      "Sediment SE", "Sediment Time Series", "Cross Section SE")
  else if (run.type == "QuasiUnsteady")
    file.path("Results", "Sediment", "Output Blocks",
      "Sediment SE", "Sediment Time Series", "Cross Section SE")
  else
    stop(sprintf('No sediment data available for run type "%s"', run.type))
  }
# parent path of 2D flow area output data
get_2darea_block = function(run.type, RAS.version) {
  run.type = match.arg(run.type, c("Steady", "Unsteady", 
    "Unsteady+Sediment", "QuasiUnsteady"))
  if (run.type == "Unsteady")
    file.path("Results", "Unsteady", "Output", "Output Blocks",
      "Base Output", "Unsteady Time Series", "2D Flow Areas")
  else
    stop(sprintf('No 2D data available for run type "%s"', run.type))
}

# Plan Information Path
get_plan_info_table = function(RAS.version) {
  switch(RAS.version,
    "5.0.3" = file.path("Plan Data", "Plan Information")
  )
}

get_2d_flow_area_table = function(RAS.version) {
  switch(RAS.version,
    "5.0.3" = file.path("Geometry", "2D Flow Areas", "Names")
  )
}

#' List Plan Information
#'
#' List basic plan information.
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
#' List sediment grain class labels.
#'
#' @inheritParams read_standard
#' @return a vector of grain glass labels.
#'
#' @examples
#' simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#'   package = "RAStestR")
#' RAStestR:::list_grain_classes(simple.quasi)
#'
#' @import hdf5r
#' @import stringr
#' @export
list_grain_classes = function(f) {
  if (!file.exists(f))
    stop("Could not find ", suppressWarnings(normalizePath(f)))
  run.type = get_run_type(f)
  ras.version =get_RAS_version(f) 
  if(!(run.type %in% c("QuasiUnsteady", "Unsteady+Sediment"))) 
    stop(sprintf('No sediment data available for run type "%s"', run.type))
  x = H5File$new(f, mode = 'r')
  on.exit(x$close_all())
  grainpath = get_grain_class_table(ras.version)
  c("ALL", str_trim(get_dataset(x, grainpath)))
}

#' List Output Times
#'
#' List output times.
#'
#' @inheritParams read_standard
#' @return a vector of cross section lengths.
#'
#' @examples
#' simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#'   package = "RAStestR")
#' RAStestR:::list_output_times(simple.quasi)
#'
#' @import hdf5r
#' @import stringr
#' @export
list_output_times = function(f) {
  if (!file.exists(f))
    stop("Could not find ", suppressWarnings(normalizePath(f)))
  run.type = get_run_type(f)
  ras.version =get_RAS_version(f) 
  timespath = get_timestep_table(run.type, ras.version)
  x = H5File$new(f, mode = 'r')
  on.exit(x$close_all())
  str_trim(get_dataset(x, timespath))
}


#' List Cross Section Lengths
#'
#' List cross section lengths.
#'
#' @inheritParams read_standard
#' @return a matrix of cross section lengths.
#'
#' @examples
#' simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#'   package = "RAStestR")
#' RAStestR:::list_lengths(simple.quasi)
#'
#' @import hdf5r
#' @export
list_lengths = function(f) {
  if (!file.exists(f))
    stop("Could not find ", suppressWarnings(normalizePath(f)))
  ras.version = get_RAS_version(f)
  x = H5File$new(f, mode = 'r')
  on.exit(x$close_all())
  lengthpath = get_lengths_table(ras.version)
  switch(ras.version,
    "5.0.3" = get_dataset(x, lengthpath),
    "5.0.4" = setNames(as.matrix(
      get_dataset(x, lengthpath)[[c("Len Left", "Len Channel", "Len Right")]]
      ), NULL)
  )
}

#' List Bank Stations
#'
#' List bank stations.
#'
#' @inheritParams read_standard
#' @return a vector of bank stations
#'
#' @examples
#' simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#'   package = "RAStestR")
#' RAStestR:::list_bank_stations(simple.quasi)
#'
#' @import hdf5r
#' @export
list_bank_stations = function(f){
  if (!file.exists(f))
    stop("Could not find ", suppressWarnings(normalizePath(f)))
  ras.version = get_RAS_version(f)
  x = H5File$new(f, mode = 'r')
  on.exit(x$close_all())
  bankpath = get_bank_stations_table(ras.version)
  switch(ras.version,
    "5.0.3" = get_dataset(x, bankpath),
    "5.0.4" = setNames(as.matrix(
      get_dataset(x, bankpath)[[c("Left Bank", "Right Bank")]]
      ), NULL)
  )
}

#' List River Stations
#'
#' List river station labels.
#'
#' @inheritParams read_standard
#' @return a vector of river station labels.
#'
#' @examples
#' simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#'   package = "RAStestR")
#' RAStestR:::list_stations(simple.quasi)
#'
#' @import hdf5r
#' @import stringr
#' @export
list_stations = function(f) {
  if (!file.exists(f))
    stop("Could not find ", suppressWarnings(normalizePath(f)))
  ras.version = get_RAS_version(f)
  x = H5File$new(f, mode = 'r')
  on.exit(x$close_all())
  stationpath = get_station_table(ras.version)
  switch(ras.version,
    "5.0.3" = str_trim(get_dataset(x, stationpath)),
    "5.0.4" = str_trim(get_dataset(x, stationpath)[["RS"]])
  )
}

#' List River Names
#'
#' List river names.
#'
#' @inheritParams read_standard
#' @return a vector of river names.
#'
#' @examples
#' simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#'   package = "RAStestR")
#' RAStestR:::list_rivers(simple.quasi)
#'
#' @import hdf5r
#' @import stringr
#' @export
list_rivers = function(f) {
  if (!file.exists(f))
    stop("Could not find ", suppressWarnings(normalizePath(f)))
  ras.version = get_RAS_version(f)
  x = H5File$new(f, mode = 'r')
  on.exit(x$close_all())
  riverpath = get_river_table(ras.version)
  switch(ras.version,
    "5.0.3" = str_trim(get_dataset(x, riverpath)),
    "5.0.4" = str_trim(get_dataset(x, riverpath)[["River"]])
  )
}

#' List Reach Names
#'
#' List reach names.
#'
#' @inheritParams read_standard
#' @return a vector of reach names.
#'
#' @examples
#' simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#'   package = "RAStestR")
#' RAStestR:::list_reaches(simple.quasi)
#'
#' @import hdf5r
#' @import stringr
#' @export
list_reaches = function(f) {
  if (!file.exists(f))
    stop("Could not find ", suppressWarnings(normalizePath(f)))
  ras.version = get_RAS_version(f)
  x = H5File$new(f, mode = 'r')
  on.exit(x$close_all())
  reachpath = get_reach_table(ras.version)
  switch(ras.version,
    "5.0.3" = str_trim(get_dataset(x, reachpath)),
    "5.0.4" = str_trim(get_dataset(x, reachpath)[["River"]])
  )
}



# List Sediment Tables
#
# List grain class-specific tables of the specified type.
#
# @inheritParams read_standard
# @return a vector of grain glass labels.
#
# @examples
# simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#   package = "RAStestR")
# RAStestR:::list_sediment(simple.quasi, file.path("Results", "Sediment", 
#     "Output Blocks", "Sediment", "Sediment Time Series", "Cross Sections", 
#     "Vol In"))
# RAStestR:::list_sediment(simple.quasi, file.path("Results", "Sediment", 
#     "Output Blocks", "Sediment", "Sediment Time Series", "Cross Sections", 
#     "Vol Inactive"))
# RAStestR:::list_sediment(simple.quasi, file.path("Results", "Sediment", 
#     "Output Blocks", "Sediment", "Sediment Time Series", "Cross Sections", 
#     "Vol In Cum"))
#
#' @import hdf5r
#' @import stringr
list_sediment = function(f, table.name) {
  if (!file.exists(f))
    stop("Could not find ", suppressWarnings(normalizePath(f)))
  x = H5File$new(f, mode = 'r')
  on.exit(x$close_all())
  group.x = x$open(dirname(table.name))
  on.exit(group.x$close(), add = TRUE)
  file.path(dirname(table.name),
    str_subset(group.x$ls(recursive = FALSE)$name,
      sprintf("%s\\b(\\s\\d)*$", basename(table.name)))
  )
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
#' @import hdf5r
#' @import stringr
list_xs = function(f, xs.block) {
  # nse workaround
  . = NULL
  if (!file.exists(f))
    stop("Could not find ", suppressWarnings(normalizePath(f)))
  x = H5File$new(f, mode = 'r')
  on.exit(x$close_all())
  tblpath = file.path(dirname(xs.block), "Time Date Stamp")
  get_dataset(x, tblpath) %>% str_trim() %>%
    sprintf("Station Elevation (%s)", .) %>% unique()
}

#' List 2D Areas
#'
#' List 2D flow areas
#'
#' @inheritParams read_standard
#' @return a vector of 2D flow area names.
#'
#' @import hdf5r
#' @export
list_2dareas = function(f) {
  if (!file.exists(f))
    stop("Could not find ", suppressWarnings(normalizePath(f)))
  ras.version = get_RAS_version(f)
  x = H5File$new(f, mode = 'r')
  on.exit(x$close_all())
  flowareapath = get_2d_flow_area_table(ras.version)
  str_trim(get_dataset(x, flowareapath) )
}

#' Force RAS Version
#'
#' Force RAStestR to assume the specified RAS version. Useful for
#' working with RAS development environments or incomplete datasets 
#' that do not include RAS version info.
#'
#' @param version The RAS version to use.
#'
#' @export
force_RAS_version = function(version) {
  version = match.arg(version, c("5.0.3"))
  if (missing(version))
    return(options()$RAStestR.RASversion)
  options(RAStestR.RASversion = version)
  invisible(version)
}

#' Get RAS Version
#'
#' Get the RAS version that generated the specified file.
#' @inheritParams read_standard
#' @return The RAS version.
#'
#' @export
get_RAS_version = function(f) {
  if (!is.null(options()[["RAStestR.ForceVersion"]]))
    return(options()[["RAStestR.ForceVersion"]])
  v = str_split(get_meta(f)[["File Version"]], " ", simplify = TRUE)[2]
  if (v %in% names(options()[["RAStestR.VersionOverride"]]))
    options()[["RAStestR.VersionOverride"]][[v]]
  else
    v
  }

#' Override RAS Version
#'
#' Specify RAS version overrides. Useful when working with files produced 
#' from both RAS development and release versions, e.g. by assuming that
#' files produced with RAS development x.x are structured in the same way
#' as a specific release version.
#'
#' @param v The RAS version to override.
#' @param override.v The RAS version to be assumed.
#'
#' @export
override_RAS_version = function(v, override.v) {
  ops = options()[["RAStestR.VersionOverride"]]
  ops[[v]] = override.v
  options(RAStestR.VersionOverride = ops)
}