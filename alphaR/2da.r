#' Read 2D Area Tables
#'
#' Read standard (not by grain class) tables for one or more 2D areas.
#"
#' @inheritParams read_standard
#' @param which.areas 2D flow areas to extract. Can accept either numeric
#'   IDs or 2D flow area names. If NULL, all 2D flow areas will be 
#'   returned.
#' @return A named list of dataframes. List names correspond to the 2D 
#'  flow area names. Dataframes contain a column "Time" containing the 
#'  Date Time Stamp data and columns "CELL_####" where ### is the 2D cell ID.
#'
#' @import stringr
#' @export
read_2d_standard = function(f, table.name, which.times = NULL,
  which.areas = NULL, override.sediment = FALSE) {
  run.type = get_run_type(f)
  if (run.type == "Unsteady+Sediment" && override.sediment)
    run.type == "Unsteady"
  # argument checks
  if (is.null(which.times))
    which.times = seq_along(list_output_times(f))
  else if (!is.numeric(which.times))
    which.times = which(list_output_times(f) %in% which.times)
  else
    which.times = which(seq_along(list_output_times(f)) %in% which.times)
  if (length(which.times) < 1L)
    stop("No data matching 'which.times' was found")
  if (is.null(which.areas))
    which.areas = seq_along(list_2dareas(f))
  else if (!is.numeric(which.areas))
    which.areas = which(list_2dareas(f) %in% which.areas)
  else
    which.areas = which(seq_along(list_2dareas(f)) %in% which.areas)
  if (length(which.areas) < 1L)
    stop("No data matching 'which.areas' was found")
  select.areas = list_2dareas(f)[which.areas]
  table.names = file.path(get_2darea_block(run.type), select.areas, table.name)
  tspath = get_timestep_table(run.type)
  res = vector("list", length(table.names))
  if (str_detect(table.name, "Face"))
    colprefix = "FACE_"
  else
    colprefix = "CELL_"
  for (i in seq_along(table.names)) {
    res[[i]] = read_2dtable(f, table.names[[i]], tspath,
      "Time", colprefix)[which.times,]
  }
  setNames(res, select.areas)
}

# Read 2D RAS Table
#
# Read RAS 2D data output. This function is used internally and should
# not be called directly by the user.
#
# @param f The HDF5 file to read.
# @param table.path The table to read in.
# @param row.table.path The table containing the row identifiers.
# @param rowcolname The name to assign to the new row id column.
# @param colprefix A prefix to apply to the column IDs.
# @return A dataframe.
#
# @examples
#
#
#' @import hdf5r
#' @import dplyr
#' @import stringr
read_2dtable = function(f, rowcolname, rlabs, clabs) {
  if (!file.exists(f))
    stop("Could not find ", suppressWarnings(normalizePath(f)))
  # get run type
  run.type = get_run_type(f)
  # open file
  x = H5File$new(f, mode = 'r')
  on.exit(x$close_all())
  this = get_dataset(x, table.path) %>% as_data_frame()
  names(this) = clabs
  this[rowcolname] = rlabs
  this[c(rowcolname, clabs)]
}
