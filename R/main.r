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

#' Read Standard Table
#'
#' Read a standard (not grain class-specific) table.
#'
#' @param f The h5 file to read.
#' @param tablelab The table to read.
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
read_standard = function(f, tablelab, run.type, which.times = NULL, 
  which.stations = NULL) {
  geompath = "Geometry/Cross Sections/River Stations"
  if (run.type == "unsteady") {
    tblpath = file.path("Results", "Unsteady", "Output", "Output Blocks",
      "Sediment", "Sediment Time Series", "Cross Sections",
      tablelab)
    tspath = file.path("Results", "Unsteady", "Output", "Output Blocks",
      "Sediment", "Sediment Time Series", "Time Date Stamp")
  } else if (run.type == "quasi") {
    tblpath = file.path("Results", "Sediment", "Output Blocks",
      "Sediment", "Sediment Time Series", "Cross Sections",
      tablelab)
    tspath = file.path("Results", "Sediment", "Output Blocks",
      "Sediment", "Sediment Time Series", "Time Date Stamp")
  }
  res = read_hdtable(f, tblpath, tspath, geompath, run.type, "Time", "XS_") 
  if (!is.null(which.times))
    res = res[res$Time %in% which.times,]
  if (!is.null(which.stations)) {
    othercols = !str_detect(names(res), "XS_")
    stationcols = names(res) %in% str_c("XS_", which.stations)
    res = res[, which(othercols | stationcols)]
  }
  res
}

#' Sediment By Grain Class Table
#'
#' Read the sediment data output for all grain classes. 
#'
#' @inheritParams read_standard
#' @param which.grains Grain class tables to extract. "" Corresponds to 
#'   the totals, "1" is the first grain class, etc. If NULL, all grain
#'   classes will be returned.
#' @return A dataframe with a column "Time" containing the Date Time 
#'   Stamp data; columns "XS_####" where ### is the cross-section ID;
#'   and column "GrainClass".
#'
#' @import h5 
#' @import dplyr 
#' @import stringr
#' @export
read_sediment = function(f, tablelab, run.type, which.times = NULL, 
  which.stations = NULL, which.grains = NULL) {
  grain.levels = c("", paste(1:20))
  grain.labels = read_grains(f)
  geompath = "Geometry/Cross Sections/River Stations"
  if (run.type == "unsteady") {
    tspath = file.path("Results", "Unsteady", "Output", "Output Blocks",
    "Sediment", "Sediment Time Series", "Time Date Stamp")
    sedimentpath = file.path("Results", "Unsteady", "Output",
    "Output Blocks", "Sediment", "Sediment Time Series",
    "Cross Sections", tablelab)
  } else if (run.type == "quasi") {
    tspath = file.path("Results", "Sediment", "Output Blocks",
    "Sediment", "Sediment Time Series", "Time Date Stamp")
    sedimentpath = file.path("Results", "Sediment", "Output Blocks",
    "Sediment", "Sediment Time Series", "Cross Sections", tablelab)
  }
  alltables = list_sediment(f, sedimentpath)
  if (length(alltables) < 1)
    stop('Table "', sedimentpath, '" could not be found.', call. = FALSE)
  if (!is.null(which.grains)) {
    whichtables = str_c(tablelab, which.grains, sep = " ") %>% str_trim()
    tablepaths = alltables[basename(alltables) %in% whichtables]
  } else {
    tablepaths = alltables
    which.grains = basename(tablepaths) %>% str_replace(tablelab, "") %>%
      str_trim()
  }
  tablelabs = basename(tablepaths)
  res = vector("list", length(tablelabs))
  for (i in seq_along(tablelabs)) {
    res[[i]] = read_standard(f, tablelabs[[i]], run.type, which.times, 
      which.stations)
    res[[i]]["GrainClass"] = factor(which.grains[i], 
      levels = grain.levels, labels = grain.labels)
  }
  do.call(bind_rows, res)
}

#' Read Grain Class Table
#'
#' Read RAS sediment grain class labels.
#'
#' @inheritParams read_standard
#' @return a vector of grain glass labels.
#'
#' @import h5
read_grains = function(f) {
  x = h5file(f)
  on.exit(h5close(x))
  grainpath = file.path("Event Conditions", "Sediment",
    "Grain Class Names")
  if (!existsDataSet(x, grainpath))
    stop('Table "', grainpath, '" could not be found.', call. = FALSE)
  c("ALL", x[grainpath][])
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
list_sediment = function(f, tablelab) {
  x = h5file(f)
  on.exit(h5close(x))
  str_subset(list.datasets(x), tablelab)
}

#' Generic Table Read Function
#'
#' Read RAS sediment data output. 
#'
#' @param f The h5 file to read.
#' @param tablepath The table to read in.
#' @param rowtablepath The table containing the row identifiers.
#' @param coltablepath The table containing the column identifiers.
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
read_hdtable = function(f, tablepath, rowtablepath, coltablepath,
  run.type, rowcolname, colprefix) {
  x = h5file(f)
  on.exit(h5close(x))
  for(pth in c(tablepath, rowtablepath, coltablepath))
    if (!existsDataSet(x, pth))
      stop('Table "', pth, '" could not be found. ',
        'Check that argument "run.type" is correct.',
        call. = FALSE)
  clabs = x[coltablepath][] %>% str_trim()
  rlabs = x[rowtablepath][] %>% str_trim()
  this = x[tablepath][] %>% as_data_frame()
  if (run.type == "unsteady") {
    nr = nrow(this)
    this = this %>% tail(-1) %>% head(-2)
    rlabs = rlabs[c(1, 3:nr)]
    rlabs = rlabs %>% tail(-1) %>% head(-1)
  }
  else if (run.type == "quasi") {
    this = this %>% tail(-1)
    rlabs = rlabs %>% tail(-1)

  }
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
#' @param tcol The time column name.
#' @param diffcol The name of the difference column to be created.
#' @param percent Logical: report differences as percent difference.
#' @return A dataframe, with difference defined as \code{d2- d1}.
#'   if \code{percent = TRUE}, the difference is defined as
#'   \code{(d2 - d1)/(0.5*(d2 + d1))}.
#'
#' @import dplyr
#' @export
diff_table = function(d1, d2, tcol, diffcol, percent = FALSE) {
  Station = NULL # workaround for nse
  datcols = intersect(names(d1), names(d2))
  datcols = datcols[datcols != tcol]
  d1 = d1 %>% arrange_(tcol)
  d2 = d2 %>% arrange_(tcol)
  if (percent)
    fun = function(x1, x2)
      2 * (x2 - x1) / (x2 + x1)
    else
      fun = function(x1, x2)
        x2 - x1
  as_data_frame(cbind(d1[tcol], fun(d1[, datcols], d2[, datcols]))) %>%
    gather_("Station", diffcol, gather_cols = datcols) %>%
    mutate(Station = factor(Station, levels = datcols))
}

#' Difference Table (Sediment)
#'
#' Compute a difference table from sediment data.
#'
#' @inheritParams diff_table
#' @param gcol the grain class column name.
#' @return A dataframe, with difference defined as \code{d2- d1}.
#'   if \code{percent = TRUE}, the difference is defined as
#'   \code{(d2 - d1)/(0.5*(d2 + d1))}
#'
#' @import dplyr
#' @import tidyr
#' @export
diff_sediment = function(d1, d2, tcol, gcol, diffcol, percent = FALSE) {
  Station = NULL # workaround for nse
  datcols = intersect(names(d1), names(d2))
  datcols = datcols[datcols != tcol & datcols != gcol]
  gvals = intersect(unique(d1[[gcol]]), unique(d2[[gcol]]))
  d1 = d1[d1[[gcol]] %in% gvals,] %>% arrange_(tcol, gcol)
  d2 = d2[d2[[gcol]] %in% gvals,] %>% arrange_(tcol, gcol)
  if (percent)
    fun = function(x1, x2) 
      2 * (x2 - x1) / (x2 + x1)
  else
    fun = function(x1, x2) 
      x2 - x1
  as_data_frame(cbind(d1[tcol], d1[gcol], fun(d1[, datcols], d2[, datcols]))) %>%
    gather_("Station", diffcol, gather_cols = datcols) %>%
    mutate(Station = factor(Station, levels = datcols))
}

#' Root Mean Square Error Table
#'
#' Compute RMSE from a difference table.
#'
#' @param d The difference table.
#' @param groupcol the column(s) to group differences by. For standard
#'   tables, \code{groupcol} will typically be either \code{"Station"}
#'    or \code{"Time"}. For sediment tables, \code{groupcol} will 
#'   typically be either \code{c("GrainClass", "Station")} or
#'   \code{c("GrainClass", "Time")}.
#' @param diffcol the column containing difference values.
#' @param rmsecol The output column containing RMSE values
#' @return A dataframe.
#'
#' @importFrom stats setNames
#' @import dplyr
#' @import stringr
#' @export
rmse_table = function(d, groupcol, diffcol, rmsecol) {
  d %>% group_by_(.dots = groupcol) %>% summarize_(
    .dots = setNames(str_c("sqrt(mean(", diffcol, "^2))"), rmsecol))
}
