#' Read Standard Table
#'
#' Read a standard (not grain class-specific) table.
#'
#' @param f The h5 file to read.
#' @param tablelab The table to read.
#' @param run.type The model run type, e.g. "quasi" or "unsteady".
#' @return A dataframe with a column "Time" containing the Date Time 
#'   Stamp data and columns "XS_####" where ### is the cross-section ID.
#'
#' @export
read_standard = function(f, tablelab, run.type) {
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
  read_hdtable(f, tblpath, tspath, geompath, run.type, "Time", "XS_")
}

#' Sediment By Grain Class Table
#'
#' Read the sediment data output for all grain classes. 
#'
#' @inheritParams read_standard
#' @param which.grains Grain class tables to extract. "" Corresponds to 
#'   the totals, "1" is the first grain class, etc.
#' @param which.rows A numeric identifying the row numbers to extract, 
#'   0 being the first row.
#' @return A dataframe with a column "Time" containing the Date Time 
#'   Stamp data and columns "XS_####" where ### is the cross-section ID.
#'
#' @import h5 
#' @import dplyr 
#' @import stringr
#' @export
read_sediment = function(f, tablelab, run.type, which.grains = "",
  which.rows = NULL) {
  x = h5file(f)
  on.exit(h5close(x))
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
  alltables = str_subset(list.datasets(x), sedimentpath)
  if (!missing(which.grains)) {
    whichtables = str_c(tablelab, which.grains, sep = " ") %>% str_trim()
    tablepaths = alltables[basename(alltables) %in% whichtables]
  } else {
    tablepaths = alltables
    which.grains = basename(tablepaths) %>% str_replace(tablelab, "") %>%
      str_trim()
  }
  res = vector("list", length(tablepaths))
  for (i in seq_along(tablepaths)) {
    res[[i]] = read_hdtable(f, tablepaths[i], tspath, geompath,
    run.type, "Time", "XS_")
    res[[i]]["GrainClass"] = which.grains[i]
  }
  if (!missing(which.rows)) {
    res = lapply(res, function(x) x[which.rows + 1,])
  }
  do.call(bind_rows, res)
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
#' @param d1 The first dataframe.
#' @param d2 The second dataframe.
#' @param tcol The time column name.
#' @param diffcol The name of the difference column to be created.
#' @return A dataframe.
#'
#' @import dplyr
#' @export
diff_table = function(d1, d2, tcol, diffcol) {
  Station = NULL # workaround for nse
  datcols = intersect(names(d1), names(d2))
  datcols = datcols[datcols != tcol]
  d1 = d1 %>% arrange_(tcol)
  d2 = d2 %>% arrange_(tcol)
  as_data_frame(cbind(d1[tcol], d1[, datcols] - d2[, datcols])) %>%
    gather_("Station", diffcol, gather_cols = datcols) %>%
    mutate(Station = factor(Station, levels = datcols))
}

#' Difference Table (Sediment)
#'
#' Compute a difference table from sediment data.
#'
#' @param d1 The first dataframe.
#' @param d2 The second dataframe.
#' @param tcol the time column name.
#' @param gcol the grain class column name.
#' @param diffcol The name of the difference column to be created.
#' @return A dataframe.
#'
#' @import dplyr
#' @import tidyr
#' @export
diff_sediment = function(d1, d2, tcol, gcol, diffcol) {
  Station = NULL # workaround for nse
  datcols = intersect(names(d1), names(d2))
  datcols = datcols[datcols != tcol & datcols != gcol]
  gvals = intersect(unique(d1[[gcol]]), unique(d2[[gcol]]))
  d1 = d1[d1[[gcol]] %in% gvals,] %>% arrange_(tcol, gcol)
  d2 = d2[d2[[gcol]] %in% gvals,] %>% arrange_(tcol, gcol)
  as_data_frame(cbind(d1[tcol], d1[gcol], d1[, datcols] - d2[, datcols])) %>%
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
