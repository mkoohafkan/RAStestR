#' Water Surface Elevation Table
#'
#' Read the Water Surface Elevation sediment data output. 
#'
#' @param f The h5 object to read.
#' @param run.type If TRUE, the data is from an unsteady flow plan
#'   and needs some additional cleanup. Use FALSE for quasi-unsteady 
#'   data.
#' @return A dataframe with a column "Time" containing the Date Time 
#'   Stamp data and columns "XS_####" where ### is the cross-section ID.
#'
#' @export
readwse = function(f, run.type) {
    geompath = "Geometry/Cross Sections/River Stations"
    if (run.type == "unsteady") {
        wsepath = file.path("Results", "Unsteady", "Output", "Output Blocks",
      "Sediment", "Sediment Time Series", "Cross Sections",
      "Water Surface")
        tspath = file.path("Results", "Unsteady", "Output", "Output Blocks",
      "Sediment", "Sediment Time Series", "Time Date Stamp")
    } else if (run.type == "quasi"){
        wsepath = file.path("Results", "Sediment", "Output Blocks",
      "Sediment", "Sediment Time Series", "Cross Sections",
      "Water Surface")
        tspath = file.path("Results", "Sediment", "Output Blocks",
      "Sediment", "Sediment Time Series", "Time Date Stamp")
    }
    read_hdtable(f, wsepath, tspath, geompath, run.type, "Time", "XS_")
}

#' Cumulative Mass In Table
#'
#' Read the Cumulative Mass In sediment data output. 
#'
#' @param f The h5 object to read.
#' @param run.type If TRUE, the data is from an unsteady flow plan
#'   and needs some additional cleanup. Use FALSE for quasi-unsteady 
#'   data.
#' @param which.grains Which grain class tables to extract data from. If
#'   missing, all grain class tables will be extracted. Character value
#'   "" extracts the total table, "1" the first grain class, etc.
#' @param which.rows Which rows to extract from each grain class table. 
#'   0 refers to the first row of the HDF5 table. Note that this index 
#'   refers to the cleaned-up data, so check the row ID to ensure the 
#'   correct rows are extracted.
#' @return A dataframe with a column "Time" containing the Date Time 
#'   Stamp data and columns "XS_####" where ### is the cross-section ID.
#'
#' @export
readcmi = function(f, run.type, which.grains, which.rows) {
    readsediment(f, "Mass In Cum", run.type, which.grains, which.rows)
}

#' Longitudinal Cumulative Mass Change Table
#'
#' Read the Longitudinal Cumulative Mass Change sediment data output. 
#'
#' @param x The h5 object to read.
#' @param run.type If TRUE, the data is from an unsteady flow plan
#'   and needs some additional cleanup. Use FALSE for quasi-unsteady 
#'   data.
#' @param which.grains Which grain class tables to extract data from. If
#'   missing, all grain class tables will be extracted. Character value
#'   "" extracts the total table, "1" the first grain class, etc.
#' @param which.rows Which rows to extract from each grain class table. 
#'   0 refers to the first row of the HDF5 table. Note that this index 
#'   refers to the cleaned-up data, so check the row ID to ensure the 
#'   correct rows are extracted.
#' @return A dataframe with a column "Time" containing the Date Time 
#'   Stamp data and columns "XS_####" where ### is the cross-section ID.
#'
#' @export
readlcmc = function(f, run.type, which.grains, which.rows) {
    readsediment(f, "Long. Cum Mass Change", run.type, which.grains,
    which.rows)
}

#' Sediment By Grain Class Table
#'
#' Read the sediment data output for all grain classes. 
#'
#' @param f The h5 object to read.
#' @param run.type If TRUE, the data is from an unsteady flow plan
#'   and needs some additional cleanup. Use FALSE for quasi-unsteady 
#'   data.
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
readsediment = function(f, tablelab, run.type, which.grains = "",
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
#' Read RAS data output. 
#'
#' @param f The h5 file to read.
#' @param tablepath The table to read in.
#' @param rowtablepath The table containing the row identifiers.
#' @param coltablepath The table containing the column identifiers.
#' @param run.type If TRUE, the data is from an unsteady flow plan
#'   and needs some additional cleanup. Use FALSE for quasi-unsteady 
#'   data.
#' @param rowcolname The name to assign to the new row id column.
#' @param colprefix A prefix to apply to the column IDs. 
#' @return A dataframe.
#'
#' @import h5 
#' @import dplyr 
#' @import stringr
#' @export
read_hdtable = function(f, tablepath, rowtablepath, coltablepath,
  run.type, rowcolname, colprefix) {
  x = h5file(f)
  on.exit(h5close(x))
  clabs = x[coltablepath][] %>% str_trim()
  rlabs = x[rowtablepath][] %>% str_trim()
  this = x[tablepath][] %>% as_data_frame()
  if (run.type == "unsteady") {
      this = this %>% tail(-1) %>% head(-1)
      rlabs[2] = rlabs[1]
      rlabs = rlabs %>% tail(-1) %>% head(-1)
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
#' @param tcol the time column name.
#' @param profcol The name of the profile column to be created.
#' @param diffcol The name of the difference column to be created.
#' @return A dataframe.
#'
#' @import dplyr
#' @export
diff_table = function(d1, d2, tcol, profcol, diffcol) {
    datcols = intersect(names(d1), names(d2))
    datcols = datcols[datcols != tcol]
    d1 = d1 %>% arrange_(tcol)
    d2 = d2 %>% arrange_(tcol)
    as_data_frame(cbind(d1[tcol], d1[, datcols] - d2[, datcols])) %>%
    gather_(profcol, diffcol, gather_cols = datcols) %>%
    mutate(profile = factor(profile, levels = datcols))
}

#' Difference Table (Sediment)
#'
#' Compute a difference table from sediment data.
#'
#' @param d1 The first dataframe.
#' @param d2 The second dataframe.
#' @param tcol the time column name.
#' @param gcol the grain class column name.
#' @param profcol The name of the profile column to be created.
#' @param diffcol The name of the difference column to be created.
#' @return A dataframe.
#'
#' @import dplyr
#' @import tidyr
#' @export
diff_sediment = function(d1, d2, tcol, gcol, profcol, diffcol) {
    datcols = intersect(names(d1), names(d2))
    datcols = datcols[datcols != tcol & datcols != gcol]
    gvals = intersect(unique(d1[[gcol]]), unique(d2[[gcol]]))
    d1 = d1[d1[[gcol]] %in% gvals,] %>% arrange_(tcol, gcol)
    d2 = d2[d2[[gcol]] %in% gvals,] %>% arrange_(tcol, gcol)
    as_data_frame(cbind(d1[tcol], d1[gcol], d1[, datcols] - d2[, datcols])) %>%
    gather_(profcol, diffcol, gather_cols = datcols) %>%
    mutate(profile = factor(profile, levels = datcols))
}

#' Water Surface Elevation Difference Table
#'
#' Compute a difference table for water surface elevation data.
#'
#' @param d1 The first dataframe.
#' @param d2 The second dataframe.
#' @param tcol the time column name.
#' @return A dataframe.
#'
#' @export
diff_wse = function(d1, d2, tcol = "Time") {
    diff_table(d1, d2, tcol, "profile", "diff_wse")
}

#' Longitudinal Cumulative Mass Change Difference Table
#'
#' Compute a difference table for longitudinal cumulative mass change data.
#'
#' @param d1 The first dataframe.
#' @param d2 The second dataframe.
#' @param tcol the time column name.
#' @param gcol the grain class column name.
#' @return A dataframe.
#'
#' @export
diff_lcmc = function(d1, d2, tcol = "Time", gcol = "GrainClass") {
    diff_sediment(d1, d2, tcol, gcol, "profile", "diff_lcmc")
}

#' Cumulative Mass In Difference Table
#'
#' Compute a difference table for cumulative mass in data.
#'
#' @param d1 The first dataframe.
#' @param d2 The second dataframe.
#' @param tcol the time column name.
#' @param gcol the grain class column name.
#' @return A dataframe.
#'
#' @export
diff_cmi = function(d1, d2, tcol = "Time", gcol = "GrainClass") {
    diff_sediment(d1, d2, tcol, gcol, "profile", "diff_cmi")
}

#' Root Mean Square Error Table
#'
#' Compute RMSE from a difference table.
#'
#' @param d The difference table.
#' @param groupcol the column to group differences by.
#' @param diffcol the column containing difference values.
#' @return A dataframe.
#'
#' @import dplyr
#' @import stringr
#' @export
rmse_table = function(d, groupcol, diffcol) {
    d %>% group_by_(.dots = groupcol) %>%
    summarize_(rmse = str_c("sqrt(mean(", diffcol, "^2))"))
}
