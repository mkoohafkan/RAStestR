#' @describeIn read_standard Read the Dredged Cum data output.
#' @export
read_dc = function(f, run.type) {
  read_standard(f, "Dredged Cum", run.type)
}

#' @describeIn read_standard Read the Effective Depth data output.
#' @export
read_ed = function(f, run.type) {
  read_standard(f, "Effective Depth", run.type)
}

#' @describeIn read_standard Read the Effective Width data output.
#' @export
read_ew = function(f, run.type) {
  read_standard(f, "Effective Width", run.type)
}

#' @describeIn read_standard Read the Flow data output.
#' @export
read_f = function(f, run.type) {
  read_standard(f, "Flow", run.type)
}

#' @describeIn read_standard Read the Froude Number Channel data output.
#' @export
read_fnc = function(f, run.type) {
  read_standard(f, "Froude Number Channel", run.type)
}

#' @describeIn read_standard Read the Invert Change data output.
#' @export
read_ic = function(f, run.type) {
  read_standard(f, "Invert Change", run.type)
}

#' @describeIn read_standard Read the Invert Elevation data output.
#' @export
read_ie = function(f, run.type) {
  read_standard(f, "Invert Elevation", run.type)
}

#' @describeIn read_standard Read the Manning's n Channel data output.
#' @export
read_mnc = function(f, run.type) {
  read_standard(f, "Manning's n Channel", run.type)
}

#' @describeIn read_standard Read the Mean Effective Invert Change data output.
#' @export
read_meic = function(f, run.type) {
  read_standard(f, "Mean Effective Invert Change", run.type)
}

#' @describeIn read_standard Read the Mean Effective Invert Elevation data output.
#' @export
read_meie = function(f, run.type) {
  read_standard(f, "Mean Effective Invert Elevation", run.type)
}

#' @describeIn read_standard Read the Sediment Concentration data output.
#' @export
read_sc = function(f, run.type) {
  read_standard(f, "Sediment Concentration", run.type)
}

#' @describeIn read_standard Read the Shear Stress data output.
#' @export
read_ss = function(f, run.type) {
  read_standard(f, "Shear Stress", run.type)
}

#' @describeIn read_standard Read the Slope data output.
#' @export
read_s = function(f, run.type) {
  read_standard(f, "Slope", run.type)
}

#' @describeIn read_standard Read the Velocity data output.
#' @export
read_v = function(f, run.type) {
  read_standard(f, "Velocity", run.type)
}

#' @describeIn read_standard Read the Water Surface data output.
#' @export
read_ws = function(f, run.type) {
  read_standard(f, "Water Surface", run.type)
}

#' @describeIn read_standard Read the d10 Active data output.
#' @export
read_d10a = function(f, run.type) {
  read_standard(f, "d10 Active", run.type)
}

#' @describeIn read_standard Read the d10 Inactive data output.
#' @export
read_d10i = function(f, run.type) {
  read_standard(f, "d10 Inactive", run.type)
}

#' @describeIn read_standard Read the d50 Active data output.
#' @export
read_d50a = function(f, run.type) {
  read_standard(f, "d50 Active", run.type)
}

#' @describeIn read_standard Read the d50 Inactive data output.
#' @export
read_d50i = function(f, run.type) {
  read_standard(f, "d50 Inactive", run.type)
}

#' @describeIn read_standard Read the d90 Active data output.
#' @export
read_d90a = function(f, run.type) {
  read_standard(f, "d90 Active", run.type)
}

#' @describeIn read_standard Read the d90 Inactive data output.
#' @export
read_d90i = function(f, run.type) {
  read_standard(f, "d90 Inactive", run.type)
}

#' @describeIn read_sediment Read the Lateral Structure Mass Divergence data output.
#' @export
read_lsmd = function(f, run.type, which.grains, which.rows) {
  read_sediment(f, "Lat Struct Mass Div", run.type, which.grains, which.rows)
}

#' @describeIn read_sediment Read the Longitudinal Cumulative Mass Change data output. 
#' @export
read_lcmc = function(f, run.type, which.grains, which.rows) {
  read_sediment(f, "Long. Cum Mass Change", run.type, which.grains,
    which.rows)
}

#' @describeIn read_sediment Read the Mass Bed Change Cumulative data output.
#' @export
read_mbcc = function(f, run.type, which.grains, which.rows) {
  read_sediment(f, "Mass Bed Change Cum", run.type, which.grains, which.rows)
}

#' @describeIn read_sediment Read the Mass In Cumulative data output.
#' @export
read_mic = function(f, run.type, which.grains, which.rows) {
  read_sediment(f, "Mass In Cum", run.type, which.grains, which.rows)
}

#' @describeIn read_sediment Read the Mass Out Cumulative data output.
#' @export
read_moc = function(f, run.type, which.grains, which.rows) {
  read_sediment(f, "Mass Out Cum", run.type, which.grains, which.rows)
}


#' @describeIn diff_table Compute a difference table for Dredged Cumulative data.
#' @export
diff_dc = function(d1, d2) {
  diff_table(d1, d2, "Time", "diff_dc")
}

#' @describeIn diff_table Compute a difference table for Effective Depth data.
#' @export
diff_ed = function(d1, d2) {
  diff_table(d1, d2, "Time", "diff_ed")
}

#' @describeIn diff_table Compute a difference table for Effective Width data.
#' @export
diff_ew = function(d1, d2) {
  diff_table(d1, d2, "Time", "diff_ew")
}

#' @describeIn diff_table Compute a difference table for Flow data.
#' @export
diff_f = function(d1, d2) {
  diff_table(d1, d2, "Time", "diff_f")
}

#' @describeIn diff_table Compute a difference table for Froude Number Channel data.
#' @export
diff_fnc = function(d1, d2) {
  diff_table(d1, d2, "Time", "diff_fnc")
}

#' @describeIn diff_table Compute a difference table for Invert Change data.
#' @export
diff_ic = function(d1, d2) {
  diff_table(d1, d2, "Time", "diff_ic")
}

#' @describeIn diff_table Compute a difference table for Invert Elevation data.
#' @export
diff_ie = function(d1, d2) {
  diff_table(d1, d2, "Time", "diff_ie")
}

#' @describeIn diff_table Compute a difference table for Manning's n Channel data.
#' @export
diff_mnc = function(d1, d2) {
  diff_table(d1, d2, "Time", "diff_mnc")
}

#' @describeIn diff_table Compute a difference table for Mean Effective Invert Change data.
#' @export
diff_meic = function(d1, d2) {
  diff_table(d1, d2, "Time", "diff_meic")
}

#' @describeIn diff_table Compute a difference table for Mean Effective Invert Elevation data.
#' @export
diff_meie = function(d1, d2) {
  diff_table(d1, d2, "Time", "diff_meie")
}

#' @describeIn diff_table Compute a difference table for Sediment Concentration data.
#' @export
diff_sc = function(d1, d2) {
  diff_table(d1, d2, "Time", "diff_sc")
}

#' @describeIn diff_table Compute a difference table for Shear Stress data.
#' @export
diff_ss = function(d1, d2) {
  diff_table(d1, d2, "Time", "diff_ss")
}

#' @describeIn diff_table Compute a difference table for Slope data.
#' @export
diff_s = function(d1, d2) {
  diff_table(d1, d2, "Time", "diff_s")
}

#' @describeIn diff_table Compute a difference table for Velocity data.
#' @export
diff_v = function(d1, d2) {
  diff_table(d1, d2, "Time", "diff_v")
}

#' @describeIn diff_table Compute a difference table for Water Surface data.
#' @export
diff_ws = function(d1, d2) {
  diff_table(d1, d2, "Time", "diff_ws")
}

#' @describeIn diff_table Compute a difference table for d10 Active data.
#' @export
diff_d10a = function(d1, d2) {
  diff_table(d1, d2, "Time", "diff_d10a")
}

#' @describeIn diff_table Compute a difference table for d10 Inactive data.
#' @export
diff_d10i = function(d1, d2) {
  diff_table(d1, d2, "Time", "diff_d10i")
}

#' @describeIn diff_table Compute a difference table for d50 Active data.
#' @export
diff_d50a = function(d1, d2) {
  diff_table(d1, d2, "Time", "diff_d50a")
}

#' @describeIn diff_table Compute a difference table for d50 Inactive data.
#' @export
diff_d50i = function(d1, d2) {
  diff_table(d1, d2, "Time", "diff_d50i")
}

#' @describeIn diff_table Compute a difference table for d90 Active data.
#' @export
diff_d90a = function(d1, d2) {
  diff_table(d1, d2, "Time", "diff_d90a")
}

#' @describeIn diff_table Compute a difference table for d90 Inactive data.
#' @export
diff_d90i = function(d1, d2) {
  diff_table(d1, d2, "Time", "diff_d90i")
}

#' @describeIn diff_sediment Compute a difference table for Lateral Structure Mass Divergence data.
#' @export
diff_lsmd = function(d1, d2) {
  diff_sediment(d1, d2, "Time", "GrainClass", "diff_lsmd")
}

#' @describeIn diff_sediment Compute a difference table for Longitudinal Cumulative Mass Change data.
#' @export
diff_lcmc = function(d1, d2) {
  diff_sediment(d1, d2, "Time", "GrainClass", "diff_lcmc")
}

#' @describeIn diff_sediment Compute a difference table for Mass Bed Change Cumulative data.
#' @export
diff_mbcc = function(d1, d2) {
  diff_sediment(d1, d2, "Time", "GrainClass", "diff_mbcc")
}

#' @describeIn diff_sediment Compute a difference table for Mass In Cumulative data.
#' @export
diff_mic = function(d1, d2) {
  diff_sediment(d1, d2, "Time", "GrainClass", "diff_mic")
}

#' @describeIn diff_sediment Compute a difference table for Mass Out Cumulative data.
#' @export
diff_moc = function(d1, d2) {
  diff_sediment(d1, d2, "Time", "GrainClass", "diff_moc")
}


#' @describeIn rmse_table Compute RMSE of Dredged Cumulative outputs. 
#' @export
rmse_dc = function(d, groupcol = "Station") {
  rmse_table(d, groupcol, "diff_dc", "rmse_dc")
}

#' @describeIn rmse_table Compute RMSE of Effective Depth outputs. 
#' @export
rmse_ed = function(d, groupcol = "Station") {
  rmse_table(d, groupcol, "diff_ed", "rmse_ed")
}

#' @describeIn rmse_table Compute RMSE of Effective Width outputs. 
#' @export
rmse_ew = function(d, groupcol = "Station") {
  rmse_table(d, groupcol, "diff_ew", "rmse_ew")
}

#' @describeIn rmse_table Compute RMSE of Flow outputs. 
#' @export
rmse_f = function(d, groupcol = "Station") {
  rmse_table(d, groupcol, "diff_f", "rmse_f")
}

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs. 
#' @export
rmse_fnc = function(d, groupcol = "Station") {
  rmse_table(d, groupcol, "diff_fnc", "rmse_fnc")
}

#' @describeIn rmse_table Compute RMSE of Invert Change outputs. 
#' @export
rmse_ic = function(d, groupcol = "Station") {
  rmse_table(d, groupcol, "diff_ic", "rmse_ic")
}

#' @describeIn rmse_table Compute RMSE of Invert Elevation outputs. 
#' @export
rmse_ie = function(d, groupcol = "Station") {
  rmse_table(d, groupcol, "diff_ie", "rmse_ie")
}

#' @describeIn rmse_table Compute RMSE of Manning's n Channel outputs. 
#' @export
rmse_mnc = function(d, groupcol = "Station") {
  rmse_table(d, groupcol, "diff_mnc", "rmse_mnc")
}

#' @describeIn rmse_table Compute RMSE of Mean Effective Invert Change outputs. 
#' @export
rmse_meic = function(d, groupcol = "Station") {
  rmse_table(d, groupcol, "diff_meic", "rmse_meic")
}

#' @describeIn rmse_table Compute RMSE of Mean Effective Invert Elevation outputs. 
#' @export
rmse_meie = function(d, groupcol = "Station") {
  rmse_table(d, groupcol, "diff_meie", "rmse_meie")
}

#' @describeIn rmse_table Compute RMSE of Sediment Concentration outputs. 
#' @export
rmse_sc = function(d, groupcol = "Station") {
  rmse_table(d, groupcol, "diff_sc", "rmse_sc")
}

#' @describeIn rmse_table Compute RMSE of Shear Stress outputs. 
#' @export
rmse_ss = function(d, groupcol = "Station") {
  rmse_table(d, groupcol, "diff_ss", "rmse_ss")
}

#' @describeIn rmse_table Compute RMSE of Slope outputs. 
#' @export
rmse_s = function(d, groupcol = "Station") {
  rmse_table(d, groupcol, "diff_s", "rmse_s")
}

#' @describeIn rmse_table Compute RMSE of Velocity outputs. 
#' @export
rmse_v = function(d, groupcol = "Station") {
  rmse_table(d, groupcol, "diff_v", "rmse_v")
}

#' @describeIn rmse_table Compute RMSE of Water Surface outputs. 
#' @export
rmse_ws = function(d, groupcol = "Station") {
  rmse_table(d, groupcol, "diff_ws", "rmse_ws")
}

#' @describeIn rmse_table Compute RMSE of d10 Active outputs. 
#' @export
rmse_d10a = function(d, groupcol = "Station") {
  rmse_table(d, groupcol, "diff_d10a", "rmse_d10a")
}

#' @describeIn rmse_table Compute RMSE of d10 Inactive outputs. 
#' @export
rmse_d10i = function(d, groupcol = "Station") {
  rmse_table(d, groupcol, "diff_d10i", "rmse_d10i")
}

#' @describeIn rmse_table Compute RMSE of d50 Active outputs. 
#' @export
rmse_d50a = function(d, groupcol = "Station") {
  rmse_table(d, groupcol, "diff_d50a", "rmse_d50a")
}

#' @describeIn rmse_table Compute RMSE of d50 Inactive outputs. 
#' @export
rmse_d50i = function(d, groupcol = "Station") {
  rmse_table(d, groupcol, "diff_d50i", "rmse_d50i")
}

#' @describeIn rmse_table Compute RMSE of d90 Active outputs. 
#' @export
rmse_d90a = function(d, groupcol = "Station") {
  rmse_table(d, groupcol, "diff_d90a", "rmse_d90a")
}

#' @describeIn rmse_table Compute RMSE of d90 Inactive outputs. 
#' @export
rmse_d90i = function(d, groupcol = "Station") {
  rmse_table(d, groupcol, "diff_d90i", "rmse_d90i")
}

#' @describeIn rmse_table Compute RMSE of Lateral Structure Mass Divergence outputs. 
#' @export
rmse_lsmd = function(d, groupcol = "Station") {
  rmse_table(d, c("GrainClass", groupcol), "diff_lsmd", "rmse_lsmd")
}

#' @describeIn rmse_table Compute RMSE of Longitudinal Cumulative Mass Change outputs. 
#' @export
rmse_lcmc = function(d, groupcol = "Station") {
  rmse_table(d, c("GrainClass", groupcol), "diff_lcmc", "rmse_lcmc")
}

#' @describeIn rmse_table Compute RMSE of Mass Bed Change Cumulative outputs. 
#' @export
rmse_mbcc = function(d, groupcol = "Station") {
  rmse_table(d, c("GrainClass", groupcol), "diff_mbcc", "rmse_mbcc")
}

#' @describeIn rmse_table Compute RMSE of Mass In Cumulative outputs. 
#' @export
rmse_mic = function(d, groupcol = "Station") {
  rmse_table(d, c("GrainClass", groupcol), "diff_mic", "rmse_mic")
}

#' @describeIn rmse_table Compute RMSE of Mass Out Cumulative outputs. 
#' @export
rmse_moc = function(d, groupcol = "Station") {
  rmse_table(d, c("GrainClass", groupcol), "diff_moc", "rmse_moc")
}
