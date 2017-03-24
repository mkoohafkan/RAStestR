#' @describeIn read_standard Read the Dredged Cum data output.
#' @export
read_dredged_cum = function(f, run.type, which.times = NULL, which.stations = NULL)
  read_standard(f, "Dredged Cum", run.type, which.times, which.stations)

#' @describeIn diff_table Compute a difference table for Dredged Cum data.
#' @export
diff_dredged_cum = function(d1, d2, relative = FALSE)
  diff_table(d1, d2, "Time", "Diff_dredged_cum", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_dredged_cum = function(d, group.col = "Station")
  rmse_table(d, group.col, "Diff_dredged_cum", "RMSE_dredged_cum")

#' @describeIn read_standard Read the Effective Depth data output.
#' @export
read_effective_depth = function(f, run.type, which.times = NULL, which.stations = NULL)
  read_standard(f, "Effective Depth", run.type, which.times, which.stations)

#' @describeIn diff_table Compute a difference table for Effective Depth data.
#' @export
diff_effective_depth = function(d1, d2, relative = FALSE)
  diff_table(d1, d2, "Time", "Diff_effective_depth", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_effective_depth = function(d, group.col = "Station")
  rmse_table(d, group.col, "Diff_effective_depth", "RMSE_effective_depth")

#' @describeIn read_standard Read the Effective_Width data output.
#' @export
read_effective_width = function(f, run.type, which.times = NULL, which.stations = NULL)
  read_standard(f, "Effective_Width", run.type, which.times, which.stations)

#' @describeIn diff_table Compute a difference table for Effective_Width data.
#' @export
diff_effective_width = function(d1, d2, relative = FALSE)
  diff_table(d1, d2, "Time", "Diff_effective_width", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_effective_width = function(d, group.col = "Station")
  rmse_table(d, group.col, "Diff_effective_width", "RMSE_effective_width")

#' @describeIn read_standard Read the Flow data output.
#' @export
read_flow = function(f, run.type, which.times = NULL, which.stations = NULL)
  read_standard(f, "Flow", run.type, which.times, which.stations)

#' @describeIn diff_table Compute a difference table for Flow data.
#' @export
diff_flow = function(d1, d2, relative = FALSE)
  diff_table(d1, d2, "Time", "Diff_flow", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_flow = function(d, group.col = "Station")
  rmse_table(d, group.col, "Diff_flow", "RMSE_flow")

#' @describeIn read_standard Read the Froude Number Channel data output.
#' @export
read_froude_number_channel = function(f, run.type, which.times = NULL, which.stations = NULL)
  read_standard(f, "Froude Number Channel", run.type, which.times, which.stations)

#' @describeIn diff_table Compute a difference table for Froude Number Channel data.
#' @export
diff_froude_number_channel = function(d1, d2, relative = FALSE)
  diff_table(d1, d2, "Time", "Diff_froude_number_channel", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_froude_number_channel = function(d, group.col = "Station")
  rmse_table(d, group.col, "Diff_froude_number_channel", "RMSE_froude_number_channel")

#' @describeIn read_standard Read the Hydraulic Radius data output.
#' @export
read_hydraulic_radius = function(f, run.type, which.times = NULL, which.stations = NULL)
  read_standard(f, "Hydraulic Radius", run.type, which.times, which.stations)

#' @describeIn diff_table Compute a difference table for Hydraulic Radius data.
#' @export
diff_hydraulic_radius = function(d1, d2, relative = FALSE)
  diff_table(d1, d2, "Time", "Diff_hydraulic_radius", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_hydraulic_radius = function(d, group.col = "Station")
  rmse_table(d, group.col, "Diff_hydraulic_radius", "RMSE_hydraulic_radius")

#' @describeIn read_standard Read the Invert Change data output.
#' @export
read_invert_change = function(f, run.type, which.times = NULL, which.stations = NULL)
  read_standard(f, "Invert Change", run.type, which.times, which.stations)

#' @describeIn diff_table Compute a difference table for Invert Change data.
#' @export
diff_invert_change = function(d1, d2, relative = FALSE)
  diff_table(d1, d2, "Time", "Diff_invert_change", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_invert_change = function(d, group.col = "Station")
  rmse_table(d, group.col, "Diff_invert_change", "RMSE_invert_change")

#' @describeIn read_standard Read the Invert Elevation data output.
#' @export
read_invert_elevation = function(f, run.type, which.times = NULL, which.stations = NULL)
  read_standard(f, "Invert Elevation", run.type, which.times, which.stations)

#' @describeIn diff_table Compute a difference table for Invert Elevation data.
#' @export
diff_invert_elevation = function(d1, d2, relative = FALSE)
  diff_table(d1, d2, "Time", "Diff_invert_elevation", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_invert_elevation = function(d, group.col = "Station")
  rmse_table(d, group.col, "Diff_invert_elevation", "RMSE_invert_elevation")

#' @describeIn read_standard Read the Mannings n Channel data output.
#' @export
read_mannings_n_channel = function(f, run.type, which.times = NULL, which.stations = NULL)
  read_standard(f, "Mannings n Channel", run.type, which.times, which.stations)

#' @describeIn diff_table Compute a difference table for Mannings n Channel data.
#' @export
diff_mannings_n_channel = function(d1, d2, relative = FALSE)
  diff_table(d1, d2, "Time", "Diff_mannings_n_channel", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_mannings_n_channel = function(d, group.col = "Station")
  rmse_table(d, group.col, "Diff_mannings_n_channel", "RMSE_mannings_n_channel")

#' @describeIn read_standard Read the Mean Effective Invert Change data output.
#' @export
read_mean_effective_invert_change = function(f, run.type, which.times = NULL, which.stations = NULL)
  read_standard(f, "Mean Effective Invert Change", run.type, which.times, which.stations)

#' @describeIn diff_table Compute a difference table for Mean Effective Invert Change data.
#' @export
diff_mean_effective_invert_change = function(d1, d2, relative = FALSE)
  diff_table(d1, d2, "Time", "Diff_mean_effective_invert_change", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_mean_effective_invert_change = function(d, group.col = "Station")
  rmse_table(d, group.col, "Diff_mean_effective_invert_change", "RMSE_mean_effective_invert_change")

#' @describeIn read_standard Read the Mean Effective Invert Elevation data output.
#' @export
read_mean_effective_invert_elevation = function(f, run.type, which.times = NULL, which.stations = NULL)
  read_standard(f, "Mean Effective Invert Elevation", run.type, which.times, which.stations)

#' @describeIn diff_table Compute a difference table for Mean Effective Invert Elevation data.
#' @export
diff_mean_effective_invert_elevation = function(d1, d2, relative = FALSE)
  diff_table(d1, d2, "Time", "Diff_mean_effective_invert_elevation", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_mean_effective_invert_elevation = function(d, group.col = "Station")
  rmse_table(d, group.col, "Diff_mean_effective_invert_elevation", "RMSE_mean_effective_invert_elevation")

#' @describeIn read_standard Read the Moveable Elv L data output.
#' @export
read_moveable_elv_l = function(f, run.type, which.times = NULL, which.stations = NULL)
  read_standard(f, "Moveable Elv L", run.type, which.times, which.stations)

#' @describeIn diff_table Compute a difference table for Moveable Elv L data.
#' @export
diff_moveable_elv_l = function(d1, d2, relative = FALSE)
  diff_table(d1, d2, "Time", "Diff_moveable_elv_l", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_moveable_elv_l = function(d, group.col = "Station")
  rmse_table(d, group.col, "Diff_moveable_elv_l", "RMSE_moveable_elv_l")

#' @describeIn read_standard Read the Moveable Elv R data output.
#' @export
read_moveable_elv_r = function(f, run.type, which.times = NULL, which.stations = NULL)
  read_standard(f, "Moveable Elv R", run.type, which.times, which.stations)

#' @describeIn diff_table Compute a difference table for Moveable Elv R data.
#' @export
diff_moveable_elv_r = function(d1, d2, relative = FALSE)
  diff_table(d1, d2, "Time", "Diff_moveable_elv_r", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_moveable_elv_r = function(d, group.col = "Station")
  rmse_table(d, group.col, "Diff_moveable_elv_r", "RMSE_moveable_elv_r")

#' @describeIn read_standard Read the Moveable Sta L data output.
#' @export
read_moveable_sta_l = function(f, run.type, which.times = NULL, which.stations = NULL)
  read_standard(f, "Moveable Sta L", run.type, which.times, which.stations)

#' @describeIn diff_table Compute a difference table for Moveable Sta L data.
#' @export
diff_moveable_sta_l = function(d1, d2, relative = FALSE)
  diff_table(d1, d2, "Time", "Diff_moveable_sta_l", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_moveable_sta_l = function(d, group.col = "Station")
  rmse_table(d, group.col, "Diff_moveable_sta_l", "RMSE_moveable_sta_l")

#' @describeIn read_standard Read the Moveable Sta R data output.
#' @export
read_moveable_sta_r = function(f, run.type, which.times = NULL, which.stations = NULL)
  read_standard(f, "Moveable Sta R", run.type, which.times, which.stations)

#' @describeIn diff_table Compute a difference table for Moveable Sta R data.
#' @export
diff_moveable_sta_r = function(d1, d2, relative = FALSE)
  diff_table(d1, d2, "Time", "Diff_moveable_sta_r", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_moveable_sta_r = function(d, group.col = "Station")
  rmse_table(d, group.col, "Diff_moveable_sta_r", "RMSE_moveable_sta_r")

#' @describeIn read_standard Read the Observed Data data output.
#' @export
read_observed_data = function(f, run.type, which.times = NULL, which.stations = NULL)
  read_standard(f, "Observed Data", run.type, which.times, which.stations)

#' @describeIn diff_table Compute a difference table for Observed Data data.
#' @export
diff_observed_data = function(d1, d2, relative = FALSE)
  diff_table(d1, d2, "Time", "Diff_observed_data", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_observed_data = function(d, group.col = "Station")
  rmse_table(d, group.col, "Diff_observed_data", "RMSE_observed_data")

#' @describeIn read_standard Read the Sediment Concentration data output.
#' @export
read_sediment_concentration = function(f, run.type, which.times = NULL, which.stations = NULL)
  read_standard(f, "Sediment Concentration", run.type, which.times, which.stations)

#' @describeIn diff_table Compute a difference table for Sediment Concentration data.
#' @export
diff_sediment_concentration = function(d1, d2, relative = FALSE)
  diff_table(d1, d2, "Time", "Diff_sediment_concentration", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_sediment_concentration = function(d, group.col = "Station")
  rmse_table(d, group.col, "Diff_sediment_concentration", "RMSE_sediment_concentration")

#' @describeIn read_standard Read the Shear Stress data output.
#' @export
read_shear_stress = function(f, run.type, which.times = NULL, which.stations = NULL)
  read_standard(f, "Shear Stress", run.type, which.times, which.stations)

#' @describeIn diff_table Compute a difference table for Shear Stress data.
#' @export
diff_shear_stress = function(d1, d2, relative = FALSE)
  diff_table(d1, d2, "Time", "Diff_shear_stress", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_shear_stress = function(d, group.col = "Station")
  rmse_table(d, group.col, "Diff_shear_stress", "RMSE_shear_stress")

#' @describeIn read_standard Read the Shear Velocity data output.
#' @export
read_shear_velocity = function(f, run.type, which.times = NULL, which.stations = NULL)
  read_standard(f, "Shear Velocity", run.type, which.times, which.stations)

#' @describeIn diff_table Compute a difference table for Shear Velocity data.
#' @export
diff_shear_velocity = function(d1, d2, relative = FALSE)
  diff_table(d1, d2, "Time", "Diff_shear_velocity", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_shear_velocity = function(d, group.col = "Station")
  rmse_table(d, group.col, "Diff_shear_velocity", "RMSE_shear_velocity")

#' @describeIn read_standard Read the Slope data output.
#' @export
read_slope = function(f, run.type, which.times = NULL, which.stations = NULL)
  read_standard(f, "Slope", run.type, which.times, which.stations)

#' @describeIn diff_table Compute a difference table for Slope data.
#' @export
diff_slope = function(d1, d2, relative = FALSE)
  diff_table(d1, d2, "Time", "Diff_slope", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_slope = function(d, group.col = "Station")
  rmse_table(d, group.col, "Diff_slope", "RMSE_slope")

#' @describeIn read_standard Read the Temperature data output.
#' @export
read_temperature = function(f, run.type, which.times = NULL, which.stations = NULL)
  read_standard(f, "Temperature", run.type, which.times, which.stations)

#' @describeIn diff_table Compute a difference table for Temperature data.
#' @export
diff_temperature = function(d1, d2, relative = FALSE)
  diff_table(d1, d2, "Time", "Diff_temperature", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_temperature = function(d, group.col = "Station")
  rmse_table(d, group.col, "Diff_temperature", "RMSE_temperature")

#' @describeIn read_standard Read the Thickness Cover data output.
#' @export
read_thickness_cover = function(f, run.type, which.times = NULL, which.stations = NULL)
  read_standard(f, "Thickness Cover", run.type, which.times, which.stations)

#' @describeIn diff_table Compute a difference table for Thickness Cover data.
#' @export
diff_thickness_cover = function(d1, d2, relative = FALSE)
  diff_table(d1, d2, "Time", "Diff_thickness_cover", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_thickness_cover = function(d, group.col = "Station")
  rmse_table(d, group.col, "Diff_thickness_cover", "RMSE_thickness_cover")

#' @describeIn read_standard Read the Thickness Inactive data output.
#' @export
read_thickness_inactive = function(f, run.type, which.times = NULL, which.stations = NULL)
  read_standard(f, "Thickness Inactive", run.type, which.times, which.stations)

#' @describeIn diff_table Compute a difference table for Thickness Inactive data.
#' @export
diff_thickness_inactive = function(d1, d2, relative = FALSE)
  diff_table(d1, d2, "Time", "Diff_thickness_inactive", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_thickness_inactive = function(d, group.col = "Station")
  rmse_table(d, group.col, "Diff_thickness_inactive", "RMSE_thickness_inactive")

#' @describeIn read_standard Read the Thickness Subsurface data output.
#' @export
read_thickness_subsurface = function(f, run.type, which.times = NULL, which.stations = NULL)
  read_standard(f, "Thickness Subsurface", run.type, which.times, which.stations)

#' @describeIn diff_table Compute a difference table for Thickness Subsurface data.
#' @export
diff_thickness_subsurface = function(d1, d2, relative = FALSE)
  diff_table(d1, d2, "Time", "Diff_thickness_subsurface", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_thickness_subsurface = function(d, group.col = "Station")
  rmse_table(d, group.col, "Diff_thickness_subsurface", "RMSE_thickness_subsurface")

#' @describeIn read_standard Read the Velocity data output.
#' @export
read_velocity = function(f, run.type, which.times = NULL, which.stations = NULL)
  read_standard(f, "Velocity", run.type, which.times, which.stations)

#' @describeIn diff_table Compute a difference table for Velocity data.
#' @export
diff_velocity = function(d1, d2, relative = FALSE)
  diff_table(d1, d2, "Time", "Diff_velocity", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_velocity = function(d, group.col = "Station")
  rmse_table(d, group.col, "Diff_velocity", "RMSE_velocity")

#' @describeIn read_standard Read the Water Surface data output.
#' @export
read_water_surface = function(f, run.type, which.times = NULL, which.stations = NULL)
  read_standard(f, "Water Surface", run.type, which.times, which.stations)

#' @describeIn diff_table Compute a difference table for Water Surface data.
#' @export
diff_water_surface = function(d1, d2, relative = FALSE)
  diff_table(d1, d2, "Time", "Diff_water_surface", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_water_surface = function(d, group.col = "Station")
  rmse_table(d, group.col, "Diff_water_surface", "RMSE_water_surface")

#' @describeIn read_standard Read the d10 Active data output.
#' @export
read_d10_active = function(f, run.type, which.times = NULL, which.stations = NULL)
  read_standard(f, "d10 Active", run.type, which.times, which.stations)

#' @describeIn diff_table Compute a difference table for d10 Active data.
#' @export
diff_d10_active = function(d1, d2, relative = FALSE)
  diff_table(d1, d2, "Time", "Diff_d10_active", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_d10_active = function(d, group.col = "Station")
  rmse_table(d, group.col, "Diff_d10_active", "RMSE_d10_active")

#' @describeIn read_standard Read the d10 Cover data output.
#' @export
read_d10_cover = function(f, run.type, which.times = NULL, which.stations = NULL)
  read_standard(f, "d10 Cover", run.type, which.times, which.stations)

#' @describeIn diff_table Compute a difference table for d10 Cover data.
#' @export
diff_d10_cover = function(d1, d2, relative = FALSE)
  diff_table(d1, d2, "Time", "Diff_d10_cover", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_d10_cover = function(d, group.col = "Station")
  rmse_table(d, group.col, "Diff_d10_cover", "RMSE_d10_cover")

#' @describeIn read_standard Read the d10 Inactive data output.
#' @export
read_d10_inactive = function(f, run.type, which.times = NULL, which.stations = NULL)
  read_standard(f, "d10 Inactive", run.type, which.times, which.stations)

#' @describeIn diff_table Compute a difference table for d10 Inactive data.
#' @export
diff_d10_inactive = function(d1, d2, relative = FALSE)
  diff_table(d1, d2, "Time", "Diff_d10_inactive", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_d10_inactive = function(d, group.col = "Station")
  rmse_table(d, group.col, "Diff_d10_inactive", "RMSE_d10_inactive")

#' @describeIn read_standard Read the d10 Subsurface data output.
#' @export
read_d10_subsurface = function(f, run.type, which.times = NULL, which.stations = NULL)
  read_standard(f, "d10 Subsurface", run.type, which.times, which.stations)

#' @describeIn diff_table Compute a difference table for d10 Subsurface data.
#' @export
diff_d10_subsurface = function(d1, d2, relative = FALSE)
  diff_table(d1, d2, "Time", "Diff_d10_subsurface", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_d10_subsurface = function(d, group.col = "Station")
  rmse_table(d, group.col, "Diff_d10_subsurface", "RMSE_d10_subsurface")

#' @describeIn read_standard Read the d16 Active data output.
#' @export
read_d16_active = function(f, run.type, which.times = NULL, which.stations = NULL)
  read_standard(f, "d16 Active", run.type, which.times, which.stations)

#' @describeIn diff_table Compute a difference table for d16 Active data.
#' @export
diff_d16_active = function(d1, d2, relative = FALSE)
  diff_table(d1, d2, "Time", "Diff_d16_active", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_d16_active = function(d, group.col = "Station")
  rmse_table(d, group.col, "Diff_d16_active", "RMSE_d16_active")

#' @describeIn read_standard Read the d16 Cover data output.
#' @export
read_d16_cover = function(f, run.type, which.times = NULL, which.stations = NULL)
  read_standard(f, "d16 Cover", run.type, which.times, which.stations)

#' @describeIn diff_table Compute a difference table for d16 Cover data.
#' @export
diff_d16_cover = function(d1, d2, relative = FALSE)
  diff_table(d1, d2, "Time", "Diff_d16_cover", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_d16_cover = function(d, group.col = "Station")
  rmse_table(d, group.col, "Diff_d16_cover", "RMSE_d16_cover")

#' @describeIn read_standard Read the d16 Inactive data output.
#' @export
read_d16_inactive = function(f, run.type, which.times = NULL, which.stations = NULL)
  read_standard(f, "d16 Inactive", run.type, which.times, which.stations)

#' @describeIn diff_table Compute a difference table for d16 Inactive data.
#' @export
diff_d16_inactive = function(d1, d2, relative = FALSE)
  diff_table(d1, d2, "Time", "Diff_d16_inactive", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_d16_inactive = function(d, group.col = "Station")
  rmse_table(d, group.col, "Diff_d16_inactive", "RMSE_d16_inactive")

#' @describeIn read_standard Read the d16 Subsurface data output.
#' @export
read_d16_subsurface = function(f, run.type, which.times = NULL, which.stations = NULL)
  read_standard(f, "d16 Subsurface", run.type, which.times, which.stations)

#' @describeIn diff_table Compute a difference table for d16 Subsurface data.
#' @export
diff_d16_subsurface = function(d1, d2, relative = FALSE)
  diff_table(d1, d2, "Time", "Diff_d16_subsurface", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_d16_subsurface = function(d, group.col = "Station")
  rmse_table(d, group.col, "Diff_d16_subsurface", "RMSE_d16_subsurface")

#' @describeIn read_standard Read the d50 Active data output.
#' @export
read_d50_active = function(f, run.type, which.times = NULL, which.stations = NULL)
  read_standard(f, "d50 Active", run.type, which.times, which.stations)

#' @describeIn diff_table Compute a difference table for d50 Active data.
#' @export
diff_d50_active = function(d1, d2, relative = FALSE)
  diff_table(d1, d2, "Time", "Diff_d50_active", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_d50_active = function(d, group.col = "Station")
  rmse_table(d, group.col, "Diff_d50_active", "RMSE_d50_active")

#' @describeIn read_standard Read the d50 Cover data output.
#' @export
read_d50_cover = function(f, run.type, which.times = NULL, which.stations = NULL)
  read_standard(f, "d50 Cover", run.type, which.times, which.stations)

#' @describeIn diff_table Compute a difference table for d50 Cover data.
#' @export
diff_d50_cover = function(d1, d2, relative = FALSE)
  diff_table(d1, d2, "Time", "Diff_d50_cover", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_d50_cover = function(d, group.col = "Station")
  rmse_table(d, group.col, "Diff_d50_cover", "RMSE_d50_cover")

#' @describeIn read_standard Read the d50 Inactive data output.
#' @export
read_d50_inactive = function(f, run.type, which.times = NULL, which.stations = NULL)
  read_standard(f, "d50 Inactive", run.type, which.times, which.stations)

#' @describeIn diff_table Compute a difference table for d50 Inactive data.
#' @export
diff_d50_inactive = function(d1, d2, relative = FALSE)
  diff_table(d1, d2, "Time", "Diff_d50_inactive", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_d50_inactive = function(d, group.col = "Station")
  rmse_table(d, group.col, "Diff_d50_inactive", "RMSE_d50_inactive")

#' @describeIn read_standard Read the d50 Subsurface data output.
#' @export
read_d50_subsurface = function(f, run.type, which.times = NULL, which.stations = NULL)
  read_standard(f, "d50 Subsurface", run.type, which.times, which.stations)

#' @describeIn diff_table Compute a difference table for d50 Subsurface data.
#' @export
diff_d50_subsurface = function(d1, d2, relative = FALSE)
  diff_table(d1, d2, "Time", "Diff_d50_subsurface", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_d50_subsurface = function(d, group.col = "Station")
  rmse_table(d, group.col, "Diff_d50_subsurface", "RMSE_d50_subsurface")

#' @describeIn read_standard Read the d84 Active data output.
#' @export
read_d84_active = function(f, run.type, which.times = NULL, which.stations = NULL)
  read_standard(f, "d84 Active", run.type, which.times, which.stations)

#' @describeIn diff_table Compute a difference table for d84 Active data.
#' @export
diff_d84_active = function(d1, d2, relative = FALSE)
  diff_table(d1, d2, "Time", "Diff_d84_active", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_d84_active = function(d, group.col = "Station")
  rmse_table(d, group.col, "Diff_d84_active", "RMSE_d84_active")

#' @describeIn read_standard Read the d84 Cover data output.
#' @export
read_d84_cover = function(f, run.type, which.times = NULL, which.stations = NULL)
  read_standard(f, "d84 Cover", run.type, which.times, which.stations)

#' @describeIn diff_table Compute a difference table for d84 Cover data.
#' @export
diff_d84_cover = function(d1, d2, relative = FALSE)
  diff_table(d1, d2, "Time", "Diff_d84_cover", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_d84_cover = function(d, group.col = "Station")
  rmse_table(d, group.col, "Diff_d84_cover", "RMSE_d84_cover")

#' @describeIn read_standard Read the d84 Inactive data output.
#' @export
read_d84_inactive = function(f, run.type, which.times = NULL, which.stations = NULL)
  read_standard(f, "d84 Inactive", run.type, which.times, which.stations)

#' @describeIn diff_table Compute a difference table for d84 Inactive data.
#' @export
diff_d84_inactive = function(d1, d2, relative = FALSE)
  diff_table(d1, d2, "Time", "Diff_d84_inactive", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_d84_inactive = function(d, group.col = "Station")
  rmse_table(d, group.col, "Diff_d84_inactive", "RMSE_d84_inactive")

#' @describeIn read_standard Read the d84 Subsurface data output.
#' @export
read_d84_subsurface = function(f, run.type, which.times = NULL, which.stations = NULL)
  read_standard(f, "d84 Subsurface", run.type, which.times, which.stations)

#' @describeIn diff_table Compute a difference table for d84 Subsurface data.
#' @export
diff_d84_subsurface = function(d1, d2, relative = FALSE)
  diff_table(d1, d2, "Time", "Diff_d84_subsurface", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_d84_subsurface = function(d, group.col = "Station")
  rmse_table(d, group.col, "Diff_d84_subsurface", "RMSE_d84_subsurface")

#' @describeIn read_standard Read the d90 Active data output.
#' @export
read_d90_active = function(f, run.type, which.times = NULL, which.stations = NULL)
  read_standard(f, "d90 Active", run.type, which.times, which.stations)

#' @describeIn diff_table Compute a difference table for d90 Active data.
#' @export
diff_d90_active = function(d1, d2, relative = FALSE)
  diff_table(d1, d2, "Time", "Diff_d90_active", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_d90_active = function(d, group.col = "Station")
  rmse_table(d, group.col, "Diff_d90_active", "RMSE_d90_active")

#' @describeIn read_standard Read the d90 Cover data output.
#' @export
read_d90_cover = function(f, run.type, which.times = NULL, which.stations = NULL)
  read_standard(f, "d90 Cover", run.type, which.times, which.stations)

#' @describeIn diff_table Compute a difference table for d90 Cover data.
#' @export
diff_d90_cover = function(d1, d2, relative = FALSE)
  diff_table(d1, d2, "Time", "Diff_d90_cover", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_d90_cover = function(d, group.col = "Station")
  rmse_table(d, group.col, "Diff_d90_cover", "RMSE_d90_cover")

#' @describeIn read_standard Read the d90 Inactive data output.
#' @export
read_d90_inactive = function(f, run.type, which.times = NULL, which.stations = NULL)
  read_standard(f, "d90 Inactive", run.type, which.times, which.stations)

#' @describeIn diff_table Compute a difference table for d90 Inactive data.
#' @export
diff_d90_inactive = function(d1, d2, relative = FALSE)
  diff_table(d1, d2, "Time", "Diff_d90_inactive", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_d90_inactive = function(d, group.col = "Station")
  rmse_table(d, group.col, "Diff_d90_inactive", "RMSE_d90_inactive")

#' @describeIn read_standard Read the d90 Subsurface data output.
#' @export
read_d90_subsurface = function(f, run.type, which.times = NULL, which.stations = NULL)
  read_standard(f, "d90 Subsurface", run.type, which.times, which.stations)

#' @describeIn diff_table Compute a difference table for d90 Subsurface data.
#' @export
diff_d90_subsurface = function(d1, d2, relative = FALSE)
  diff_table(d1, d2, "Time", "Diff_d90_subsurface", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_d90_subsurface = function(d, group.col = "Station")
  rmse_table(d, group.col, "Diff_d90_subsurface", "RMSE_d90_subsurface")

#' @describeIn read_sediment Read the Fall Velocity data output.
#' @export
read_fall_velocity = function(f, run.type, which.times = NULL, which.stations = NULL, which.grains = NULL)
  read_sediment(f, "Fall Velocity", run.type, which.times, which.stations, which.grains)

#' @describeIn diff_sediment Compute a difference table for Fall Velocity data.
#' @export
diff_fall_velocity = function(d1, d2, relative = FALSE)
  diff_sediment(d1, d2, "Time", "GrainClass", "Diff_fall_velocity", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_fall_velocity = function(d, group.col = "Station")
  rmse_table(d, c("GrainClass", group.col), "Diff_fall_velocity", "RMSE_fall_velocity")

#' @describeIn read_sediment Read the Lat Struc Mass Div data output.
#' @export
read_lat_struc_mass_div = function(f, run.type, which.times = NULL, which.stations = NULL, which.grains = NULL)
  read_sediment(f, "Lat Struc Mass Div", run.type, which.times, which.stations, which.grains)

#' @describeIn diff_sediment Compute a difference table for Lat Struc Mass Div data.
#' @export
diff_lat_struc_mass_div = function(d1, d2, relative = FALSE)
  diff_sediment(d1, d2, "Time", "GrainClass", "Diff_lat_struc_mass_div", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_lat_struc_mass_div = function(d, group.col = "Station")
  rmse_table(d, c("GrainClass", group.col), "Diff_lat_struc_mass_div", "RMSE_lat_struc_mass_div")

#' @describeIn read_sediment Read the Long. Cum Mass Change data output.
#' @export
read_long_cum_mass_change = function(f, run.type, which.times = NULL, which.stations = NULL, which.grains = NULL)
  read_sediment(f, "Long. Cum Mass Change", run.type, which.times, which.stations, which.grains)

#' @describeIn diff_sediment Compute a difference table for Long. Cum Mass Change data.
#' @export
diff_long_cum_mass_change = function(d1, d2, relative = FALSE)
  diff_sediment(d1, d2, "Time", "GrainClass", "Diff_long_cum_mass_change", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_long_cum_mass_change = function(d, group.col = "Station")
  rmse_table(d, c("GrainClass", group.col), "Diff_long_cum_mass_change", "RMSE_long_cum_mass_change")

#' @describeIn read_sediment Read the Long. Cum Vol Change data output.
#' @export
read_long_cum_vol_change = function(f, run.type, which.times = NULL, which.stations = NULL, which.grains = NULL)
  read_sediment(f, "Long. Cum Vol Change", run.type, which.times, which.stations, which.grains)

#' @describeIn diff_sediment Compute a difference table for Long. Cum Vol Change data.
#' @export
diff_long_cum_vol_change = function(d1, d2, relative = FALSE)
  diff_sediment(d1, d2, "Time", "GrainClass", "Diff_long_cum_vol_change", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_long_cum_vol_change = function(d, group.col = "Station")
  rmse_table(d, c("GrainClass", group.col), "Diff_long_cum_vol_change", "RMSE_long_cum_vol_change")

#' @describeIn read_sediment Read the Long. Cum Mass Moveable Limit data output.
#' @export
read_long_cum_mass_moveable_limit = function(f, run.type, which.times = NULL, which.stations = NULL, which.grains = NULL)
  read_sediment(f, "Long. Cum Mass Moveable Limit", run.type, which.times, which.stations, which.grains)

#' @describeIn diff_sediment Compute a difference table for Long. Cum Mass Moveable Limit data.
#' @export
diff_long_cum_mass_moveable_limit = function(d1, d2, relative = FALSE)
  diff_sediment(d1, d2, "Time", "GrainClass", "Diff_long_cum_mass_moveable_limit", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_long_cum_mass_moveable_limit = function(d, group.col = "Station")
  rmse_table(d, c("GrainClass", group.col), "Diff_long_cum_mass_moveable_limit", "RMSE_long_cum_mass_moveable_limit")

#' @describeIn read_sediment Read the Mass Bed Change data output.
#' @export
read_mass_bed_change = function(f, run.type, which.times = NULL, which.stations = NULL, which.grains = NULL)
  read_sediment(f, "Mass Bed Change", run.type, which.times, which.stations, which.grains)

#' @describeIn diff_sediment Compute a difference table for Mass Bed Change data.
#' @export
diff_mass_bed_change = function(d1, d2, relative = FALSE)
  diff_sediment(d1, d2, "Time", "GrainClass", "Diff_mass_bed_change", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_mass_bed_change = function(d, group.col = "Station")
  rmse_table(d, c("GrainClass", group.col), "Diff_mass_bed_change", "RMSE_mass_bed_change")

#' @describeIn read_sediment Read the Mass Bed Change Cum data output.
#' @export
read_mass_bed_change_cum = function(f, run.type, which.times = NULL, which.stations = NULL, which.grains = NULL)
  read_sediment(f, "Mass Bed Change Cum", run.type, which.times, which.stations, which.grains)

#' @describeIn diff_sediment Compute a difference table for Mass Bed Change Cum data.
#' @export
diff_mass_bed_change_cum = function(d1, d2, relative = FALSE)
  diff_sediment(d1, d2, "Time", "GrainClass", "Diff_mass_bed_change_cum", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_mass_bed_change_cum = function(d, group.col = "Station")
  rmse_table(d, c("GrainClass", group.col), "Diff_mass_bed_change_cum", "RMSE_mass_bed_change_cum")

#' @describeIn read_sediment Read the Mass Capacity data output.
#' @export
read_mass_capacity = function(f, run.type, which.times = NULL, which.stations = NULL, which.grains = NULL)
  read_sediment(f, "Mass Capacity", run.type, which.times, which.stations, which.grains)

#' @describeIn diff_sediment Compute a difference table for Mass Capacity data.
#' @export
diff_mass_capacity = function(d1, d2, relative = FALSE)
  diff_sediment(d1, d2, "Time", "GrainClass", "Diff_mass_capacity", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_mass_capacity = function(d, group.col = "Station")
  rmse_table(d, c("GrainClass", group.col), "Diff_mass_capacity", "RMSE_mass_capacity")

#' @describeIn read_sediment Read the Mass Cover data output.
#' @export
read_mass_cover = function(f, run.type, which.times = NULL, which.stations = NULL, which.grains = NULL)
  read_sediment(f, "Mass Cover", run.type, which.times, which.stations, which.grains)

#' @describeIn diff_sediment Compute a difference table for Mass Cover data.
#' @export
diff_mass_cover = function(d1, d2, relative = FALSE)
  diff_sediment(d1, d2, "Time", "GrainClass", "Diff_mass_cover", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_mass_cover = function(d, group.col = "Station")
  rmse_table(d, c("GrainClass", group.col), "Diff_mass_cover", "RMSE_mass_cover")

#' @describeIn read_sediment Read the Mass In data output.
#' @export
read_mass_in = function(f, run.type, which.times = NULL, which.stations = NULL, which.grains = NULL)
  read_sediment(f, "Mass In", run.type, which.times, which.stations, which.grains)

#' @describeIn diff_sediment Compute a difference table for Mass In data.
#' @export
diff_mass_in = function(d1, d2, relative = FALSE)
  diff_sediment(d1, d2, "Time", "GrainClass", "Diff_mass_in", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_mass_in = function(d, group.col = "Station")
  rmse_table(d, c("GrainClass", group.col), "Diff_mass_in", "RMSE_mass_in")

#' @describeIn read_sediment Read the Mass Inactive data output.
#' @export
read_mass_inactive = function(f, run.type, which.times = NULL, which.stations = NULL, which.grains = NULL)
  read_sediment(f, "Mass Inactive", run.type, which.times, which.stations, which.grains)

#' @describeIn diff_sediment Compute a difference table for Mass Inactive data.
#' @export
diff_mass_inactive = function(d1, d2, relative = FALSE)
  diff_sediment(d1, d2, "Time", "GrainClass", "Diff_mass_inactive", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_mass_inactive = function(d, group.col = "Station")
  rmse_table(d, c("GrainClass", group.col), "Diff_mass_inactive", "RMSE_mass_inactive")

#' @describeIn read_sediment Read the Mass In Cum data output.
#' @export
read_mass_in_cum = function(f, run.type, which.times = NULL, which.stations = NULL, which.grains = NULL)
  read_sediment(f, "Mass In Cum", run.type, which.times, which.stations, which.grains)

#' @describeIn diff_sediment Compute a difference table for Mass In Cum data.
#' @export
diff_mass_in_cum = function(d1, d2, relative = FALSE)
  diff_sediment(d1, d2, "Time", "GrainClass", "Diff_mass_in_cum", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_mass_in_cum = function(d, group.col = "Station")
  rmse_table(d, c("GrainClass", group.col), "Diff_mass_in_cum", "RMSE_mass_in_cum")

#' @describeIn read_sediment Read the Mass Out data output.
#' @export
read_mass_out = function(f, run.type, which.times = NULL, which.stations = NULL, which.grains = NULL)
  read_sediment(f, "Mass Out", run.type, which.times, which.stations, which.grains)

#' @describeIn diff_sediment Compute a difference table for Mass Out data.
#' @export
diff_mass_out = function(d1, d2, relative = FALSE)
  diff_sediment(d1, d2, "Time", "GrainClass", "Diff_mass_out", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_mass_out = function(d, group.col = "Station")
  rmse_table(d, c("GrainClass", group.col), "Diff_mass_out", "RMSE_mass_out")

#' @describeIn read_sediment Read the Mass Out Cum data output.
#' @export
read_mass_out_cum = function(f, run.type, which.times = NULL, which.stations = NULL, which.grains = NULL)
  read_sediment(f, "Mass Out Cum", run.type, which.times, which.stations, which.grains)

#' @describeIn diff_sediment Compute a difference table for Mass Out Cum data.
#' @export
diff_mass_out_cum = function(d1, d2, relative = FALSE)
  diff_sediment(d1, d2, "Time", "GrainClass", "Diff_mass_out_cum", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_mass_out_cum = function(d, group.col = "Station")
  rmse_table(d, c("GrainClass", group.col), "Diff_mass_out_cum", "RMSE_mass_out_cum")

#' @describeIn read_sediment Read the Mass Subsurface data output.
#' @export
read_mass_subsurface = function(f, run.type, which.times = NULL, which.stations = NULL, which.grains = NULL)
  read_sediment(f, "Mass Subsurface", run.type, which.times, which.stations, which.grains)

#' @describeIn diff_sediment Compute a difference table for Mass Subsurface data.
#' @export
diff_mass_subsurface = function(d1, d2, relative = FALSE)
  diff_sediment(d1, d2, "Time", "GrainClass", "Diff_mass_subsurface", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_mass_subsurface = function(d, group.col = "Station")
  rmse_table(d, c("GrainClass", group.col), "Diff_mass_subsurface", "RMSE_mass_subsurface")

#' @describeIn read_sediment Read the Reduce Armor Factor data output.
#' @export
read_reduce_armor_factor = function(f, run.type, which.times = NULL, which.stations = NULL, which.grains = NULL)
  read_sediment(f, "Reduce Armor Factor", run.type, which.times, which.stations, which.grains)

#' @describeIn diff_sediment Compute a difference table for Reduce Armor Factor data.
#' @export
diff_reduce_armor_factor = function(d1, d2, relative = FALSE)
  diff_sediment(d1, d2, "Time", "GrainClass", "Diff_reduce_armor_factor", relative)

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_reduce_armor_factor = function(d, group.col = "Station")
  rmse_table(d, c("GrainClass", group.col), "Diff_reduce_armor_factor", "RMSE_reduce_armor_factor")

