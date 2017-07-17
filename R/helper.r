#' @describeIn read_standard Read the Dredged Cum data output.
#' @export
read_dredged_cum = function(f, which.times = NULL, which.stations = NULL)
  read_standard(f, "Dredged Cum", which.times, which.stations)

#' @describeIn difference_table Compute a difference table for Dredged Cum data.
#' @export
difference_dredged_cum = function(d1, d2, relative = FALSE)
  difference_table(d1, d2, "Difference_dredged_cum", relative, "Time")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_dredged_cum = function(d, group.col = "Station")
  rmse_table(d, group.col, "Difference_dredged_cum", "RMSE_dredged_cum")

#' @describeIn read_standard Read the Effective Depth data output.
#' @export
read_effective_depth = function(f, which.times = NULL, which.stations = NULL)
  read_standard(f, "Effective Depth", which.times, which.stations)

#' @describeIn difference_table Compute a difference table for Effective Depth data.
#' @export
difference_effective_depth = function(d1, d2, relative = FALSE)
  difference_table(d1, d2, "Difference_effective_depth", relative, "Time")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_effective_depth = function(d, group.col = "Station")
  rmse_table(d, group.col, "Difference_effective_depth", "RMSE_effective_depth")

#' @describeIn read_standard Read the Effective_Width data output.
#' @export
read_effective_width = function(f, which.times = NULL, which.stations = NULL)
  read_standard(f, "Effective_Width", which.times, which.stations)

#' @describeIn difference_table Compute a difference table for Effective_Width data.
#' @export
difference_effective_width = function(d1, d2, relative = FALSE)
  difference_table(d1, d2, "Difference_effective_width", relative, "Time")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_effective_width = function(d, group.col = "Station")
  rmse_table(d, group.col, "Difference_effective_width", "RMSE_effective_width")

#' @describeIn read_standard Read the Flow data output.
#' @export
read_flow = function(f, which.times = NULL, which.stations = NULL)
  read_standard(f, "Flow", which.times, which.stations)

#' @describeIn difference_table Compute a difference table for Flow data.
#' @export
difference_flow = function(d1, d2, relative = FALSE)
  difference_table(d1, d2, "Difference_flow", relative, "Time")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_flow = function(d, group.col = "Station")
  rmse_table(d, group.col, "Difference_flow", "RMSE_flow")

#' @describeIn read_standard Read the Froude Number Channel data output.
#' @export
read_froude_number_channel = function(f, which.times = NULL, which.stations = NULL)
  read_standard(f, "Froude Number Channel", which.times, which.stations)

#' @describeIn difference_table Compute a difference table for Froude Number Channel data.
#' @export
difference_froude_number_channel = function(d1, d2, relative = FALSE)
  difference_table(d1, d2, "Difference_froude_number_channel", relative, "Time")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_froude_number_channel = function(d, group.col = "Station")
  rmse_table(d, group.col, "Difference_froude_number_channel", "RMSE_froude_number_channel")

#' @describeIn read_standard Read the Hydraulic Radius data output.
#' @export
read_hydraulic_radius = function(f, which.times = NULL, which.stations = NULL)
  read_standard(f, "Hydraulic Radius", which.times, which.stations)

#' @describeIn difference_table Compute a difference table for Hydraulic Radius data.
#' @export
difference_hydraulic_radius = function(d1, d2, relative = FALSE)
  difference_table(d1, d2, "Difference_hydraulic_radius", relative, "Time")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_hydraulic_radius = function(d, group.col = "Station")
  rmse_table(d, group.col, "Difference_hydraulic_radius", "RMSE_hydraulic_radius")

#' @describeIn read_standard Read the Invert Change data output.
#' @export
read_invert_change = function(f, which.times = NULL, which.stations = NULL)
  read_standard(f, "Invert Change", which.times, which.stations)

#' @describeIn difference_table Compute a difference table for Invert Change data.
#' @export
difference_invert_change = function(d1, d2, relative = FALSE)
  difference_table(d1, d2, "Difference_invert_change", relative, "Time")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_invert_change = function(d, group.col = "Station")
  rmse_table(d, group.col, "Difference_invert_change", "RMSE_invert_change")

#' @describeIn read_standard Read the Invert Elevation data output.
#' @export
read_invert_elevation = function(f, which.times = NULL, which.stations = NULL)
  read_standard(f, "Invert Elevation", which.times, which.stations)

#' @describeIn difference_table Compute a difference table for Invert Elevation data.
#' @export
difference_invert_elevation = function(d1, d2, relative = FALSE)
  difference_table(d1, d2, "Difference_invert_elevation", relative, "Time")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_invert_elevation = function(d, group.col = "Station")
  rmse_table(d, group.col, "Difference_invert_elevation", "RMSE_invert_elevation")

#' @describeIn read_standard Read the Mannings n Channel data output.
#' @export
read_mannings_n_channel = function(f, which.times = NULL, which.stations = NULL)
  read_standard(f, "Mannings n Channel", which.times, which.stations)

#' @describeIn difference_table Compute a difference table for Mannings n Channel data.
#' @export
difference_mannings_n_channel = function(d1, d2, relative = FALSE)
  difference_table(d1, d2, "Difference_mannings_n_channel", relative, "Time")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_mannings_n_channel = function(d, group.col = "Station")
  rmse_table(d, group.col, "Difference_mannings_n_channel", "RMSE_mannings_n_channel")

#' @describeIn read_standard Read the Mean Effective Invert Change data output.
#' @export
read_mean_effective_invert_change = function(f, which.times = NULL, which.stations = NULL)
  read_standard(f, "Mean Effective Invert Change", which.times, which.stations)

#' @describeIn difference_table Compute a difference table for Mean Effective Invert Change data.
#' @export
difference_mean_effective_invert_change = function(d1, d2, relative = FALSE)
  difference_table(d1, d2, "Difference_mean_effective_invert_change", relative, "Time")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_mean_effective_invert_change = function(d, group.col = "Station")
  rmse_table(d, group.col, "Difference_mean_effective_invert_change", "RMSE_mean_effective_invert_change")

#' @describeIn read_standard Read the Mean Effective Invert Elevation data output.
#' @export
read_mean_effective_invert_elevation = function(f, which.times = NULL, which.stations = NULL)
  read_standard(f, "Mean Effective Invert Elevation", which.times, which.stations)

#' @describeIn difference_table Compute a difference table for Mean Effective Invert Elevation data.
#' @export
difference_mean_effective_invert_elevation = function(d1, d2, relative = FALSE)
  difference_table(d1, d2, "Difference_mean_effective_invert_elevation", relative, "Time")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_mean_effective_invert_elevation = function(d, group.col = "Station")
  rmse_table(d, group.col, "Difference_mean_effective_invert_elevation", "RMSE_mean_effective_invert_elevation")

#' @describeIn read_standard Read the Moveable Elv L data output.
#' @export
read_moveable_elv_l = function(f, which.times = NULL, which.stations = NULL)
  read_standard(f, "Moveable Elv L", which.times, which.stations)

#' @describeIn difference_table Compute a difference table for Moveable Elv L data.
#' @export
difference_moveable_elv_l = function(d1, d2, relative = FALSE)
  difference_table(d1, d2, "Difference_moveable_elv_l", relative, "Time")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_moveable_elv_l = function(d, group.col = "Station")
  rmse_table(d, group.col, "Difference_moveable_elv_l", "RMSE_moveable_elv_l")

#' @describeIn read_standard Read the Moveable Elv R data output.
#' @export
read_moveable_elv_r = function(f, which.times = NULL, which.stations = NULL)
  read_standard(f, "Moveable Elv R", which.times, which.stations)

#' @describeIn difference_table Compute a difference table for Moveable Elv R data.
#' @export
difference_moveable_elv_r = function(d1, d2, relative = FALSE)
  difference_table(d1, d2, "Difference_moveable_elv_r", relative, "Time")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_moveable_elv_r = function(d, group.col = "Station")
  rmse_table(d, group.col, "Difference_moveable_elv_r", "RMSE_moveable_elv_r")

#' @describeIn read_standard Read the Moveable Sta L data output.
#' @export
read_moveable_sta_l = function(f, which.times = NULL, which.stations = NULL)
  read_standard(f, "Moveable Sta L", which.times, which.stations)

#' @describeIn difference_table Compute a difference table for Moveable Sta L data.
#' @export
difference_moveable_sta_l = function(d1, d2, relative = FALSE)
  difference_table(d1, d2, "Difference_moveable_sta_l", relative, "Time")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_moveable_sta_l = function(d, group.col = "Station")
  rmse_table(d, group.col, "Difference_moveable_sta_l", "RMSE_moveable_sta_l")

#' @describeIn read_standard Read the Moveable Sta R data output.
#' @export
read_moveable_sta_r = function(f, which.times = NULL, which.stations = NULL)
  read_standard(f, "Moveable Sta R", which.times, which.stations)

#' @describeIn difference_table Compute a difference table for Moveable Sta R data.
#' @export
difference_moveable_sta_r = function(d1, d2, relative = FALSE)
  difference_table(d1, d2, "Difference_moveable_sta_r", relative, "Time")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_moveable_sta_r = function(d, group.col = "Station")
  rmse_table(d, group.col, "Difference_moveable_sta_r", "RMSE_moveable_sta_r")

#' @describeIn read_standard Read the Observed Data data output.
#' @export
read_observed_data = function(f, which.times = NULL, which.stations = NULL)
  read_standard(f, "Observed Data", which.times, which.stations)

#' @describeIn difference_table Compute a difference table for Observed Data data.
#' @export
difference_observed_data = function(d1, d2, relative = FALSE)
  difference_table(d1, d2, "Difference_observed_data", relative, "Time")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_observed_data = function(d, group.col = "Station")
  rmse_table(d, group.col, "Difference_observed_data", "RMSE_observed_data")

#' @describeIn read_standard Read the Sediment Concentration data output.
#' @export
read_sediment_concentration = function(f, which.times = NULL, which.stations = NULL)
  read_standard(f, "Sediment Concentration", which.times, which.stations)

#' @describeIn difference_table Compute a difference table for Sediment Concentration data.
#' @export
difference_sediment_concentration = function(d1, d2, relative = FALSE)
  difference_table(d1, d2, "Difference_sediment_concentration", relative, "Time")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_sediment_concentration = function(d, group.col = "Station")
  rmse_table(d, group.col, "Difference_sediment_concentration", "RMSE_sediment_concentration")

#' @describeIn read_standard Read the Shear Stress data output.
#' @export
read_shear_stress = function(f, which.times = NULL, which.stations = NULL)
  read_standard(f, "Shear Stress", which.times, which.stations)

#' @describeIn difference_table Compute a difference table for Shear Stress data.
#' @export
difference_shear_stress = function(d1, d2, relative = FALSE)
  difference_table(d1, d2, "Difference_shear_stress", relative, "Time")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_shear_stress = function(d, group.col = "Station")
  rmse_table(d, group.col, "Difference_shear_stress", "RMSE_shear_stress")

#' @describeIn read_standard Read the Shear Velocity data output.
#' @export
read_shear_velocity = function(f, which.times = NULL, which.stations = NULL)
  read_standard(f, "Shear Velocity", which.times, which.stations)

#' @describeIn difference_table Compute a difference table for Shear Velocity data.
#' @export
difference_shear_velocity = function(d1, d2, relative = FALSE)
  difference_table(d1, d2, "Difference_shear_velocity", relative, "Time")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_shear_velocity = function(d, group.col = "Station")
  rmse_table(d, group.col, "Difference_shear_velocity", "RMSE_shear_velocity")

#' @describeIn read_standard Read the Slope data output.
#' @export
read_slope = function(f, which.times = NULL, which.stations = NULL)
  read_standard(f, "Slope", which.times, which.stations)

#' @describeIn difference_table Compute a difference table for Slope data.
#' @export
difference_slope = function(d1, d2, relative = FALSE)
  difference_table(d1, d2, "Difference_slope", relative, "Time")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_slope = function(d, group.col = "Station")
  rmse_table(d, group.col, "Difference_slope", "RMSE_slope")

#' @describeIn read_standard Read the Temperature data output.
#' @export
read_temperature = function(f, which.times = NULL, which.stations = NULL)
  read_standard(f, "Temperature", which.times, which.stations)

#' @describeIn difference_table Compute a difference table for Temperature data.
#' @export
difference_temperature = function(d1, d2, relative = FALSE)
  difference_table(d1, d2, "Difference_temperature", relative, "Time")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_temperature = function(d, group.col = "Station")
  rmse_table(d, group.col, "Difference_temperature", "RMSE_temperature")

#' @describeIn read_standard Read the Thickness Cover data output.
#' @export
read_thickness_cover = function(f, which.times = NULL, which.stations = NULL)
  read_standard(f, "Thickness Cover", which.times, which.stations)

#' @describeIn difference_table Compute a difference table for Thickness Cover data.
#' @export
difference_thickness_cover = function(d1, d2, relative = FALSE)
  difference_table(d1, d2, "Difference_thickness_cover", relative, "Time")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_thickness_cover = function(d, group.col = "Station")
  rmse_table(d, group.col, "Difference_thickness_cover", "RMSE_thickness_cover")

#' @describeIn read_standard Read the Thickness Inactive data output.
#' @export
read_thickness_inactive = function(f, which.times = NULL, which.stations = NULL)
  read_standard(f, "Thickness Inactive", which.times, which.stations)

#' @describeIn difference_table Compute a difference table for Thickness Inactive data.
#' @export
difference_thickness_inactive = function(d1, d2, relative = FALSE)
  difference_table(d1, d2, "Difference_thickness_inactive", relative, "Time")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_thickness_inactive = function(d, group.col = "Station")
  rmse_table(d, group.col, "Difference_thickness_inactive", "RMSE_thickness_inactive")

#' @describeIn read_standard Read the Thickness Subsurface data output.
#' @export
read_thickness_subsurface = function(f, which.times = NULL, which.stations = NULL)
  read_standard(f, "Thickness Subsurface", which.times, which.stations)

#' @describeIn difference_table Compute a difference table for Thickness Subsurface data.
#' @export
difference_thickness_subsurface = function(d1, d2, relative = FALSE)
  difference_table(d1, d2, "Difference_thickness_subsurface", relative, "Time")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_thickness_subsurface = function(d, group.col = "Station")
  rmse_table(d, group.col, "Difference_thickness_subsurface", "RMSE_thickness_subsurface")

#' @describeIn read_standard Read the Velocity data output.
#' @export
read_velocity = function(f, which.times = NULL, which.stations = NULL)
  read_standard(f, "Velocity", which.times, which.stations)

#' @describeIn difference_table Compute a difference table for Velocity data.
#' @export
difference_velocity = function(d1, d2, relative = FALSE)
  difference_table(d1, d2, "Difference_velocity", relative, "Time")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_velocity = function(d, group.col = "Station")
  rmse_table(d, group.col, "Difference_velocity", "RMSE_velocity")

#' @describeIn read_standard Read the Water Surface data output.
#' @export
read_water_surface = function(f, which.times = NULL, which.stations = NULL)
  read_standard(f, "Water Surface", which.times, which.stations)

#' @describeIn difference_table Compute a difference table for Water Surface data.
#' @export
difference_water_surface = function(d1, d2, relative = FALSE)
  difference_table(d1, d2, "Difference_water_surface", relative, "Time")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_water_surface = function(d, group.col = "Station")
  rmse_table(d, group.col, "Difference_water_surface", "RMSE_water_surface")

#' @describeIn read_standard Read the d10 Active data output.
#' @export
read_d10_active = function(f, which.times = NULL, which.stations = NULL)
  read_standard(f, "d10 Active", which.times, which.stations)

#' @describeIn difference_table Compute a difference table for d10 Active data.
#' @export
difference_d10_active = function(d1, d2, relative = FALSE)
  difference_table(d1, d2, "Difference_d10_active", relative, "Time")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_d10_active = function(d, group.col = "Station")
  rmse_table(d, group.col, "Difference_d10_active", "RMSE_d10_active")

#' @describeIn read_standard Read the d10 Cover data output.
#' @export
read_d10_cover = function(f, which.times = NULL, which.stations = NULL)
  read_standard(f, "d10 Cover", which.times, which.stations)

#' @describeIn difference_table Compute a difference table for d10 Cover data.
#' @export
difference_d10_cover = function(d1, d2, relative = FALSE)
  difference_table(d1, d2, "Difference_d10_cover", relative, "Time")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_d10_cover = function(d, group.col = "Station")
  rmse_table(d, group.col, "Difference_d10_cover", "RMSE_d10_cover")

#' @describeIn read_standard Read the d10 Inactive data output.
#' @export
read_d10_inactive = function(f, which.times = NULL, which.stations = NULL)
  read_standard(f, "d10 Inactive", which.times, which.stations)

#' @describeIn difference_table Compute a difference table for d10 Inactive data.
#' @export
difference_d10_inactive = function(d1, d2, relative = FALSE)
  difference_table(d1, d2, "Difference_d10_inactive", relative, "Time")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_d10_inactive = function(d, group.col = "Station")
  rmse_table(d, group.col, "Difference_d10_inactive", "RMSE_d10_inactive")

#' @describeIn read_standard Read the d10 Subsurface data output.
#' @export
read_d10_subsurface = function(f, which.times = NULL, which.stations = NULL)
  read_standard(f, "d10 Subsurface", which.times, which.stations)

#' @describeIn difference_table Compute a difference table for d10 Subsurface data.
#' @export
difference_d10_subsurface = function(d1, d2, relative = FALSE)
  difference_table(d1, d2, "Difference_d10_subsurface", relative, "Time")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_d10_subsurface = function(d, group.col = "Station")
  rmse_table(d, group.col, "Difference_d10_subsurface", "RMSE_d10_subsurface")

#' @describeIn read_standard Read the d16 Active data output.
#' @export
read_d16_active = function(f, which.times = NULL, which.stations = NULL)
  read_standard(f, "d16 Active", which.times, which.stations)

#' @describeIn difference_table Compute a difference table for d16 Active data.
#' @export
difference_d16_active = function(d1, d2, relative = FALSE)
  difference_table(d1, d2, "Difference_d16_active", relative, "Time")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_d16_active = function(d, group.col = "Station")
  rmse_table(d, group.col, "Difference_d16_active", "RMSE_d16_active")

#' @describeIn read_standard Read the d16 Cover data output.
#' @export
read_d16_cover = function(f, which.times = NULL, which.stations = NULL)
  read_standard(f, "d16 Cover", which.times, which.stations)

#' @describeIn difference_table Compute a difference table for d16 Cover data.
#' @export
difference_d16_cover = function(d1, d2, relative = FALSE)
  difference_table(d1, d2, "Difference_d16_cover", relative, "Time")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_d16_cover = function(d, group.col = "Station")
  rmse_table(d, group.col, "Difference_d16_cover", "RMSE_d16_cover")

#' @describeIn read_standard Read the d16 Inactive data output.
#' @export
read_d16_inactive = function(f, which.times = NULL, which.stations = NULL)
  read_standard(f, "d16 Inactive", which.times, which.stations)

#' @describeIn difference_table Compute a difference table for d16 Inactive data.
#' @export
difference_d16_inactive = function(d1, d2, relative = FALSE)
  difference_table(d1, d2, "Difference_d16_inactive", relative, "Time")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_d16_inactive = function(d, group.col = "Station")
  rmse_table(d, group.col, "Difference_d16_inactive", "RMSE_d16_inactive")

#' @describeIn read_standard Read the d16 Subsurface data output.
#' @export
read_d16_subsurface = function(f, which.times = NULL, which.stations = NULL)
  read_standard(f, "d16 Subsurface", which.times, which.stations)

#' @describeIn difference_table Compute a difference table for d16 Subsurface data.
#' @export
difference_d16_subsurface = function(d1, d2, relative = FALSE)
  difference_table(d1, d2, "Difference_d16_subsurface", relative, "Time")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_d16_subsurface = function(d, group.col = "Station")
  rmse_table(d, group.col, "Difference_d16_subsurface", "RMSE_d16_subsurface")

#' @describeIn read_standard Read the d50 Active data output.
#' @export
read_d50_active = function(f, which.times = NULL, which.stations = NULL)
  read_standard(f, "d50 Active", which.times, which.stations)

#' @describeIn difference_table Compute a difference table for d50 Active data.
#' @export
difference_d50_active = function(d1, d2, relative = FALSE)
  difference_table(d1, d2, "Difference_d50_active", relative, "Time")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_d50_active = function(d, group.col = "Station")
  rmse_table(d, group.col, "Difference_d50_active", "RMSE_d50_active")

#' @describeIn read_standard Read the d50 Cover data output.
#' @export
read_d50_cover = function(f, which.times = NULL, which.stations = NULL)
  read_standard(f, "d50 Cover", which.times, which.stations)

#' @describeIn difference_table Compute a difference table for d50 Cover data.
#' @export
difference_d50_cover = function(d1, d2, relative = FALSE)
  difference_table(d1, d2, "Difference_d50_cover", relative, "Time")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_d50_cover = function(d, group.col = "Station")
  rmse_table(d, group.col, "Difference_d50_cover", "RMSE_d50_cover")

#' @describeIn read_standard Read the d50 Inactive data output.
#' @export
read_d50_inactive = function(f, which.times = NULL, which.stations = NULL)
  read_standard(f, "d50 Inactive", which.times, which.stations)

#' @describeIn difference_table Compute a difference table for d50 Inactive data.
#' @export
difference_d50_inactive = function(d1, d2, relative = FALSE)
  difference_table(d1, d2, "Difference_d50_inactive", relative, "Time")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_d50_inactive = function(d, group.col = "Station")
  rmse_table(d, group.col, "Difference_d50_inactive", "RMSE_d50_inactive")

#' @describeIn read_standard Read the d50 Subsurface data output.
#' @export
read_d50_subsurface = function(f, which.times = NULL, which.stations = NULL)
  read_standard(f, "d50 Subsurface", which.times, which.stations)

#' @describeIn difference_table Compute a difference table for d50 Subsurface data.
#' @export
difference_d50_subsurface = function(d1, d2, relative = FALSE)
  difference_table(d1, d2, "Difference_d50_subsurface", relative, "Time")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_d50_subsurface = function(d, group.col = "Station")
  rmse_table(d, group.col, "Difference_d50_subsurface", "RMSE_d50_subsurface")

#' @describeIn read_standard Read the d84 Active data output.
#' @export
read_d84_active = function(f, which.times = NULL, which.stations = NULL)
  read_standard(f, "d84 Active", which.times, which.stations)

#' @describeIn difference_table Compute a difference table for d84 Active data.
#' @export
difference_d84_active = function(d1, d2, relative = FALSE)
  difference_table(d1, d2, "Difference_d84_active", relative, "Time")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_d84_active = function(d, group.col = "Station")
  rmse_table(d, group.col, "Difference_d84_active", "RMSE_d84_active")

#' @describeIn read_standard Read the d84 Cover data output.
#' @export
read_d84_cover = function(f, which.times = NULL, which.stations = NULL)
  read_standard(f, "d84 Cover", which.times, which.stations)

#' @describeIn difference_table Compute a difference table for d84 Cover data.
#' @export
difference_d84_cover = function(d1, d2, relative = FALSE)
  difference_table(d1, d2, "Difference_d84_cover", relative, "Time")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_d84_cover = function(d, group.col = "Station")
  rmse_table(d, group.col, "Difference_d84_cover", "RMSE_d84_cover")

#' @describeIn read_standard Read the d84 Inactive data output.
#' @export
read_d84_inactive = function(f, which.times = NULL, which.stations = NULL)
  read_standard(f, "d84 Inactive", which.times, which.stations)

#' @describeIn difference_table Compute a difference table for d84 Inactive data.
#' @export
difference_d84_inactive = function(d1, d2, relative = FALSE)
  difference_table(d1, d2, "Difference_d84_inactive", relative, "Time")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_d84_inactive = function(d, group.col = "Station")
  rmse_table(d, group.col, "Difference_d84_inactive", "RMSE_d84_inactive")

#' @describeIn read_standard Read the d84 Subsurface data output.
#' @export
read_d84_subsurface = function(f, which.times = NULL, which.stations = NULL)
  read_standard(f, "d84 Subsurface", which.times, which.stations)

#' @describeIn difference_table Compute a difference table for d84 Subsurface data.
#' @export
difference_d84_subsurface = function(d1, d2, relative = FALSE)
  difference_table(d1, d2, "Difference_d84_subsurface", relative, "Time")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_d84_subsurface = function(d, group.col = "Station")
  rmse_table(d, group.col, "Difference_d84_subsurface", "RMSE_d84_subsurface")

#' @describeIn read_standard Read the d90 Active data output.
#' @export
read_d90_active = function(f, which.times = NULL, which.stations = NULL)
  read_standard(f, "d90 Active", which.times, which.stations)

#' @describeIn difference_table Compute a difference table for d90 Active data.
#' @export
difference_d90_active = function(d1, d2, relative = FALSE)
  difference_table(d1, d2, "Difference_d90_active", relative, "Time")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_d90_active = function(d, group.col = "Station")
  rmse_table(d, group.col, "Difference_d90_active", "RMSE_d90_active")

#' @describeIn read_standard Read the d90 Cover data output.
#' @export
read_d90_cover = function(f, which.times = NULL, which.stations = NULL)
  read_standard(f, "d90 Cover", which.times, which.stations)

#' @describeIn difference_table Compute a difference table for d90 Cover data.
#' @export
difference_d90_cover = function(d1, d2, relative = FALSE)
  difference_table(d1, d2, "Difference_d90_cover", relative, "Time")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_d90_cover = function(d, group.col = "Station")
  rmse_table(d, group.col, "Difference_d90_cover", "RMSE_d90_cover")

#' @describeIn read_standard Read the d90 Inactive data output.
#' @export
read_d90_inactive = function(f, which.times = NULL, which.stations = NULL)
  read_standard(f, "d90 Inactive", which.times, which.stations)

#' @describeIn difference_table Compute a difference table for d90 Inactive data.
#' @export
difference_d90_inactive = function(d1, d2, relative = FALSE)
  difference_table(d1, d2, "Difference_d90_inactive", relative, "Time")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_d90_inactive = function(d, group.col = "Station")
  rmse_table(d, group.col, "Difference_d90_inactive", "RMSE_d90_inactive")

#' @describeIn read_standard Read the d90 Subsurface data output.
#' @export
read_d90_subsurface = function(f, which.times = NULL, which.stations = NULL)
  read_standard(f, "d90 Subsurface", which.times, which.stations)

#' @describeIn difference_table Compute a difference table for d90 Subsurface data.
#' @export
difference_d90_subsurface = function(d1, d2, relative = FALSE)
  difference_table(d1, d2, "Difference_d90_subsurface", relative, "Time")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_d90_subsurface = function(d, group.col = "Station")
  rmse_table(d, group.col, "Difference_d90_subsurface", "RMSE_d90_subsurface")

#' @describeIn read_sediment Read the Fall Velocity data output.
#' @export
read_fall_velocity = function(f, which.times = NULL, which.stations = NULL, which.grains = NULL)
  read_sediment(f, "Fall Velocity", which.times, which.stations, which.grains)

#' @describeIn difference_sediment Compute a difference table for Fall Velocity data.
#' @export
difference_fall_velocity = function(d1, d2, relative = FALSE)
  difference_sediment(d1, d2, "Difference_fall_velocity", relative, "Time", "GrainClass")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_fall_velocity = function(d, group.col = "Station")
  rmse_table(d, c("GrainClass", group.col), "Difference_fall_velocity", "RMSE_fall_velocity")

#' @describeIn read_sediment Read the Lat Struc Mass Div data output.
#' @export
read_lat_struc_mass_div = function(f, which.times = NULL, which.stations = NULL, which.grains = NULL)
  read_sediment(f, "Lat Struc Mass Div", which.times, which.stations, which.grains)

#' @describeIn difference_sediment Compute a difference table for Lat Struc Mass Div data.
#' @export
difference_lat_struc_mass_div = function(d1, d2, relative = FALSE)
  difference_sediment(d1, d2, "Difference_lat_struc_mass_div", relative, "Time", "GrainClass")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_lat_struc_mass_div = function(d, group.col = "Station")
  rmse_table(d, c("GrainClass", group.col), "Difference_lat_struc_mass_div", "RMSE_lat_struc_mass_div")

#' @describeIn read_sediment Read the Long. Cum Mass Change data output.
#' @export
read_long_cum_mass_change = function(f, which.times = NULL, which.stations = NULL, which.grains = NULL)
  read_sediment(f, "Long. Cum Mass Change", which.times, which.stations, which.grains)

#' @describeIn difference_sediment Compute a difference table for Long. Cum Mass Change data.
#' @export
difference_long_cum_mass_change = function(d1, d2, relative = FALSE)
  difference_sediment(d1, d2, "Difference_long_cum_mass_change", relative, "Time", "GrainClass")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_long_cum_mass_change = function(d, group.col = "Station")
  rmse_table(d, c("GrainClass", group.col), "Difference_long_cum_mass_change", "RMSE_long_cum_mass_change")

#' @describeIn read_sediment Read the Long. Cum Vol Change data output.
#' @export
read_long_cum_vol_change = function(f, which.times = NULL, which.stations = NULL, which.grains = NULL)
  read_sediment(f, "Long. Cum Vol Change", which.times, which.stations, which.grains)

#' @describeIn difference_sediment Compute a difference table for Long. Cum Vol Change data.
#' @export
difference_long_cum_vol_change = function(d1, d2, relative = FALSE)
  difference_sediment(d1, d2, "Difference_long_cum_vol_change", relative, "Time", "GrainClass")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_long_cum_vol_change = function(d, group.col = "Station")
  rmse_table(d, c("GrainClass", group.col), "Difference_long_cum_vol_change", "RMSE_long_cum_vol_change")

#' @describeIn read_sediment Read the Long. Cum Mass Moveable Limit data output.
#' @export
read_long_cum_mass_moveable_limit = function(f, which.times = NULL, which.stations = NULL, which.grains = NULL)
  read_sediment(f, "Long. Cum Mass Moveable Limit", which.times, which.stations, which.grains)

#' @describeIn difference_sediment Compute a difference table for Long. Cum Mass Moveable Limit data.
#' @export
difference_long_cum_mass_moveable_limit = function(d1, d2, relative = FALSE)
  difference_sediment(d1, d2, "Difference_long_cum_mass_moveable_limit", relative, "Time", "GrainClass")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_long_cum_mass_moveable_limit = function(d, group.col = "Station")
  rmse_table(d, c("GrainClass", group.col), "Difference_long_cum_mass_moveable_limit", "RMSE_long_cum_mass_moveable_limit")

#' @describeIn read_sediment Read the Mass Bed Change data output.
#' @export
read_mass_bed_change = function(f, which.times = NULL, which.stations = NULL, which.grains = NULL)
  read_sediment(f, "Mass Bed Change", which.times, which.stations, which.grains)

#' @describeIn difference_sediment Compute a difference table for Mass Bed Change data.
#' @export
difference_mass_bed_change = function(d1, d2, relative = FALSE)
  difference_sediment(d1, d2, "Difference_mass_bed_change", relative, "Time", "GrainClass")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_mass_bed_change = function(d, group.col = "Station")
  rmse_table(d, c("GrainClass", group.col), "Difference_mass_bed_change", "RMSE_mass_bed_change")

#' @describeIn read_sediment Read the Mass Bed Change Cum data output.
#' @export
read_mass_bed_change_cum = function(f, which.times = NULL, which.stations = NULL, which.grains = NULL)
  read_sediment(f, "Mass Bed Change Cum", which.times, which.stations, which.grains)

#' @describeIn difference_sediment Compute a difference table for Mass Bed Change Cum data.
#' @export
difference_mass_bed_change_cum = function(d1, d2, relative = FALSE)
  difference_sediment(d1, d2, "Difference_mass_bed_change_cum", relative, "Time", "GrainClass")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_mass_bed_change_cum = function(d, group.col = "Station")
  rmse_table(d, c("GrainClass", group.col), "Difference_mass_bed_change_cum", "RMSE_mass_bed_change_cum")

#' @describeIn read_sediment Read the Mass Capacity data output.
#' @export
read_mass_capacity = function(f, which.times = NULL, which.stations = NULL, which.grains = NULL)
  read_sediment(f, "Mass Capacity", which.times, which.stations, which.grains)

#' @describeIn difference_sediment Compute a difference table for Mass Capacity data.
#' @export
difference_mass_capacity = function(d1, d2, relative = FALSE)
  difference_sediment(d1, d2, "Difference_mass_capacity", relative, "Time", "GrainClass")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_mass_capacity = function(d, group.col = "Station")
  rmse_table(d, c("GrainClass", group.col), "Difference_mass_capacity", "RMSE_mass_capacity")

#' @describeIn read_sediment Read the Mass Cover data output.
#' @export
read_mass_cover = function(f, which.times = NULL, which.stations = NULL, which.grains = NULL)
  read_sediment(f, "Mass Cover", which.times, which.stations, which.grains)

#' @describeIn difference_sediment Compute a difference table for Mass Cover data.
#' @export
difference_mass_cover = function(d1, d2, relative = FALSE)
  difference_sediment(d1, d2, "Difference_mass_cover", relative, "Time", "GrainClass")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_mass_cover = function(d, group.col = "Station")
  rmse_table(d, c("GrainClass", group.col), "Difference_mass_cover", "RMSE_mass_cover")

#' @describeIn read_sediment Read the Mass In data output.
#' @export
read_mass_in = function(f, which.times = NULL, which.stations = NULL, which.grains = NULL)
  read_sediment(f, "Mass In", which.times, which.stations, which.grains)

#' @describeIn difference_sediment Compute a difference table for Mass In data.
#' @export
difference_mass_in = function(d1, d2, relative = FALSE)
  difference_sediment(d1, d2, "Difference_mass_in", relative, "Time", "GrainClass")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_mass_in = function(d, group.col = "Station")
  rmse_table(d, c("GrainClass", group.col), "Difference_mass_in", "RMSE_mass_in")

#' @describeIn read_sediment Read the Mass Inactive data output.
#' @export
read_mass_inactive = function(f, which.times = NULL, which.stations = NULL, which.grains = NULL)
  read_sediment(f, "Mass Inactive", which.times, which.stations, which.grains)

#' @describeIn difference_sediment Compute a difference table for Mass Inactive data.
#' @export
difference_mass_inactive = function(d1, d2, relative = FALSE)
  difference_sediment(d1, d2, "Difference_mass_inactive", relative, "Time", "GrainClass")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_mass_inactive = function(d, group.col = "Station")
  rmse_table(d, c("GrainClass", group.col), "Difference_mass_inactive", "RMSE_mass_inactive")

#' @describeIn read_sediment Read the Mass In Cum data output.
#' @export
read_mass_in_cum = function(f, which.times = NULL, which.stations = NULL, which.grains = NULL)
  read_sediment(f, "Mass In Cum", which.times, which.stations, which.grains)

#' @describeIn difference_sediment Compute a difference table for Mass In Cum data.
#' @export
difference_mass_in_cum = function(d1, d2, relative = FALSE)
  difference_sediment(d1, d2, "Difference_mass_in_cum", relative, "Time", "GrainClass")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_mass_in_cum = function(d, group.col = "Station")
  rmse_table(d, c("GrainClass", group.col), "Difference_mass_in_cum", "RMSE_mass_in_cum")

#' @describeIn read_sediment Read the Mass Out data output.
#' @export
read_mass_out = function(f, which.times = NULL, which.stations = NULL, which.grains = NULL)
  read_sediment(f, "Mass Out", which.times, which.stations, which.grains)

#' @describeIn difference_sediment Compute a difference table for Mass Out data.
#' @export
difference_mass_out = function(d1, d2, relative = FALSE)
  difference_sediment(d1, d2, "Difference_mass_out", relative, "Time", "GrainClass")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_mass_out = function(d, group.col = "Station")
  rmse_table(d, c("GrainClass", group.col), "Difference_mass_out", "RMSE_mass_out")

#' @describeIn read_sediment Read the Mass Out Cum data output.
#' @export
read_mass_out_cum = function(f, which.times = NULL, which.stations = NULL, which.grains = NULL)
  read_sediment(f, "Mass Out Cum", which.times, which.stations, which.grains)

#' @describeIn difference_sediment Compute a difference table for Mass Out Cum data.
#' @export
difference_mass_out_cum = function(d1, d2, relative = FALSE)
  difference_sediment(d1, d2, "Difference_mass_out_cum", relative, "Time", "GrainClass")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_mass_out_cum = function(d, group.col = "Station")
  rmse_table(d, c("GrainClass", group.col), "Difference_mass_out_cum", "RMSE_mass_out_cum")

#' @describeIn read_sediment Read the Mass Subsurface data output.
#' @export
read_mass_subsurface = function(f, which.times = NULL, which.stations = NULL, which.grains = NULL)
  read_sediment(f, "Mass Subsurface", which.times, which.stations, which.grains)

#' @describeIn difference_sediment Compute a difference table for Mass Subsurface data.
#' @export
difference_mass_subsurface = function(d1, d2, relative = FALSE)
  difference_sediment(d1, d2, "Difference_mass_subsurface", relative, "Time", "GrainClass")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_mass_subsurface = function(d, group.col = "Station")
  rmse_table(d, c("GrainClass", group.col), "Difference_mass_subsurface", "RMSE_mass_subsurface")

#' @describeIn read_sediment Read the Reduce Armor Factor data output.
#' @export
read_reduce_armor_factor = function(f, which.times = NULL, which.stations = NULL, which.grains = NULL)
  read_sediment(f, "Reduce Armor Factor", which.times, which.stations, which.grains)

#' @describeIn difference_sediment Compute a difference table for Reduce Armor Factor data.
#' @export
difference_reduce_armor_factor = function(d1, d2, relative = FALSE)
  difference_sediment(d1, d2, "Difference_reduce_armor_factor", relative, "Time", "GrainClass")

#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.
#' @export
rmse_reduce_armor_factor = function(d, group.col = "Station")
  rmse_table(d, c("GrainClass", group.col), "Difference_reduce_armor_factor", "RMSE_reduce_armor_factor")

