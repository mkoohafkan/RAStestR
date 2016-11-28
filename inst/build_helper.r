library(stringr)
output.file = "R/helper.r"

  standard = c(
    "Dredged Cum", "Effective Depth", "Effective_Width", "Flow",
    "Froude Number Channel", "Hydraulic Radius", 
    "Invert Change", "Invert Elevation", "Mannings n Channel", 
    "Mean Effective Invert Change", "Mean Effective Invert Elevation", 
    "Moveable Elv L", "Moveable Elv R", 
    "Moveable Sta L", "Moveable Sta R", 
    "Observed Data", "Sediment Concentration", 
    "Shear Stress", "Shear Velocity", "Slope", "Temperature", 
    "Thickness Cover", "Thickness Inactive", "Thickness Subsurface", 
    "Velocity", "Water Surface", 
    "d10 Active", "d10 Cover", "d10 Inactive", "d10 Subsurface",
    "d16 Active", "d16 Cover", "d16 Inactive", "d16 Subsurface",
    "d50 Active", "d50 Cover", "d50 Inactive", "d50 Subsurface",
    "d84 Active", "d84 Cover", "d84 Inactive", "d84 Subsurface",
    "d90 Active", "d90 Cover", "d90 Inactive", "d90 Subsurface"    
    )
  sediment = c(
     "Fall Velocity", "Lat Struc Mass Div", 
     "Long. Cum Mass Change", "Long. Cum Mass Moveable Limit", 
     "Mass Bed Change", "Mass Bed Change Cum", 
     "Mass Capacity", "Mass Cover",  
     "Mass In", "Mass Inactive", "Mass In Cum", 
     "Mass Out", "Mass Out Cum", "Mass Subsurface", 
     "Reduce Armor Factor")


make_helper_standard = function(table){
  tstring = table %>% str_replace_all(c(" " = "\\_", "\\." = "")) %>% 
    str_to_lower()
  
  read = paste(
    sprintf("#' @describeIn read_standard Read the %s data output.", table),
    "#' @export",
    sprintf("read_%s = function(f, run.type, which.times = NULL, which.stations = NULL)", tstring),
    sprintf('  read_standard(f, "%s", run.type, which.times, which.stations)', table),
    sep = "\n"  
  )
  
  diff = paste(
    sprintf("#' @describeIn diff_table Compute a difference table for %s data.", table),
    "#' @export",
    sprintf('diff_%s = function(d1, d2)', tstring),
    sprintf('  diff_table(d1, d2, "Time", "Diff_%s")', tstring),
    sep = '\n'
  )

  rmse = paste(
    sprintf("#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.", table),
    "#' @export",    
    sprintf('rmse_%s = function(d, groupcol = "Station")', tstring),
    sprintf('  rmse_table(d, groupcol, "diff_%s", "rmse_%s")', tstring, tstring),
    sep = "\n"
  )

  paste(read, diff, rmse, sep = "\n\n")
}

make_helper_sediment = function(table){
  tstring = table %>% str_replace_all(c(" " = "\\_", "\\." = "")) %>% 
    str_to_lower()
  
  read = paste(
    sprintf("#' @describeIn read_sediment Read the %s data output.", table),
    "#' @export",
    sprintf("read_%s = function(f, run.type, which.times = NULL, which.stations = NULL, which.grains = NULL)", tstring),
    sprintf('  read_sediment(f, "%s", run.type, which.times, which.stations, which.grains)', table),
    sep = "\n"  
  )
  
  diff = paste(
    sprintf("#' @describeIn diff_sediment Compute a difference table for %s data.", table),
    "#' @export", 
    sprintf("diff_%s = function(d1, d2)", tstring),
    sprintf('  diff_sediment(d1, d2, "Time", "GrainClass", "Diff_%s")', tstring),
    sep = "\n"
  )

  rmse = paste(
    sprintf("#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.", table),
    "#' @export",    
    sprintf('rmse_%s = function(d, groupcol = "Station")', tstring),
    sprintf('  rmse_table(d, c("GrainClass", groupcol), "diff_%s", "rmse_%s")', tstring, tstring),
    sep = "\n"
  )

  paste(read, diff, rmse, sep = "\n\n")
}

funcs = unlist(c(
  lapply(standard, make_helper_standard),
  lapply(sediment, make_helper_sediment)
))

writeLines(funcs, output.file, sep = "\n\n")



