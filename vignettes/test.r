library(RAStestR)

folder = "C:/PROJECTS/FARGO/NEW - HWF-WF Sediment 2015_clip"           


quasi = file.path(folder, "HWF-WF_2015.p08.hdf")
u30s = file.path(folder, "HWF-WF_2015.p07.hdf")
u1m = file.path(folder, "HWF-WF_2015.p06.hdf")
u5m = file.path(folder, "HWF-WF_2015.p05.hdf")

generate_report(quasi, u30s, "quasi", "unsteady", "quasi-unsteady",
  "unsteady (dt = 30s)", c("Water Surface", "Invert Elevation", 
    "Long. Cum Mass Change"), c("", 1:7), 4777, output.type = "html")

generate_report(u30s, u1m, "unsteady", "unsteady", "unsteady (dt = 30s)", 
  "unsteady (dt = 1m)", c("Water Surface", "Invert Elevation",
    "Long. Cum Mass Change"), c("", 1:7), 4777, output.type = "pdf")

generate_report(u30s, u5m, "unsteady", "unsteady", "unsteady (dt = 30s)",
  "unsteady (dt = 5m)", c("Water Surface", "Invert Elevation",
    "Long. Cum Mass Change"), c("", 1:7), 4777, output.type = "pdf")
