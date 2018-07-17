#' Generate Report
#'
#' Generate a RAStestR report.
#'
#' @param base.file The path to the first or "base" RAS model HDF output.
#' @param new.file The path to the second or "new" RAS model HDF output.
#' @param sections The sections to include in the report.
#' @param standard.opts List with elements \code{which.times} and 
#'   \code{which.stations} specifying the times and stations to output
#'   for standard tables. See \code{\link{read_standard}} for details.
#' @param sediment.opts List with elements \code{which.times},
#'   \code{which.stations} and \code{which.grains} specifying the times,
#'   stations and grain classes to output for sediment tables. See 
#'   \code{\link{read_sediment}} for details.
#' @param output.name The output filename, without an extension.
#' @param output.folder The folder to write out files to. Defaults to
#'   the R temporary directory.
#' @param output.type The file type to generate, either "html" or "pdf".
#'
#' @details Generating a report requires that the packages 
#'   \pkg{knitr}, \pkg{rmarkdown} and \pkg{ggplot2}
#'   are installed. The following sections can be included in a report:
#'   \itemize{
#'     \item dredged cum 
#'     \item effective depth
#'     \item effective_width
#'     \item flow
#'     \item froude number channel
#'     \item invert change
#'     \item invert elevation
#'     \item lat struc mass div
#'     \item long. cum mass change
#'     \item mannings n channel
#'     \item mass bed change cum
#'     \item mass in cum
#'     \item mass out cum
#'     \item mean effective invert change
#'     \item mean effective invert elevation
#'     \item sediment concentration
#'     \item shear stress
#'     \item slope
#'     \item velocity
#'     \item water surface
#'     \item d10 active
#'     \item d10 inactive
#'     \item d50 active
#'     \item d50 inactive
#'     \item d90 active
#'     \item d90 inactive
#'   }
#'
#' @examples
#' \dontrun{
#' simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#'   package = "RAStestR")
#' generate_report(simple.quasi, simple.quasi, 
#'   sections = c("Flow", "Vol In Cum"),
#'   output.name = "test", output.type = "html")
#' }
#'
#' @seealso \link{read_standard}
#' @seealso \link{read_sediment}
#' @export
generate_report = function(base.file, new.file, sections, 
  standard.opts = list(), sediment.opts = list(),  
  output.name, output.folder = tempdir(), 
  output.type = c("html", "pdf")) {
  if (!requireNamespace("knitr"))
    stop("Package 'knitr' is required to generate RAStestR reports.")
  if (!requireNamespace("rmarkdown"))
    stop("Package 'rmarkdown' is required to generate RAStestR reports.")
  if (!requireNamespace("scales"))
    stop("Package 'scales' is required to generate RAStestR reports.")
  if (!requireNamespace("ggplot2"))
    stop("Package 'ggplot2' is required to generate RAStestR reports.")
  
  if(missing(output.name))
    output.name = basename(tempfile(tmpdir = output.folder))
    
  base.file = normalizePath(base.file, winslash = "/")
  new.file = normalizePath(new.file, winslash = "/")
  if (!(file.exists(base.file)))
    stop("file ", base.file, " could not be found.")
  if (!(file.exists(new.file)))
    stop("file ", new.file, " could not be found.")

  # get plan attributes
  base.attr = get_plan_meta(base.file)
  new.attr = get_plan_meta(new.file)
  # get plan run types
  base.type = get_run_type(base.file)
  new.type = get_run_type(new.file)

  # prepare table options
  if (!all(names(standard.opts) %in% c("which.times", "which.stations")))
    stop("Some elements of argument 'standard.opts' were not recognized.")
  if (!all(names(sediment.opts) %in% c("which.times", "which.stations", 
      "which.grains")))
    stop("Some elements of argument 'sediment.opts' were not recognized.")
  which.times.standard = standard.opts[["which.times"]]
  which.stations.standard = standard.opts[["which.stations"]]
  which.times.sediment = sediment.opts[["which.times"]]
  which.stations.sediment = sediment.opts[["which.stations"]]
  which.grains = sediment.opts[["which.grains"]]

  # prepare output folder
  oldwd = getwd()
  setwd(output.folder)
  doc.template = system.file("report-templates/generic-spin.r", package = "RAStestR")
  standard.template = system.file("report-templates/sections/standard-spin.r", package = "RAStestR")
  sediment.template = system.file("report-templates/sections/sediment-spin.r", package = "RAStestR")
  file.copy(doc.template, file.path(output.folder, basename(doc.template)))
  file.copy(standard.template, file.path(output.folder, basename(standard.template)))
  file.copy(sediment.template, file.path(output.folder, basename(sediment.template)))

  on.exit({
  file.remove(file.path(output.folder, basename(doc.template)))
  file.remove(file.path(output.folder, basename(standard.template)))
    file.remove(file.path(output.folder, basename(sediment.template)))
  setwd(oldwd)
  })

  output.type = match.arg(str_to_lower(output.type), c("html", "pdf"))
  spin.format = switch(output.type,
    pdf = "pdf_document",
    html = "html_document"
  )
  output.name = str_c(output.name, ".", output.type)

  standard = list_tables()$standard
  sediment = list_tables()$sediment
  selectedstandard = standard[str_to_lower(standard) %in%
    str_to_lower(sections)]
  selectedsediment = sediment[str_to_lower(sediment) %in%
    str_to_lower(sections)]
  sections = c(selectedstandard, selectedsediment)
  if (length(sections) < 1)
    stop("No recognizable sections specified.")

  doc.env = new.env()
  assign("childsections", sections, pos = doc.env)
  assign("file1", base.file, pos = doc.env)
  assign("file2", new.file, pos = doc.env)
  assign("type1", base.type, pos = doc.env)
  assign("type2", new.type, pos = doc.env)
  assign("label1", base.attr[["Plan Name"]], pos = doc.env)
  assign("label2", new.attr[["Plan Name"]], pos = doc.env)
  assign("sectionchunk", spin_chunk, pos = doc.env)
  assign("table.times.standard", which.times.standard, pos = doc.env)
  assign("table.stations.standard", which.stations.standard, pos = doc.env)  
  assign("table.times.sediment", which.times.sediment, pos = doc.env)
  assign("table.stations.sediment", which.stations.sediment, pos = doc.env)
  assign("table.grains", which.grains, pos = doc.env)

  rmarkdown::render(basename(doc.template), output_format = spin.format,
     output_file = output.name, output_dir = output.folder, 
     runtime = "static", envir = doc.env, quiet = TRUE)
  shell.exec(file.path(output.folder, output.name))
  file.path(output.folder, output.name)
}


spin_chunk = function(label) {
  if (label %in% list_tables()$standard)
    return("standard-spin.r")
  if (label %in% list_tables()$sediment)
    return("sediment-spin.r")
  stop("Table '", label, "' not recognized.")
}

#' HEC-RAS Output Tables
#'
#' List all potential output tables produced by an HEC-RAS sediment
#' transport model.
#'
#' @return A list named list of character vectors containing the names
#'   of "standard" and "sediment" tables.
#'
#' @export
list_tables = function() {
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
    "Fall Velocity", 
    "Lat Struc Mass Div", "Lat Struc Vol Div",
    "Long. Cum Mass Change", "Long. Cum Vol Change", 
    "Long. Cum Mass Moveable Limit", "Long. Cum Vol Moveable Limit",
    "Mass Bed Change", "Vol Bed Change",
    "Mass Bed Change Cum", "Vol Bed Change Cum",
    "Mass Capacity", "Vol Capacity",
    "Mass Cover", "Vol Cover",
    "Mass In", "Vol In",
    "Mass Inactive", "Vol Inactive", 
    "Mass In Cum", "Vol In Cum",
    "Mass Out", "Vol Out", 
    "Mass Out Cum", "Vol Out Cum", 
    "Mass Subsurface", "Vol Subsurface",
    "Reduce Armor Factor"
  )
  list(standard = standard, sediment = sediment)
}
