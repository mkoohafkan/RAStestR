#' Generate Report
#'
#' Generate a RAStestR report.
#'
#' @param model1.file The path to the first RAS model HDF output.
#' @param model2.file The path to the second RAS model HDF output.
#' @param model1.type The type ("quasi" or "unsteady") of the first RAS model.
#' @param model2.type The type of the second RAS model.
#' @param sections The sections to include in the report.
#' @param sediment.table.classes If grain class sections are specified,
#'   the grain classes to include in the report.
#' @param sediment.table.rows If grain class sections are specified,
#'   which rows of the grain class tables to include in the report.
#' @param output.file The output filename, without an extension.
#' @param output.folder The folder to write out files to. Defaults to
#'   the R temporary directory.
#' @param output.html If \code{TRUE}, generate an HTML report.
#' @details Generating a report requires that the packages 
#'   \pkg{knitr}, \pkg{markdown} and \pkg{ggplot2}
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
#' @seealso \link{read_standard}
#' @seealso \link{read_sediment}
#' @export
generate_report = function(model1.file, model2.file, model1.type, model2.type,
  sections, sediment.table.classes, sediment.table.rows, output.file = "report", 
  output.folder = tempdir(),
  output.html = TRUE) {
  if (!requireNamespace("knitr"))
    stop("Package 'knitr' is required to generate RAStestR reports.")
  if (!requireNamespace("markdown"))
    stop("Package 'knitr' is required to generate RAStestR reports.")
  if (!requireNamespace("ggplot2"))
    stop("Package 'ggplot2' is required to generate RAStestR reports.")

  oldwd = getwd()
  on.exit(setwd(oldwd))
  setwd(output.folder)

  standard = c(
    "Dredged Cum", "Effective Depth", "Effective_Width", "Flow",
    "Froude Number Channel", "Invert Change", "Invert Elevation",
    "Mannings n Channel", "Mean Effective Invert Change",
      "Mean Effective Invert Elevation", "Sediment Concentration",
      "Shear Stress", "Slope", "Velocity", "Water Surface",
      "d10 Active", "d10 Inactive", "d50 Active", "d50 Inactive",
      "d90 Active", "d90 Inactive")
  sediment = c(
     "Lat Struc Mass Div", "Long. Cum Mass Change",
     "Mass Bed Change Cum", "Mass In Cum", "Mass Out Cum")
  selectedstandard = standard[str_to_lower(standard) %in% 
    str_to_lower(sections)]
  selectedsediment = sediment[str_to_lower(sediment) %in% 
    str_to_lower(sections)]
  sections = c(selectedstandard, selectedsediment)
  if (length(sections) < 1)
    stop("No recognizable sections specified.")
  if (length(selectedsediment) > 0)
    if (missing(sediment.table.classes) || missing(sediment.table.rows))
      stop("One or more sections require the arguments ",
      "'sediment.table.classes' and 'sediment.table.rows'.")

  doc.env = new.env()
#  assign("childpaths", paths, pos = doc.env)
  assign("childsections", sections, pos = doc.env)
  assign("file1", model1.file, pos = doc.env)
  assign("file2", model2.file, pos = doc.env)
  assign("type1", model1.type, pos = doc.env)
  assign("type2", model2.type, pos = doc.env)
  assign("type2", model2.type, pos = doc.env)
  assign("sectionchunk", report_chunk, pos = doc.env)
  if (!missing(sediment.table.classes))
    assign("grain.classes", sediment.table.classes, pos = doc.env)
  if (!missing(sediment.table.rows))
    assign("grain.rows", sediment.table.rows, pos = doc.env)
  doc.template = system.file("report-templates/generic.rmd", package = "RAStestR")
  outmd = str_c(output.file, ".md")
  suppressWarnings(knitr::knit(doc.template, outmd, envir = doc.env,
    quiet = TRUE))
  if (output.html) {
    outhtml = str_c(output.file, ".html")
    markdown::markdownToHTML(file = outmd, output = outhtml)
    shell.exec(file.path(output.folder, outhtml))
    file.path(output.folder, outhtml)
  } else
    shell.exec(file.path(output.folder, outmd))
  file.path(output.folder, outmd)
}

report_chunk = function(label) { 
  if(tolower(label) %in% c("dredged cum", "effective depth", 
      "effective_width", "flow", "froude number channel", 
      "invert change", "invert elevation", "mannings n channel", 
      "mean effective invert change", "mean effective invert elevation", 
      "sediment concentration", "shear stress", "slope", "velocity", 
      "water surface", "d10 active", "d10inactive", "d50active", 
      "d50inactive", "d90 active", "d90 inactive"))
    system.file("report-templates/sections/standard-chunk.rmd", package = "RAStestR")
  else
    system.file("report-templates/sections/sediment-chunk.rmd", package = "RAStestR")
}
