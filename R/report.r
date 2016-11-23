#' Generate Report
#'
#' Generate a RAStestR report.
#'
#' @param model1.file The path to the first or "base" RAS model HDF output.
#' @param model2.file The path to the second or "new" RAS model HDF output.
#' @param model1.type The type ("quasi" or "unsteady") of the base RAS model.
#' @param model2.type The type of the new RAS model.
#' @param model1.label Optional label for the base RAS model.
#' @param model2.label Optional label for the new RAS model.
#' @param sections The sections to include in the report.
#' @param sediment.table.classes If grain class sections are specified,
#'   the grain classes to include in the report.
#' @param sediment.table.rows If grain class sections are specified,
#'   which rows of the grain class tables to include in the report.
#' @param output.file The output filename, without an extension.
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
#' @seealso \link{read_standard}
#' @seealso \link{read_sediment}
#' @export
generate_report = function(model1.file, model2.file, model1.type, model2.type,
  model1.label = NULL, model2.label = NULL, sections, sediment.table.classes,
  sediment.table.rows, output.file = "report", output.folder = tempdir(),
  output.type = c("html", "pdf")) {
  if (!requireNamespace("knitr"))
    stop("Package 'knitr' is required to generate RAStestR reports.")
  if (!requireNamespace("rmarkdown"))
    stop("Package 'rmarkdown' is required to generate RAStestR reports.")
  if (!requireNamespace("ggplot2"))
    stop("Package 'ggplot2' is required to generate RAStestR reports.")

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
  file.remove(basename(doc.template))
  file.remove(basename(standard.template))
  file.remove(basename(sediment.template))
  setwd(oldwd)
  })

  output.type = match.arg(str_to_lower(output.type), c("html", "pdf"))
  spin.format = switch(output.type,
    pdf = "pdf_document",
    html = "html_document"
  )
  output.file = str_c(output.file, ".", output.type)

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
  assign("label1", model1.label, pos = doc.env)
  assign("label2", model2.label, pos = doc.env)
  assign("sectionchunk", spin_chunk, pos = doc.env)
  if (!missing(sediment.table.classes))
    assign("grain.classes", sediment.table.classes, pos = doc.env)
  if (!missing(sediment.table.rows))
    assign("grain.rows", sediment.table.rows, pos = doc.env)

  rmarkdown::render(basename(doc.template), output_format = spin.format,
     output_file = output.file, output_dir = output.folder, 
     runtime = "static", envir = doc.env, quiet = TRUE)
  shell.exec(file.path(output.folder, output.file))
  file.path(output.folder, output.file)
}


spin_chunk = function(label) {
  if (tolower(label) %in% c("dredged cum", "effective depth",
      "effective_width", "flow", "froude number channel",
      "invert change", "invert elevation", "mannings n channel",
      "mean effective invert change", "mean effective invert elevation",
      "sediment concentration", "shear stress", "slope", "velocity",
      "water surface", "d10 active", "d10inactive", "d50active",
      "d50inactive", "d90 active", "d90 inactive"))
    "standard-spin.r"
  else
    "sediment-spin.r"
}
