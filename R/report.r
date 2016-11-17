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
#' @param output.folder The folder to write out files to.
#' @param output.html If \code{TRUE}, generate an HTML report.
#' @details Generating a report requires that the packages 
#'   \pkg{knitr}, \pkg{markdown} and \pkg{ggplot2}
#'   are installed. The following sections can be included in a report:
#'   \itemize{
#'     \item dredged cum
#'     \item effective depth
#'     \item effective_width
#'     \item flow
#'     \item invert change
#'     \item froude number channel
#'     \item invert change
#'     \item mannings n channel
#'     \item mean effective invert change
#'     \item mean effective invert elevation
#'     \item sediment concentration
#'     \item shear stress
#'     \item slope
#'     \item velocity
#'     \item water surface
#'     \item d10 active
#'     \item d10inactive
#'     \item d50active
#'     \item d50inactive
#'     \item d90 active
#'     \item d90 inactive
#'   }
#'
#' @seealso \link{read_standard}
#' @seealso \link{read_sediment}
#' @export
generate_report = function(model1.file, model2.file, model1.type, model2.type,
  sections, sediment.table.classes, sediment.table.rows, output.folder = "", 
  output.html = TRUE) {
  if (!requireNamespace("knitr"))
    stop("Package 'knitr' is required to generate RAStestR reports.")
  if (!requireNamespace("markdown"))
    stop("Package 'knitr' is required to generate RAStestR reports.")
  if (!requireNamespace("ggplot2"))
    stop("Package 'ggplot2' is required to generate RAStestR reports.")

  if (missing(output.folder))
    output.folder = getwd()
  oldwd = getwd()
  on.exit(setwd(oldwd))
  setwd(output.folder)
  
  standardsections = tolower(sections) %>% intersect(c(
    "dredged cum", "effective depth", "effective_width", "flow", 
    "invert change", "froude number channel", "invert change", 
    "mannings n channel", "mean effective invert change", 
      "mean effective invert elevation", "sediment concentration", 
      "shear stress", "slope", "velocity", "water surface", 
      "d10 active", "d10inactive", "d50active", "d50inactive", 
      "d90 active", "d90 inactive"))
  sedimentsections = tolower(sections) %>% intersect(c(
     "lat struct mass div", "long cum mass change", 
     "mass bed change cum", "mass in cum", "mass out cum"))
  if (length(sedimentsections) > 0)
    if (missing(sediment.table.classes) || missing(sediment.table.rows))
      stop("One or more sections require the arguments ",
      "'sediment.table.classes' and 'sediment.table.rows'.")
  sections = c(standardsections, sedimentsections)
  children = sections %>% str_replace(" ", "-") %>% str_c(".rmd")
  paths = system.file("report-templates/sections", children,
    package = "RAStestR")

  if (length(paths) < 1)
    stop("No recognizable sections specified.")

  doc.env = new.env()
  assign("childpaths", paths, pos = doc.env)
  assign("childsections", sections, pos = doc.env)
  assign("file1", model1.file, pos = doc.env)
  assign("file2", model2.file, pos = doc.env)
  assign("type1", model1.type, pos = doc.env)
  assign("type2", model2.type, pos = doc.env)
  assign("type2", model2.type, pos = doc.env)
  if (!missing(sediment.table.classes))
    assign("grain.classes", sediment.table.classes, pos = doc.env)
  if (!missing(sediment.table.rows))
    assign("grain.rows", sediment.table.rows, pos = doc.env)
  doc.template = system.file("report-templates/base.rmd", package = "RAStestR")
  out = knitr::knit(doc.template, "output.md", envir = doc.env)
  if (output.html)
    out = markdown::markdownToHTML(out)
  out
}
