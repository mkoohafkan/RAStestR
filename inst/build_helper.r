library(stringr)
devtools::load_all()
output.file = "R/helper.r"

standard = list_tables()$standard
sediment = list_tables()$sediment

make_helper_standard = function(table){
  tstring = table %>% str_replace_all(c(" " = "\\_", "\\." = "")) %>%
    str_to_lower()

  read = paste(
    sprintf("#' @describeIn read_standard Read the %s data output.", table),
    "#' @export",
    sprintf("read_%s = function(f, which.times = NULL, which.stations = NULL)", tstring),
    sprintf('  read_standard(f, "%s", which.times, which.stations)', table),
    sep = "\n"
  )

  diff = paste(
    sprintf("#' @describeIn difference_table Compute a difference table for %s data.", table),
    "#' @export",
    sprintf('difference_%s = function(d1, d2, relative = FALSE)', tstring),
    sprintf('  difference_table(d1, d2, "Difference_%s", relative, "Time")', tstring),
    sep = '\n'
  )

  rmse = paste(
    sprintf("#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.", table),
    "#' @export",
    sprintf('rmse_%s = function(d, group.col = "Station")', tstring),
    sprintf('  rmse_table(d, group.col, "Difference_%s", "RMSE_%s")', tstring, tstring),
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
    sprintf("read_%s = function(f, which.times = NULL, which.stations = NULL, which.grains = NULL)", tstring),
    sprintf('  read_sediment(f, "%s", which.times, which.stations, which.grains)', table),
    sep = "\n"
  )

  diff = paste(
    sprintf("#' @describeIn difference_sediment Compute a difference table for %s data.", table),
    "#' @export",
    sprintf("difference_%s = function(d1, d2, relative = FALSE)", tstring),
    sprintf('  difference_sediment(d1, d2, "Difference_%s", relative, "Time", "GrainClass")', tstring),
    sep = "\n"
  )

  rmse = paste(
    sprintf("#' @describeIn rmse_table Compute RMSE of Froude Number Channel outputs.", table),
    "#' @export",
    sprintf('rmse_%s = function(d, group.col = "Station")', tstring),
    sprintf('  rmse_table(d, c("GrainClass", group.col), "Difference_%s", "RMSE_%s")', tstring, tstring),
    sep = "\n"
  )

  paste(read, diff, rmse, sep = "\n\n")
}

funcs = unlist(c(
  lapply(standard, make_helper_standard),
  lapply(sediment, make_helper_sediment)
))

writeLines(funcs, output.file, sep = "\n\n")
