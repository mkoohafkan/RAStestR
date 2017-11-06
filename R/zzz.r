RAStestR_default_options = list(
    RAStestR.RASversion = "5.0.3"
)

.onLoad = function(libname, pkgname) {
  op = options()
  toset = !(names(RAStestR_default_options) %in% names(op))
  if (any(toset))
    options(RAStestR_default_options[toset])
  invisible(NULL)
}
