RAStestR_default_options = list(
    RAStestR.DefaultVersion = "5.0.3",
    RAStestR.ForceVersion = NULL,
    RAStestR.VersionOverride = NULL
)

.onLoad = function(libname, pkgname) {
  op = options()
  toset = !(names(RAStestR_default_options) %in% names(op))
  if (any(toset))
    options(RAStestR_default_options[toset])
  invisible(NULL)
}
