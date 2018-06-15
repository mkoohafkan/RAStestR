fit_gradation = function(d) {




d = data_frame(
  nphi = seq(-8, 3),
  frac= c(0.0067, 0.019, 0.04, 0.06, 0.076, 0.3, 3.8, 52.5, 98.4,
    99.7, 99.9, 100)/100
)

res = nls(frac ~ pnorm(nphi, mu, sigma), start = with(d, list(mu = mean(nphi), sigma = sd(nphi))),
  data = d)

plot(d)
lines(d$nphi, predict(res))


grads = read_excel("C:/PROJECTS/GRAND RIVER/INPUT DATA - FINALIZED/bed gradations.xlsx",
  sheet = "Gradations")

grad.list = list()
for (n in names(grads)[2:ncol(grads)]) {
  this.grad = grads[c("Class", n)]
  this.grad["nphi"] = seq(-8, 11)
  nona = !is.na(this.grad[[n]])
  grad.list[[n]] = with(this.grad,
    data.frame(nphi = nphi[nona], frac = frac[nona]))


  na.omit(this.grad[c("nphi"), ])
}

}










#' Make Flow-Load Curve
#'
#' Generate a flow-load rating curve from suspended load 
#' and/or bedload data.
#'
#' @param d A dataframe of sediment load data. The first column is
#'   the measured flow and the second column is the measured load.
#' @param flows.out The flows to output loads for. Loads will be 
#'   extrapolated for flows outside the range of flows provided in 
#'   \code{load}. If \code{NULL}, The minimum and maximum flows will 
#'   be used and 50 flows will be output.
#' @param bedload An optional second dataframe containing bedload data.
#' @return A flow load curve.
#'
#' @details The flow load curve is computed as a log-linear 
#'   relationship between flow and load. If both \code{d} and 
#'   \code{bd} are provided, a total load curve is computed by 
#'   generating separate log-linear relationships for suspended load
#'   and bed load, and adding the results for the specified output 
#'   flows.
#'
#' @importFrom stats setNames
#' @importFrom stats lm
#' @export
make_flowload_curve = function(d, flows.out, bd = NULL) {
  # flows
  if (is.null(flows.out))
    flows.out = exp(seq(min(log(d[[1]])), max(log(d[[1]])),
      length.out = 50L))
  # suspended load
  ssload = setNames(d, c("flow", "load"))
  sslm = lm(log(load) ~ log(flow), data = ssload)
  ssfun = function(x)
    exp(sslm$coefficients[[1]] + log(x)*sslm$coefficients[[2]])
  # bed load
  if (!is.null(bedload)) {
    bedload = setNames(bd, c("flow", "load"))
    blm = lm(log(load) ~ log(flow), data = bedload)
    bfun = function(x)
      exp(blm$coefficients[[1]] + log(x)*blm$coefficients[[2]])
    } else {
      bfun = function(x) 0
    }
  # total load
  data.frame(flow = flows.out,
    load = ssfun(flows.out) + bfun(flows.out))
}


#' Make Gradation
#'
#' Generate a gradation curve from measured data.
#'
#' @param d A two column data frame. The first column is the grain size
#'   (e.g. mm) and the second column is the mass or percentage of the 
#'   sample in that class.
#' @param is.percent.finer If \code{TRUE}, the mass/percentage data
#'   in \code{d} is assumed to be cumulative.
#' @param grain.classes A vector of grain size classes, in the same 
#'   units as provided in \code{d}. If \code{NULL}, the standard grain 
#'   classes in RAS (in mm) is assumed.
#' @param interpolate.phi If \code{TRUE}, all interpolations between
#'   grain classes will be performed using the phi scale for grain 
#'   sizes (e.g. log base 2 scale).
#' @return A sediment gradation curve.
#'
#' @importFrom stats approx
#' @export
make_gradation = function(d, is.cumulative = FALSE, grain.classes = NULL,
  interpolate.phi = TRUE) {
  if (is.null(grain.classes))
    grain.classes = c(0.004, 0.008, 0.016, 0.032, 0.0625, 0.125,
      0.25, 0.5, 1, 2, 4, 8, 16, 32, 64)
  d.order = order(d[[1]])
  gs = d[d.order, 1]
  m = d[d.order, 2]
  if (!is.percent.finer)
    m = cumsum(m)
  if (interpolate.phi)
    fun = function(x) log(x, base = 2)
  else
    fun = identity
  m.out = approx(fun(gs), m, xout = fun(grain.classes),
    yleft = NA, yright = 100)$y
  data.frame(grain.size = grain.classes, percent.finer = m.out)
}

#' Representative Grain Size
#' 
#' Get the nth percentile representative grain size.
#'
#' @param d A data frame containing a gradation curve, e.g. the 
#'   output of \code{make_gradation}. The first column contains 
#'   the grain size classes and the second column contains the 
#'   percent finer score.
#' @param n The percentiles for which to compute a representative 
#'   grain class.
#' @inheritParams make_gradation
#' @return A vector of grain sizes of same length as \code{n}.
#'
#' @importFrom stats approx
#' @export
representative_size = function(d, n = c(30, 50, 86),
  interpolate.phi = TRUE) {
  gs = d[[1]]
  pf = d[[2]]
  if (interpolate.phi) {
    fun = function(x) log(x, base = 2)
    invfun = function(x) 2^x
  }
  else {
    fun = identity
    invfun = identity
  }
  out.n = rep(NA, length(n))
  for (i in seq_along(n)) {
    lbound = max(which(pf < n[i]))
    ubound = min(which(pf > n[i]))
    out.n[i] = invfun(approx(pf[lbound:ubound], fun(gs[lbound:ubound]),
      xout = n[i])$y)
  }
  out.n
}


