#' Stochasim Algorithm
#'
#' @param x,y Hydrographs, as a list or bare. Recycled to the same length.
#' @param cross_section A `"cross_section"` object representing a stream's
#' cross section.
#' @param rv_rate Rate parameter for revegetation, passed to `revegetate()`.
#' For `stochasim2()`, possibly a vector of length two corresponding to
#' different revegetation rates for hydrographs `x` and `y`. Note: a rate of
#' 0 means no revegetation happens.
#' @param nsim Number of event hydrographs to run; positive integer.
#' @param niter Number of iterations for `gbem::gbem()` when running
#' each hydrograph.
#' @returns A stochasim object, containing all the event hydrographs, and
#' cross sections.
#' @examples
#' library(distionary)
#' regime <- hydist_snow(distionary::dst_gev(1000, 3, 0.1), baseflow = 50)
#' cs <- gbem::cross_section(3, grad = 0.01, d50 = 0.1, d84 = 0.5, roughness = 0.01)
#' ss <- stochasim(regime, cross_section = cs)
#' ss_flows(ss)
#' ss_widths(ss)
#' @rdname stochasim
#' @export
stochasim <- function(x, cross_section, rv_rate = 0.1, niter = 300) {
  if (is_hydrograph(x)) x <- list(x)
  if (length(rv_rate) != 1) {
    stop("Revegetation rate must be a length 1 vector. Received length ",
         length(rv_rate), ".")
  }
  Q_base <- mean(vapply(x, \(h) h(0), FUN.VALUE = numeric(1L)))
  #estimate the low flow width based on base flow using std hydraulic geometry eq
  width_base <- 3 * sqrt(Q_base)
  cs <- list(cross_section)
  for (i in seq_along(x)) {
    g <- gbem(x[[i]], cross_section = cs[[i]], niter = niter)
    cs0 <- erode(g)
    if (ch_width(cs0) == ch_width(cs[[i]])) {
      cs0 <- revegetate(cs0, width_base, rate = rv_rate)
    }
    cs[[i + 1]] <- cs0
  }
  res <- list(
    x = x,
    cross_sections = cs,
    nsim = length(x),
    niter = niter,
    rv_rate = rv_rate
  )
  new_stochasim(res)
}

#' Constructor Function for Stochasim Objects
new_stochasim <- function(l, ..., class = character()) {
  structure(l, ..., class = c(class, "stochasim"))
}

#' @exportS3Method base::print
print.stochasim <- function(x, ...) {
  ellipsis::check_dots_empty()
  cat("Stochasim object with ", x$nsim, " runs.")
}

#' Get peak flows and channel widths from stochasim
#' @param stochasim Stochasim object.
#' @export
#' @rdname ss
ss_flows <- function(stochasim) {
  h <- stochasim$hydrographs
  vapply(h, gbem::peak, FUN.VALUE = numeric(1L))
}

#' @export
#' @rdname ss
ss_widths <- function(stochasim) {
  cs <- stochasim$cross_sections
  vapply(cs, gbem::ch_width, FUN.VALUE = numeric(1L))
}

#' @exportS3Method graphics::plot
plot.stochasim <- function(stochasim, what = c("widths", "flows"), from = 0) {
  what <- match.arg(what)
  if (what == "widths") {
    ss_plot_widths(stochasim, from = from)
  }
  if (what == "flows") {
    ss_plot_flows(stochasim, from = from)
  }
}

ss_plot_flows <- function(stochasim, from) {
  nsim <- stochasim$nsim
  ss_flows <- ss_flows(stochasim)
  plot(from:nsim, ss_flows[from:nsim], xlab = "Year", ylab = "Flow", type = "l",
       col = "#1639FF")
}

ss_plot_widths <- function(stochasim, from) {
  nsim <- stochasim$nsim
  w <- ss_widths(stochasim)
  plot(from:nsim, w[from:nsim + 1], xlab = "Year", ylab = "Width", type = "l",
       col = "#1639FF")
}