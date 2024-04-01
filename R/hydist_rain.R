#' Make a Rainfall or Snowmelt-Related Event Hydrograph Distribution
#'
#' @param dst_peak Distionary distribution of the peak. Left-truncated so as
#' to always exceed the baseflow.
#' @param baseflow Baseflow discharge; single numeric.
#' @param duration Length of the event. Defaults to 3 days for
#' rainfall-related events, and 9 days for snowmelt-related events (in hours).
#' @returns A hydrograph distribution for rainfall-related and snowmelt-related
#' events, with a constant baseflow and a stochastic peak discharge.
#' @examples
#' d <- distionary::dst_gev(100, 3, 0.1)
#' set.seed(4)
#' hs <- hydist_snow(d, 50)
#' plot(hs, n = 1000, col = "orange3")
#' hr <- hydist_rain(d, 50)
#' plot(hr, n = 1000, col = "blue3", add = TRUE)
#' @rdname hydist_snowrain
#' @export
hydist_rain <- function(dst_peak, baseflow, duration = 3 * 24) {
  if (is.numeric(dst_peak)) dst_peak <- distionary::dst_degenerate(dst_peak)
  dst_peak <- distplyr::slice_left(dst_peak, baseflow)
  realise <- function(n) {
    if (n == 0) return(list())
    x <- distionary::realise(dst_peak, n)
    if (n == 1) return(gbem::hyd_rain(x, baseflow, duration = duration))
    lapply(x, function(x_) gbem::hyd_rain(x_, baseflow, duration = duration))
  }
  hydist(realise)
}

#' @export
#' @rdname hydist_snowrain
hydist_snow <- function(dst_peak, baseflow, duration = 9 * 24) {
  if (is.numeric(dst_peak)) dst_peak <- distionary::dst_degenerate(dst_peak)
  dst_peak <- distplyr::slice_left(dst_peak, baseflow)
  realise <- function(n) {
    if (n == 0) return(list())
    x <- distionary::realise(dst_peak, n)
    if (n == 1) return(gbem::hyd_snow(x, baseflow, duration = duration))
    lapply(x, function(x_) gbem::hyd_snow(x_, baseflow, duration = duration))
  }
  hydist(realise)
}

#' Triangular Hydrograph Distribution
#'
#' @param dst_peak,dst_baseflow Distributions of the peak and baseflow
#' discharges. Baseflow distribution is possibly modified to ensure baseflow
#' realizations are smaller than the peak discharge.
#' @param dst_when Distribution of the timing of the peak; must return
#' values between 0 and 1.
#' @param duration How long is the event? Single positive numeric.
#' @return Hydrograph distribution returning triangular hydrographs.
#' @examples
#' h <- hydist_tri(
#'    dst_peak = dst_gev(100, 3, 0.1),
#'    dst_baseflow = dst_norm(50, 10^2),
#'    dst_when = dst_beta(20, 50),
#'    duration = 3 * 24
#' )
#' set.seed(42)
#' plot(h, n = 1000, ylim = c(30, 150))
#' plot(h, n = 1000, add = TRUE)
#' @export
hydist_tri <- function(dst_peak, dst_baseflow, dst_when, duration = 1) {
  if (is.numeric(dst_peak)) dst_peak <- distionary::dst_degenerate(dst_peak)
  if (is.numeric(dst_baseflow)) dst_baseflow <- distionary::dst_degenerate(dst_baseflow)
  if (is.numeric(dst_when)) dst_when <- distionary::dst_degenerate(dst_when)
  r <- function(n) {
    if (n == 0) return(list())
    p <- realise(dst_peak, n)
    db <- lapply(p, function(p_) distplyr::slice_right(dst_baseflow, p_))
    b <- vapply(db, realise, FUN.VALUE = numeric(1L))
    t <- realise(dst_when, n)
    if (n == 1) return(gbem::hydrograph(b ~ 0, p ~ t, b ~ 1, unit = duration))
    res <- list()
    for (i in 1:n) {
      res[[i]] <- gbem::hydrograph(b[i] ~ 0, p[i] ~ t[i], b[i] ~ 1, unit = duration)
    }
    res
  }
  hydist(r)
}
