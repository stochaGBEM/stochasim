#' Stochasim Algorithm
#'
#' @param hydist Hydrograph Distribution
#' @param cross_section A `"cross_section"` object representing a stream's
#' cross section.
#' @param rv_rate Rate parameter for revegetation, passed to `revegetate()`.
#' @param nsim Number of event hydrographs to run; positive integer.
#' @param niter Number of iterations for `gbem::gbem()` when running
#' each hydrograph.
#' @returns A stochasim object, containing all the event hydrographs, and
#' cross sections.
#' @examples
#' # Start with a cross section
#' cs <- cross_section(3, grad = 0.01, d50 = 0.1, d84 = 0.5, roughness = 0.01)
#'
#' # Make 1000 rain hydrographs and run stochasim
#' library(distionary)
#' dst_rain <- hydist_rain(dst_gev(1000, 3, 0.1), baseflow = 50)
#' rain <- realise(dst_rain, n = 1000)
#' ss <- stochasim(hydrographs, cross_section = cs)
#' plot(ss)
#' plot(ss, "flows")
#'
#' # Make 1000 snow hydrographs and run stochasim on both hydrographs.
#' dst_snow <- hydist_snow(dst_norm(1000, 10^2), baseflow = 50)
#' snow <- realise(dst_snow, n = 1000)
#' ss2 <- stochasim2(snow, rain, cross_section = cs)
#' plot(ss2)
#' @rdname stochasim
#' @export
stochasim2 <- function(x, y, cross_section, rv_rate = c(0, 0.1), niter = 300) {
  if (is_hydrograph(x)) x <- list(x)
  if (is_hydrograph(y)) y <- list(y)
  if (length(rv_rate) == 1) rv_rate <- c(rv_rate, rv_rate)
  if (length(rv_rate) != 2) {
    stop("Revegetation rate must be a length 1 or 2 vector. Received length ",
         length(rv_rate), ".")
  }
  xy <- vctrs::vec_recycle_common(x, y)
  x <- xy[[1]]
  y <- xy[[2]]
  Q_base_x <- mean(vapply(x, \(h) h(0), FUN.VALUE = numeric(1L)))
  Q_base_y <- mean(vapply(y, \(h) h(0), FUN.VALUE = numeric(1L)))
  Q_base <- min(Q_base_x, Q_base_y)
  #estimate the low flow width based on base flow using std hydraulic geometry eq
  width_base <- 3 * sqrt(Q_base)
  cs <- list(cross_section)
  for (i in seq_along(x)) {
    gx <- gbem(x[[i]], cross_section = cs[[i]], niter = niter)
    csx <- erode(gx)
    if (ch_width(csx) == ch_width(cs[[i]])) {
      csx <- revegetate(csx, width_base, rate = rv_rate[1])
    }
    gy <- gbem(y[[i]], cross_section = csx, niter = niter)
    csy <- erode(gy)
    if (ch_width(csy) == ch_width(csx)) {
      csy <- revegetate(csy, width_base, rate = rv_rate[2])
    }
    cs[[i + 1]] <- csy
  }
  res <- list(
    x = x,
    y = y,
    cross_sections = cs,
    nsim = length(x),
    niter = niter,
    rv_rate = rv_rate
  )
  new_stochasim(res)
}
