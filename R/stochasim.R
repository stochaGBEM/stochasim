#' Stochasim Algorithm
#'
#' Send multiple annual event hydrograph(s) through a channel, with either
#' bank erosion or revegetation occurring.
#' Erosion is determined by the `gbem::gbem()` algorithm, and revegetation
#' occurs at a specified rate if no erosion occurs, using `revegetate()`.
#'
#' @param x,y Hydrographs, as a list or bare. Recycled to the same length.
#' @param cross_section A `"cross_section"` object representing a stream's
#' cross section.
#' @param rv_rate Rate parameter for revegetation, passed to `revegetate()`.
#' For `stochasim2()`, possibly a vector of length two corresponding to
#' different revegetation rates for hydrographs `x` and `y`. Note: a rate of
#' 0 means no revegetation happens.
#' @param niter Number of iterations for `gbem::gbem()` when running
#' each hydrograph.
#' @param progress Display a progress bar? Logical; defaults to `FALSE`.
#' @returns A stochasim object, containing all the event hydrographs, and
#' cross sections.
#' @details
#' For `stochasim2()`, `x` and `y` are paired and looped through, with
#' the `x` hydrograph occurring before `y` in each iteration.
#'
#' The default interpretation of `x` and `y` are snowmelt-related
#' and rainfall-related event hydrographs (respectively), with each iteration
#' corresponding to a year. This is why the default revegetation rate is
#' `c(0, 0.1)`, so that revegetation only occurs in the fall if no
#' rainfall-related erosion occurs.
#' @examples
#' # Start with a cross section
#' cs <- cross_section(40, grad = 0.01, d50 = 65, d84 = 100, roughness = 0.01)
#'
#' # Make 500 rain hydrographs and run stochasim
#' library(distionary)
#' dst_rain <- hydist_rain(dst_gev(11, 3, 0.1), baseflow = 2)
#' set.seed(42)
#' rain <- realise(dst_rain, n = 500)
#' (ss <- stochasim(rain, cross_section = cs, progress = TRUE))
#'
#' # View evolution of channel width; view peak flows.
#' plot(ss)
#' plot(ss, what = "flows")
#'
#' # Or, make your own plot by extracting the data
#' ss_widths(ss)
#' ss_flows(ss)
#'
#' # Make 500 snow hydrographs and run stochasim on both hydrographs.
#' dst_snow <- hydist_snow(dst_norm(13, 2^2), baseflow = 2)
#' set.seed(43)
#' snow <- realise(dst_snow, n = 500)
#' (ss2 <- stochasim2(snow, rain, cross_section = cs, progress = TRUE))
#'
#' # Plot again.
#' plot(ss2)
#' plot(ss2, what = "flows")
#' ss_widths(ss2)
#' ss_flows(ss2)
#' @rdname stochasim
#' @export
stochasim <- function(x, cross_section, rv_rate = 0.1, niter = 300, progress = FALSE) {
  if (is_hydrograph(x)) x <- list(x)
  if (length(rv_rate) != 1) {
    stop("Revegetation rate must be a length 1 vector. Received length ",
         length(rv_rate), ".")
  }
  Q_base <- mean(vapply(x, \(h) h(0), FUN.VALUE = numeric(1L)))
  #estimate the low flow width based on base flow using std hydraulic geometry eq
  width_base <- 3 * sqrt(Q_base)
  cs <- list(cross_section)
  if (progress) cli::cli_progress_bar("Iterating", total = length(x))
  for (i in seq_along(x)) {
    g <- gbem(x[[i]], cross_section = cs[[i]], niter = niter)
    cs0 <- erode(g)
    if (ch_width(cs0) == ch_width(cs[[i]])) {
      cs0 <- revegetate(cs0, width_base, rate = rv_rate)
    }
    cs[[i + 1]] <- cs0
    if (progress) cli::cli_progress_update()
  }
  if (progress) cli::cli_progress_done()
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
  cat("Stochasim object with", x$nsim, "runs.")
}
