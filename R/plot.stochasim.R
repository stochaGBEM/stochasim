#' Plotting a stochasim object
#'
#' @param x Stochasim object, as a result of running `stochasim()` or
#' `stochasim2()`.
#' @param ... Other arguments to pass to the default `plot()` function.
#' @param what What to plot from the stochasim object. One of `"widths"`,
#' for the evolution of channel width, or `"flows"`, for peak flows over time.
#' @param from Iteration number to begin plotting from.
#' @exportS3Method graphics::plot
plot.stochasim <- function(x, ..., what = c("widths", "flows"), from = 0) {
  what <- match.arg(what)
  if (what == "widths") {
    ss_plot_widths(x, from = from, ...)
  }
  if (what == "flows") {
    ss_plot_flows(x, from = from, ...)
  }
}

#' Plot flows from stochasim object
#' @param stochasim Stochasim object, as a result of running `stochasim()` or
#' `stochasim2()`.
#' @importFrom graphics par
#' @inheritParams plot.stochasim
ss_plot_flows <- function(stochasim, from, ...) {
  nsim <- stochasim$nsim
  from <- max(from, 1)
  ss_flows <- ss_flows(stochasim)
  if (!is.list(ss_flows)) {
    ylim <- range(ss_flows)
    ss_flows <- list(x = ss_flows)
  } else {
    par(mfrow = c(1, 2))
    ylim <- range(unlist(ss_flows))
  }
  plot(from:nsim, ss_flows$x[from:nsim], xlab = "Year", ylab = "Flow",
       type = "l", col = "#1639FF", ylim = ylim, ...)
  if (!is.null(stochasim$y)) {
    plot(from:nsim, ss_flows$y[from:nsim], xlab = "Year", ylab = "Flow",
         type = "l", col = "orange3", ylim = ylim, ...)
  }
  par(mfrow = c(1, 1))
}

#' Plot width evolution from stochasim object
#' @param stochasim Stochasim object, as a result of running `stochasim()` or
#' `stochasim2()`.
#' @inheritParams plot.stochasim
ss_plot_widths <- function(stochasim, from, ...) {
  nsim <- stochasim$nsim
  w <- ss_widths(stochasim)
  plot(from:nsim, w[from:nsim + 1], xlab = "Year", ylab = "Width", type = "l",
       col = "#1639FF", ...)
}
