#' @rdname stochasim
#' @export
stochasim2 <- function(x, y, cross_section, rv_rate = c(0, 0.1), niter = 300,
                       progress = FALSE) {
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
  if (progress) cli::cli_progress_bar("Iterating", total = length(x))
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
    if (progress) cli::cli_progress_update()
  }
  if (progress) cli::cli_progress_done()
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
