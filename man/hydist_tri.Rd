% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hydist_tri.R
\name{hydist_tri}
\alias{hydist_tri}
\title{Triangular Hydrograph Distribution}
\usage{
hydist_tri(dst_peak, dst_baseflow, dst_when, duration = 1)
}
\arguments{
\item{dst_peak, dst_baseflow}{Distributions of the peak and baseflow
discharges. Baseflow distribution is possibly modified to ensure baseflow
realizations are smaller than the peak discharge.}

\item{dst_when}{Distribution of the timing of the peak; must return
values between 0 and 1.}

\item{duration}{How long is the event? Single positive numeric.}
}
\value{
Hydrograph distribution returning triangular hydrographs.
}
\description{
Triangular Hydrograph Distribution
}
\examples{
library(distionary)
h <- hydist_tri(
   dst_peak = dst_gev(100, 3, 0.1),
   dst_baseflow = dst_norm(50, 10^2),
   dst_when = dst_beta(20, 50),
   duration = 3 * 24
)
set.seed(42)
plot(h, n = 1000, ylim = c(30, 150))
plot(h, n = 1000, add = TRUE)
}
