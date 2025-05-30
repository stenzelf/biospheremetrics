% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_ecorisk.R
\name{plot_ecorisk_maps}
\alias{plot_ecorisk_maps}
\title{Plot EcoRisk maps}
\usage{
plot_ecorisk_maps(ecorisk, out_folder, year = 1)
}
\arguments{
\item{ecorisk}{EcoRisk object e.g. returned from calc_ecorisk}

\item{out_folder}{folder to plot the data into}

\item{year}{which year to plot, supply either as index, or character string
of year (default = 1)}
}
\description{
Function to plot with one command maps of all components of EcoRisk to files
}
\examples{
\dontrun{
plot_ecorisk_maps(
  ecorisk = ecorisk,
  out_folder = "./plots/ecorisk/"
)
}

}
