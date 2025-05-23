% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ecorisk.R
\name{read_ecorisk_data}
\alias{read_ecorisk_data}
\title{Read in output data from LPJmL to calculate the ecosystem change metric
EcoRisk. This function is called by the wrapper function (ecorisk_wrapper),
unless you know what you are doing, don't use this function directly.}
\usage{
read_ecorisk_data(
  files_reference,
  files_scenario,
  save_file = NULL,
  time_span_reference,
  time_span_scenario,
  nitrogen,
  debug_mode = FALSE,
  suppress_warnings = TRUE
)
}
\arguments{
\item{files_reference}{folder of reference run}

\item{files_scenario}{folder of scenario run}

\item{save_file}{file to save read in data to (default NULL)}

\item{time_span_reference}{vector of years to use as scenario period}

\item{time_span_scenario}{vector of years to use as scenario period}

\item{nitrogen}{include nitrogen outputs for pools and fluxes into EcoRisk
calculation (default FALSE)}

\item{debug_mode}{write out all nitrogen state variables (default FALSE)}

\item{suppress_warnings}{suppress writing of Warnings, default: TRUE}
}
\value{
list data object containing arrays of state_ref, mean_state_ref,
        state_scen, mean_state_scen, fpc_ref, fpc_scen, bft_ref, bft_scen,
        cft_ref, cft_scen, lat, lon, cell_area
}
\description{
Utility function to read in output data from LPJmL for calculation of EcoRisk
}
