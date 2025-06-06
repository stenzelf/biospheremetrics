% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/roc.R
\name{calc_roc_data}
\alias{calc_roc_data}
\title{Calculate ROC curve data}
\usage{
calc_roc_data(
  external_binary,
  internal_continuous,
  external_name,
  cellArea,
  range_internal,
  sampling_res = 0.01
)
}
\arguments{
\item{external_binary}{array with binary transgression value: 0 - no, 1 - yes}

\item{internal_continuous}{array with continuous values}

\item{external_name}{name of external indicator}

\item{cellArea}{array with area for each cell of internal indicator}

\item{range_internal}{range of internal data, e.g. c(0,1)}

\item{sampling_res}{sampling rate for internal indicator. default: 0.01}
}
\description{
Calculate data for ROC curve comparison between binary external and
continuous internal indicator.
}
