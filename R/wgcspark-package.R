#' wgcspark: A Comprehensive Toolkit for Time Series Analysis and Visualization
#'
#' The **wgcspark** package provides an integrated set of functions for converting,
#' plotting, and analyzing time series data. Designed primarily for applied
#' time series research and exploratory data analysis, the package includes functions
#' that can:
#'
#' - Convert data frame columns to \code{ts} objects with an associated Date column
#'   (see \code{\link{tsconvert}}).
#'
#' - Generate combined or individual time series plots with customizable frequencies
#'   and date sequences (see \code{\link{tsplot}}, \code{\link{tsplot_ind}}, and
#'   \code{\link{tspanel_plot}}).
#'
#' - Compute and visualize autocorrelation (ACF) and partial autocorrelation (PACF)
#'   functions as bar graphs with the Stata theme and critical bounds (see
#'   \code{\link{acfplot}} and \code{\link{pacfplot}}).
#'
#' - Run multiple unit root tests (ADF, KPSS, Phillips–Perron, and NG–Perron) on time series,
#'   report key statistics, and export the output to a Word document for further reporting
#'   and analysis (see \code{\link{urtest}}).
#'
#' Each function is documented with detailed parameter descriptions, usage examples, and
#' return values to facilitate ease-of-use and rapid deployment in time series analyses.
#'
#' @docType package
#' @name wgcspark
NULL
