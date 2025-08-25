#' Run Unit Root Tests on Multiple Time Series
#'
#' This function runs several unit root tests (ADF, KPSS, Phillips-Perron, and NG-Perron)
#' on specified time series variables contained in a data frame. It extracts the test statistics,
#' p-values, and the critical values (1%, 5%, and 10%) into separate columns.
#' The results are printed to the console and exported to a Word document using \code{officer::read_docx},
#' \code{officer::body_add_par}, \code{flextable::flextable}, and \code{officer::print}.
#'
#' @param data A data frame containing at least one time series (of class \code{ts}).
#' @param varnames A character vector specifying the column names in \code{data} that are time series.
#'
#' @return Invisibly returns a combined data frame that contains the results for each variable.
#'
#' @examples
#' \dontrun{
#' # Simulate three monthly time series (10 years; 120 observations each)
#' set.seed(123)
#' ts1 <- ts(rnorm(120, mean = 0, sd = 1), start = c(2000, 1), frequency = 12)
#' set.seed(456)
#' ts2 <- ts(rnorm(120, mean = 5, sd = 2), start = c(2000, 1), frequency = 12)
#' set.seed(789)
#' ts3 <- ts(rnorm(120, mean = -3, sd = 1.5), start = c(2000, 1), frequency = 12)
#'
#' # Create a data frame with the simulated time series.
#' monthly <- data.frame(
#'   CPI = ts1,
#'   EXC_END = ts2,
#'   DEPRECIATION = ts3,
#'   stringsAsFactors = FALSE
#' )
#'
#' # Run the unit root tests on the selected variables.
#' results_multi <- urtest(
#'   data     = monthly,
#'   varnames = c("CPI", "EXC_END", "DEPRECIATION")
#' )
#'
#' # Print the combined results.
#' print(results_multi)
#' }
#'
#' @export
urtest <- function(data, varnames) {
  # Check that varnames is a character vector.
  if (!is.character(varnames)) {
    stop("'varnames' must be a character vector of column names.")
  }

  # Verify that the provided variables exist in data.
  missing_vars <- setdiff(varnames, names(data))
  if (length(missing_vars) > 0) {
    stop("These columns are not found in data: ", paste(missing_vars, collapse = ", "))
  }

  # Check if required packages are installed
  required_pkgs <- c("tseries", "urca", "officer", "flextable")
  for(pkg in required_pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE))
      stop(sprintf("Package '%s' is required but not installed.", pkg))
  }

  # Create a new Word document using officer::read_docx.
  doc <- officer::read_docx()
  doc <- officer::body_add_par(doc, "Unit Root Test Results (Multiple Variables)", style = "heading 1")

  # Overall list for storing results for each variable.
  all_results_list <- list()

  # Iterate over each requested time-series variable.
  for (varname in varnames) {
    ts_data <- data[[varname]]

    # Check that the selected variable is a time-series object.
    if (!inherits(ts_data, "ts")) {
      warning(sprintf("'%s' is not a ts object. Skipping this variable.", varname))
      next
    }

    #### 1. ADF Test using urca::ur.df with type = "drift"
    # For type = "drift", the relevant statistic is "tau2".
    adf_ur <- urca::ur.df(ts_data, type = "drift", selectlags = "AIC")
    adf_stat <- adf_ur@teststat["tau2"]
    adf_cvals_vec <- adf_ur@cval["tau2", ]
    adf_1pct <- round(adf_cvals_vec["1pct"], 3)
    adf_5pct <- round(adf_cvals_vec["5pct"], 3)
    adf_10pct <- round(adf_cvals_vec["10pct"], 3)
    # Use tseries::adf.test to obtain a p-value.
    adf_tseries <- tseries::adf.test(ts_data)
    adf_pval <- adf_tseries$p.value

    #### 2. KPSS Test using tseries::kpss.test
    kpss_test <- tseries::kpss.test(ts_data)
    kpss_stat <- as.numeric(kpss_test$statistic)
    kpss_pval <- kpss_test$p.value
    # Set default critical values for KPSS (for level test); these are typical defaults.
    kpss_cvals <- c("1pct" = 0.739, "5pct" = 0.463, "10pct" = 0.347)
    kpss_1pct <- kpss_cvals["1pct"]
    kpss_5pct <- kpss_cvals["5pct"]
    kpss_10pct <- kpss_cvals["10pct"]

    #### 3. Phillips–Perron (PP) Test using urca::ur.pp
    pp_test <- urca::ur.pp(ts_data, type = "Z-tau", model = "constant", lags = "short")
    pp_stat <- pp_test@teststat
    pp_pval <- NA  # p-value is not directly provided.
    pp_cvals_vec <- pp_test@cval
    pp_1pct <- round(pp_cvals_vec["1pct"], 3)
    pp_5pct <- round(pp_cvals_vec["5pct"], 3)
    pp_10pct <- round(pp_cvals_vec["10pct"], 3)

    #### 4. NG–Perron Test using urca::ur.ers
    ng_test <- urca::ur.ers(ts_data, model = "trend", lag.max = 4)
    # Use the first test statistic as representative.
    ng_stat <- ng_test@teststat[1]
    ng_pval <- NA  # p-value not provided.
    ng_cvals_vec <- ng_test@cval[1, ]
    ng_1pct <- round(ng_cvals_vec["1pct"], 3)
    ng_5pct <- round(ng_cvals_vec["5pct"], 3)
    ng_10pct <- round(ng_cvals_vec["10pct"], 3)

    # Create a summary data frame for these results.
    results_df <- data.frame(
      Variable   = rep(varname, 4),
      Test       = c("ADF", "KPSS", "Phillips–Perron", "NG–Perron"),
      Statistic  = c(adf_stat, kpss_stat, pp_stat, ng_stat),
      P_Value    = c(adf_pval, kpss_pval, pp_pval, ng_pval),
      Crit_1pct  = c(adf_1pct, kpss_1pct, pp_1pct, ng_1pct),
      Crit_5pct  = c(adf_5pct, kpss_5pct, pp_5pct, ng_5pct),
      Crit_10pct = c(adf_10pct, kpss_10pct, pp_10pct, ng_10pct),
      stringsAsFactors = FALSE
    )

    # Print results for this variable to the console.
    cat("\nResults for variable:", varname, "\n")
    print(results_df)

    # Append the results to the overall list.
    all_results_list[[varname]] <- results_df

    # Add a section in the Word document for this variable.
    doc <- officer::body_add_par(doc, paste("Results for:", varname), style = "heading 2")
    ft <- flextable::flextable(results_df)
    doc <- officer::body_add_flextable(doc, ft)
  }

  # Save the combined results into one Word document.
  officer::print(doc, target = "UnitRootTestResults_Multiple.docx")

  # Combine individual results into one data frame for returning.
  combined_results <- do.call(rbind, all_results_list)
  invisible(combined_results)
}
