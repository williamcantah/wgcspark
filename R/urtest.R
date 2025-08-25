#' Run Stationarity Tests on All Numeric Variables
#'
#' Applies the Augmented Dickey–Fuller (ADF), Phillips–Perron (PP),
#' and Kwiatkowski–Phillips–Schmidt–Shin (KPSS) tests to each numeric
#' column in a data frame, handling missing values and small sample sizes.
#'
#' @param df A `data.frame` containing one or more numeric time‐series columns.
#'
#' @return A `data.frame` with one row per test per variable, containing:
#'   \itemize{
#'     \item `Variable` – column name
#'     \item `Observations` – number of non‐NA obs.
#'     \item `Test` – `"ADF"`, `"PP"`, `"KPSS"` or `"Insufficient Data"`
#'     \item `Statistic` – test statistic
#'     \item `P_Value` – approximate p‐value
#'     \item `Critical_Value_1pct`, `Critical_Value_5pct`, `Critical_Value_10pct`
#'     \item `Decision` – `"A"` (stationary at 1%), `"B"` (5%), `"C"` (10%), or `"NS"`
#'   }
#'
#' @details
#' Observations fewer than 10 are flagged as `"Insufficient Data"`.
#' The function uses fully‐qualified namespace calls, so you do not need
#' to `library(urca)` or `library(tseries)` before running it.
#'
#' @examples
#' \dontrun{
#' library(stationarityTestR)
#' df <- data.frame(
#'   x = cumsum(rnorm(50)),
#'   y = arima.sim(list(ar = 0.5), n = 50)
#' )
#' stationarity_test(df)
#' }
#'
#' @export
urtests <- function(df) {
  # Filter numeric variables only
  numeric_vars <- df[, sapply(df, is.numeric)]
  all_results <- list()

  run_tests_for_variable <- function(series, var_name) {
    series <- stats::na.omit(series)
    num_obs <- length(series)

    if (num_obs < 10) {
      return(data.frame(
        Variable              = var_name,
        Observations          = num_obs,
        Test                  = "Insufficient Data",
        Statistic             = NA,
        P_Value               = NA,
        Critical_Value_1pct   = NA,
        Critical_Value_5pct   = NA,
        Critical_Value_10pct  = NA,
        Decision              = "NA",
        stringsAsFactors      = FALSE
      ))
    }

    # ADF Test
    adf_test <- urca::ur.df(series, type = "drift", selectlags = "BIC")
    adf_stat <- round(adf_test@teststat[1], 4)
    adf_cv   <- round(adf_test@cval[1, ], 4)
    adf_p    <- round(urca::punitroot(adf_stat, N = num_obs, trend = "c"), 4)

    # PP Test
    pp_test <- urca::ur.pp(series, type = "Z-tau", model = "constant", lags = "long")
    pp_stat <- round(pp_test@teststat[1], 4)
    pp_cv   <- round(pp_test@cval[1, ], 4)
    pp_p    <- round(urca::punitroot(pp_stat, N = num_obs, trend = "c"), 4)

    # KPSS Test
    opt_lags <- max(1, floor(12 * (num_obs / 100)^(1 / 4)))
    kpss_test <- tryCatch(
      urca::ur.kpss(series, type = "tau", lags = opt_lags),
      error = function(e) urca::ur.kpss(series, type = "tau", lags = "short")
    )
    kpss_stat <- round(kpss_test@teststat[1], 4)
    kpss_cv   <- round(kpss_test@cval[1, ], 4)
    kpss_p    <- round(tseries::kpss.test(series, null = "Level")$p.value, 4)

    # Decision rule
    get_decision <- function(stat, c1, c5, c10, type) {
      if (stat < c1)      return("A")
      else if (stat < c5) return("B")
      else if (stat < c10)return("C")
      else                return("NS")
    }

    data.frame(
      Variable             = var_name,
      Observations         = num_obs,
      Test                 = c("ADF", "PP", "KPSS"),
      Statistic            = c(adf_stat, pp_stat, kpss_stat),
      P_Value              = c(adf_p, pp_p, kpss_p),
      Critical_Value_1pct  = c(adf_cv["1pct"], pp_cv["1pct"], kpss_cv["1pct"]),
      Critical_Value_5pct  = c(adf_cv["5pct"], pp_cv["5pct"], kpss_cv["5pct"]),
      Critical_Value_10pct = c(adf_cv["10pct"], pp_cv["10pct"], kpss_cv["10pct"]),
      Decision             = c(
        get_decision(adf_stat, adf_cv["1pct"], adf_cv["5pct"], adf_cv["10pct"], "ADF"),
        get_decision(pp_stat, pp_cv["1pct"], pp_cv["5pct"], pp_cv["10pct"], "PP"),
        get_decision(kpss_stat, kpss_cv["1pct"], kpss_cv["5pct"], kpss_cv["10pct"], "KPSS")
      ),
      stringsAsFactors     = FALSE
    )
  }

  for (var in names(numeric_vars)) {
    all_results[[var]] <- run_tests_for_variable(numeric_vars[[var]], var)
  }

  do.call(rbind, all_results)
}
