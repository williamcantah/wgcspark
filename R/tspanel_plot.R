#' Plot Time Series Grid with the Stata Theme
#'
#' This function generates colorful time series plots for selected variables in a data frame.
#' It automatically creates a date variable based on a specified frequency (monthly, quarterly, or annual)
#' and arranges each time series in its own facet with a free y-axis using ggthemes' Stata theme.
#'
#' @param data A data.frame containing your time series data (without a date column).
#' @param vars A character vector of column names representing the time series variables to plot.
#' @param freq A character string indicating the frequency of the data. One of \code{"monthly"}, \code{"quarterly"}, or \code{"annual"}.
#' @param start_date A character string representing the starting date in "YYYY-MM-DD" format. Defaults to "2000-01-01".
#'
#' @return A ggplot object representing the time series plots.
#'
#' @examples
#' \dontrun{
#'   # Simulate example data with four time series variables (24 observations each)
#'   set.seed(123)
#'   mydata <- data.frame(
#'     stock_price       = cumsum(rnorm(24, mean = 3, sd = 5)) + 1000,
#'     restaurant_sales  = rpois(24, lambda = 10),
#'     commodity_sales   = rpois(24, lambda = 5),
#'     satisfaction_rate = runif(24, min = 0.7, max = 1.0)
#'   )
#'
#'   # Generate the time series plot with monthly frequency
#'   ts_plot <- tspanel_plot(
#'     data = mydata,
#'     vars = c("stock_price", "restaurant_sales", "commodity_sales", "satisfaction_rate"),
#'     freq = "monthly",
#'     start_date = "2000-01-01"
#'   )
#'   print(ts_plot)
#' }
#'
#' @importFrom ggplot2 ggplot geom_line facet_wrap scale_color_brewer labs theme element_text
#' @importFrom ggthemes theme_stata
#' @importFrom tidyr pivot_longer
#' @importFrom grid unit
#'
#' @export
tspanel_plot <- function(data, vars, freq = "monthly", start_date = "2000-01-01") {
  # Validate the frequency input
  if (!freq %in% c("monthly", "quarterly", "annual")) {
    stop("Frequency must be one of: 'monthly', 'quarterly', or 'annual'.")
  }

  # Convert the start_date to Date class
  start_date <- as.Date(start_date)

  # Automatically generate the date sequence based on the frequency
  if (freq == "monthly") {
    data$date <- seq(start_date, by = "month", length.out = nrow(data))
  } else if (freq == "quarterly") {
    data$date <- seq(start_date, by = "3 months", length.out = nrow(data))
  } else if (freq == "annual") {
    data$date <- seq(start_date, by = "year", length.out = nrow(data))
  }

  # Reshape the data from wide to long format using tidyr::pivot_longer
  data_long <- tidyr::pivot_longer(
    data,
    cols = all_of(vars),
    names_to = "variable",
    values_to = "value"
  )

  # Create the time series plot using ggplot2 and ggthemes' Stata theme.
  p <- ggplot2::ggplot(data_long, ggplot2::aes(x = date, y = value, color = variable)) +
    ggplot2::geom_line(size = 0.8) +
    ggplot2::facet_wrap(~ variable, scales = "free_y", ncol = 2) +
    ggplot2::scale_color_brewer(palette = "Set1") +
    ggplot2::labs(
      title = "Time Series Plots",
      x = "Date",
      y = "Value"
    ) +
    ggthemes::theme_stata() +
    ggplot2::theme(
      panel.spacing = grid::unit(1, "lines"),
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
    )

  return(p)
}
