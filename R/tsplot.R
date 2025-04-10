#' Create a Combined Time Series Plot
#'
#' Generates a single-pane time series plot with multiple variables using a colorful theme.
#'
#' @param data A data.frame containing your time series data (without a date column).
#' @param vars A character vector of time series variable names to plot.
#' @param freq A character string specifying the frequency of the time series. It can be "monthly", "quarterly", or "annual". Default is "monthly".
#' @param start_date A string representing the first date (e.g. "2000-01-01"). Default is "2000-01-01".
#'
#' @return A ggplot object with all time series plotted on a single panel.
#'
#' @examples
#' set.seed(123)
#' mydata <- data.frame(
#'   bioRxiv         = cumsum(rnorm(24, mean = 10, sd = 5)),
#'   arXiv_q_bio     = cumsum(rnorm(24, mean = 8,  sd = 3)),
#'   PeerJ_Preprints = cumsum(rnorm(24, mean = 6,  sd = 2))
#' )
#'
#' ts_combined_plot <- tsplot(
#'   data       = mydata,
#'   vars       = c("bioRxiv", "arXiv_q_bio", "PeerJ_Preprints"),
#'   freq       = "monthly",
#'   start_date = "2014-01-01"
#' )
#'
#' print(ts_combined_plot)
#'
#' @import ggplot2 ggthemes tidyr
#' @export
tsplot <- function(data, vars, freq = "monthly", start_date = "2000-01-01") {
  # Validate the frequency input
  if (!freq %in% c("monthly", "quarterly", "annual")) {
    stop("Frequency must be one of: 'monthly', 'quarterly', or 'annual'.")
  }

  # Convert the start_date to Date class
  start_date <- as.Date(start_date)

  # Create the date sequence based on the frequency
  if (freq == "monthly") {
    data$date <- seq(start_date, by = "month", length.out = nrow(data))
  } else if (freq == "quarterly") {
    data$date <- seq(start_date, by = "3 months", length.out = nrow(data))
  } else if (freq == "annual") {
    data$date <- seq(start_date, by = "year", length.out = nrow(data))
  }

  # Reshape the data from wide to long format
  data_long <- tidyr::pivot_longer(
    data,
    cols = all_of(vars),
    names_to = "variable",
    values_to = "value"
  )

  # Create a combined time series plot
  p <- ggplot2::ggplot(data_long, ggplot2::aes(x = date, y = value, color = variable, group = variable)) +
    ggplot2::geom_line(size = 1) +
    ggplot2::scale_color_brewer(palette = "Set1") +
    ggplot2::labs(
      title = "Time Series Plot (Combined)",
      x     = "Date",
      y     = "Value",
      color = "Series"
    ) +
    ggthemes::theme_stata() +
    ggplot2::theme(
      plot.title   = ggplot2::element_text(hjust = 0.5, face = "bold"),
      legend.title = ggplot2::element_text(face = "bold"),
      legend.text  = ggplot2::element_text(size = 9)
    )

  return(p)
}
