#' Plot Individual Time Series
#'
#' This function generates individual time series plots for each selected variable.
#'
#' @param data A data frame containing your time series data (without a date column).
#' @param vars A character vector of time series variable names to plot individually.
#' @param freq Character. The frequency of the data: "monthly", "quarterly", or "annual". Defaults to "monthly".
#' @param start_date A string representing the first date (e.g., "2000-01-01"). Defaults to "2000-01-01".
#'
#' @return A named list of ggplot objects, one per specified variable.
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a sample data set
#' set.seed(123)
#' mydata <- data.frame(
#'   bioRxiv         = cumsum(rnorm(24, mean = 10, sd = 5)),
#'   arXiv_q_bio     = cumsum(rnorm(24, mean = 8, sd = 3)),
#'   PeerJ_Preprints = cumsum(rnorm(24, mean = 6, sd = 2))
#' )
#'
#' # Generate individual time series plots
#' plots <- tsplot_ind(
#'   data       = mydata,
#'   vars       = c("bioRxiv", "arXiv_q_bio", "PeerJ_Preprints"),
#'   freq       = "monthly",
#'   start_date = "2014-01-01"
#' )
#'
#' # Display each plot
#' for (p in plots) {
#'   print(p)
#' }
#' }
tsplot_ind <- function(data, vars, freq = "monthly", start_date = "2000-01-01") {
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

  # Initialize a list to store individual plots
  plot_list <- list()

  # Loop through each specified variable
  for (var in vars) {
    # Check if the variable exists in the data
    if (!var %in% names(data)) {
      warning(paste("Variable", var, "not found in the data. Skipping."))
      next
    }

    # Create a temporary data frame for the current variable
    temp_df <- data.frame(date = data$date, value = data[[var]])

    # Create the individual time series plot
    p <- ggplot2::ggplot(temp_df, ggplot2::aes(x = date, y = value)) +
      ggplot2::geom_line(size = 1, color = "blue") +
      ggplot2::labs(
        title = paste("Time Series Plot:", var),
        x     = "Date",
        y     = var
      ) +
      ggthemes::theme_stata() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
      )

    # Store the plot in the list using the variable name as key
    plot_list[[var]] <- p
  }

  return(plot_list)
}
