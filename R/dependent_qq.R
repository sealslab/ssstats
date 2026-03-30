#' QQ Plot for Paired Differences
#'
#' @description Produces a QQ plot and histogram of paired differences for
#'   assessing the normality assumption in a dependent (paired) samples analysis.
#'
#' @param data A data frame or tibble.
#' @param col1 Unquoted column name for the first measurement.
#' @param col2 Unquoted column name for the second measurement.
#'
#' @return A \code{ggpubr} arranged plot object containing a QQ plot and
#'   histogram of the paired differences (\code{col1 - col2}).
#'
#' @export
#' @importFrom rlang enquo as_name `!!`
#' @importFrom dplyr mutate select filter
#' @importFrom ggplot2 ggplot aes stat_qq_line stat_qq theme_bw labs geom_histogram
#' @importFrom ggpubr ggarrange
#' @importFrom magrittr %>%
dependent_qq <- function(data, col1, col2) {
  # Capture expressions
  var1_q <- rlang::enquo(col1)
  var2_q <- rlang::enquo(col2)
  
  # Get variable names as strings (for labeling)
  var1_name <- rlang::as_name(var1_q)
  var2_name <- rlang::as_name(var2_q)
  
  # Calculate differences
  diff_df <- data %>%
    dplyr::mutate(diff = !!var1_q - !!var2_q) %>%
    dplyr::select(diff) %>%
    dplyr::filter(!is.na(diff))
  
  # QQ Plot
  qq_plot <- ggplot2::ggplot(diff_df, ggplot2::aes(sample = diff)) +
    ggplot2::stat_qq_line(linetype = "dashed", color = "black", linewidth = 1) +
    ggplot2::stat_qq(color = "#666666") +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title = paste("QQ Plot of Paired Differences:", var1_name, "-", var2_name),
      x = "Theoretical Quantiles",
      y = "Sample Quantiles"
    )
  
  # Histogram
  hist_plot <- ggplot2::ggplot(diff_df, ggplot2::aes(x = diff)) +
    ggplot2::geom_histogram(color = "black", fill = "#666666", bins = 30) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title = paste("Histogram of Paired Differences:", var1_name, "-", var2_name),
      x = "Difference",
      y = "Count"
    )
  
  # Arrange and return both plots
  ggpubr::ggarrange(qq_plot, hist_plot, ncol = 2)
}
