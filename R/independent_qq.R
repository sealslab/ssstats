#' QQ Plots by Group
#'
#' @description Produces QQ plots and histograms for each group in an
#'   independent samples analysis, for assessing the normality assumption.
#'
#' @param data A data frame or tibble.
#' @param outcome Unquoted column name for the continuous outcome (numeric).
#' @param grouping Unquoted column name for the grouping variable. Must have
#'   exactly two levels.
#'
#' @return A \code{ggpubr} arranged plot object containing one QQ plot and
#'   one histogram per group level.
#'
#' @export
#' @importFrom rlang enquo as_name `!!`
#' @importFrom dplyr filter pull
#' @importFrom ggplot2 ggplot aes stat_qq_line stat_qq theme_bw labs geom_histogram
#' @importFrom ggpubr ggarrange
#' @importFrom magrittr %>%
independent_qq <- function(data, outcome, grouping) {
  # Capture variables using tidy evaluation
  variable_q <- rlang::enquo(outcome)
  group_q    <- rlang::enquo(grouping)
  
  # Extract column names as characters for later labeling
  variable_name <- rlang::as_name(variable_q)
  group_name    <- rlang::as_name(group_q)
  
  # Filter out missing group values and get group levels
  levels_group <- data %>%
    dplyr::filter(!is.na(!!group_q)) %>%
    dplyr::pull(!!group_q) %>%
    unique()
  
  if (length(levels_group) != 2) {
    stop(paste("Grouping variable must have exactly 2 levels. Found:", paste(levels_group, collapse = ", ")))
  }
  
  # Create QQ plots
  plots <- lapply(levels_group, function(level_val) {
    ggplot2::ggplot(dplyr::filter(data, !!group_q == level_val),
                    ggplot2::aes(sample = !!variable_q)) +
      ggplot2::stat_qq_line(linetype = "dashed", color = "black", linewidth = 1) +
      ggplot2::stat_qq(color = "#666666") +
      ggplot2::theme_bw() +
      ggplot2::labs(title = paste("QQ Plot for", variable_name, "|", group_name, "=", level_val),
                    x = "Theoretical Quantiles", y = "Sample Quantiles")
  })
  
  # Create histograms
  plots2 <- lapply(levels_group, function(level_val) {
    ggplot2::ggplot(dplyr::filter(data, !!group_q == level_val),
                    ggplot2::aes(x = !!variable_q)) +
      ggplot2::geom_histogram(color = "black", fill = "#666666", bins = 30) +
      ggplot2::theme_bw() +
      ggplot2::labs(title = paste("Histogram for", variable_name, "|", group_name, "=", level_val),
                    x = variable_name, y = "Count")
  })
  
  # Combine and arrange plots
  ggpubr::ggarrange(plotlist = c(plots, plots2))
}
