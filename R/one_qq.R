#' QQ Plot and Histogram for a Single Variable
#'
#' @description Produces a normal QQ plot alongside a histogram for a single numeric
#'   variable, useful for visually assessing the normality assumption. Points are
#'   plotted against theoretical quantiles with a dashed reference line, paired with
#'   a histogram of the raw values.
#'
#' @param data A dataframe or tibble.
#' @param outcome Unquoted column name for the numeric variable to plot.
#'
#' @return A combined \code{ggplot}/\code{ggarrange} object.
#'
#' @export
#' @importFrom ggplot2 ggplot aes stat_qq stat_qq_line geom_histogram theme_bw labs
#' @importFrom rlang enquo as_name `!!`
#' @importFrom ggpubr ggarrange
one_qq <- function(data, 
                   outcome) {
  var_q   <- rlang::enquo(outcome)
  var_chr <- rlang::as_name(var_q)
  
  qq_plot <- ggplot2::ggplot(data, ggplot2::aes(sample = !!var_q)) +
    ggplot2::stat_qq_line(linetype = "dashed", color = "black", linewidth = 1) +
    ggplot2::stat_qq(color = "#6A6C6E") +
    ggplot2::theme_bw(base_size = 16) +
    ggplot2::labs(x = "Theoretical Quantiles", y = "Sample Quantiles",
                  title = paste("QQ Plot for", var_chr))
  
  hist_plot <- ggplot2::ggplot(data, ggplot2::aes(x = !!var_q)) +
    ggplot2::geom_histogram(bins = 10, fill = "gray70", color = "black") +
    ggplot2::theme_bw(base_size = 16) +
    ggplot2::labs(x = var_chr, y = "Num. Observations",
                  title = paste("Histogram for", var_chr))
  
  ggpubr::ggarrange(qq_plot, hist_plot, ncol = 2)
}