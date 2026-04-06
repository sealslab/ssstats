#' QQ Plot for a Single Variable
#'
#' @description Produces a normal QQ plot for a single numeric variable, useful for visually
#'   assessing the normality assumption. Points are plotted against theoretical
#'   quantiles with a dashed reference line.
#'
#' @param data A dataframe or tibble.
#' @param outcome Unquoted column name for the numeric variable to plot.
#'
#' @return A \code{ggplot} object.
#'
#' @export
#' @importFrom ggplot2 ggplot aes stat_qq stat_qq_line theme_bw labs
#' @importFrom rlang enquo as_name `!!`
one_qq <- function(data, outcome) {
  var_q   <- rlang::enquo(outcome)
  var_chr <- rlang::as_name(var_q)

  ggplot2::ggplot(data, ggplot2::aes(sample = !!var_q)) +
    ggplot2::stat_qq_line(linetype = "dashed", color = "black", linewidth = 1) +
    ggplot2::stat_qq(color = "#6A6C6E") +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "Theoretical Quantiles", y = "Sample Quantiles",
                  title = paste("QQ Plot for", var_chr))
}
