#' QQ Plots for Correlation Assessment
#'
#' @description Produces side-by-side QQ plots for two numeric variables to
#'   visually assess normality.
#'
#' @param data A data frame or tibble.
#' @param x Unquoted column name of the first numeric variable.
#' @param y Unquoted column name of the second numeric variable.
#'
#' @return A \code{ggplot} object.
#'
#' @export
#' @importFrom rlang enquo as_name `!!`
#' @importFrom dplyr pull select
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes stat_qq stat_qq_line facet_wrap theme_minimal labs
#' @importFrom magrittr %>%
correlation_qq <- function(data, x, y) {

  x_q <- rlang::enquo(x)
  y_q <- rlang::enquo(y)

  x_chr <- rlang::as_name(x_q)
  y_chr <- rlang::as_name(y_q)

  if (!is.numeric(dplyr::pull(data, !!x_q)) || !is.numeric(dplyr::pull(data, !!y_q)))
    stop("Both x and y must be numeric variables.", call. = FALSE)

  df_long <- data %>%
    dplyr::select(x = !!x_q, y = !!y_q) %>%
    tidyr::pivot_longer(cols = dplyr::everything(),
                        names_to  = "variable",
                        values_to = "value")

  ggplot2::ggplot(df_long, ggplot2::aes(sample = value)) +
    ggplot2::stat_qq() +
    ggplot2::stat_qq_line(color = "steelblue") +
    ggplot2::facet_wrap(~variable, scales = "free") +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::labs(title = "QQ Plots for Normality",
                  x     = "Theoretical Quantiles",
                  y     = "Sample Quantiles")
}
