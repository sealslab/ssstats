#' Profile Plot
#'
#' @description Creates a profile plot (interaction plot) showing the mean of
#'   a continuous outcome across levels of one variable, with separate lines
#'   for each level of a second variable. Useful for visualizing main effects
#'   and interactions in factorial designs.
#'
#' @param data A data frame or tibble.
#' @param continuous Unquoted column name for the continuous outcome (numeric).
#' @param xaxis Unquoted column name for the variable whose levels appear on
#'   the x-axis.
#' @param lines Unquoted column name for the variable whose levels define the
#'   separate lines.
#'
#' @return A \code{ggplot} object.
#'
#' @export
#' @importFrom rlang enquo as_name `!!`
#' @importFrom dplyr pull group_by summarise
#' @importFrom glue glue
#' @importFrom ggplot2 ggplot aes geom_line geom_point labs theme theme_bw element_text
#' @importFrom magrittr %>%


profile_plot <- function(data,
                         outcome,
                         xaxis,
                         lines) {

  y_q <- rlang::enquo(outcome)
  x_q <- rlang::enquo(xaxis)
  l_q <- rlang::enquo(lines)

  y_name <- rlang::as_name(y_q)
  x_name <- rlang::as_name(x_q)
  l_name <- rlang::as_name(l_q)

  y <- dplyr::pull(data, !!y_q)
  x <- dplyr::pull(data, !!x_q)
  l <- dplyr::pull(data, !!l_q)

  if (!is.numeric(y))
    stop(glue::glue("`{y_name}` must be a numeric column."), call. = FALSE)

  complete  <- stats::complete.cases(y, x, l)
  n_dropped <- sum(!complete)
  if (n_dropped > 0) {
    message(glue::glue("{n_dropped} incomplete observation(s) removed before plotting."))
  }

  df_means <- data[complete, ] %>%
    dplyr::group_by(!!x_q, !!l_q) %>%
    dplyr::summarise(mean_y = mean(!!y_q, na.rm = TRUE), .groups = "drop")

  ggplot2::ggplot(df_means, ggplot2::aes(x     = !!x_q,
                                          y     = mean_y,
                                          group = !!l_q,
                                          color = !!l_q)) +
    ggplot2::geom_line(linewidth = 1.1) +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(
      x     = x_name,
      y     = glue::glue("Mean {y_name}"),
      color = l_name
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title   = ggplot2::element_text(size = 16, face = "bold", hjust = 0.5),
      axis.title   = ggplot2::element_text(size = 13),
      legend.title = ggplot2::element_text(size = 12)
    )
}
