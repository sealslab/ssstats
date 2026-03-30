#' Check ANOVA Assumptions
#'
#' Fits a one-way or two-way ANOVA model and returns a combined diagnostic plot
#' for checking model assumptions. The output includes QQ plots of residuals
#' faceted by group (or treatment cell for two-way models) and a residuals vs.
#' fitted values plot. For two-way models, the interaction term is included by
#' default but can be suppressed via the \code{interaction} argument.
#'
#' @param data Dataframe or tibble.
#' @param continuous Unquoted column name for continuous outcome.
#' @param A Unquoted column name for grouping variable (factor).
#' @param B Unquoted column name for grouping variable (factor).
#' @param interaction Indicator for inclusion of A:B interaction.

#' @export
#' @importFrom ggplot2 ggplot aes stat_qq stat_qq_line facet_wrap vars theme_bw labs geom_point geom_hline
#' @import patchwork
#' @importFrom magrittr %>%
#' @importFrom rlang enquo as_name quo_is_null sym `!!`
#' @importFrom dplyr select filter mutate
#' @importFrom stats aov as.formula fitted residuals
#' @importFrom glue glue

anova_check <- function(data, continuous, A, B = NULL, interaction = TRUE) {

  y_q <- rlang::enquo(continuous)
  A_q <- rlang::enquo(A)
  B_q <- rlang::enquo(B)

  y_chr <- rlang::as_name(y_q)
  A_chr <- rlang::as_name(A_q)

  two_way <- !rlang::quo_is_null(B_q)
  if (two_way) B_chr <- rlang::as_name(B_q)

  # Clean data
  if (two_way) {
    df <- data %>%
      dplyr::select(!!A_q, !!B_q, !!y_q) %>%
      dplyr::filter(!is.na(!!A_q), !is.na(!!B_q), !is.na(!!y_q)) %>%
      dplyr::mutate(!!A_chr := as.factor(!!rlang::sym(A_chr)),
                    !!B_chr := as.factor(!!rlang::sym(B_chr)))
  } else {
    df <- data %>%
      dplyr::select(!!A_q, !!y_q) %>%
      dplyr::filter(!is.na(!!A_q), !is.na(!!y_q)) %>%
      dplyr::mutate(!!A_q := as.factor(!!A_q))
  }

  # Build formula and fit model
  rhs   <- if (two_way) {
    if (interaction) paste(A_chr, "*", B_chr) else paste(A_chr, "+", B_chr)
  } else A_chr
  model <- stats::aov(stats::as.formula(paste(y_chr, "~", rhs)), data = df)

  model_df <- df %>%
    dplyr::mutate(fitted = stats::fitted(model), residuals = stats::residuals(model))

  # QQ plot
  if (two_way) {
    model_df <- model_df %>%
      dplyr::mutate(AB_cell = interaction(!!rlang::sym(A_chr), !!rlang::sym(B_chr), sep = " : "))
    qq_facet <- ggplot2::vars(AB_cell)
    qq_title <- "QQ Plots of Residuals by Treatment Group"
  } else {
    qq_facet <- ggplot2::vars(!!rlang::sym(A_chr))
    qq_title <- "QQ Plots of Residuals by Group"
  }

  qq_plot <- model_df %>%
    ggplot2::ggplot(ggplot2::aes(sample = residuals)) +
    ggplot2::stat_qq() + ggplot2::stat_qq_line() +
    ggplot2::facet_wrap(qq_facet) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = qq_title, y = "Sample Quantiles", x = "Theoretical Quantiles")

  # Residuals vs Fitted plot
  rvf_base <- model_df %>%
    ggplot2::ggplot(ggplot2::aes(x = fitted, y = residuals))

  rvf_plot <- if (two_way) {
    rvf_base +
      ggplot2::aes(colour = !!rlang::sym(A_chr), shape = !!rlang::sym(B_chr)) +
      ggplot2::geom_point(alpha = 0.7, size = 2) +
      ggplot2::labs(colour = A_chr, shape = B_chr)
  } else {
    rvf_base + ggplot2::geom_point()
  }

  rvf_plot <- rvf_plot +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals")

  # Combine
  (qq_plot / rvf_plot) +
    patchwork::plot_annotation(
      title = glue::glue("{if (two_way) 'Two-Way' else 'One-Way'} ANOVA Assumptions")
    )
}
