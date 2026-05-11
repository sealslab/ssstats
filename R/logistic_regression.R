#' Logistic Regression Table
#'
#' @description Fits a logistic regression model and returns a formatted table
#'   of coefficients (or odds ratios), confidence intervals, and p-values.
#'
#' @param data A data frame or tibble.
#' @param outcome Unquoted column name for the binary outcome variable.
#' @param function_of An unquoted expression for the right-hand side of the
#'   model formula (e.g., \code{x1 + x2}).
#' @param alpha Numeric significance level used to compute CIs (CI level =
#'   1 - alpha). Defaults to \code{0.05}.
#' @param exp Logical. If \code{TRUE} (default), estimates and CIs are
#'   exponentiated and the table reports odds ratios. If \code{FALSE}, the
#'   table reports log-odds (\eqn{\beta}) coefficients.
#'
#' @return A tibble with columns \code{Term}, OR or \eqn{\beta} (CI), and
#'   \code{p-value}.
#'
#' @export
#' @importFrom rlang enexpr as_name `!!`
#' @importFrom broom tidy
#' @importFrom dplyr mutate select
#' @importFrom stats glm
#' @importFrom magrittr %>%
logistic_regression <- function(data, 
                                outcome, 
                                function_of, 
                                alpha = 0.05,
                                exp = TRUE) {

  # Capture variables
  outcome_expr <- rlang::enexpr(outcome)
  rhs_expr     <- substitute(function_of)

  # Construct formula
  fmla <- as.formula(paste0(rlang::as_name(outcome_expr), " ~ ", deparse(rhs_expr)))

  # Fit logistic regression
  model <- stats::glm(fmla, data = data, family = binomial)

  confidence <- 1 - alpha

  # Extract tidy output with CIs
  results <- broom::tidy(model, conf.int = TRUE, conf.level = confidence)

  if (exp) {
    ci_label <- paste0("OR (", round(confidence * 100), "% CI)")

    results %>%
      dplyr::mutate(
        estimate  = base::exp(estimate),
        conf.low  = base::exp(conf.low),
        conf.high = base::exp(conf.high),
        est_ci    = paste0(sprintf("%.2f", estimate), " (",
                           sprintf("%.2f", conf.low), ", ",
                           sprintf("%.2f", conf.high), ")"),
        p_value   = format.pval(p.value, digits = 3, eps = 0.001)
      ) %>%
      dplyr::select(
        Term      = term,
        !!ci_label := est_ci,
        `p-value` = p_value
      )
  } else {
    ci_label <- paste0("\u03b2 (", round(confidence * 100), "% CI)")

    results %>%
      dplyr::mutate(
        est_ci  = paste0(sprintf("%.2f", estimate), " (",
                         sprintf("%.2f", conf.low), ", ",
                         sprintf("%.2f", conf.high), ")"),
        p_value = format.pval(p.value, digits = 3, eps = 0.001)
      ) %>%
      dplyr::select(
        Term      = term,
        !!ci_label := est_ci,
        `p-value` = p_value
      )
  }
}
