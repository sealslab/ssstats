#' Linear Regression Table
#'
#' @description Fits a linear regression model and returns a formatted table of
#'   coefficients, confidence intervals, and p-values.
#'
#' @param data A data frame or tibble.
#' @param outcome Unquoted column name for the numeric outcome variable.
#' @param function_of An unquoted expression for the right-hand side of the
#'   model formula (e.g., \code{x1 + x2}).
#' @param confidence Numeric confidence level for the CIs. Defaults to
#'   \code{0.95}.
#'
#' @return A tibble with columns \code{Term}, \eqn{\beta} (CI), and
#'   \code{p-value}.
#'
#' @export
#' @importFrom rlang enexpr as_name `!!`
#' @importFrom broom tidy
#' @importFrom dplyr mutate select
#' @importFrom stats lm
#' @importFrom magrittr %>%
linear_regression <- function(data, outcome, function_of, confidence = 0.95) {
  # Capture expressions
  outcome_expr <- rlang::enexpr(outcome)
  rhs_expr     <- substitute(function_of)

  # Construct formula
  fmla <- as.formula(paste0(rlang::as_name(outcome_expr), " ~ ", deparse(rhs_expr)))

  # Fit model
  model <- stats::lm(fmla, data = data)

  # Dynamic CI column label
  ci_label <- paste0("\u03b2 (", round(confidence * 100), "% CI)")

  # Helper to combine estimate and CI into one string
  format_beta_ci <- function(estimate, conf.low, conf.high) {
    paste0(sprintf("%.2f", estimate), " (",
           sprintf("%.2f", conf.low), ", ",
           sprintf("%.2f", conf.high), ")")
  }

  # Extract and format output
  broom::tidy(model, conf.int = TRUE, conf.level = confidence) %>%
    dplyr::mutate(
      beta_ci = format_beta_ci(estimate, conf.low, conf.high),
      p_value = format.pval(p.value, digits = 3, eps = 0.001)
    ) %>%
    dplyr::select(
      Term    = term,
      !!ci_label := beta_ci,
      `p-value` = p_value
    )
}
