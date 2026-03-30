#' Likelihood Ratio Test for GLM
#'
#' @description Performs a likelihood ratio test (LRT) comparing a fitted GLM
#'   to an intercept-only null model, assessing whether the regression line is
#'   statistically significant. Prints a formatted summary including hypotheses,
#'   test statistic, p-value, and conclusion.
#'
#' @param data A data frame or tibble.
#' @param outcome Unquoted column name for the outcome variable.
#' @param function_of The right-hand side of the model formula. Supports any
#'   valid R formula syntax, e.g. \code{x}, \code{x + z}, \code{x * z}.
#' @param family A GLM family object. Defaults to \code{gaussian()}.
#' @param alpha Numeric. Significance level between 0 and 1 (exclusive).
#'   Defaults to \code{0.05}.
#'
#' @return Called for its side effect (printed output); returns \code{NULL}
#'   invisibly.
#'
#' @export
#' @importFrom rlang enquo as_name quo_text parse_expr
#' @importFrom dplyr select all_of
#' @importFrom tidyr drop_na
#' @importFrom glue glue
#' @importFrom stats glm anova
#' @importFrom magrittr %>%
significant_line <- function(data, outcome, function_of, family = gaussian(), alpha = 0.05) {

  if (!is.numeric(alpha) || alpha <= 0 || alpha >= 1)
    stop("`alpha` must be numeric and between 0 and 1 (exclusive).", call. = FALSE)

  y       <- rlang::enquo(outcome)
  y_chr   <- rlang::as_name(y)
  rhs     <- rlang::enquo(function_of)
  rhs_chr <- rlang::quo_text(rhs)

  vars_rhs <- all.vars(rlang::parse_expr(rhs_chr))
  vars_all <- c(y_chr, vars_rhs)

  df <- data %>%
    dplyr::select(dplyr::all_of(vars_all)) %>%
    tidyr::drop_na()

  full_model <- stats::glm(
    formula = as.formula(glue::glue("{y_chr} ~ {rhs_chr}")),
    data    = df,
    family  = family
  )
  null_model <- stats::glm(
    formula = as.formula(glue::glue("{y_chr} ~ 1")),
    data    = df,
    family  = family
  )

  lrt <- stats::anova(null_model, full_model, test = "LRT")

  if (!("Deviance" %in% names(lrt)) || nrow(lrt) < 2)
    stop("LRT failed: no valid deviance results found.", call. = FALSE)

  df_diff  <- lrt$Df[2]
  dev_diff <- round(lrt$Deviance[2], 3)
  p_val    <- lrt$`Pr(>Chi)`[2]
  p_text   <- if (is.na(p_val)) "NA" else if (p_val < 0.001) "< 0.001" else sprintf("%.3f", p_val)

  # --- Output ---
  cat(glue::glue("Significant regression line\n\n\n"))
  cat(glue::glue("Hypotheses:\n\n"))
  cat(glue::glue("H\u2080: \u03b2\u2081 = \u03b2\u2082 = \u22ef = \u03b2\u2096 = 0\n\n"))
  cat(glue::glue("H\u2081: At least one \u03b2\u1d62 \u2260 0\n\n"))
  cat(glue::glue("Test statistic: \u03c7\u00b2({df_diff}) = {dev_diff}\n\n"))
  cat(glue::glue("p-value: {p_text}\n\n"))
  conclusion(p_val, alpha)

  invisible(NULL)
}
