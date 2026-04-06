#' One-Sample Variance Test
#'
#' @description Performs a one-sample chi-squared test for a population
#'   variance. Prints a formatted summary including the sample variance and
#'   standard deviation, a two-sided confidence interval for \eqn{\sigma^2},
#'   hypotheses, chi-squared test statistic, p-value, and conclusion.
#'
#' @param data A data frame or tibble.
#' @param outcome Unquoted column name for the continuous outcome (numeric).
#' @param sigma2 Numeric. Hypothesized variance under the null (default = 1). Must be
#'   strictly greater than 0.
#' @param alternative Character string specifying the alternative hypothesis.
#'   One of \code{"two.sided"} (default), \code{"two"}, \code{"less"}, or
#'   \code{"greater"}. The confidence interval is always two-sided regardless
#'   of this setting.
#' @param alpha Numeric. Significance level between 0 and 1 (exclusive).
#'   Defaults to \code{0.05}. The confidence interval is computed at
#'   \code{(1 - alpha) * 100\%}.
#'
#' @return Called for its side effect (printed output); returns \code{NULL}
#'   invisibly.
#'
#' @export
#' @importFrom rlang enquo as_name `!!`
#' @importFrom dplyr pull
#' @importFrom glue glue
one_variance <- function(data,
                         outcome,
                         sigma2      = 1,
                         alternative = "two.sided",
                         alpha       = 0.05) {

  alternative <- match.arg(alternative, choices = c("two.sided", "two", "less", "greater"))
  if (alternative == "two") alternative <- "two.sided"

  if (!is.numeric(sigma2) || sigma2 <= 0)
    stop("`sigma2` must be numeric and strictly greater than 0.", call. = FALSE)
  if (!is.numeric(alpha) || alpha <= 0 || alpha >= 1)
    stop("`alpha` must be numeric and between 0 and 1 (exclusive).", call. = FALSE)

  outcome_q    <- rlang::enquo(outcome)
  outcome_name <- rlang::as_name(outcome_q)

  x <- dplyr::pull(data, !!outcome_q)

  if (!is.numeric(x))
    stop(glue::glue("`{outcome_name}` must be a numeric column."), call. = FALSE)

  complete  <- stats::complete.cases(x)
  n_dropped <- sum(!complete)
  if (n_dropped > 0) {
    message(glue::glue("{n_dropped} incomplete observation(s) removed before analysis."))
  }

  x <- x[complete]
  n <- length(x)

  if (n < 2)
    stop("At least 2 complete observations are required.", call. = FALSE)

  s2       <- round(stats::var(x), 4)
  s        <- round(stats::sd(x),  4)
  df       <- n - 1
  conf_pct <- round((1 - alpha) * 100)

  # CI: always two-sided, for sigma^2
  ci_lower <- round(df * s2 / stats::qchisq(1 - alpha / 2, df), 4)
  ci_upper <- round(df * s2 / stats::qchisq(alpha / 2,     df), 4)

  # HT: chi-squared statistic
  chi2_stat <- (df * s2) / sigma2

  p_val <- switch(alternative,
                  "two.sided" = 2 * min(stats::pchisq(chi2_stat, df),
                                        stats::pchisq(chi2_stat, df, lower.tail = FALSE)),
                  "greater"   = stats::pchisq(chi2_stat, df, lower.tail = FALSE),
                  "less"      = stats::pchisq(chi2_stat, df)
  )

  null_sym <- null_symbol(alternative)
  alt_sym  <- alt_symbol(alternative)

  p_text <- if (p_val < 0.001) "< 0.001" else sprintf("%.3f", p_val)

  # --- Output ---
  cat(glue::glue("One-sample variance\n\n\n"))
  cat(glue::glue("s\u00b2 = {s2} (s = {s})\n\n\n"))
  cat(glue::glue("{conf_pct}% CI for \u03c3\u00b2: ({ci_lower}, {ci_upper})\n\n\n"))
  cat(glue::glue("Hypotheses:\n\n"))
  cat(glue::glue("H\u2080: \u03c3\u00b2 {null_sym} {sigma2}\n\n"))
  cat(glue::glue("H\u2081: \u03c3\u00b2 {alt_sym} {sigma2}\n\n"))
  cat(glue::glue("Test statistic: \u03c7\u00b2({df}) = {round(chi2_stat, 3)}\n\n"))
  cat(glue::glue("p-value: {p_text}\n\n"))
  conclusion(p_val, alpha)

  invisible(NULL)
}
