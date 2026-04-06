#' One-Sample Mean 
#'
#' @description Performs a one-sample mean analysis. Prints a formatted
#'   summary including the sample mean and standard deviation, a two-sided
#'   t-interval confidence interval, hypotheses, t-test statistic, p-value,
#'   and conclusion.
#'
#' @param data A data frame or tibble.
#' @param outcome Unquoted column name for the continuous outcome (numeric).
#' @param mu Numeric. Hypothesized mean under the null (default = 0).
#' @param alternative Character string specifying the alternative hypothesis.
#'   One of `"two.sided"` (default), `"two"`, `"less"`, or `"greater"`. The
#'   confidence interval is always two-sided regardless of this setting.
#' @param alpha Numeric. Significance level between 0 and 1 (exclusive).
#'   Defaults to `0.05`. The confidence interval is computed at
#'   \code{(1 - alpha) * 100\%}.
#'
#' @return Called for its side effect (printed output); returns `NULL`
#'   invisibly.
#'
#' @export
#' @importFrom rlang enquo as_name `!!`
#' @importFrom dplyr pull
#' @importFrom glue glue
one_mean <- function(data,
                     outcome,
                     mu          = 0,
                     alternative = "two.sided",
                     alpha       = 0.05) {

  alternative <- match.arg(alternative, choices = c("two.sided", "two", "less", "greater"))
  if (alternative == "two") alternative <- "two.sided"

  if (!is.numeric(mu))
    stop("`mu` must be numeric.", call. = FALSE)
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

  if (length(x) < 2)
    stop("At least 2 complete observations are required.", call. = FALSE)

  xbar     <- round(mean(x), 4)
  sd_x     <- round(stats::sd(x), 4)
  conf_pct <- round((1 - alpha) * 100)

  # CI: always two-sided
  ttest_ci <- stats::t.test(x,
                            mu          = mu,
                            alternative = "two.sided",
                            conf.level  = 1 - alpha)
  ci_lower <- round(ttest_ci$conf.int[1], 4)
  ci_upper <- round(ttest_ci$conf.int[2], 4)

  # HT: respects specified alternative
  ttest_ht <- stats::t.test(x,
                            mu          = mu,
                            alternative = alternative,
                            conf.level  = 1 - alpha)
  t_stat <- round(ttest_ht$statistic, 3)
  df     <- round(as.numeric(ttest_ht$parameter), 2)
  p_val  <- ttest_ht$p.value

  null_sym <- null_symbol(alternative)
  alt_sym  <- alt_symbol(alternative)

  p_text <- if (p_val < 0.001) "< 0.001" else sprintf("%.3f", p_val)

  # --- Output ---
  cat(glue::glue("One-sample mean\n\n\n"))
  cat(glue::glue("x\u0305 = {xbar} (SD = {sd_x})\n\n\n"))
  cat(glue::glue("{conf_pct}% CI for \u03bc: ({ci_lower}, {ci_upper})\n\n\n"))
  cat(glue::glue("Hypotheses:\n\n"))
  cat(glue::glue("H\u2080: \u03bc {null_sym} {mu}\n\n"))
  cat(glue::glue("H\u2081: \u03bc {alt_sym} {mu}\n\n"))
  cat(glue::glue("Test statistic: t({df}) = {t_stat}\n\n"))
  cat(glue::glue("p-value: {p_text}\n\n"))
  conclusion(p_val, alpha)

  invisible(NULL)
}
