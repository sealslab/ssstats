#' Paired Means Analysis
#'
#' @description Computes a paired confidence interval and performs a paired
#'   t-test for the mean difference between two dependent samples. Prints a
#'   formatted summary including the number of complete pairs, point estimate,
#'   standard deviation of differences, two-sided confidence interval,
#'   hypotheses, test statistic, p-value, and conclusion.
#'
#' @param data A data frame or tibble.
#' @param col1 Unquoted column name for the first measurement (numeric).
#' @param col2 Unquoted column name for the second measurement (numeric).
#' @param alternative Character string specifying the alternative hypothesis.
#'   One of `"two.sided"` (default), `"two"`, `"less"`, or `"greater"`. The
#'   confidence interval is always two-sided regardless of this setting.
#' @param mu Numeric. Hypothesised mean difference under the null. Defaults
#'   to `0`.
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
dependent_means <- function(data,
                            col1,
                            col2,
                            alternative = "two.sided",
                            mu = 0,
                            alpha = 0.05) {

  alternative <- match.arg(alternative, choices = c("two.sided", "two", "less", "greater"))
  if (alternative == "two") alternative <- "two.sided"

  if (!is.numeric(mu))
    stop("`mu` must be numeric.", call. = FALSE)
  if (!is.numeric(alpha) || alpha <= 0 || alpha >= 1)
    stop("`alpha` must be numeric and between 0 and 1 (exclusive).", call. = FALSE)

  col1_q <- rlang::enquo(col1)
  col2_q <- rlang::enquo(col2)

  group1 <- rlang::as_name(col1_q)
  group2 <- rlang::as_name(col2_q)

  x <- dplyr::pull(data, !!col1_q)
  y <- dplyr::pull(data, !!col2_q)

  if (!is.numeric(x)) stop(glue::glue("`{group1}` must be a numeric column."), call. = FALSE)
  if (!is.numeric(y)) stop(glue::glue("`{group2}` must be a numeric column."), call. = FALSE)

  complete  <- stats::complete.cases(x, y)
  n_dropped <- sum(!complete)
  if (n_dropped > 0) {
    message(glue::glue("{n_dropped} incomplete pair(s) removed before analysis."))
  }

  x <- x[complete]
  y <- y[complete]
  n <- length(x)

  if (n < 2)
    stop("At least 2 complete pairs are required.", call. = FALSE)

  differences <- x - y
  mean_diff   <- round(mean(differences), 4)
  sd_diff     <- round(stats::sd(differences), 4)
  conf_pct    <- round((1 - alpha) * 100)

  # CI is always two-sided
  ttest_ci <- stats::t.test(x, y, paired = TRUE,
                            alternative = "two.sided",
                            conf.level  = 1 - alpha)
  ci_lower <- round(ttest_ci$conf.int[1], 4)
  ci_upper <- round(ttest_ci$conf.int[2], 4)

  # HT respects the specified alternative
  ttest_ht <- stats::t.test(x, y, paired = TRUE,
                            alternative = alternative,
                            mu          = mu,
                            conf.level  = 1 - alpha)
  t_stat <- round(ttest_ht$statistic, 3)
  df     <- round(as.numeric(ttest_ht$parameter), 2)
  p_val  <- ttest_ht$p.value

  null_sym <- null_symbol(alternative)
  alt_sym  <- alt_symbol(alternative)

  p_text <- if (p_val < 0.001) "< 0.001" else sprintf("%.3f", p_val)

  cat(glue::glue("Paired (dependent) means\n\n\n"))
  cat(glue::glue("Complete pairs: {n}\n\n"))
  cat(glue::glue("Point estimate: x\u0305[{group1}] \u2212 x\u0305[{group2}] = {mean_diff}\n\n"))
  cat(glue::glue("SD of differences: {sd_diff}\n\n\n"))
  cat(glue::glue("{conf_pct}% CI for \u03bc[{group1}] \u2212 \u03bc[{group2}]: ({ci_lower}, {ci_upper})\n\n\n"))
  cat(glue::glue("Hypotheses:\n\n"))
  cat(glue::glue("H\u2080: \u03bc[{group1}] \u2212 \u03bc[{group2}] {null_sym} {mu}\n\n"))
  cat(glue::glue("H\u2081: \u03bc[{group1}] \u2212 \u03bc[{group2}] {alt_sym} {mu}\n\n"))
  cat(glue::glue("Test statistic: t({df}) = {t_stat}\n\n"))
  cat(glue::glue("p-value: {p_text}\n\n"))
  conclusion(p_val, alpha)

  invisible(NULL)
}
