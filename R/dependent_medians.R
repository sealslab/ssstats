#' Wilcoxon Signed-Rank Test
#'
#' @description Performs a Wilcoxon signed-rank test for dependent (paired)
#'   medians. Prints a formatted summary including the number of complete and
#'   effective pairs, individual column medians, a Hodges-Lehmann point
#'   estimate of the median difference with a two-sided confidence interval,
#'   hypotheses, test statistic, p-value, and conclusion.
#'
#'   The Hodges-Lehmann estimate is the pseudomedian of the differences, which
#'   equals the median difference when the differences are symmetrically
#'   distributed — the assumption underlying the signed-rank test.
#'
#' @param data A data frame or tibble.
#' @param col1 Unquoted column name for the first measurement (numeric).
#' @param col2 Unquoted column name for the second measurement (numeric).
#' @param m Numeric. Hypothesized median difference under the null (default = 0).
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
dependent_medians <- function(data,
                              col1,
                              col2,
                              m           = 0,
                              alternative = "two.sided",
                              alpha       = 0.05) {

  alternative <- match.arg(alternative, choices = c("two.sided", "two", "less", "greater"))
  if (alternative == "two") alternative <- "two.sided"

  if (!is.numeric(m))
    stop("`m` must be numeric.", call. = FALSE)
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
  n_eff       <- sum(differences != 0)

  if (n_eff == 0)
    stop("All differences are zero; the Wilcoxon signed-rank test cannot be computed.",
         call. = FALSE)

  med1     <- round(stats::median(x), 4)
  med2     <- round(stats::median(y), 4)
  conf_pct <- round((1 - alpha) * 100)

  # Wilcoxon signed-rank test with Hodges-Lehmann CI (always two-sided for CI)
  wtest_ci <- stats::wilcox.test(x, y,
                                 paired      = TRUE,
                                 mu          = m,
                                 alternative = "two.sided",
                                 conf.int    = TRUE,
                                 conf.level  = 1 - alpha)

  wtest_ht <- stats::wilcox.test(x, y,
                                 paired      = TRUE,
                                 mu          = m,
                                 alternative = alternative,
                                 conf.int    = FALSE)

  hl_est   <- round(wtest_ci$estimate, 4)
  ci_lower <- round(wtest_ci$conf.int[1], 4)
  ci_upper <- round(wtest_ci$conf.int[2], 4)
  t_stat   <- wtest_ht$statistic
  p_val    <- wtest_ht$p.value

  null_sym <- null_symbol(alternative)
  alt_sym  <- alt_symbol(alternative)

  p_text <- if (p_val < 0.001) "< 0.001" else sprintf("%.3f", p_val)

  # --- Output ---
  cat(glue::glue("Paired (dependent) medians\n\n\n"))
  cat(glue::glue("Complete pairs: {n}\n\n"))
  cat(glue::glue("Effective pairs (non-zero differences): {n_eff}\n\n\n"))
  cat(glue::glue("M[{group1}] = {med1}\n\n"))
  cat(glue::glue("M[{group2}] = {med2}\n\n"))
  cat(glue::glue("Hodges-Lehmann estimate of median difference: {hl_est}\n\n\n"))
  cat(glue::glue("{conf_pct}% CI for M[{group1}] \u2212 M[{group2}]: ({ci_lower}, {ci_upper})\n\n\n"))
  cat(glue::glue("Hypotheses:\n\n"))
  cat(glue::glue("H\u2080: M[{group1}] \u2212 M[{group2}] {null_sym} {m}\n\n"))
  cat(glue::glue("H\u2081: M[{group1}] \u2212 M[{group2}] {alt_sym} {m}\n\n"))
  cat(glue::glue("Test statistic: T = {t_stat}\n\n"))
  cat(glue::glue("p-value: {p_text}\n\n"))
  conclusion(p_val, alpha)

  invisible(NULL)
}
