#' Two-Sample F-Test for Equality of Variances
#'
#' @description Performs a two-sample F-test for equality of variances. Prints
#'   a formatted summary including per-group sample variances and standard
#'   deviations, the variance ratio point estimate, a two-sided confidence
#'   interval for \eqn{\sigma^2_1 / \sigma^2_2}, hypotheses, F test statistic,
#'   p-value, and conclusion.
#'
#' @param data A data frame or tibble.
#' @param outcome Unquoted column name for the continuous outcome (numeric).
#' @param A Unquoted column name for the grouping variable. Must have exactly
#'   two levels.
#' @param first Character string. The group level to treat as the first term
#'   in the ratio (i.e., group1 in \eqn{\sigma^2_{group1} / \sigma^2_{group2}}).
#'   Defaults to \code{NULL}, in which case groups are ordered alphabetically.
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
two_variances <- function(data,
                          outcome,
                          A,
                          first       = NULL,
                          alternative = "two.sided",
                          alpha       = 0.05) {

  alternative <- match.arg(alternative, choices = c("two.sided", "two", "less", "greater"))
  if (alternative == "two") alternative <- "two.sided"

  if (!is.numeric(alpha) || alpha <= 0 || alpha >= 1)
    stop("`alpha` must be numeric and between 0 and 1 (exclusive).", call. = FALSE)

  outcome_q <- rlang::enquo(outcome)
  A_q       <- rlang::enquo(A)

  outcome_name <- rlang::as_name(outcome_q)
  A_name       <- rlang::as_name(A_q)

  y <- dplyr::pull(data, !!outcome_q)
  g <- dplyr::pull(data, !!A_q)

  if (!is.numeric(y))
    stop(glue::glue("`{outcome_name}` must be a numeric column."), call. = FALSE)

  complete  <- stats::complete.cases(y, g)
  n_dropped <- sum(!complete)
  if (n_dropped > 0) {
    message(glue::glue("{n_dropped} incomplete observation(s) removed before analysis."))
  }

  y <- y[complete]
  g <- as.character(g[complete])

  group_levels <- unique(g)
  if (length(group_levels) != 2)
    stop("Grouping variable must have exactly two levels. Found: ",
         paste(group_levels, collapse = ", "), call. = FALSE)

  if (is.null(first)) {
    group1 <- sort(group_levels)[1]
    group2 <- sort(group_levels)[2]
  } else {
    if (!first %in% group_levels)
      stop(glue::glue(
        "`first` must be one of: {paste(group_levels, collapse = ', ')}."
      ), call. = FALSE)
    group1 <- first
    group2 <- setdiff(group_levels, first)
  }

  x1 <- y[g == group1]
  x2 <- y[g == group2]

  if (length(x1) < 2 || length(x2) < 2)
    stop("Each group must have at least 2 observations.", call. = FALSE)

  s2_1     <- round(stats::var(x1), 4)
  s2_2     <- round(stats::var(x2), 4)
  s_1      <- round(stats::sd(x1),  4)
  s_2      <- round(stats::sd(x2),  4)
  ratio    <- round(s2_1 / s2_2, 4)
  conf_pct <- round((1 - alpha) * 100)

  # CI: always two-sided, for sigma^2_1 / sigma^2_2
  vtest_ci <- stats::var.test(x1, x2,
                               alternative = "two.sided",
                               conf.level  = 1 - alpha)
  # HT: respects specified alternative
  vtest_ht <- stats::var.test(x1, x2,
                               alternative = alternative,
                               conf.level  = 1 - alpha)

  ci_lower <- round(vtest_ci$conf.int[1], 4)
  ci_upper <- round(vtest_ci$conf.int[2], 4)
  f_stat   <- round(vtest_ht$statistic,   3)
  df1      <- round(vtest_ht$parameter[1])
  df2      <- round(vtest_ht$parameter[2])
  p_val    <- vtest_ht$p.value

  null_sym <- null_symbol(alternative)
  alt_sym  <- alt_symbol(alternative)

  p_text <- if (p_val < 0.001) "< 0.001" else sprintf("%.3f", p_val)

  # --- Output ---
  cat(glue::glue("Two-sample variances\n\n\n"))
  cat(glue::glue("s\u00b2[{group1}] = {s2_1} (s = {s_1})\n\n"))
  cat(glue::glue("s\u00b2[{group2}] = {s2_2} (s = {s_2})\n\n"))
  cat(glue::glue("Ratio: s\u00b2[{group1}] / s\u00b2[{group2}] = {ratio}\n\n\n"))
  cat(glue::glue("{conf_pct}% CI for \u03c3\u00b2[{group1}] / \u03c3\u00b2[{group2}]: ({ci_lower}, {ci_upper})\n\n\n"))
  cat(glue::glue("Hypotheses:\n\n"))
  cat(glue::glue("H\u2080: \u03c3\u00b2[{group1}] / \u03c3\u00b2[{group2}] {null_sym} 1\n\n"))
  cat(glue::glue("H\u2081: \u03c3\u00b2[{group1}] / \u03c3\u00b2[{group2}] {alt_sym} 1\n\n"))
  cat(glue::glue("Test statistic: F({df1}, {df2}) = {f_stat}\n\n"))
  cat(glue::glue("p-value: {p_text}\n\n"))
  conclusion(p_val, alpha)

  invisible(NULL)
}
