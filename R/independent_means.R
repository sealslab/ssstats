#' Two-Sample Independent Means Analysis
#'
#' @description Performs a two-sample independent means analysis using
#'   Welch's t-test (unequal variances assumed). Prints a formatted summary
#'   including per-group point estimates, a two-sided confidence interval,
#'   hypotheses, test statistic, p-value, and conclusion.
#'
#' @param data A data frame or tibble.
#' @param outcome Unquoted column name for the continuous outcome (numeric).
#' @param group Unquoted column name for the grouping variable. Must have
#'   exactly two levels.
#' @param first Character string. The group level to treat as the first term
#'   in the subtraction (i.e., group1 in μ[group1] − μ[group2]). Defaults to
#'   `NULL`, in which case groups are ordered alphabetically.
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
independent_means <- function(data,
                              outcome,
                              group,
                              first = NULL,
                              alternative = "two.sided",
                              mu = 0,
                              alpha = 0.05) {

  alternative <- match.arg(alternative, choices = c("two.sided", "two", "less", "greater"))
  if (alternative == "two") alternative <- "two.sided"

  if (!is.numeric(mu))
    stop("`mu` must be numeric.", call. = FALSE)
  if (!is.numeric(alpha) || alpha <= 0 || alpha >= 1)
    stop("`alpha` must be numeric and between 0 and 1 (exclusive).", call. = FALSE)

  outcome_q <- rlang::enquo(outcome)
  group_q   <- rlang::enquo(group)

  outcome_name <- rlang::as_name(outcome_q)
  group_name   <- rlang::as_name(group_q)

  y <- dplyr::pull(data, !!outcome_q)
  g <- dplyr::pull(data, !!group_q)

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
  n1 <- length(x1)
  n2 <- length(x2)

  if (n1 < 2 || n2 < 2)
    stop("Each group must have at least 2 observations.", call. = FALSE)

  mean1 <- round(mean(x1), 4)
  mean2 <- round(mean(x2), 4)
  diff  <- round(mean1 - mean2, 4)

  conf_pct <- round((1 - alpha) * 100)

  # Welch's t-test: CI always two-sided; HT uses specified alternative
  ttest_ci <- stats::t.test(x1, x2,
                            var.equal   = FALSE,
                            alternative = "two.sided",
                            conf.level  = 1 - alpha)
  ttest_ht <- stats::t.test(x1, x2,
                            var.equal   = FALSE,
                            alternative = alternative,
                            mu          = mu,
                            conf.level  = 1 - alpha)

  ci_lower <- round(ttest_ci$conf.int[1], 4)
  ci_upper <- round(ttest_ci$conf.int[2], 4)
  t_stat   <- round(ttest_ht$statistic, 3)
  df       <- round(as.numeric(ttest_ht$parameter), 2)
  p_val    <- ttest_ht$p.value

  null_sym <- null_symbol(alternative)
  alt_sym  <- alt_symbol(alternative)

  p_text <- if (p_val < 0.001) "< 0.001" else sprintf("%.3f", p_val)

  # --- Output ---
  cat(glue::glue("Independent means (Welch's t-test)\n\n\n"))
  cat(glue::glue("x\u0305[{group1}] = {mean1}\n\n"))
  cat(glue::glue("x\u0305[{group2}] = {mean2}\n\n"))
  cat(glue::glue("Point estimate: x\u0305[{group1}] \u2212 x\u0305[{group2}] = {diff}\n\n\n"))
  cat(glue::glue("{conf_pct}% CI for \u03bc[{group1}] \u2212 \u03bc[{group2}]: ({ci_lower}, {ci_upper})\n\n\n"))
  cat(glue::glue("Hypotheses:\n\n"))
  cat(glue::glue("H\u2080: \u03bc[{group1}] \u2212 \u03bc[{group2}] {null_sym} {mu}\n\n"))
  cat(glue::glue("H\u2081: \u03bc[{group1}] \u2212 \u03bc[{group2}] {alt_sym} {mu}\n\n"))
  cat(glue::glue("Test statistic: t({df}) = {t_stat}\n\n"))
  cat(glue::glue("p-value: {p_text}\n\n"))
  conclusion(p_val, alpha)

  invisible(NULL)
}
