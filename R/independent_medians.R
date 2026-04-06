#' Wilcoxon Rank Sum Test
#'
#' @description Performs a Wilcoxon rank sum test (Mann-Whitney U test) for
#'   two independent medians. Prints a formatted summary including per-group
#'   medians, a Hodges-Lehmann point estimate with a two-sided confidence
#'   interval, hypotheses, test statistic, p-value, and conclusion.
#'
#'   The Hodges-Lehmann estimate is the median of all n1 x n2 pairwise
#'   differences (x_i - y_j), which estimates the shift in location between
#'   the two groups under the location-shift model.
#'
#' @param data A data frame or tibble.
#' @param outcome Unquoted column name for the continuous outcome (numeric).
#' @param grouping Unquoted column name for the grouping variable. Must have
#'   exactly two levels.
#' @param first Character string. The group level to treat as the first term
#'   in the subtraction (i.e., group1 in M[group1] - M[group2]). Defaults to
#'   `NULL`, in which case groups are ordered alphabetically.
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
independent_medians <- function(data,
                                outcome,
                                grouping,
                                first       = NULL,
                                m           = 0,
                                alternative = "two.sided",
                                alpha       = 0.05) {

  alternative <- match.arg(alternative, choices = c("two.sided", "two", "less", "greater"))
  if (alternative == "two") alternative <- "two.sided"

  if (!is.numeric(m))
    stop("`m` must be numeric.", call. = FALSE)
  if (!is.numeric(alpha) || alpha <= 0 || alpha >= 1)
    stop("`alpha` must be numeric and between 0 and 1 (exclusive).", call. = FALSE)

  outcome_q <- rlang::enquo(outcome)
  group_q   <- rlang::enquo(grouping)

  outcome_name <- rlang::as_name(outcome_q)

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

  if (length(x1) < 2 || length(x2) < 2)
    stop("Each group must have at least 2 observations.", call. = FALSE)

  med1     <- round(stats::median(x1), 4)
  med2     <- round(stats::median(x2), 4)
  conf_pct <- round((1 - alpha) * 100)

  # CI: Hodges-Lehmann, always two-sided
  wtest_ci <- stats::wilcox.test(x1, x2,
                                 paired      = FALSE,
                                 mu          = m,
                                 alternative = "two.sided",
                                 conf.int    = TRUE,
                                 conf.level  = 1 - alpha)

  # HT: respects specified alternative
  wtest_ht <- stats::wilcox.test(x1, x2,
                                 paired      = FALSE,
                                 mu          = m,
                                 alternative = alternative,
                                 conf.int    = FALSE)

  hl_est   <- round(wtest_ci$estimate, 4)
  ci_lower <- round(wtest_ci$conf.int[1], 4)
  ci_upper <- round(wtest_ci$conf.int[2], 4)
  w_stat   <- wtest_ht$statistic
  p_val    <- wtest_ht$p.value

  null_sym <- null_symbol(alternative)
  alt_sym  <- alt_symbol(alternative)

  p_text <- if (p_val < 0.001) "< 0.001" else sprintf("%.3f", p_val)

  # --- Output ---
  cat(glue::glue("Independent medians\n\n\n"))
  cat(glue::glue("M[{group1}] = {med1}\n\n"))
  cat(glue::glue("M[{group2}] = {med2}\n\n"))
  cat(glue::glue("Hodges-Lehmann estimate of median difference: {hl_est}\n\n\n"))
  cat(glue::glue("{conf_pct}% CI for M[{group1}] \u2212 M[{group2}]: ({ci_lower}, {ci_upper})\n\n\n"))
  cat(glue::glue("Hypotheses:\n\n"))
  cat(glue::glue("H\u2080: M[{group1}] \u2212 M[{group2}] {null_sym} {m}\n\n"))
  cat(glue::glue("H\u2081: M[{group1}] \u2212 M[{group2}] {alt_sym} {m}\n\n"))
  cat(glue::glue("Test statistic: W = {w_stat}\n\n"))
  cat(glue::glue("p-value: {p_text}\n\n"))
  conclusion(p_val, alpha)

  invisible(NULL)
}
