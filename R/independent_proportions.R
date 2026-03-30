#' Two-Sample Independent Proportions Analysis
#'
#' @description Performs a two-sample independent proportions analysis.
#'   Prints a formatted summary including per-group sample proportions, a
#'   two-sided Wald confidence interval, hypotheses, z-test statistic,
#'   p-value, and conclusion. The hypothesis test uses a pooled or unpooled
#'   standard error depending on the `pooled` argument.
#'
#' @param data A data frame or tibble.
#' @param outcome Unquoted column name for the binary outcome variable.
#' @param group Unquoted column name for the grouping variable. Must have
#'   exactly two levels.
#' @param event The value of `outcome` to treat as a success.
#' @param first Character string. The group level to treat as the first term
#'   in the subtraction (i.e., group1 in π[group1] − π[group2]). Defaults to
#'   `NULL`, in which case groups are ordered alphabetically.
#' @param alternative Character string specifying the alternative hypothesis.
#'   One of `"two.sided"` (default), `"two"`, `"less"`, or `"greater"`. The
#'   confidence interval is always two-sided regardless of this setting.
#' @param p Numeric. Hypothesized difference in proportions under the null.
#'   Defaults to `0`.
#' @param alpha Numeric. Significance level between 0 and 1 (exclusive).
#'   Defaults to `0.05`. The confidence interval is computed at
#'   \code{(1 - alpha) * 100\%}.
#' @param pooled Logical. If `TRUE` (default), uses the pooled proportion to
#'   compute the standard error for the hypothesis test. If `FALSE`, uses the
#'   unpooled group proportions.
#'
#' @return Called for its side effect (printed output); returns `NULL`
#'   invisibly.
#'
#' @export
#' @importFrom rlang enquo as_name `!!`
#' @importFrom dplyr pull
#' @importFrom glue glue
independent_proportions <- function(data,
                                    outcome,
                                    group,
                                    event,
                                    first       = NULL,
                                    alternative = "two.sided",
                                    p           = 0,
                                    alpha       = 0.05,
                                    pooled      = TRUE) {

  alternative <- match.arg(alternative, choices = c("two.sided", "two", "less", "greater"))
  if (alternative == "two") alternative <- "two.sided"

  if (!is.numeric(p))
    stop("`p` must be numeric.", call. = FALSE)
  if (!is.numeric(alpha) || alpha <= 0 || alpha >= 1)
    stop("`alpha` must be numeric and between 0 and 1 (exclusive).", call. = FALSE)
  if (!is.logical(pooled))
    stop("`pooled` must be TRUE or FALSE.", call. = FALSE)

  outcome_q <- rlang::enquo(outcome)
  group_q   <- rlang::enquo(group)

  outcome_name <- rlang::as_name(outcome_q)
  group_name   <- rlang::as_name(group_q)

  y <- dplyr::pull(data, !!outcome_q)
  g <- dplyr::pull(data, !!group_q)

  complete  <- stats::complete.cases(y, g)
  n_dropped <- sum(!complete)
  if (n_dropped > 0) {
    message(glue::glue("{n_dropped} incomplete observation(s) removed before analysis."))
  }

  y <- as.character(y[complete])
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

  event_chr <- as.character(event)
  x1 <- sum(y[g == group1] == event_chr)
  x2 <- sum(y[g == group2] == event_chr)
  n1 <- sum(g == group1)
  n2 <- sum(g == group2)

  p1        <- x1 / n1
  p2        <- x2 / n2
  phat_diff <- p1 - p2
  conf_pct  <- round((1 - alpha) * 100)

  ci_test  <- stats::prop.test(x = c(x1, x2), n = c(n1, n2),
                               conf.level  = 1 - alpha,
                               alternative = "two.sided",
                               correct     = FALSE)
  ci_lower <- round(ci_test$conf.int[1], 4)
  ci_upper <- round(ci_test$conf.int[2], 4)

  # HT: pooled or unpooled SE
  if (pooled) {
    p_pool <- (x1 + x2) / (n1 + n2)
    SE     <- sqrt(p_pool * (1 - p_pool) * (1 / n1 + 1 / n2))
  } else {
    SE <- sqrt((p1 * (1 - p1)) / n1 + (p2 * (1 - p2)) / n2)
  }

  z_stat <- (phat_diff - p) / SE

  p_val <- switch(alternative,
                  "two.sided" = 2 * stats::pnorm(-abs(z_stat)),
                  "greater"   = stats::pnorm(-z_stat),
                  "less"      = stats::pnorm(z_stat)
  )

  null_sym <- null_symbol(alternative)
  alt_sym  <- alt_symbol(alternative)

  p_text <- if (p_val < 0.001) "< 0.001" else sprintf("%.3f", p_val)

  cat(glue::glue("Independent proportions\n\n\n"))
  cat(glue::glue("p\u0302[{group1}] = {round(p1, 4)}\n\n"))
  cat(glue::glue("p\u0302[{group2}] = {round(p2, 4)}\n\n"))
  cat(glue::glue("Point estimate: p\u0302[{group1}] \u2212 p\u0302[{group2}] = {round(phat_diff, 4)}\n\n\n"))
  cat(glue::glue("{conf_pct}% CI for \u03c0[{group1}] \u2212 \u03c0[{group2}]: ({ci_lower}, {ci_upper})\n\n\n"))
  cat(glue::glue("Hypotheses:\n\n"))
  cat(glue::glue("H\u2080: \u03c0[{group1}] \u2212 \u03c0[{group2}] {null_sym} {p}\n\n"))
  cat(glue::glue("H\u2081: \u03c0[{group1}] \u2212 \u03c0[{group2}] {alt_sym} {p}\n\n"))
  cat(glue::glue("Test statistic: z = {round(z_stat, 2)}\n\n"))
  cat(glue::glue("p-value: {p_text}\n\n"))
  conclusion(p_val, alpha)

  invisible(NULL)
}
