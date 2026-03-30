#' Brown-Forsythe-Levene Test for Equal Variances
#'
#' @description Performs the Brown-Forsythe-Levene test for equality of
#'   variances across two or more independent groups. Prints a formatted summary
#'   including per-group sample variances and standard deviations, hypotheses,
#'   F test statistic, p-value, and conclusion.
#'
#'   The test computes absolute deviations from each group's median, then
#'   applies a one-way ANOVA to those deviations. Using the median (rather than
#'   the mean as in Levene's original formulation) makes the test robust to
#'   non-normal distributions.
#'
#' @param data A data frame or tibble.
#' @param outcome Unquoted column name for the continuous outcome (numeric).
#' @param A Unquoted column name for the grouping variable. Must have at least
#'   2 levels.
#' @param alpha Numeric. Significance level between 0 and 1 (exclusive).
#'   Defaults to \code{0.05}.
#'
#' @return Called for its side effect (printed output); returns \code{NULL}
#'   invisibly.
#'
#' @export
#' @importFrom rlang enquo as_name `!!`
#' @importFrom dplyr pull
#' @importFrom glue glue
multiple_variances <- function(data,
                               outcome,
                               A,
                               alpha = 0.05) {

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
  g <- g[complete]

  if (is.factor(g)) {
    grp_levels <- levels(droplevels(g))
  } else {
    grp_levels <- sort(unique(as.character(g)))
    g <- as.character(g)
  }

  k <- length(grp_levels)

  if (k < 2)
    stop(glue::glue("`{A_name}` must have at least 2 levels."), call. = FALSE)

  grp_s2 <- sapply(grp_levels, function(lv) round(stats::var(y[g == lv]), 4))
  grp_s  <- sapply(grp_levels, function(lv) round(stats::sd(y[g == lv]),  4))
  grp_n  <- sapply(grp_levels, function(lv) sum(g == lv))

  if (any(grp_n < 2))
    stop("Each group must have at least 2 observations.", call. = FALSE)

  # Brown-Forsythe-Levene: absolute deviations from each group's median
  z <- numeric(length(y))
  for (lv in grp_levels) {
    idx    <- g == lv
    z[idx] <- abs(y[idx] - stats::median(y[idx]))
  }

  data_sub <- data.frame(z = z, g = factor(g, levels = grp_levels))
  fit      <- stats::aov(z ~ g, data = data_sub)
  aov_sum  <- summary(fit)[[1]]

  f_stat <- round(aov_sum[["F value"]][1], 3)
  df1    <- aov_sum[["Df"]][1]
  df2    <- aov_sum[["Df"]][2]
  p_val  <- aov_sum[["Pr(>F)"]][1]

  p_text <- if (p_val < 0.001) "< 0.001" else sprintf("%.3f", p_val)

  # --- Output ---
  cat(glue::glue("Brown-Forsythe-Levene test for equality of variances\n\n\n"))

  for (lv in grp_levels) {
    cat(glue::glue("s\u00b2[{lv}] = {grp_s2[lv]} (s = {grp_s[lv]})\n\n"))
  }

  cat("\n")
  cat(glue::glue("Hypotheses:\n\n"))
  cat(glue::glue(
    "H\u2080: All group variances are equal (no effect of {A_name} on variance)\n\n"
  ))
  cat(glue::glue(
    "H\u2081: At least one group variance differs (effect of {A_name} on variance exists)\n\n"
  ))
  cat(glue::glue("Test statistic: F({df1}, {df2}) = {f_stat}\n\n"))
  cat(glue::glue("p-value: {p_text}\n\n"))
  conclusion(p_val, alpha)

  invisible(NULL)
}
