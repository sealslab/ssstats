#' Kruskal-Wallis H Test
#'
#' @description Performs a Kruskal-Wallis H test for differences in location
#'   across two or more independent groups. Prints a formatted summary
#'   including per-group medians and sample sizes, hypotheses, the H test
#'   statistic with degrees of freedom, p-value, and conclusion.
#'
#'   The H statistic is referred to a chi-squared distribution with k - 1
#'   degrees of freedom, where k is the number of groups. Ties are handled
#'   automatically via the standard correction applied by
#'   \code{\link[stats]{kruskal.test}}.
#'
#' @param data A data frame or tibble.
#' @param outcome Unquoted column name for the continuous outcome (numeric).
#' @param group Unquoted column name for the grouping variable. Must have at
#'   least 2 levels.
#' @param alpha Numeric. Significance level between 0 and 1 (exclusive).
#'   Defaults to `0.05`.
#'
#' @return Called for its side effect (printed output); returns `NULL`
#'   invisibly.
#'
#' @export
#' @importFrom rlang enquo as_name `!!`
#' @importFrom dplyr pull
#' @importFrom glue glue
kruskal_wallis <- function(data,
                           outcome,
                           A,
                           alpha = 0.05) {

  if (!is.numeric(alpha) || alpha <= 0 || alpha >= 1)
    stop("`alpha` must be numeric and between 0 and 1 (exclusive).", call. = FALSE)

  outcome_q <- rlang::enquo(outcome)
  group_q   <- rlang::enquo(A)

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
  g <- g[complete]

  # Preserve factor level order if factor; otherwise alphabetical
  if (is.factor(g)) {
    grp_levels <- levels(droplevels(g))
  } else {
    grp_levels <- sort(unique(as.character(g)))
    g <- as.character(g)
  }

  k <- length(grp_levels)

  if (k < 2)
    stop(glue::glue("`{group_name}` must have at least 2 levels."), call. = FALSE)

  grp_n   <- sapply(grp_levels, function(lv) sum(g == lv))
  grp_med <- sapply(grp_levels, function(lv) round(stats::median(y[g == lv]), 4))

  if (any(grp_n < 1))
    stop("Each group must have at least 1 observation.", call. = FALSE)

  # Kruskal-Wallis test
  kt   <- stats::kruskal.test(y ~ factor(g, levels = grp_levels))
  h    <- round(kt$statistic, 3)
  df   <- kt$parameter
  pval <- kt$p.value

  p_text <- if (pval < 0.001) "< 0.001" else sprintf("%.3f", pval)

  # --- Output ---
  cat(glue::glue("Kruskal-Wallis test\n\n\n"))

  for (lv in grp_levels) {
    cat(glue::glue("M[{lv}] = {grp_med[lv]} (n = {grp_n[lv]})\n\n"))
  }

  cat("\n")
  cat(glue::glue("Hypotheses:\n\n"))
  cat(glue::glue(
    "H\u2080: All group distributions are equal (no effect of {group_name})\n\n"
  ))
  cat(glue::glue(
    "H\u2081: At least one group distribution differs in location (effect of {group_name} exists)\n\n"
  ))
  cat(glue::glue("Test statistic: H({df}) = {h}\n\n"))
  cat(glue::glue("p-value: {p_text}\n\n"))
  conclusion(pval, alpha)

  invisible(NULL)
}
