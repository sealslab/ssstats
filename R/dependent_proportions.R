#' McNemar Test for Paired Proportions
#'
#' @description Performs a McNemar test for dependent (paired) proportions.
#'   When the number of discordant pairs (b + c) is >= 25, the normal
#'   approximation (z-test) is used. When b + c < 25, the exact binomial
#'   test is used. Prints a formatted summary including per-column sample
#'   proportions and counts, a two-sided Wald confidence interval for the
#'   difference in marginal proportions, hypotheses, test statistic or
#'   discordant pair counts, p-value, and conclusion.
#'
#' @param data A data frame or tibble.
#' @param col1 Unquoted column name for the first binary measurement.
#' @param col2 Unquoted column name for the second binary measurement.
#' @param event The value of `col1` and `col2` to treat as a success.
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
dependent_proportions <- function(data,
                                  col1,
                                  col2,
                                  event,
                                  alternative = "two.sided",
                                  alpha       = 0.05) {

  alternative <- match.arg(alternative, choices = c("two.sided", "two", "less", "greater"))
  if (alternative == "two") alternative <- "two.sided"

  if (!is.numeric(alpha) || alpha <= 0 || alpha >= 1)
    stop("`alpha` must be numeric and between 0 and 1 (exclusive).", call. = FALSE)

  col1_q <- rlang::enquo(col1)
  col2_q <- rlang::enquo(col2)

  group1 <- rlang::as_name(col1_q)
  group2 <- rlang::as_name(col2_q)

  x <- dplyr::pull(data, !!col1_q)
  y <- dplyr::pull(data, !!col2_q)

  complete  <- stats::complete.cases(x, y)
  n_dropped <- sum(!complete)
  if (n_dropped > 0) {
    message(glue::glue("{n_dropped} incomplete pair(s) removed before analysis."))
  }

  x <- as.character(x[complete])
  y <- as.character(y[complete])
  n <- length(x)

  if (n < 2)
    stop("At least 2 complete pairs are required.", call. = FALSE)

  event_chr <- as.character(event)

  # 2x2 discordant/concordant pair counts
  a  <- sum(x == event_chr & y == event_chr)  # both event     (concordant)
  b  <- sum(x == event_chr & y != event_chr)  # col1=event, col2=not (discordant)
  cc <- sum(x != event_chr & y == event_chr)  # col1=not, col2=event (discordant)
  d  <- sum(x != event_chr & y != event_chr)  # both not event (concordant)

  bc <- b + cc

  if (bc == 0)
    stop("All pairs are concordant (b + c = 0); the McNemar test cannot be computed.",
         call. = FALSE)

  # Marginal proportions and counts
  n1_event <- a + b
  n2_event <- a + cc
  p1  <- n1_event / n
  p2  <- n2_event / n
  p_d <- p1 - p2   # equivalent to (b - cc) / n

  conf_pct <- round((1 - alpha) * 100)

  # CI: Wald for difference in dependent proportions (always two-sided)
  z_crit   <- stats::qnorm(1 - alpha / 2)
  se_ci    <- sqrt((b + cc - (b - cc)^2 / n) / n^2)
  ci_lower <- round(p_d - z_crit * se_ci, 4)
  ci_upper <- round(p_d + z_crit * se_ci, 4)

  # Test: normal approximation when bc >= 25, exact binomial otherwise
  if (bc >= 25) {
    test_label <- "McNemar's z-test (normal approximation)"
    z_stat     <- (b - cc) / sqrt(bc)
    p_val <- switch(alternative,
                    "two.sided" = 2 * stats::pnorm(-abs(z_stat)),
                    "greater"   = stats::pnorm(-z_stat),
                    "less"      = stats::pnorm(z_stat)
    )
  } else {
    test_label <- "McNemar's exact test"
    z_stat     <- NULL
    # b ~ Binomial(b + c, 0.5) under H0: pi_1 = pi_2
    p_val <- switch(alternative,
                    "two.sided" = 2 * stats::pbinom(min(b, cc), bc, 0.5),
                    "greater"   = stats::pbinom(b - 1, bc, 0.5, lower.tail = FALSE),
                    "less"      = stats::pbinom(b, bc, 0.5)
    )
  }

  null_sym <- null_symbol(alternative)
  alt_sym  <- alt_symbol(alternative)

  p_text <- if (p_val < 0.001) "< 0.001" else sprintf("%.3f", p_val)

  # --- Output ---
  cat(glue::glue("Dependent proportions ({test_label})\n\n\n"))
  cat(glue::glue("p\u0302[{group1}] = {round(p1, 4)} ({n1_event}/{n})\n\n"))
  cat(glue::glue("p\u0302[{group2}] = {round(p2, 4)} ({n2_event}/{n})\n\n"))
  cat(glue::glue("Point estimate: p\u0302[{group1}] \u2212 p\u0302[{group2}] = {round(p_d, 4)}\n\n\n"))
  cat(glue::glue("{conf_pct}% CI for \u03c0[{group1}] \u2212 \u03c0[{group2}]: ({ci_lower}, {ci_upper})\n\n\n"))
  cat(glue::glue("Hypotheses:\n\n"))
  cat(glue::glue("H\u2080: \u03c0[{group1}] \u2212 \u03c0[{group2}] {null_sym} 0\n\n"))
  cat(glue::glue("H\u2081: \u03c0[{group1}] \u2212 \u03c0[{group2}] {alt_sym} 0\n\n"))

  if (!is.null(z_stat)) {
    cat(glue::glue("Test statistic: z = {round(z_stat, 2)}\n\n"))
  } else {
    cat(glue::glue("Discordant pairs: b = {b}, c = {cc}\n\n"))
  }

  cat(glue::glue("p-value: {p_text}\n\n"))
  conclusion(p_val, alpha)

  invisible(NULL)
}
