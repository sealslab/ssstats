#' One-Sample Proportion 
#'
#' @description Performs a one-sample proportion analysis. Prints a formatted
#'   summary including the sample proportion and count, a two-sided Wald
#'   confidence interval, hypotheses, z-test statistic, p-value, and
#'   conclusion.
#'
#' @param data A data frame or tibble.
#' @param outcome Unquoted column name for the binary outcome variable.
#' @param event The value of `outcome` to treat as a success.
#' @param p Numeric. Hypothesised proportion under the null. Must be between
#'   0 and 1 (exclusive).
#' @param alternative Character string specifying the alternative hypothesis.
#'   One of `"two.sided"` (default), `"two"`, `"less"`, or `"greater"`. The
#'   confidence interval is always two-sided regardless of this setting.
#' @param alpha Numeric. Significance level between 0 and 1 (exclusive).
#'   Defaults to `0.05`. The confidence interval is computed at
#'   `(1 - alpha) * 100 pct`.
#'
#' @return Called for its side effect (printed output); returns `NULL`
#'   invisibly.
#'
#' @export
#' @importFrom rlang enquo as_name `!!`
#' @importFrom dplyr pull
#' @importFrom glue glue
one_proportion <- function(data,
                           outcome,
                           event,
                           p,
                           alternative = "two.sided",
                           alpha       = 0.05) {

  alternative <- match.arg(alternative, choices = c("two.sided", "two", "less", "greater"))
  if (alternative == "two") alternative <- "two.sided"

  if (!is.numeric(p) || p <= 0 || p >= 1)
    stop("`p` must be numeric and between 0 and 1 (exclusive).", call. = FALSE)
  if (!is.numeric(alpha) || alpha <= 0 || alpha >= 1)
    stop("`alpha` must be numeric and between 0 and 1 (exclusive).", call. = FALSE)

  outcome_q    <- rlang::enquo(outcome)
  outcome_name <- rlang::as_name(outcome_q)

  y <- dplyr::pull(data, !!outcome_q)

  complete  <- stats::complete.cases(y)
  n_dropped <- sum(!complete)
  if (n_dropped > 0) {
    message(glue::glue("{n_dropped} incomplete observation(s) removed before analysis."))
  }

  y <- as.character(y[complete])
  n <- length(y)

  if (n < 2)
    stop("At least 2 complete observations are required.", call. = FALSE)

  x    <- sum(y == as.character(event))
  phat <- x / n

  conf_pct <- round((1 - alpha) * 100)

  # CI: Wald (always two-sided)
  z_crit   <- stats::qnorm(1 - alpha / 2)
  se_ci    <- sqrt(phat * (1 - phat) / n)
  ci_lower <- round(phat - z_crit * se_ci, 4)
  ci_upper <- round(phat + z_crit * se_ci, 4)

  # HT: SE uses null hypothesis value p
  se_ht  <- sqrt(p * (1 - p) / n)
  z_stat <- (phat - p) / se_ht

  p_val <- switch(alternative,
                  "two.sided" = 2 * stats::pnorm(-abs(z_stat)),
                  "greater"   = stats::pnorm(-z_stat),
                  "less"      = stats::pnorm(z_stat)
  )

  null_sym <- null_symbol(alternative)
  alt_sym  <- alt_symbol(alternative)

  p_text <- if (p_val < 0.001) "< 0.001" else sprintf("%.3f", p_val)

  # --- Output ---
  cat(glue::glue("One-sample proportion\n\n\n"))
  cat(glue::glue("p\u0302 = {round(phat, 4)} ({x}/{n})\n\n\n"))
  cat(glue::glue("{conf_pct}% CI for \u03c0: ({ci_lower}, {ci_upper})\n\n\n"))
  cat(glue::glue("Hypotheses:\n\n"))
  cat(glue::glue("H\u2080: \u03c0 {null_sym} {p}\n\n"))
  cat(glue::glue("H\u2081: \u03c0 {alt_sym} {p}\n\n"))
  cat(glue::glue("Test statistic: z = {round(z_stat, 2)}\n\n"))
  cat(glue::glue("p-value: {p_text}\n\n"))
  conclusion(p_val, alpha)

  invisible(NULL)
}
