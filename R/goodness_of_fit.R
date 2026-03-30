#' Chi-Square Goodness-of-Fit Test
#'
#' @description Performs a chi-square goodness-of-fit test comparing observed
#'   frequencies to expected proportions. Prints a formatted summary including
#'   hypotheses, test statistic, p-value, and conclusion.
#'
#' @param data A data frame or tibble.
#' @param categorical Unquoted column name for the categorical variable.
#' @param expected Named numeric vector of expected proportions, one per
#'   category level. Names must match the observed category labels and must
#'   sum to 1. If \code{NULL} (default), equal proportions are assumed.
#' @param alpha Numeric. Significance level between 0 and 1 (exclusive).
#'   Defaults to \code{0.05}.
#'
#' @return Called for its side effect (printed output); returns \code{NULL}
#'   invisibly.
#'
#' @export
#' @importFrom rlang enquo as_name `!!`
#' @importFrom dplyr filter count arrange
#' @importFrom glue glue
#' @importFrom magrittr %>%
goodness_of_fit <- function(data, categorical, expected = NULL, alpha = 0.05) {

  if (!is.numeric(alpha) || alpha <= 0 || alpha >= 1)
    stop("`alpha` must be numeric and between 0 and 1 (exclusive).", call. = FALSE)

  cat_q <- rlang::enquo(categorical)

  n_before <- nrow(data)

  observed <- data %>%
    dplyr::filter(!is.na(!!cat_q)) %>%
    dplyr::count(category = !!cat_q) %>%
    dplyr::arrange(category)

  n_dropped <- n_before - sum(observed$n)
  if (n_dropped > 0) {
    message(glue::glue("{n_dropped} incomplete observation(s) removed before analysis."))
  }

  observed$category <- as.character(observed$category)

  k <- nrow(observed)

  if (is.null(expected)) {
    expected_props <- rep(1 / k, k)
    names(expected_props) <- observed$category
  } else {
    if (!is.numeric(expected))
      stop("`expected` must be a named numeric vector.", call. = FALSE)
    if (!isTRUE(all.equal(sum(expected), 1)))
      stop("`expected` proportions must sum to 1.", call. = FALSE)
    if (!all(sort(names(expected)) == sort(observed$category)))
      stop("Names of `expected` must match the observed category labels.", call. = FALSE)
    expected_props <- expected[observed$category]
  }

  test   <- suppressWarnings(stats::chisq.test(x = observed$n, p = expected_props))
  df     <- test$parameter
  chi2   <- round(test$statistic, 2)
  p_val  <- test$p.value
  p_text <- if (p_val < 0.001) "< 0.001" else sprintf("%.3f", p_val)

  # --- Output ---
  cat(glue::glue("Chi-square goodness-of-fit test\n\n\n"))
  cat(glue::glue("Hypotheses:\n\n"))
  cat(glue::glue("H\u2080: Observed frequencies match expected proportions\n\n"))
  cat(glue::glue("H\u2081: Observed frequencies do not match expected proportions\n\n"))
  cat(glue::glue("Test statistic: \u03c7\u00b2({df}) = {chi2}\n\n"))
  cat(glue::glue("p-value: {p_text}\n\n"))
  conclusion(p_val, alpha)

  invisible(NULL)
}
