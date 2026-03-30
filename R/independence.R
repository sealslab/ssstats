#' Chi-Square Test of Independence
#'
#' @description Performs a chi-square test of independence between two
#'   categorical variables. Prints a formatted summary including hypotheses,
#'   test statistic, p-value, and conclusion.
#'
#' @param data A data frame or tibble.
#' @param var1 Unquoted column name of the first categorical variable.
#' @param var2 Unquoted column name of the second categorical variable.
#' @param alpha Numeric. Significance level between 0 and 1 (exclusive).
#'   Defaults to \code{0.05}.
#'
#' @return Called for its side effect (printed output); returns \code{NULL}
#'   invisibly.
#'
#' @export
#' @importFrom rlang enquo as_label `!!`
#' @importFrom dplyr filter mutate
#' @importFrom glue glue
#' @importFrom stats chisq.test
#' @importFrom magrittr %>%
independence <- function(data, var1, var2, alpha = 0.05) {

  if (!is.numeric(alpha) || alpha <= 0 || alpha >= 1)
    stop("`alpha` must be numeric and between 0 and 1 (exclusive).", call. = FALSE)

  var1_q <- rlang::enquo(var1)
  var2_q <- rlang::enquo(var2)

  var1_name <- rlang::as_label(var1_q)
  var2_name <- rlang::as_label(var2_q)

  n_before <- nrow(data)

  df <- data %>%
    dplyr::filter(!is.na(!!var1_q), !is.na(!!var2_q)) %>%
    dplyr::mutate(
      var1_chr = as.character(!!var1_q),
      var2_chr = as.character(!!var2_q)
    )

  n_dropped <- n_before - nrow(df)
  if (n_dropped > 0) {
    message(glue::glue("{n_dropped} incomplete observation(s) removed before analysis."))
  }

  tbl     <- table(df$var1_chr, df$var2_chr)
  test    <- suppressWarnings(stats::chisq.test(tbl, correct = FALSE))
  df_stat <- test$parameter
  chi2    <- round(test$statistic, 2)
  p_val   <- test$p.value
  p_text  <- if (p_val < 0.001) "< 0.001" else sprintf("%.3f", p_val)

  # --- Output ---
  cat(glue::glue("Chi-square test of independence\n\n\n"))
  cat(glue::glue("Hypotheses:\n\n"))
  cat(glue::glue("H\u2080: {var1_name} and {var2_name} are independent\n\n"))
  cat(glue::glue("H\u2081: {var1_name} and {var2_name} are not independent\n\n"))
  cat(glue::glue("Test statistic: \u03c7\u00b2({df_stat}) = {chi2}\n\n"))
  cat(glue::glue("p-value: {p_text}\n\n"))
  conclusion(p_val, alpha)

  invisible(NULL)
}
