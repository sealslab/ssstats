#' Fisher's Exact Test
#'
#' @description Performs Fisher's exact test for independence between two
#'   categorical variables in a 2x2 contingency table. Prints a formatted
#'   summary including hypotheses, p-value, and conclusion.
#'
#' @param data A data frame or tibble.
#' @param col1 Unquoted column name of the first categorical variable.
#' @param col2 Unquoted column name of the second categorical variable.
#' @param alpha Numeric. Significance level between 0 and 1 (exclusive).
#'   Defaults to \code{0.05}.
#'
#' @return Called for its side effect (printed output); returns \code{NULL}
#'   invisibly.
#'
#' @details Fisher's exact test is appropriate for 2x2 contingency tables,
#'   particularly when expected cell frequencies are low. This function will
#'   return an error if the resulting table is larger than 2x2.
#'
#' @export
#' @importFrom rlang enquo as_label `!!`
#' @importFrom dplyr filter mutate
#' @importFrom glue glue
#' @importFrom stats fisher.test
#' @importFrom magrittr %>%
fishers_exact <- function(data,
                          col1,
                          col2,
                          alpha = 0.05) {

  if (!is.numeric(alpha) || alpha <= 0 || alpha >= 1)
    stop("`alpha` must be numeric and between 0 and 1 (exclusive).", call. = FALSE)

  var1_q <- rlang::enquo(col1)
  var2_q <- rlang::enquo(col2)

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

  tbl <- table(df$var1_chr, df$var2_chr)

  # Check that table is exactly 2x2
  if (!isTRUE(all.equal(dim(tbl), c(2L, 2L)))) {
    stop(
      glue::glue(
        "Fisher's exact test requires a 2x2 contingency table. ",
        "Your data produced a {nrow(tbl)}x{ncol(tbl)} table. ",
        "Please verify your variables have exactly 2 levels each, ",
        "or use chi-square test of independence for larger tables."
      ),
      call. = FALSE
    )
  }

  test  <- stats::fisher.test(tbl)
  p_val <- test$p.value
  p_text <- if (p_val < 0.001) "< 0.001" else sprintf("%.3f", p_val)

  # --- Output ---
  cat(glue::glue("Fisher's exact test\n\n\n"))
  cat(glue::glue("Hypotheses:\n\n"))
  cat(glue::glue("H₀: {var1_name} and {var2_name} are independent\n\n"))
  cat(glue::glue("H₁: {var1_name} and {var2_name} are not independent\n\n"))
  cat(glue::glue("p-value: {p_text}\n\n"))
  conclusion(p_val, alpha)

  invisible(NULL)
}
