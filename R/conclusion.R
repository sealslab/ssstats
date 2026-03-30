#' Conclusion Helper
#'
#' @description Internal helper that prints a formatted conclusion based on
#'   a p-value and significance level.
#' @param p_val Numeric. The p-value from a hypothesis test. Must be between
#'   0 and 1.
#' @param alpha Numeric. The significance level (e.g., `0.05`). Must be
#'   between 0 and 1.
#' @return Called for its side effect (printed output); returns `NULL` invisibly.
#' @keywords internal
conclusion <- function(p_val, alpha) {
  if (is.na(p_val) || is.na(alpha)) stop("`p_val` and `alpha` must not be NA.")
  if (p_val < 0 || p_val > 1)       stop("`p_val` must be between 0 and 1.")
  if (alpha <= 0 || alpha >= 1)      stop("`alpha` must be between 0 and 1 (exclusive).")
  
  p_text <- if (p_val < 0.001) "< 0.001" else sprintf("%.3f", p_val)
  a_text <- sprintf("%.3f", alpha)
  
  if (p_val < alpha) {
    cat(glue::glue("Conclusion: Reject the null hypothesis (p = {p_text} < α = {a_text})\n\n"))
  } else {
    cat(glue::glue("Conclusion: Fail to reject the null hypothesis (p = {p_text} \u2265 α = {a_text})\n\n"))
  }
  
  invisible(NULL)
}
