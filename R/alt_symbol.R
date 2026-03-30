#' Alt Symbol Helper
#'
#' @description Internal helper that converts an `alt` string to a
#' display symbol for use in formatted output.
#' @param alt Character string. One of `"less"`, `"greater"`, `"two"`,
#'   or `"two.sided"`.
#' @return A character symbol: `"<"`, `">"`, or `≠`.
#' @keywords internal
alt_symbol <- function(alt) {
  valid <- c("less", "greater", "two", "two.sided")
  if (!alt %in% valid) {
    stop("`alt` must be one of: ", paste(valid, collapse = ", "))
  }
  if (alt == "less")                    return("<")
  if (alt == "greater")                 return(">")
  if (alt %in% c("two", "two.sided"))  return("≠")
}
