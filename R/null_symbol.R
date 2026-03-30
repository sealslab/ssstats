#' Null Symbol Helper
#'
#' @description Internal helper that converts an `alternative` string to the
#'   corresponding null hypothesis symbol for use in formatted output.
#' @param alternative Character string. One of `"less"`, `"greater"`, or
#'   `"two.sided"`.
#' @return A character symbol: `"="`, `"≤"`, or `"≥"`.
#' @keywords internal
null_symbol <- function(alternative) {
  alternative <- match.arg(alternative, choices = c("two.sided", "less", "greater"))
  switch(alternative,
         "two.sided" = "=",
         "greater"   = "\u2264",
         "less"      = "\u2265"
  )
}
