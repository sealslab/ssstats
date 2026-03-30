#' Frequency Table with Counts and Percentages
#'
#' @description Produces a one- or two-way frequency table with counts and
#'   percentages combined into "n (pct)" cells. When the result is truncated
#'   to `show`, a message is emitted.
#'
#' @param data A data frame or tibble.
#' @param row_var Unquoted column name for the row variable.
#' @param col_var Unquoted column name for the column variable. If `NULL`
#'   (default), a one-way table is returned.
#' @param show Maximum number of rows to return (or show). Defaults to 
#'   `10`. Set to `Inf` to return all rows.
#' @param digits Number of decimal places for percentages. Defaults to `1`.
#' @param denom Denominator for percentages in two-way tables: `"col"`
#'   (default), `"row"`, or `"all"`.
#'
#' @return A tibble with one row per level of `row_var` and an `n (pct)`
#'   column for each level of `col_var` (or a single `n (pct)` column for
#'   one-way tables).
#'
#' @export
#' @importFrom rlang enquo quo_is_null as_name
#' @importFrom dplyr mutate select slice_head left_join all_of
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom janitor tabyl adorn_percentages adorn_pct_formatting
#' @importFrom scales percent
#' @importFrom magrittr %>%
n_pct <- function(data, row, col = NULL,
                  show = 10, digits = 1, denom = "col") {

  row_var_enquo <- rlang::enquo(row)
  col_var_enquo <- rlang::enquo(col)

  if (rlang::quo_is_null(col_var_enquo)) {

    tab_n <- janitor::tabyl(data, !!row_var_enquo)
    total_n <- sum(tab_n[[2]])

    out <- tab_n %>%
      dplyr::mutate(
        pct     = .[[2]] / total_n,
        pct_fmt = scales::percent(pct, accuracy = 10^(-digits)),
        `n (pct)` = paste0(.[[2]], " (", pct_fmt, ")")
      ) %>%
      dplyr::select(!!rlang::as_name(row_var_enquo), `n (pct)`)

  } else {

    row_var_name <- rlang::as_name(row_var_enquo)

    tab_n   <- janitor::tabyl(data, !!row_var_enquo, !!col_var_enquo)
    tab_fmt <- tab_n %>%
      janitor::adorn_percentages(denominator = denom) %>%
      janitor::adorn_pct_formatting(digits = digits)

    tab_n_long   <- tab_n   %>% tidyr::pivot_longer(-1, names_to = "col", values_to = "n")
    tab_fmt_long <- tab_fmt %>% tidyr::pivot_longer(-1, names_to = "col", values_to = "pct")

    out <- dplyr::left_join(tab_n_long, tab_fmt_long, by = c(row_var_name, "col")) %>%
      dplyr::mutate(`n (pct)` = paste0(n, " (", pct, ")")) %>%
      dplyr::select(dplyr::all_of(row_var_name), col, `n (pct)`) %>%
      tidyr::pivot_wider(names_from = col, values_from = `n (pct)`)

  }

  if (!is.infinite(show) && nrow(out) > show) {
    message(sprintf(
      "n_pct(): returning first %d of %d rows. Set show = Inf to see all.",
      show, nrow(out)
    ))
    out <- dplyr::slice_head(out, n = show)
  }

  out
}
