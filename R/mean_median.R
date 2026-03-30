#' Mean and Median Summary Table
#'
#' @description Produces a summary table of mean (SD) and median (IQR) for one
#'   or more numeric variables. Prints the result and returns it invisibly.
#'
#' @param data A data frame or tibble.
#' @param ... Unquoted column names of numeric variables to summarise.
#'
#' @return The summary tibble, returned invisibly. Called primarily for its
#'   printed output.
#'
#' @export
#' @importFrom dplyr summarise across arrange matches
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr %>%
mean_median <- function(data, ...) {
  if (!is.data.frame(data)) stop("`data` must be a data frame or tibble.")

  cols <- rlang::enquos(...)
  if (length(cols) == 0) stop("At least one column must be supplied via `...`.")

  col_names <- purrr::map_chr(cols, rlang::as_name)
  non_numeric <- col_names[!purrr::map_lgl(col_names, ~ is.numeric(data[[.x]]))]
  if (length(non_numeric) > 0) {
    stop("All columns must be numeric. Non-numeric: ",
         paste(non_numeric, collapse = ", "))
  }

  summary_tbl <- data %>%
    dplyr::summarise(
      dplyr::across(
        c(...),
        list(
          mean_sd    = ~ sprintf("%.1f (%.1f)", mean(.x, na.rm = TRUE), sd(.x, na.rm = TRUE)),
          median_iqr = ~ sprintf("%.1f (%.1f)", median(.x, na.rm = TRUE), IQR(.x, na.rm = TRUE))
        ),
        .names = "{.col}__{.fn}"
      ),
      .groups = "drop"
    ) %>%
    tidyr::pivot_longer(
      cols      = dplyr::matches("__"),
      names_to  = c("variable", ".value"),
      names_sep = "__"
    ) %>%
    dplyr::arrange(variable)

  print(summary_tbl, n = Inf)
  invisible(summary_tbl)
}
