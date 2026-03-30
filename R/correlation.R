#' Pearson and Spearman Correlation
#'
#' @description Computes Pearson or Spearman correlation(s) for a data frame.
#'   If \code{x} and \code{y} are provided, returns a one-row tibble for that
#'   pair. Otherwise, returns a tibble of all pairwise correlations among
#'   numeric columns.
#'
#' @param data A data frame or tibble.
#' @param x Unquoted column name of the first variable. Optional; if omitted
#'   all pairwise correlations are computed.
#' @param y Unquoted column name of the second variable. Optional; if omitted
#'   all pairwise correlations are computed.
#' @param method Character string. One of \code{"pearson"} (default) or
#'   \code{"spearman"}.
#'
#' @return A tibble with columns \code{var1}, \code{var2},
#'   \code{correlation}, \code{test_statistic}, and \code{p_value}. The
#'   attribute \code{test_stat_name} records whether the statistic is
#'   \code{"t"} (Pearson) or \code{"S"} (Spearman).
#'
#' @export
#' @importFrom rlang enexpr as_name
#' @importFrom tibble tibble
#' @importFrom dplyr select filter where
#' @importFrom tidyr expand_grid
#' @importFrom purrr map2 list_rbind
#' @importFrom stats cor.test
#' @importFrom magrittr %>%
correlation <- function(data, x = NULL, y = NULL, method = "pearson") {

  method <- match.arg(method, choices = c("pearson", "spearman"))

  extract_cor_info <- function(var1, var2, data, method) {
    test      <- stats::cor.test(data[[var1]], data[[var2]], method = method)
    stat_name <- if (method == "pearson") "t" else "S"

    tibble::tibble(
      var1           = var1,
      var2           = var2,
      correlation    = round(test$estimate, 3),
      test_statistic = round(test$statistic, 3),
      p_value        = if (test$p.value < 0.001) "< 0.001"
                       else formatC(test$p.value, digits = 3, format = "f")
    )
  }

  stat_name <- if (method == "pearson") "t" else "S"

  # Case 1: specific pair provided
  if (!is.null(rlang::enexpr(x)) && !is.null(rlang::enexpr(y))) {
    var1   <- rlang::as_name(rlang::enexpr(x))
    var2   <- rlang::as_name(rlang::enexpr(y))
    result <- extract_cor_info(var1, var2, data, method)
    attr(result, "test_stat_name") <- stat_name
    return(result)
  }

  # Case 2: all pairwise correlations among numeric columns
  numeric_data <- data %>% dplyr::select(dplyr::where(is.numeric))
  var_names    <- names(numeric_data)

  all_pairs <- tidyr::expand_grid(var1 = var_names, var2 = var_names) %>%
    dplyr::filter(var1 < var2)

  results <- purrr::map2(
    all_pairs$var1, all_pairs$var2,
    function(v1, v2) extract_cor_info(v1, v2, numeric_data, method)
  ) %>%
    purrr::list_rbind()

  attr(results, "test_stat_name") <- stat_name

  results
}
