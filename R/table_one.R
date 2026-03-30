#' Descriptive Statistics Table
#'
#' @description
#' Generates a descriptive statistics table (commonly called "Table 1") with
#' optional column splitting by a grouping variable. Continuous variables are
#' summarized as mean (SD) or median (Q1, Q3). Categorical variables are
#' summarized as n (col pct) or n (row pct).
#'
#' @param data A data frame.
#' @param cont_vars <[`tidy-select`][dplyr::dplyr_tidy_select]> Continuous
#'   variables to summarize, e.g. `c(age, bmi)`.
#' @param cat_vars <[`tidy-select`][dplyr::dplyr_tidy_select]> Categorical
#'   variables to summarize, e.g. `c(sex, race)`.
#' @param split_var <[`tidy-select`][dplyr::dplyr_tidy_select]> A single
#'   variable whose levels define the columns (e.g. `sex`). If `NULL`
#'   (default), only an Overall column is produced.
#' @param cont_summary Summary statistic for continuous variables. One of
#'   `"mean"` (default, reports mean (SD)) or `"median"` (reports
#'   median (Q1, Q3)).
#' @param cat_pct Percentage type for categorical variables. One of `"col"`
#'   (default, column percentages) or `"row"` (row percentages; the Overall
#'   column will always show 100 pct).
#' @param show_miss_cont If `TRUE`, append a "Missing: n (pct)" row beneath each
#'   continuous variable. Default `FALSE`.
#' @param drop_miss_cat If `TRUE`, `NA` values are dropped from categorical
#'   variables before computing counts and percentages (denominator excludes
#'   `NA`). If `FALSE` (default), `NA` is treated as its own level and the
#'   denominator includes all observations.
#' @param digits_cont Number of decimal places for continuous statistics.
#'   Default `2`.
#' @param digits_pct Number of decimal places for percentages. Default `1`.
#' @param cat_order Level ordering for categorical variables. One of:
#'   \describe{
#'     \item{`"factor"`}{(default) Use existing factor level order; falls back
#'       to alphabetical for non-factors.}
#'     \item{`"alpha"`}{Alphabetical order.}
#'     \item{`"freq"`}{Descending frequency order (computed on the full
#'       dataset).}
#'   }
#' @param labels A named character vector of display labels for variable names,
#'   e.g. `c(age = "Age (years)", bmi = "BMI (kg/m²)")`. Variables not
#'   listed use the raw column name.
#' @param output Output format. One of `"tibble"` (default) or `"gt"`.
#' @param allow_wide Logical. If `FALSE` (default), an error is raised when
#'   `split_var` has more than 4 levels to prevent accidentally producing a
#'   very wide table. Set to `TRUE` to proceed anyway.
#'
#' @details
#' **Column order:** Continuous variables appear first (in the order supplied
#' to `cont_vars`), followed by categorical variables (in the order supplied
#' to `cat_vars`).
#'
#' **Missing values in `split_var`:** Observations where `split_var` is `NA`
#' are excluded from all group columns but are included in the Overall column.
#'
#' **`gt` output:** Requires the `gt` package. Variable name rows are
#' bolded, and the level column header is left blank for a clean appearance.
#'
#' @return A tibble (or a `gt` table if `output = "gt"`) with the following
#'   columns: `variable`, `level`, one column per level of `split_var`
#'   (header includes n and column pct), and an Overall column.
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' table_one(
#'   data     = trial_data,
#'   cont_vars = c(age, bmi),
#'   cat_vars  = c(sex, race),
#'   split_var = treatment,
#'   labels    = c(age = "Age (years)", bmi = "BMI (kg/m²)", sex = "Sex",
#'                 race = "Race/Ethnicity", treatment = "Treatment Group")
#' )
#'
#' # Median (IQR) with missing data reported
#' table_one(
#'   data           = trial_data,
#'   cont_vars      = c(age, bmi),
#'   cont_summary   = "median",
#'   show_miss_cont = TRUE
#' )
#' }
#'
#' @importFrom dplyr bind_rows
#' @importFrom purrr map
#' @importFrom tibble as_tibble
#' @importFrom tidyselect eval_select
#' @importFrom rlang enquo quo_is_null
#' @importFrom stats na.omit sd quantile
#'
#' @export
#' @importFrom magrittr %>%
table_one <- function(
  data,
  cont_vars      = NULL,
  cat_vars       = NULL,
  split_var      = NULL,
  cont_summary   = "mean",
  cat_pct        = "col",
  show_miss_cont = FALSE,
  drop_miss_cat  = FALSE,
  digits_cont    = 2,
  digits_pct     = 1,
  cat_order      = "factor",
  labels         = NULL,
  output         = "tibble",
  allow_wide     = FALSE
) {

  cont_summary <- match.arg(cont_summary, c("mean", "median"))
  cat_pct      <- match.arg(cat_pct,      c("col", "row"))
  cat_order    <- match.arg(cat_order,    c("factor", "alpha", "freq"))
  output       <- match.arg(output,       c("tibble", "gt"))

  resolve_vars <- function(expr) {
    if (rlang::quo_is_null(expr)) return(character(0))
    names(tidyselect::eval_select(expr, data))
  }

  cont_cols  <- resolve_vars(rlang::enquo(cont_vars))
  cat_cols   <- resolve_vars(rlang::enquo(cat_vars))
  split_cols <- resolve_vars(rlang::enquo(split_var))

  if (length(cont_cols) == 0 && length(cat_cols) == 0) {
    stop("At least one of `cont_vars` or `cat_vars` must be specified.")
  }

  if (length(split_cols) > 1) {
    warning("`split_var` must be a single variable. Using '", split_cols[1], "'.")
    split_cols <- split_cols[1]
  }

  split_col <- if (length(split_cols) == 0) NULL else split_cols

  overall_n <- nrow(data)

  if (!is.null(split_col)) {
    sv <- data[[split_col]]

    grp_levels <- if (is.factor(sv)) {
      levels(sv)
    } else {
      as.character(sort(unique(stats::na.omit(sv))))
    }

    if (length(grp_levels) > 4) {
      if (!allow_wide) {
        stop(
          paste0(
            "'", split_col, "' has ", length(grp_levels),
            " levels, which may produce a very wide table. ",
            "Set `allow_wide = TRUE` to proceed anyway."
          ),
          call. = FALSE
        )
      }
      message(paste0(
        "'", split_col, "' has ", length(grp_levels),
        " levels; proceeding because `allow_wide = TRUE`."
      ))
    }

    grp_ns      <- sapply(grp_levels, function(g) sum(!is.na(sv) & as.character(sv) == g))
    grp_pcts    <- round(100 * grp_ns / overall_n, digits_pct)
    grp_headers <- paste0(grp_levels, " (n=", grp_ns, ", ", grp_pcts, "%)")

  } else {
    grp_levels  <- character(0)
    grp_headers <- character(0)
  }

  overall_header <- paste0("Overall (n=", overall_n, ")")
  all_headers    <- c(grp_headers, overall_header)

  get_label <- function(var) {
    if (!is.null(labels) && var %in% names(labels)) labels[[var]] else var
  }

  # Build a single-row tibble from named values
  make_row <- function(variable, level, vals) {
    tibble::as_tibble(c(list(variable = variable, level = level), vals))
  }

  # Named list of empty strings for all stat columns
  empty_vals <- function() {
    stats::setNames(as.list(rep("", length(all_headers))), all_headers)
  }

  fmt_n_pct <- function(n, denom) {
    pct <- if (denom == 0) 0 else round(100 * n / denom, digits_pct)
    paste0(n, " (", pct, "%)")
  }

  fmt_mean_sd <- function(x) {
    x <- stats::na.omit(x)
    if (length(x) == 0) return(NA_character_)
    paste0(round(mean(x), digits_cont), " (", round(stats::sd(x), digits_cont), ")")
  }

  fmt_median_iqr <- function(x) {
    x <- stats::na.omit(x)
    if (length(x) == 0) return(NA_character_)
    q <- stats::quantile(x, c(0.25, 0.5, 0.75))
    paste0(
      round(q[2], digits_cont),
      " (", round(q[1], digits_cont),
      ", ", round(q[3], digits_cont), ")"
    )
  }

  fmt_cont <- function(x) {
    if (cont_summary == "mean") fmt_mean_sd(x) else fmt_median_iqr(x)
  }

  # Subset x to observations belonging to group g
  get_group_x <- function(x_all, g) {
    sv <- data[[split_col]]
    x_all[!is.na(sv) & as.character(sv) == g]
  }

  # Return ordered levels for a categorical variable
  get_cat_levels <- function(x) {
    has_na <- anyNA(x)

    levs <- if (cat_order == "factor" && is.factor(x)) {
      levels(x)
    } else if (cat_order == "alpha") {
      sort(unique(as.character(x[!is.na(x)])))
    } else if (cat_order == "freq") {
      names(sort(table(as.character(x[!is.na(x)])), decreasing = TRUE))
    } else {
      # cat_order == "factor" but variable is not a factor: fall back to alpha
      sort(unique(as.character(x[!is.na(x)])))
    }

    if (!drop_miss_cat && has_na) c(levs, NA_character_) else levs
  }

  process_cont <- function(var) {
    label <- get_label(var)
    x_all <- data[[var]]

    stat_vals <- empty_vals()

    if (!is.null(split_col)) {
      for (i in seq_along(grp_levels)) {
        stat_vals[[grp_headers[i]]] <- fmt_cont(get_group_x(x_all, grp_levels[i]))
      }
    }
    stat_vals[[overall_header]] <- fmt_cont(x_all)

    rows <- make_row(label, "", stat_vals)

    if (!show_miss_cont) return(rows)

    # Optional missing row
    miss_vals <- empty_vals()

    if (!is.null(split_col)) {
      for (i in seq_along(grp_levels)) {
        x_g <- get_group_x(x_all, grp_levels[i])
        miss_vals[[grp_headers[i]]] <- fmt_n_pct(sum(is.na(x_g)), length(x_g))
      }
    }
    miss_vals[[overall_header]] <- fmt_n_pct(sum(is.na(x_all)), length(x_all))

    dplyr::bind_rows(rows, make_row("", "Missing", miss_vals))
  }

  process_cat <- function(var) {
    label <- get_label(var)
    x_all <- data[[var]]
    levs  <- get_cat_levels(x_all)

    # Denominators for col %
    if (!is.null(split_col)) {
      sv <- data[[split_col]]
      grp_denoms <- if (drop_miss_cat) {
        sapply(grp_levels, function(g) sum(!is.na(sv) & as.character(sv) == g & !is.na(x_all)))
      } else {
        sapply(grp_levels, function(g) sum(!is.na(sv) & as.character(sv) == g))
      }
      names(grp_denoms) <- grp_levels
    }

    overall_denom <- if (drop_miss_cat) sum(!is.na(x_all)) else nrow(data)

    # Header row: variable name in `variable`, blank stats
    header_row <- make_row(label, "", empty_vals())

    # One row per level
    level_rows <- purrr::map(levs, function(lev) {
      lev_label <- if (is.na(lev)) "Missing" else as.character(lev)
      match_fn  <- if (is.na(lev)) {
        function(x) is.na(x)
      } else {
        function(x) !is.na(x) & as.character(x) == lev
      }

      vals <- empty_vals()

      if (cat_pct == "col") {
        if (!is.null(split_col)) {
          sv <- data[[split_col]]
          for (i in seq_along(grp_levels)) {
            g   <- grp_levels[i]
            n_g <- sum(!is.na(sv) & as.character(sv) == g & match_fn(x_all))
            vals[[grp_headers[i]]] <- fmt_n_pct(n_g, grp_denoms[[g]])
          }
        }
        n_all <- sum(match_fn(x_all))
        vals[[overall_header]] <- fmt_n_pct(n_all, overall_denom)

      } else {
        # Row percentages: denom is count of this level across all non-NA split groups
        n_all <- sum(match_fn(x_all))

        if (!is.null(split_col)) {
          sv        <- data[[split_col]]
          row_total <- sum(match_fn(x_all) & !is.na(sv))

          for (i in seq_along(grp_levels)) {
            g     <- grp_levels[i]
            n_g   <- sum(!is.na(sv) & as.character(sv) == g & match_fn(x_all))
            pct_g <- if (row_total == 0) 0 else round(100 * n_g / row_total, digits_pct)
            vals[[grp_headers[i]]] <- paste0(n_g, " (", pct_g, "%)")
          }
        }
        vals[[overall_header]] <- paste0(n_all, " (100%)")
      }

      make_row("", lev_label, vals)
    })

    dplyr::bind_rows(header_row, level_rows)
  }

  result <- dplyr::bind_rows(
    purrr::map(cont_cols, process_cont),
    purrr::map(cat_cols,  process_cat)
  )

  if (length(cat_cols) == 0) {
    result <- dplyr::select(result, -level)
  }

  if (output == "gt") {
    if (!requireNamespace("gt", quietly = TRUE)) {
      stop("Package 'gt' is required for `output = 'gt'`. Install with install.packages('gt').")
    }

    has_level_col <- length(cat_cols) > 0

    result <- gt::gt(result) %>%
      gt::cols_label(variable = "Variable") %>%
      gt::tab_style(
        style     = gt::cell_text(weight = "bold"),
        locations = gt::cells_body(
          columns = "variable",
          rows    = variable != ""
        )
      ) %>%
      gt::tab_style(
        style     = gt::cell_fill(color = "#F7F7F7"),
        locations = gt::cells_body(
          rows = variable != ""
        )
      ) %>%
      gt::sub_missing(missing_text = "") %>%
      gt::cols_align(align = "center", columns = all_headers)

    if (has_level_col) {
      result <- result %>%
        gt::cols_label(level = "") %>%
        gt::cols_align(align = "left", columns = c("variable", "level"))
    } else {
      result <- result %>%
        gt::cols_align(align = "left", columns = "variable")
    }
  }

  result
}
