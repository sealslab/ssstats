#' Simultaneous Confidence Intervals
#'
#' @description Constructs simultaneous confidence intervals for a specified
#'   parameter using the Bonferroni correction. The per-interval confidence
#'   level is \code{1 - alpha / k}, where \code{k} is the number of intervals,
#'   ensuring a family-wise confidence level of at least
#'   \code{(1 - alpha) * 100\%}.
#'
#'   For \code{parameter = "proportion"}, \code{outcome} is treated as a
#'   categorical variable and a CI is constructed for each category's overall
#'   proportion. \code{group} is not used. \code{k} equals the number of
#'   categories.
#'
#'   For all other parameters, \code{outcome} must be numeric, \code{group} is
#'   required, and one CI is produced per group. \code{k} equals the number of
#'   groups.
#'
#' @param data A data frame or tibble.
#' @param outcome Unquoted column name for the outcome variable. Must be
#'   numeric for \code{"mean"}, \code{"median"}, \code{"variance"}, and
#'   \code{"sd"}. May be any type for \code{"proportion"}.
#' @param group Unquoted column name for the grouping variable. Required for
#'   all parameters except \code{"proportion"}.
#' @param parameter Character string specifying the parameter to estimate.
#'   One of \code{"mean"} (default), \code{"median"}, \code{"variance"},
#'   \code{"sd"}, or \code{"proportion"}.
#' @param alpha Numeric. Family-wise significance level between 0 and 1
#'   (exclusive). Defaults to \code{0.05}.
#'
#' @return A tibble with one row per group (numeric parameters) or one row per
#'   category (\code{"proportion"}) containing the point estimate and
#'   simultaneous confidence interval.
#'
#' @export
#' @importFrom rlang enquo as_label `!!` quo_is_missing
#' @importFrom dplyr pull
#' @importFrom tibble tibble
#' @importFrom purrr list_rbind
#' @importFrom glue glue
#' @importFrom stats complete.cases t.test wilcox.test var sd median
#' @importFrom magrittr %>%
#' @importFrom stats qchisq prop.test
simultaneous_ci <- function(data,
                             outcome,
                             grouping,
                             parameter = "mean",
                             alpha     = 0.05) {

  parameter <- match.arg(parameter,
                         choices = c("mean", "median", "variance", "sd", "proportion"))

  if (!is.numeric(alpha) || alpha <= 0 || alpha >= 1)
    stop("`alpha` must be numeric and between 0 and 1 (exclusive).", call. = FALSE)

  outcome_q <- rlang::enquo(outcome)
  conf_pct  <- round((1 - alpha) * 100)

  # ── Proportion: outcome only, one row per category ──────────────────────────
  if (parameter == "proportion") {

    y <- dplyr::pull(data, !!outcome_q)

    complete  <- !is.na(y)
    n_dropped <- sum(!complete)
    if (n_dropped > 0)
      message(glue::glue("{n_dropped} incomplete observation(s) removed before analysis."))

    y    <- as.character(y[complete])
    cats <- sort(unique(y))
    k    <- length(cats)
    n    <- length(y)

    alpha_per <- alpha / k
    ci_label  <- paste0(conf_pct, "% Simultaneous CI")

    results <- lapply(cats, function(cat) {
      x  <- sum(y == cat)
      pt <- stats::prop.test(x, n, conf.level = 1 - alpha_per, correct = FALSE)
      tibble::tibble(
        Category     = cat,
        n            = n,
        Proportion   = sprintf("%.2f", round(x / n, 4)),
        !!ci_label  := sprintf("(%.2f, %.2f)",
                               round(pt$conf.int[1], 4),
                               round(pt$conf.int[2], 4))
      )
    }) %>%
      purrr::list_rbind()

    return(results)
  }

  # ── Numeric parameters: group required, one row per group ───────────────────
  group_q <- rlang::enquo(grouping)

  if (rlang::quo_is_missing(group_q))
    stop('`grouping` is required when parameter is not "proportion".', call. = FALSE)

  y <- dplyr::pull(data, !!outcome_q)
  g <- dplyr::pull(data, !!group_q)

  if (!is.numeric(y))
    stop(glue::glue(
      "`{rlang::as_label(outcome_q)}` must be numeric for parameter = \"{parameter}\"."
    ), call. = FALSE)

  complete  <- stats::complete.cases(y, g)
  n_dropped <- sum(!complete)
  if (n_dropped > 0)
    message(glue::glue("{n_dropped} incomplete observation(s) removed before analysis."))

  y <- y[complete]
  g <- as.character(g[complete])

  groups    <- sort(unique(g))
  k         <- length(groups)
  alpha_per <- alpha / k

  est_label <- switch(parameter,
    "mean"     = "Mean",
    "median"   = "Median",
    "variance" = "Variance",
    "sd"       = "SD"
  )

  ci_label <- paste0(conf_pct, "% Simultaneous CI")

  compute_ci <- function(vals) {
    n <- length(vals)

    if (parameter == "mean") {
      tt    <- stats::t.test(vals, conf.level = 1 - alpha_per)
      est   <- mean(vals)
      lower <- tt$conf.int[1]
      upper <- tt$conf.int[2]

    } else if (parameter == "median") {
      wt    <- suppressWarnings(
        stats::wilcox.test(vals, conf.int = TRUE, conf.level = 1 - alpha_per)
      )
      est   <- stats::median(vals)
      lower <- wt$conf.int[1]
      upper <- wt$conf.int[2]

    } else if (parameter == "variance") {
      est    <- stats::var(vals)
      chi_lo <- stats::qchisq(alpha_per / 2,     df = n - 1)
      chi_hi <- stats::qchisq(1 - alpha_per / 2, df = n - 1)
      lower  <- (n - 1) * est / chi_hi
      upper  <- (n - 1) * est / chi_lo

    } else if (parameter == "sd") {
      v      <- stats::var(vals)
      chi_lo <- stats::qchisq(alpha_per / 2,     df = n - 1)
      chi_hi <- stats::qchisq(1 - alpha_per / 2, df = n - 1)
      est    <- sqrt(v)
      lower  <- sqrt((n - 1) * v / chi_hi)
      upper  <- sqrt((n - 1) * v / chi_lo)
    }

    list(n = n, estimate = round(est, 4), lower = round(lower, 4), upper = round(upper, 4))
  }

  results <- lapply(groups, function(grp) {
    ci <- compute_ci(y[g == grp])
    tibble::tibble(
      Group        = grp,
      n            = ci$n,
      !!est_label := sprintf("%.2f", ci$estimate),
      !!ci_label  := sprintf("(%.2f, %.2f)", ci$lower, ci$upper)
    )
  }) %>%
    purrr::list_rbind()

  results
}
