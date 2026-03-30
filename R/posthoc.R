#' Post-Hoc Pairwise Comparisons
#'
#' @description Performs pairwise post-hoc comparisons following a one-way
#'   ANOVA or Kruskal-Wallis test. For ANOVA, runs Tukey's HSD when
#'   \code{adjust = TRUE} or Fisher's LSD when \code{adjust = FALSE}. For
#'   Kruskal-Wallis, runs Dunn's test with Bonferroni adjustment when
#'   \code{adjust = TRUE} or without adjustment when \code{adjust = FALSE}.
#'   Prints a formatted table of all pairwise comparisons and restates the
#'   model the post-hoc follows.
#'
#' @param data A data frame or tibble.
#' @param outcome Unquoted column name for the continuous outcome (numeric).
#' @param group Unquoted column name for the grouping variable. Must have at
#'   least 2 levels.
#' @param adjust Logical. If \code{TRUE} (default), applies Type I error
#'   adjustment (Tukey's HSD for ANOVA; Bonferroni for Kruskal-Wallis). If
#'   \code{FALSE}, no adjustment is applied (Fisher's LSD for ANOVA; unadjusted
#'   Dunn's test for Kruskal-Wallis).
#' @param nonparametric Logical. If \code{FALSE} (default), runs ANOVA-based
#'   post-hoc tests. If \code{TRUE}, runs Dunn's test following a
#'   Kruskal-Wallis analysis. Requires the \pkg{dunn.test} package.
#' @param alpha Numeric. Significance level between 0 and 1 (exclusive).
#'   Defaults to \code{0.05}.
#'
#' @return Called for its side effect (printed output); returns \code{NULL}
#'   invisibly.
#'
#' @export
#' @importFrom rlang enquo as_name `!!`
#' @importFrom dplyr pull
#' @importFrom glue glue
posthoc_test <- function(data,
                         outcome,
                         A,
                         adjust        = TRUE,
                         nonparametric = FALSE,
                         alpha         = 0.05) {

  if (!is.logical(adjust) || length(adjust) != 1)
    stop("`adjust` must be TRUE or FALSE.", call. = FALSE)
  if (!is.logical(nonparametric) || length(nonparametric) != 1)
    stop("`nonparametric` must be TRUE or FALSE.", call. = FALSE)
  if (!is.numeric(alpha) || alpha <= 0 || alpha >= 1)
    stop("`alpha` must be numeric and between 0 and 1 (exclusive).", call. = FALSE)

  outcome_q <- rlang::enquo(outcome)
  group_q   <- rlang::enquo(A)

  outcome_name <- rlang::as_name(outcome_q)
  group_name   <- rlang::as_name(group_q)

  y <- dplyr::pull(data, !!outcome_q)
  g <- dplyr::pull(data, !!group_q)

  if (!is.numeric(y))
    stop(glue::glue("`{outcome_name}` must be a numeric column."), call. = FALSE)

  complete  <- stats::complete.cases(y, g)
  n_dropped <- sum(!complete)
  if (n_dropped > 0) {
    message(glue::glue("{n_dropped} incomplete observation(s) removed before analysis."))
  }

  y <- y[complete]
  g <- g[complete]

  if (is.factor(g)) {
    grp_levels <- levels(droplevels(g))
  } else {
    grp_levels <- sort(unique(as.character(g)))
    g <- as.character(g)
  }

  k <- length(grp_levels)

  if (k < 2)
    stop(glue::glue("`{group_name}` must have at least 2 levels."), call. = FALSE)

  # Labels for output header
  a_text <- sprintf("%.3f", alpha)

  if (!nonparametric) {
    test_label      <- if (adjust) "Tukey's HSD" else "Fisher's LSD"
    following_label <- glue::glue("one-way ANOVA ({outcome_name} ~ {group_name})")
  } else {
    test_label      <- if (adjust) "Dunn's test (Bonferroni adjustment)" else "Dunn's test (no adjustment)"
    following_label <- glue::glue("Kruskal-Wallis ({outcome_name} ~ {group_name})")
  }

  if (!nonparametric) {

    # Build sub-frame for aov()
    data_sub <- data.frame(row.names = seq_along(y))
    data_sub[[outcome_name]] <- y
    data_sub[[group_name]]   <- factor(g, levels = grp_levels)

    fmla <- stats::as.formula(paste(outcome_name, "~", group_name))
    fit  <- stats::aov(fmla, data = data_sub)

    if (adjust) {

      # Tukey's HSD
      tk     <- stats::TukeyHSD(fit, conf.level = 1 - alpha)
      tk_mat <- tk[[group_name]]

      # Labels from TukeyHSD are "B-A"; convert to "B vs A"
      comparisons_fmt <- gsub("-", " vs ", rownames(tk_mat))
      estimates       <- round(tk_mat[, "diff"],  4)
      p_vals          <- tk_mat[, "p adj"]

    } else {

      # Fisher's LSD: pairwise t-tests, no adjustment, pooled SD
      pt     <- stats::pairwise.t.test(y, factor(g, levels = grp_levels),
                                       p.adjust.method = "none",
                                       pool.sd         = TRUE)
      pt_mat <- pt$p.value

      comparisons_fmt <- character(0)
      estimates       <- numeric(0)
      p_vals          <- numeric(0)

      for (i in seq(2, k)) {
        for (j in seq(1, i - 1)) {
          g2 <- grp_levels[i]
          g1 <- grp_levels[j]
          comparisons_fmt <- c(comparisons_fmt, paste(g2, "vs", g1))
          estimates       <- c(estimates, round(mean(y[g == g2]) - mean(y[g == g1]), 4))
          p_vals          <- c(p_vals, pt_mat[g2, g1])
        }
      }
    }

    sig     <- ifelse(p_vals < alpha, "*", "")
    fmt_est <- sprintf("%.4f", estimates)
    fmt_p   <- vapply(p_vals,
                      function(p) if (p < 0.001) "< 0.001" else sprintf("%.3f", p),
                      character(1))

    w_comp <- max(nchar(c("Comparison", comparisons_fmt)))
    w_est  <- max(nchar(c("Difference", fmt_est)))
    w_p    <- max(nchar(c("p-value",    fmt_p)))

    sep_len <- w_comp + w_est + w_p + 1 + 6
    sep     <- paste(rep("\u2500", sep_len), collapse = "")

    header <- sprintf("%-*s  %*s  %*s  %s",
                      w_comp, "Comparison",
                      w_est,  "Difference",
                      w_p,    "p-value",
                      "")

    rows <- vapply(seq_along(comparisons_fmt), function(i) {
      sprintf("%-*s  %*s  %*s  %s",
              w_comp, comparisons_fmt[i],
              w_est,  fmt_est[i],
              w_p,    fmt_p[i],
              sig[i])
    }, character(1))

  } else {

    # Dunn's test
    if (!requireNamespace("dunn.test", quietly = TRUE))
      stop(paste0("Package 'dunn.test' is required for Kruskal-Wallis post-hoc ",
                  "testing. Install with install.packages('dunn.test')."),
           call. = FALSE)

    adj_method <- if (adjust) "bonferroni" else "none"

    invisible(utils::capture.output({
      dt <- dunn.test::dunn.test(y,
                                 factor(g, levels = grp_levels),
                                 method = adj_method,
                                 kw     = FALSE,
                                 table  = FALSE,
                                 list   = FALSE)
    }))

    # Labels from dunn.test are "A - B"; convert to "A vs B"
    comparisons_fmt <- gsub(" - ", " vs ", dt$comparisons)
    z_vals          <- round(dt$Z, 3)
    p_vals          <- dt$P.adjusted

    sig   <- ifelse(p_vals < alpha, "*", "")
    fmt_z <- sprintf("%.3f", z_vals)
    fmt_p <- vapply(p_vals,
                    function(p) if (p < 0.001) "< 0.001" else sprintf("%.3f", p),
                    character(1))

    w_comp <- max(nchar(c("Comparison", comparisons_fmt)))
    w_z    <- max(nchar(c("z",          fmt_z)))
    w_p    <- max(nchar(c("p-value",    fmt_p)))

    sep_len <- w_comp + w_z + w_p + 1 + 6
    sep     <- paste(rep("\u2500", sep_len), collapse = "")

    header <- sprintf("%-*s  %*s  %*s  %s",
                      w_comp, "Comparison",
                      w_z,    "z",
                      w_p,    "p-value",
                      "")

    rows <- vapply(seq_along(comparisons_fmt), function(i) {
      sprintf("%-*s  %*s  %*s  %s",
              w_comp, comparisons_fmt[i],
              w_z,    fmt_z[i],
              w_p,    fmt_p[i],
              sig[i])
    }, character(1))

  }

  cat(glue::glue("Post-hoc analysis: {test_label}\n\n\n"))
  cat(glue::glue("Following: {following_label}\n\n\n"))
  cat(header, "\n")
  cat(sep, "\n")
  for (row in rows) cat(row, "\n")
  cat(sep, "\n\n")
  cat(glue::glue("* p < \u03b1 = {a_text}\n\n"))

  invisible(NULL)
}
