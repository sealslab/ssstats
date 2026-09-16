#' Post-Hoc Pairwise Comparisons
#'
#' @description Performs pairwise post-hoc comparisons following a one-way
#'   ANOVA, a two-way ANOVA, or a Kruskal-Wallis test. For one-way ANOVA, runs
#'   Tukey's HSD when \code{adjust = TRUE} or Fisher's LSD when
#'   \code{adjust = FALSE}. For a two-way ANOVA (supply \code{A} and \code{B}
#'   instead of \code{grouping}), runs the same comparisons separately for
#'   both main effects, and — when \code{interaction = TRUE} — for the
#'   interaction (all pairwise cell-mean comparisons) as well, using
#'   \code{adjust} the same way. Two-way post-hoc testing is only available
#'   for the normal-theory (ANOVA) case; the nonparametric two-way case is
#'   not implemented. For Kruskal-Wallis, runs Dunn's test with Bonferroni
#'   adjustment when \code{adjust = TRUE} or without adjustment when
#'   \code{adjust = FALSE}. Prints a formatted table of all pairwise
#'   comparisons and restates the model the post-hoc follows.
#'
#' @param data A data frame or tibble.
#' @param outcome Unquoted column name for the continuous outcome (numeric).
#' @param grouping Unquoted column name for the grouping variable. Must have
#'   at least 2 levels. Used for a one-way post-hoc analysis; omit this and
#'   supply \code{A} and \code{B} instead for a two-way analysis.
#' @param A Unquoted column name for the first grouping variable in a
#'   two-way post-hoc analysis. Must be supplied together with \code{B}.
#' @param B Unquoted column name for the second grouping variable in a
#'   two-way post-hoc analysis. Must be supplied together with \code{A}.
#' @param interaction Logical. Only relevant when \code{A} and \code{B} are
#'   supplied (two-way analysis). If \code{TRUE} (default), fits
#'   \code{outcome ~ A * B} and prints three blocks: the \code{A} main
#'   effect, the \code{B} main effect, and the \code{A:B} interaction
#'   (all pairwise cell-mean comparisons). If \code{FALSE}, fits the additive
#'   model \code{outcome ~ A + B} instead and prints only the two main-effect
#'   blocks — no interaction term is fit or compared. Ignored for a one-way
#'   analysis.
#' @param adjust Logical. If \code{TRUE} (default), applies Type I error
#'   adjustment (Tukey's HSD for ANOVA; Bonferroni for Kruskal-Wallis). If
#'   \code{FALSE}, no adjustment is applied (Fisher's LSD for ANOVA; unadjusted
#'   Dunn's test for Kruskal-Wallis).
#' @param nonparametric Logical. If \code{FALSE} (default), runs ANOVA-based
#'   post-hoc tests. If \code{TRUE}, runs Dunn's test following a
#'   Kruskal-Wallis analysis. Requires the \pkg{dunn.test} package. Not
#'   available for a two-way analysis (\code{A}/\code{B} supplied) — calling
#'   with \code{nonparametric = TRUE} and both \code{A} and \code{B} set
#'   raises an error, since a two-way rank-based post-hoc procedure is not
#'   implemented in \pkg{ssstats}.
#' @param alpha Numeric. Significance level between 0 and 1 (exclusive).
#'   Defaults to \code{0.05}.
#'
#' @return Called for its side effect (printed output); returns \code{NULL}
#'   invisibly.
#'
#' @export
#' @importFrom rlang enquo as_name quo_is_null `!!`
#' @importFrom dplyr pull
#' @importFrom glue glue
posthoc <- function(data,
                    outcome,
                    grouping      = NULL,
                    A             = NULL,
                    B             = NULL,
                    interaction   = TRUE,
                    adjust        = TRUE,
                    nonparametric = FALSE,
                    alpha         = 0.05) {
  
  if (!is.logical(adjust) || length(adjust) != 1)
    stop("`adjust` must be TRUE or FALSE.", call. = FALSE)
  if (!is.logical(nonparametric) || length(nonparametric) != 1)
    stop("`nonparametric` must be TRUE or FALSE.", call. = FALSE)
  if (!is.logical(interaction) || length(interaction) != 1)
    stop("`interaction` must be TRUE or FALSE.", call. = FALSE)
  if (!is.numeric(alpha) || alpha <= 0 || alpha >= 1)
    stop("`alpha` must be numeric and between 0 and 1 (exclusive).", call. = FALSE)
  
  outcome_q  <- rlang::enquo(outcome)
  grouping_q <- rlang::enquo(grouping)
  A_q        <- rlang::enquo(A)
  B_q        <- rlang::enquo(B)
  
  grouping_supplied <- !rlang::quo_is_null(grouping_q)
  A_supplied        <- !rlang::quo_is_null(A_q)
  B_supplied        <- !rlang::quo_is_null(B_q)
  
  if (A_supplied != B_supplied)
    stop("`A` and `B` must be supplied together for a two-way post-hoc analysis.", call. = FALSE)
  
  two_way <- A_supplied && B_supplied
  
  if (two_way && grouping_supplied)
    stop("Supply either `grouping` (one-way) or `A`/`B` (two-way), not both.", call. = FALSE)
  if (!two_way && !grouping_supplied)
    stop("Supply `grouping` for a one-way post-hoc analysis, or `A` and `B` for a two-way analysis.", call. = FALSE)
  
  # ---------------------------------------------------------------------
  # Two-way ANOVA post-hoc (A and B supplied)
  # ---------------------------------------------------------------------
  if (two_way) {
    
    if (nonparametric) {
      stop(paste0("Two-way post-hoc testing is not available for the ",
                  "nonparametric case (nonparametric = TRUE). A two-way ",
                  "rank-based post-hoc procedure is not implemented in ssstats."),
           call. = FALSE)
    }
    
    outcome_name <- rlang::as_name(outcome_q)
    A_name       <- rlang::as_name(A_q)
    B_name       <- rlang::as_name(B_q)
    
    y  <- dplyr::pull(data, !!outcome_q)
    fa <- dplyr::pull(data, !!A_q)
    fb <- dplyr::pull(data, !!B_q)
    
    if (!is.numeric(y))
      stop(glue::glue("`{outcome_name}` must be a numeric column."), call. = FALSE)
    
    complete  <- stats::complete.cases(y, fa, fb)
    n_dropped <- sum(!complete)
    if (n_dropped > 0) {
      message(glue::glue("{n_dropped} incomplete observation(s) removed before analysis."))
    }
    
    y  <- y[complete]
    fa <- fa[complete]
    fb <- fb[complete]
    
    a_levels <- if (is.factor(fa)) levels(droplevels(fa)) else sort(unique(as.character(fa)))
    b_levels <- if (is.factor(fb)) levels(droplevels(fb)) else sort(unique(as.character(fb)))
    
    if (length(a_levels) < 2)
      stop(glue::glue("`{A_name}` must have at least 2 levels."), call. = FALSE)
    if (length(b_levels) < 2)
      stop(glue::glue("`{B_name}` must have at least 2 levels."), call. = FALSE)
    
    data_sub <- data.frame(row.names = seq_along(y))
    data_sub[[outcome_name]] <- y
    data_sub[[A_name]]       <- factor(as.character(fa), levels = a_levels)
    data_sub[[B_name]]       <- factor(as.character(fb), levels = b_levels)
    
    # `interaction` toggles * vs + — mirrors anovas()'s own interaction argument
    op   <- if (interaction) "*" else "+"
    fmla <- stats::as.formula(paste(outcome_name, "~", A_name, op, B_name))
    fit  <- stats::aov(fmla, data = data_sub)
    
    a_text          <- sprintf("%.3f", alpha)
    test_label      <- if (adjust) "Tukey's HSD" else "Fisher's LSD"
    model_notation  <- glue::glue("{outcome_name} ~ {A_name} {op} {B_name}")
    following_label <- glue::glue("two-way ANOVA ({model_notation})")
    
    if (interaction) {
      terms       <- c(A_name, B_name, paste0(A_name, ":", B_name))
      term_titles <- c(
        glue::glue("Main effect: {A_name}"),
        glue::glue("Main effect: {B_name}"),
        glue::glue("Interaction: {A_name} \u00d7 {B_name}")
      )
    } else {
      terms       <- c(A_name, B_name)
      term_titles <- c(
        glue::glue("Main effect: {A_name}"),
        glue::glue("Main effect: {B_name}")
      )
    }
    
    cat(glue::glue("Post-hoc analysis: {test_label}\n\n\n"))
    cat(glue::glue("Following: {following_label}\n\n\n"))
    
    if (adjust) {
      
      # Tukey's HSD — run once on the fitted model; returns one matrix per
      # term (A and B, plus A:B only when interaction = TRUE).
      tk <- stats::TukeyHSD(fit, conf.level = 1 - alpha)
      
      for (i in seq_along(terms)) {
        tk_mat <- tk[[terms[i]]]
        
        comparisons_fmt <- gsub("-", " vs ", rownames(tk_mat))
        estimates       <- round(tk_mat[, "diff"], 4)
        p_vals          <- tk_mat[, "p adj"]
        
        .posthoc_print_block(term_titles[i], comparisons_fmt, estimates, p_vals, alpha)
      }
      
    } else {
      
      # Fisher's LSD: pairwise t-tests using the shared MSE from the fitted
      # model, run separately for A, B, and (when interaction = TRUE) the
      # A:B cell means.
      aov_tbl <- summary(fit)[[1]]
      mse     <- aov_tbl[["Mean Sq"]][nrow(aov_tbl)]
      df_res  <- fit$df.residual
      
      group_vecs <- list(data_sub[[A_name]], data_sub[[B_name]])
      if (interaction) {
        cell_factor <- interaction(data_sub[[A_name]], data_sub[[B_name]],
                                   sep = ":", drop = TRUE)
        group_vecs  <- c(group_vecs, list(cell_factor))
      }
      
      for (i in seq_along(terms)) {
        
        g        <- group_vecs[[i]]
        g_levels <- levels(droplevels(as.factor(g)))
        k        <- length(g_levels)
        
        comparisons_fmt <- character(0)
        estimates       <- numeric(0)
        p_vals          <- numeric(0)
        
        for (r in seq(2, k)) {
          for (c in seq(1, r - 1)) {
            g2   <- g_levels[r]
            g1   <- g_levels[c]
            n2   <- sum(g == g2); n1 <- sum(g == g1)
            diff <- mean(y[g == g2]) - mean(y[g == g1])
            se   <- sqrt(mse * (1 / n1 + 1 / n2))
            t0   <- diff / se
            p    <- 2 * stats::pt(-abs(t0), df = df_res)
            
            comparisons_fmt <- c(comparisons_fmt, paste(g2, "vs", g1))
            estimates       <- c(estimates, round(diff, 4))
            p_vals          <- c(p_vals, p)
          }
        }
        
        .posthoc_print_block(term_titles[i], comparisons_fmt, estimates, p_vals, alpha)
      }
    }
    
    cat(glue::glue("* p < \u03b1 = {a_text}\n\n"))
    
    return(invisible(NULL))
  }
  
  # ---------------------------------------------------------------------
  # One-way ANOVA / Kruskal-Wallis post-hoc (grouping supplied) — unchanged
  # ---------------------------------------------------------------------
  group_q <- grouping_q
  
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

#' Print one post-hoc comparison table (internal helper)
#'
#' @description Formats and prints a single block of pairwise comparisons
#'   (a title line followed by a comparison table), used by the two-way
#'   branch of \code{posthoc()} to print one block per term (A, B, and
#'   A:B when applicable). Not exported.
#'
#' @noRd
.posthoc_print_block <- function(title, comparisons_fmt, estimates, p_vals, alpha) {
  
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
  
  cat(title, "\n")
  cat(header, "\n")
  cat(sep, "\n")
  for (row in rows) cat(row, "\n")
  cat(sep, "\n\n")
}