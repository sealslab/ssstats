#' One-Way and Two-Way ANOVA
#'
#' @description Performs a one-way or two-way ANOVA. Prints a formatted ANOVA
#'   table and the relevant hypothesis test(s). For one-way ANOVA, the test for
#'   the single factor is printed. For two-way ANOVA with an interaction term,
#'   only the interaction test is printed. For two-way ANOVA without an
#'   interaction term, tests for both main effects are printed.
#'
#' @param data A data frame or tibble.
#' @param outcome Unquoted column name for the continuous outcome (numeric).
#' @param A Unquoted column name for factor A (the grouping variable in a
#'   one-way ANOVA, or the first factor in a two-way ANOVA).
#' @param B Unquoted column name for the second factor in a two-way ANOVA.
#'   Omit (or leave as `NULL`) for a one-way ANOVA.
#' @param interaction Logical. Only relevant when `B` is specified. If `TRUE`
#'   (default), fits the model `outcome ~ A * B` (main effects plus
#'   interaction). If `FALSE`, fits `outcome ~ A + B` (main effects only).
#' @param alpha Numeric. Significance level between 0 and 1 (exclusive).
#'   Defaults to `0.05`.
#'
#' @return Called for its side effect (printed output); returns `NULL`
#'   invisibly.
#'
#' @export
#' @importFrom rlang enquo as_name `!!` quo_is_null
#' @importFrom dplyr pull
#' @importFrom glue glue
anovas <- function(data,
                   outcome,
                   A,
                   B           = NULL,
                   interaction = TRUE,
                   alpha       = 0.05) {

  if (!is.logical(interaction) || length(interaction) != 1)
    stop("`interaction` must be TRUE or FALSE.", call. = FALSE)
  if (!is.numeric(alpha) || alpha <= 0 || alpha >= 1)
    stop("`alpha` must be numeric and between 0 and 1 (exclusive).", call. = FALSE)

  outcome_q <- rlang::enquo(outcome)
  A_q       <- rlang::enquo(A)
  B_q       <- rlang::enquo(B)

  outcome_name <- rlang::as_name(outcome_q)
  A_name       <- rlang::as_name(A_q)

  two_way <- !rlang::quo_is_null(B_q)

  if (!two_way && interaction) {
    message("`interaction` is ignored for one-way ANOVA.")
  }

  y <- dplyr::pull(data, !!outcome_q)
  a <- dplyr::pull(data, !!A_q)

  if (!is.numeric(y))
    stop(glue::glue("`{outcome_name}` must be a numeric column."), call. = FALSE)

  if (two_way) {
    B_name   <- rlang::as_name(B_q)
    b_raw    <- dplyr::pull(data, !!B_q)
    complete <- stats::complete.cases(y, a, b_raw)
  } else {
    complete <- stats::complete.cases(y, a)
  }

  n_dropped <- sum(!complete)
  if (n_dropped > 0) {
    message(glue::glue("{n_dropped} incomplete observation(s) removed before analysis."))
  }

  y <- y[complete]
  a <- as.factor(a[complete])

  if (nlevels(a) < 2)
    stop(glue::glue("`{A_name}` must have at least 2 levels."), call. = FALSE)

  # Build data sub-frame and formula
  data_sub <- data.frame(row.names = seq_along(y))
  data_sub[[outcome_name]] <- y
  data_sub[[A_name]]       <- a

  if (two_way) {
    b <- as.factor(b_raw[complete])

    if (nlevels(b) < 2)
      stop(glue::glue("`{B_name}` must have at least 2 levels."), call. = FALSE)

    data_sub[[B_name]] <- b

    if (interaction) {
      fmla  <- stats::as.formula(paste(outcome_name, "~", A_name, "*", B_name))
      title <- "Two-way ANOVA (with interaction)"
    } else {
      fmla  <- stats::as.formula(paste(outcome_name, "~", A_name, "+", B_name))
      title <- "Two-way ANOVA (no interaction)"
    }
  } else {
    fmla  <- stats::as.formula(paste(outcome_name, "~", A_name))
    title <- "One-way ANOVA"
  }

  fit     <- stats::aov(fmla, data = data_sub)
  aov_tbl <- summary(fit)[[1]]

  sources <- trimws(rownames(aov_tbl))
  dfs     <- aov_tbl[["Df"]]
  ss_vals <- aov_tbl[["Sum Sq"]]
  ms_vals <- aov_tbl[["Mean Sq"]]
  f_vals  <- aov_tbl[["F value"]]
  p_vals  <- aov_tbl[["Pr(>F)"]]

  # Pre-format all table values for dynamic column width calculation
  fmt_p_tbl <- function(p) {
    if (is.na(p)) return("")
    if (p < 0.001) "< 0.001" else sprintf("%.3f", p)
  }
  fmt_f_tbl <- function(f) {
    if (is.na(f)) return("") else sprintf("%.3f", f)
  }

  fmt_df  <- as.character(dfs)
  fmt_ss  <- sprintf("%.4f", ss_vals)
  fmt_ms  <- vapply(ms_vals, function(x) if (is.na(x)) "" else sprintf("%.4f", x), character(1))
  fmt_f   <- vapply(f_vals,  fmt_f_tbl, character(1))
  fmt_pv  <- vapply(p_vals,  fmt_p_tbl, character(1))

  # Dynamic column widths
  w_src <- max(nchar(c("Source",  sources)))
  w_df  <- max(nchar(c("df",     fmt_df)))
  w_ss  <- max(nchar(c("SS",     fmt_ss)))
  w_ms  <- max(nchar(c("MS",     fmt_ms)))
  w_f   <- max(nchar(c("F",      fmt_f)))
  w_p   <- max(nchar(c("p-value", fmt_pv)))

  sep_len <- w_src + w_df + w_ss + w_ms + w_f + w_p + 5
  sep     <- paste(rep("\u2500", sep_len), collapse = "")

  fmt_row <- function(src, df, ss, ms, f, p) {
    sprintf("%-*s  %*s  %*s  %*s  %*s  %*s",
            w_src, src,
            w_df,  df,
            w_ss,  ss,
            w_ms,  ms,
            w_f,   f,
            w_p,   p)
  }

  header <- fmt_row("Source", "df", "SS", "MS", "F", "p-value")

  tbl_rows <- vapply(seq_along(sources), function(i) {
    fmt_row(sources[i], fmt_df[i], fmt_ss[i], fmt_ms[i], fmt_f[i], fmt_pv[i])
  }, character(1))

  # --- ANOVA table output ---
  cat(glue::glue("{title}\n\n\n"))
  cat(header, "\n")
  cat(sep, "\n")
  for (row in tbl_rows) cat(row, "\n")
  cat(sep, "\n\n\n")

  # --- Hypothesis test(s) ---
  res_idx  <- which(sources == "Residuals")
  df_resid <- dfs[res_idx]

  print_ht <- function(source_label, term_idx, h0_text, h1_text) {
    f_stat <- round(f_vals[term_idx], 3)
    df_t   <- dfs[term_idx]
    p_val  <- p_vals[term_idx]
    p_text <- if (p_val < 0.001) "< 0.001" else sprintf("%.3f", p_val)

    cat(glue::glue("Hypotheses ({source_label}):\n\n"))
    cat(glue::glue("H\u2080: {h0_text}\n\n"))
    cat(glue::glue("H\u2081: {h1_text}\n\n"))
    cat(glue::glue("Test statistic: F({df_t}, {df_resid}) = {f_stat}\n\n"))
    cat(glue::glue("p-value: {p_text}\n\n"))
    conclusion(p_val, alpha)
  }

  if (!two_way) {

    # One-way: test factor A
    a_idx <- which(sources == A_name)
    print_ht(
      source_label = A_name,
      term_idx     = a_idx,
      h0_text      = glue::glue("All group means are equal (no effect of {A_name})"),
      h1_text      = glue::glue("At least one group mean differs (effect of {A_name} exists)")
    )

  } else if (interaction) {

    # Two-way with interaction: test A:B only
    ab_label <- paste0(A_name, ":", B_name)
    ab_idx   <- which(sources == ab_label)
    print_ht(
      source_label = ab_label,
      term_idx     = ab_idx,
      h0_text      = glue::glue("There is no interaction between {A_name} and {B_name}"),
      h1_text      = glue::glue("There is an interaction between {A_name} and {B_name}")
    )

  } else {

    # Two-way, no interaction: test A then B
    a_idx <- which(sources == A_name)
    b_idx <- which(sources == B_name)

    print_ht(
      source_label = A_name,
      term_idx     = a_idx,
      h0_text      = glue::glue("All means across levels of {A_name} are equal"),
      h1_text      = glue::glue("At least one mean across levels of {A_name} differs")
    )

    cat("\n")

    print_ht(
      source_label = B_name,
      term_idx     = b_idx,
      h0_text      = glue::glue("All means across levels of {B_name} are equal"),
      h1_text      = glue::glue("At least one mean across levels of {B_name} differs")
    )

  }

  invisible(NULL)
}
