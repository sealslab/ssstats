#' reg_check
#' 
#' Produces diagnostic plots to assess ANOVA (and regression) assumptions
#' visually from a fitted linear model object.
#' This includes residuals vs fitted values plot, histogram of residuals
#' with normal curve overlay, and Q-Q plot of standardized residuals.
#'
#' @title Diagnostic Plots for Regression Models
#' @description Creates diagnostic plots for a fitted regression model object or directly from data and variables.
#'
#' @param model Either a fitted regression model object of class \code{lm}, \code{glm}, or \code{aov}, 
#'              or \code{NULL} if using \code{data}, \code{continuous}, and \code{function_of}.
#' @param data (Optional) A data frame containing the data.
#' @param continuous (Optional) The numeric predictor or outcome column in \code{data}.
#' @param function_of (Optional) The grouping or explanatory column in \code{data}.
#'
#' @return A \code{ggpubr} arranged ggplot object containing three diagnostic plots.
#'
#' @examples
#' \dontrun{
#' # Using an lm() object
#' fit <- lm(bill_length_mm ~ bill_depth_mm, data = palmerpenguins::penguins)
#' reg_check(fit)
#'
#' # Using data, continuous and function_of
#' reg_check(data = palmerpenguins::penguins, 
#'           continuous = bill_length_mm, function_of = bill_depth_mm)
#' }
#'
#' @import ggplot2
#' @import ggpubr
#' @export

reg_check <- function(model = NULL, data = NULL, continuous = NULL, function_of = NULL) {
  # Check for model input or data-driven input
  if (!is.null(model)) {
    # Ensure the provided model is valid
    if (!inherits(model, c("lm", "glm", "aov"))) {
      stop("The `model` argument must be a regression model of class 'lm', 'glm', or 'aov'.")
    }
  } else if (!is.null(data) && !is.null(continuous) && !is.null(function_of)) {
    # Ensure data-driven inputs are provided correctly
    outcome_q <- rlang::enquo(continuous)
    group_q <- rlang::enquo(function_of)
    
    # Prepare formula for lm() using data and columns
    outcome_chr <- rlang::as_name(outcome_q)
    group_chr <- rlang::as_name(group_q)
    formula <- as.formula(paste0(outcome_chr, " ~ ", group_chr))
    model <- lm(formula, data = data)
  } else {
    stop("Must provide either a `model` object or `data`, `continuous`, and `function_of`.")
  }
  
  # Extract residuals and fitted values
  res <- residuals(model)
  fitted <- fitted.values(model)
  stdresid <- rstandard(model)  # Standardized residuals
  
  # Create data frame for plotting
  df <- data.frame(fitted = fitted, resid = res, stdresid = stdresid)
  
  # Residuals vs Fitted plot
  res_fitted <- ggplot2::ggplot(df, ggplot2::aes(x = fitted, y = resid)) +
    ggplot2::geom_point(color = "#C5C4C4") +
    ggplot2::labs(y = "Residuals", x = "Fitted values",
                  title = "Scatterplot of Residuals") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                        color = "black", linewidth = 1) +
    ggplot2::theme_classic()
  
  # Histogram of residuals with normal curve
  res_his <- ggplot2::ggplot(df, ggplot2::aes(x = resid)) +
    ggplot2::geom_histogram(aes(y = after_stat(density)),
                            colour = "black", fill = "#C5C4C4", bins = 30) +
    ggplot2::stat_function(fun = stats::dnorm,
                           args = list(mean = mean(res), sd = sd(res)),
                           color = "black", linewidth = 1) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                        color = "black", linewidth = 1.5) +
    ggplot2::labs(x = "Residuals", y = "Density", title = "Histogram of Residuals") +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.ticks.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank())
  
  # Q-Q plot of standardized residuals
  qq_res <- ggplot2::ggplot(df, ggplot2::aes(sample = stdresid)) +
    ggplot2::stat_qq(color = "#C5C4C4") +
    ggplot2::stat_qq_line(color = "black", linewidth = 1, linetype = "dashed") +
    ggplot2::labs(x = "Theoretical Quantiles", y = "Standardized Residuals",
                  title = "Q-Q Plot of Standardized Residuals") +
    ggplot2::theme_classic()
  
  # Arrange plots together
  out <- ggpubr::ggarrange(qq_res, res_his, res_fitted, ncol = 2, nrow = 2,
                           labels = c("A", "B", "C"))
  
  return(out)
}

#' @title Residuals vs Fitted Diagnostic Plot
#' @description Creates a residuals vs fitted values scatterplot for a fitted regression model object or directly from data and variables.
#'
#' @param model Either a fitted regression model object of class \code{lm}, \code{glm}, or \code{aov}, 
#'              or \code{NULL} if using \code{data}, \code{continuous}, and \code{function_of}.
#' @param data (Optional) A data frame containing the data.
#' @param continuous (Optional) The numeric predictor or outcome column in \code{data}.
#' @param function_of (Optional) The grouping or explanatory column in \code{data}.
#'
#' @return A \code{ggplot2} object representing the scatterplot of residuals vs fitted values.
#'
#' @examples
#' \dontrun{
#' # Using an lm() object
#' fit <- lm(bill_length_mm ~ bill_depth_mm, data = palmerpenguins::penguins)
#' resid_fitted_plot(fit)
#'
#' # Using data, continuous and function_of
#' resid_fitted_plot(data = palmerpenguins::penguins, 
#'                   continuous = bill_length_mm, function_of = bill_depth_mm)
#' }
#'
#' @import ggplot2
#' @export

variance_check <- function(model = NULL, data = NULL, continuous = NULL, function_of = NULL) {
  # Check for model input or data-driven input
  if (!is.null(model)) {
    # Ensure the provided model is valid
    if (!inherits(model, c("lm", "glm", "aov"))) {
      stop("The `model` argument must be a regression model of class 'lm', 'glm', or 'aov'.")
    }
  } else if (!is.null(data) && !is.null(continuous) && !is.null(function_of)) {
    # Ensure data-driven inputs are provided correctly
    outcome_q <- rlang::enquo(continuous)
    group_q <- rlang::enquo(function_of)
    
    # Prepare formula for lm() using data and columns
    outcome_chr <- rlang::as_name(outcome_q)
    group_chr <- rlang::as_name(group_q)
    formula <- as.formula(paste0(outcome_chr, " ~ ", group_chr))
    model <- lm(formula, data = data)
  } else {
    stop("Must provide either a `model` object or `data`, `continuous`, and `function_of`.")
  }
  
  # Create residuals vs fitted plot
  df <- data.frame(
    fitted = fitted.values(model),
    resid = residuals(model)
  )
  
  res_fitted <- ggplot2::ggplot(df, ggplot2::aes(x = fitted, y = resid)) +
    ggplot2::geom_point(color = "#C5C4C4") +
    ggplot2::labs(y = "Residuals", x = "Fitted values",
                  title = "Scatterplot of Residuals vs Fitted") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                        color = "black", linewidth = 1) +
    ggplot2::theme_classic()
  
  return(res_fitted)
}



#' Summarize numeric variables with mean (SD) and median (IQR)
#'
#' @param data A data frame containing variables to summarize.
#' @param ... One or more unquoted numeric variables to summarize.
#'
#' @return A tibble with columns: variable, mean_sd, median_iqr.
#' @importFrom dplyr summarise across
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr arrange
#' @export
mean_median <- function(data, ...) {
  summary_tbl <- data %>%
    summarise(
      across(
        c(...),
        list(
          mean_sd = ~ sprintf("%.1f (%.1f)", mean(.x, na.rm = TRUE), sd(.x, na.rm = TRUE)),
          median_iqr = ~ sprintf("%.1f (%.1f)", median(.x, na.rm = TRUE), IQR(.x, na.rm = TRUE))
        ),
        .names = "{.col}__{.fn}"
      ),
      .groups = "drop"
    ) %>%
    pivot_longer(
      cols = matches("__"),
      names_to = c("variable", ".value"),
      names_sep = "__"
    ) %>%
    arrange(variable)

  print(summary_tbl, n = Inf)

  invisible(summary_tbl)
}

#' Compute Counts and Percentages for One- or Two-Way Tables
#'
#' @param data A data frame or tibble.
#' @param row_var A column name (unquoted) for the row variable.
#' @param col_var Optional column name (unquoted) for the column variable. If NULL, a one-way table is returned.
#' @param rows Number of rows to print (default is 3).
#' @param digits Number of decimal places for percentages (default is 1).
#' @param denom Denominator for percentage calculation in two-way tables; one of "row", "col", or "all" (default is "col").
#' @importFrom dplyr slice_head left_join
#' @importFrom tidyr pivot_wider
#' @return A tibble with counts and percentages formatted as "n (pct)".
#' @examples
#' \dontrun{
#' n_pct(mtcars, cyl)
#' n_pct(mtcars, cyl, gear)
#' }
#' @export
n_pct <- function(data, row_var, col_var = NULL,
                  rows = NULL, digits = 1, denom = "col",
                  default_rows = 10, require_rows_if_truncated = TRUE) {
  
  row_var_enquo <- rlang::enquo(row_var)
  col_var_enquo <- rlang::enquo(col_var)
  
  rows_user_supplied <- !is.null(rows)
  if (is.null(rows)) rows <- default_rows
  
  if (rlang::quo_is_null(col_var_enquo)) {
    tab_n <- janitor::tabyl(data, !!row_var_enquo)
    total_n <- sum(tab_n[[2]])
    
    tab_oneway <- tab_n %>%
      dplyr::mutate(
        pct = .[[2]] / total_n,
        pct_fmt = scales::percent(pct, accuracy = 10^(-digits)),
        `n (pct)` = paste0(.[[2]], " (", pct_fmt, ")")
      ) %>%
      dplyr::select(!!rlang::as_name(row_var_enquo), `n (pct)`)
    
    n_total <- nrow(tab_oneway)
    
    if (require_rows_if_truncated && !rows_user_supplied && n_total > rows) {
      stop(
        sprintf(
          "n_pct(): showing first %d of %d rows. Re-run with rows = %d (or larger).",
          rows, n_total, n_total
        ),
        call. = FALSE
      )
    }
    
    tab_oneway %>% dplyr::slice_head(n = rows)
    
  } else {
    tab_n <- janitor::tabyl(data, !!row_var_enquo, !!col_var_enquo)
    tab_pct <- janitor::adorn_percentages(tab_n, denominator = denom)
    tab_fmt <- janitor::adorn_pct_formatting(tab_pct, digits = digits)
    
    row_var_name <- rlang::as_name(row_var_enquo)
    
    tab_n_long <- tab_n %>%
      tidyr::pivot_longer(-1, names_to = "col", values_to = "n")
    
    tab_fmt_long <- tab_fmt %>%
      tidyr::pivot_longer(-1, names_to = "col", values_to = "pct")
    
    tab_joined <- dplyr::left_join(tab_n_long, tab_fmt_long, by = c(row_var_name, "col")) %>%
      dplyr::mutate(`n (pct)` = paste0(n, " (", pct, ")")) %>%
      dplyr::select(dplyr::all_of(row_var_name), col, `n (pct)`) %>%
      tidyr::pivot_wider(names_from = col, values_from = `n (pct)`)
    
    n_total <- nrow(tab_joined)
    
    if (require_rows_if_truncated && !rows_user_supplied && n_total > rows) {
      stop(
        sprintf(
          "n_pct(): showing first %d of %d rows. Re-run with rows = %d (or larger).",
          rows, n_total, n_total
        ),
        call. = FALSE
      )
    }
    
    tab_joined %>% dplyr::slice_head(n = rows)
  }
}



#' Calculate Mean and Confidence Interval for a Continuous Variable
#'
#' @param data A data frame or tibble.
#' @param continuous A continuous variable (unquoted) for which to calculate mean and confidence interval.
#' @param confidence Confidence level for the interval (default 0.95).
#' @importFrom tibble tibble
#' @return A tibble with mean, standard deviation, confidence level, and confidence interval bounds.
#' @examples
#' \dontrun{
#' one_mean_CI(mtcars, mpg)
#' }
#' @export
one_mean_CI <- function(data, continuous, confidence = 0.95) {
  var_values <- data %>% dplyr::pull({{ continuous }})
  ttest <- t.test(var_values, conf.level = confidence)
  
  mean_val <- mean(var_values, na.rm = TRUE)
  sd_val <- sd(var_values, na.rm = TRUE)
  ci_lower <- ttest$conf.int[1]
  ci_upper <- ttest$conf.int[2]
  conf_percent <- round(confidence * 100)
  
  cat(glue::glue("Sample mean: x̄ = {round(mean_val, 4)}\n\n"))
  cat(glue::glue("Sample standard deviation: s = {round(sd_val, 4)}\n\n"))
  cat(glue::glue("{conf_percent}% confidence interval for the mean: ({round(ci_lower, 4)}, {round(ci_upper, 4)})\n\n"))
}

#' Calculate Proportion and Confidence Interval for a Binary Outcome
#'
#' @param data A data frame or tibble.
#' @param binary A binary grouping variable (unquoted).
#' @param event The value in `binary` considered a "success" or the "event of interest" (can be numeric, character, or factor).
#' @param confidence Confidence level for the interval (default 0.95).
#' @importFrom tibble tibble
#' @return A tibble with proportion estimate, number of successes, sample size, confidence level, and confidence interval bounds.
#' @examples
#' one_prop_CI(mtcars, am, event = 1)
#' @export
one_prop_CI <- function(data, binary, event, confidence = 0.95) {
  binary_vector <- data %>% dplyr::pull({{ binary }}) %>% na.omit()
  
  # Convert to character for consistent comparison
  binary_vector_chr <- as.character(binary_vector)
  success_chr <- as.character(event)
  
  n <- length(binary_vector_chr)
  x <- sum(binary_vector_chr == success_chr)
  p_hat <- x / n
  
  test <- prop.test(x = x, n = n, conf.level = confidence, correct = FALSE)
  ci_lower <- test$conf.int[1]
  ci_upper <- test$conf.int[2]
  conf_percent <- round(confidence * 100)
  
  cat(glue::glue("p̂= {round(p_hat, 3)} ({x}/{n})\n\n"))
  cat(glue::glue("{conf_percent}% confidence interval for π: ({round(ci_lower, 4)}, {round(ci_upper, 4)})\n"))
}

#' Get Symbol Corresponding to Alternative Hypothesis
#'
#' @param alt Character string specifying alternative hypothesis. One of "less", "greater", or "two.sided".
#' @return A character symbol ("<", ">", or "!=") representing the alternative.
alt_symbol <- function(alt) {
  if (alt == "less") return("<")
  if (alt == "greater") return(">")
  return("≠")
}

#' Get Symbol Corresponding to Null Hypothesis
#'
#' @param alt Character string specifying alternative hypothesis. One of "less", "greater", or "two.sided".
#' @return A character symbol ("≥", "≤", or "=") representing the null.
null_symbol <- function(alt) {
  if (alt == "less") return("≥")
  if (alt == "greater") return("≤")
  return("=")
}

#' Print Conclusion Based on p-value and Significance Level
#'
#' @param p_val Numeric p-value.
#' @param alpha Numeric significance level.
#' @return None. Prints conclusion to console.
#' @export
conclusion <- function(p_val, alpha) {
  p_text <- if (p_val < 0.001) "< 0.001" else as.character(round(p_val, 3))

  if (p_val < alpha) {
    cat(glue::glue("Conclusion: Reject the null hypothesis (p = {p_text} < α = {alpha})\n\n"))
  } else {
    cat(glue::glue("Conclusion: Fail to reject the null hypothesis (p = {p_text} ≥ α = {alpha})\n\n"))
  }
}

#' One-sample t-test for a population mean
#'
#' @param data Data frame or tibble.
#' @param continuous Unquoted column name for the continuous variable.
#' @param mu Numeric hypothesized mean value (default 0).
#' @param alternative Character string specifying alternative hypothesis; "two.sided", "less", or "greater" (default "two.sided").
#' @param alpha Numeric significance level (default 0.05).
#' @return None. Prints formatted test results and conclusion.
#' @export
one_mean_HT <- function(data, continuous, mu = 0, alternative = "two.sided", alpha = 0.05) {
  var_values <- data %>% dplyr::pull({{ continuous }}) %>% na.omit()

  ttest <- t.test(var_values, mu = mu, alternative = alternative, conf.level = 1 - alpha)

  t_stat <- round(ttest$statistic, 3)
  p_val <- ttest$p.value
  df <- ttest$parameter
  p_text <- if (p_val < 0.001) "p < 0.001" else glue::glue("p = {formatC(round(p_val, 3), format = 'f', digits = 3)}")

  cat(glue::glue("One-sample t-test for the population mean:\n\n"))
  cat(glue::glue("Null: H0: μ {null_symbol(alternative)} {mu}\n\n"))
  cat(glue::glue("Alternative: H1: μ {alt_symbol(alternative)} {mu}\n\n"))
  cat(glue::glue("Test statistic: t({df}) = {t_stat}\n\n"))
  cat(glue::glue("p-value: {p_text}\n\n"))
  conclusion(p_val, alpha)
}

#' One-sample z-test for a population proportion
#'
#' @param data Data frame or tibble.
#' @param binary Unquoted column name for the binary variable.
#' @param event Character value indicating which level is considered "success" or the "event of interest".
#' @param p Numeric hypothesized proportion (default 0.5).
#' @param alternative Character string specifying alternative hypothesis; "two.sided", "less", or "greater" (default "two.sided").
#' @param alpha Numeric significance level (default 0.05).
#' @return None. Prints formatted test results and conclusion.
#' @export
one_prop_HT <- function(data, binary, event, p = 0.5, alternative = "two.sided", alpha = 0.05) {
  binary_vector <- data %>% dplyr::pull({{ binary }}) %>% na.omit()
  binary_vector_chr <- as.character(binary_vector)
  success_chr <- as.character(event)

  n <- length(binary_vector_chr)
  x <- sum(binary_vector_chr == success_chr)

  test <- prop.test(x = x, n = n, p = p, alternative = alternative, correct = FALSE)

  z_stat <- round(sqrt(test$statistic), 2)  # chi-sq stat is z^2
  p_val <- test$p.value
  p_text <- if (p_val < 0.001) "p < 0.001" else glue::glue("p = {formatC(round(p_val, 3), format = 'f', digits = 3)}")

  cat(glue::glue("One-sample z-test for the population proportion:\n\n"))
  cat(glue::glue("Null: H0: π {null_symbol(alternative)} {p}\n\n"))
  cat(glue::glue("Alternative: H1: π {alt_symbol(alternative)} {p}\n\n"))
  cat(glue::glue("Test statistic: z = {z_stat}\n\n"))
  cat(glue::glue("p-value: {p_text}\n\n"))
  conclusion(p_val, alpha)
}

#' Independent samples mean confidence interval (equal variance assumed)
#'
#' @param data Data frame or tibble.
#' @param continuous Unquoted column name for continuous outcome.
#' @param grouping Unquoted column name for grouping variable (factor or character).
#' @param confidence Confidence level for interval (default 0.95).
#' @param variance Character string; "equal" (default) or "unequal" variance assumption.
#' @param reference Optional character string naming the reference group. Must be quoted and
#'   match one of the grouping values exactly. If provided, the output and CI will be reported
#'   as μ[reference] - μ[other]. If NULL (default) the order is determined by:
#'   - if grouping is a factor, the factor level order (only levels present after NA removal),
#'   - otherwise, the order of first appearance in the data after NA removal.
#' @return None. Prints the confidence interval and point estimate.
#' @export
independent_mean_CI <- function(data,
                                continuous,
                                grouping,
                                confidence = 0.95,
                                variance = "equal",
                                reference = NULL) {
  grouping_q  <- rlang::enquo(grouping)
  continuous_q <- rlang::enquo(continuous)
  
  # validate confidence
  if (!is.numeric(confidence) || confidence <= 0 || confidence >= 1) {
    stop("`confidence` must be a numeric value strictly between 0 and 1.")
  }
  
  var_equal <- ifelse(variance == "equal", TRUE, FALSE)
  
  df <- data %>%
    dplyr::select(!!grouping_q, !!continuous_q) %>%
    tidyr::drop_na()
  
  grp_name <- rlang::quo_name(grouping_q)
  cont_name <- rlang::quo_name(continuous_q)
  
  # Ensure exactly two groups present after NA removal
  groups_present <- unique(df[[grp_name]])
  groups_chr <- as.character(groups_present)
  if (length(groups_chr) != 2) {
    stop("Grouping variable must have exactly two levels after removing NAs.")
  }
  
  # Determine order: respect factor levels if provided, otherwise first appearance.
  if (is.factor(df[[grp_name]])) {
    orig_levels <- base::levels(df[[grp_name]])
    levels_order <- orig_levels[orig_levels %in% groups_chr]
    if (length(levels_order) != 2) {
      # fallback to appearance order if factor levels do not produce exactly two present levels
      levels_order <- groups_chr
    }
  } else {
    levels_order <- groups_chr
  }
  
  # If reference supplied, validate and set as first level
  if (!is.null(reference)) {
    if (!is.character(reference) || length(reference) != 1) {
      stop("`reference` must be a single quoted character string matching a group value.")
    }
    if (!(reference %in% levels_order)) {
      stop(glue::glue("`reference` '{reference}' not found among grouping values: {paste(levels_order, collapse = ', ')}"))
    }
    other <- setdiff(levels_order, reference)
    levels_order <- c(reference, other)
  }
  
  # Relevel the grouping column so that t.test and our displays follow the chosen order
  df[[grp_name]] <- factor(as.character(df[[grp_name]]), levels = levels_order)
  
  # Check sample sizes
  group_counts <- df %>% dplyr::group_by(!!grouping_q) %>% dplyr::summarise(n = dplyr::n(), .groups = "drop")
  if (any(group_counts$n < 2)) {
    bad <- group_counts %>% dplyr::filter(n < 2)
    stop(glue::glue("Each group must have at least 2 non-missing observations. Problem group(s): {paste(bad[[grp_name]], collapse=', ')}"))
  }
  
  # Compute group summaries (we'll use these for display and for the point estimate)
  grp_stats <- df %>%
    dplyr::group_by(!!grouping_q) %>%
    dplyr::summarise(
      n = dplyr::n(),
      mean = mean(!!continuous_q),
      sd = stats::sd(!!continuous_q),
      .groups = "drop"
    )
  
  g1 <- levels_order[1]
  g2 <- levels_order[2]
  
  mean1 <- grp_stats %>% dplyr::filter(!!grouping_q == g1) %>% dplyr::pull(mean)
  mean2 <- grp_stats %>% dplyr::filter(!!grouping_q == g2) %>% dplyr::pull(mean)
  
  # Run the t.test with the chosen order and var.equal mapping
  fml <- stats::reformulate(termlabels = grp_name, response = cont_name)
  ttest <- stats::t.test(
    formula    = fml,
    data       = df,
    conf.level = confidence,
    var.equal  = var_equal
  )
  
  # Format outputs: means 2 digits, ttest-related values keep previous precision choices
  mean1_f <- formatC(round(mean1, 2), format = "f", digits = 2)
  mean2_f <- formatC(round(mean2, 2), format = "f", digits = 2)
  mean_diff <- formatC(round(mean1 - mean2, 2), format = "f", digits = 2)
  
  ci_lower <- formatC(round(ttest$conf.int[1], 4), format = "f", digits = 4)
  ci_upper <- formatC(round(ttest$conf.int[2], 4), format = "f", digits = 4)
  
  conf_pct <- round(confidence * 100)
  
  cat(glue::glue("The point estimate for the difference in means is x̄[{g1}] − x̄[{g2}] = {mean_diff}\n\n"))
  cat(glue::glue("Mean of {g1}: {mean1_f}\n\n"))
  cat(glue::glue("Mean of {g2}: {mean2_f}\n\n"))
  cat(glue::glue("The {conf_pct}% confidence interval for μ[{g1}] − μ[{g2}] is ({ci_lower}, {ci_upper})\n"))
  
  invisible(NULL)
}

#' Independent samples t-test for mean difference
#'
#' @param data Data frame or tibble.
#' @param continuous Unquoted column name for continuous outcome.
#' @param grouping Unquoted column name for grouping variable (factor or character).
#' @param alternative Character string specifying alternative hypothesis; "two.sided", "two",
#'   "less", or "greater" (default "two.sided"). "two" is accepted as an alias for "two.sided".
#' @param mu Numeric hypothesized mean difference (default 0).
#' @param alpha Numeric significance level (default 0.05).
#' @param variance Character string; "equal" (default) or "unequal" variance assumption.
#' @param reference Optional character string naming the reference group. Must be quoted and
#'   match one of the grouping values exactly. See independent_mean_CI for ordering rules.
#' @return None. Prints formatted test results and conclusion.
#' @export
independent_mean_HT <- function(data,
                                continuous,
                                grouping,
                                alternative = "two.sided",
                                mu = 0,
                                alpha = 0.05,
                                variance = "equal",
                                reference = NULL) {
  grouping_q   <- rlang::enquo(grouping)
  continuous_q <- rlang::enquo(continuous)
  
  # Normalize and validate alternative (accept "two" as alias)
  alt_norm <- tolower(alternative)
  if (alt_norm == "two") alt_norm <- "two.sided"
  if (!alt_norm %in% c("two.sided", "less", "greater")) {
    stop('`alternative` must be one of "two", "two.sided", "less", or "greater".')
  }
  
  var_equal <- ifelse(variance == "equal", TRUE, FALSE)
  
  df <- data %>%
    dplyr::select(!!grouping_q, !!continuous_q) %>%
    tidyr::drop_na()
  
  grp_name  <- rlang::quo_name(grouping_q)
  cont_name <- rlang::quo_name(continuous_q)
  
  # Ensure exactly two groups present after NA removal
  groups_present <- unique(df[[grp_name]])
  groups_chr <- as.character(groups_present)
  if (length(groups_chr) != 2) {
    stop("Grouping variable must have exactly two levels after removing NAs.")
  }
  
  # Determine order: respect factor levels if provided, otherwise first appearance.
  if (is.factor(df[[grp_name]])) {
    orig_levels <- base::levels(df[[grp_name]])
    levels_order <- orig_levels[orig_levels %in% groups_chr]
    if (length(levels_order) != 2) {
      # fallback to appearance order
      levels_order <- groups_chr
    }
  } else {
    levels_order <- groups_chr
  }
  
  # If reference supplied, validate and set as SECOND level
  # so the reported difference is (other − reference)
  if (!is.null(reference)) {
    if (!is.character(reference) || length(reference) != 1) {
      stop("`reference` must be a single quoted character string matching a group value.")
    }
    if (!(reference %in% levels_order)) {
      stop(glue::glue("`reference` '{reference}' not found among grouping values: {paste(levels_order, collapse = ', ')}"))
    }
    other <- setdiff(levels_order, reference)
    # other is now g1, reference is g2
    levels_order <- c(other, reference)
  }
  
  # Relevel grouping to chosen order
  df[[grp_name]] <- factor(as.character(df[[grp_name]]), levels = levels_order)
  
  # Check sample sizes
  group_counts <- df %>% dplyr::group_by(!!grouping_q) %>% dplyr::summarise(n = dplyr::n(), .groups = "drop")
  if (any(group_counts$n < 2)) {
    bad <- group_counts %>% dplyr::filter(n < 2)
    stop(glue::glue("Each group must have at least 2 non-missing observations. Problem group(s): {paste(bad[[grp_name]], collapse=', ')}"))
  }
  
  # Compute group summaries
  grp_stats <- df %>%
    dplyr::group_by(!!grouping_q) %>%
    dplyr::summarise(
      n = dplyr::n(),
      mean = mean(!!continuous_q),
      sd = stats::sd(!!continuous_q),
      .groups = "drop"
    )
  
  g1 <- levels_order[1]
  g2 <- levels_order[2]
  
  mean1 <- grp_stats %>% dplyr::filter(!!grouping_q == g1) %>% dplyr::pull(mean)
  mean2 <- grp_stats %>% dplyr::filter(!!grouping_q == g2) %>% dplyr::pull(mean)
  
  # Run the t.test
  fml <- stats::reformulate(termlabels = grp_name, response = cont_name)
  ttest <- stats::t.test(
    formula    = fml,
    data       = df,
    alternative= alt_norm,
    mu         = mu,
    conf.level = 1 - alpha,
    var.equal  = var_equal
  )
  
  t_stat  <- formatC(round(as.numeric(ttest$statistic), 3), format = "f", digits = 3)
  df_val  <- formatC(round(as.numeric(ttest$parameter), 2), format = "f", digits = 2)
  p_val   <- as.numeric(ttest$p.value)
  
  # Format means and difference (means -> 2 digits)
  mean1_f <- formatC(round(mean1, 2), format = "f", digits = 2)
  mean2_f <- formatC(round(mean2, 2), format = "f", digits = 2)
  mean_diff <- formatC(round(mean1 - mean2, 2), format = "f", digits = 2)
  
  # p-value text
  p_text <- if (p_val < 0.001) {
    "p < 0.001"
  } else {
    glue::glue("p = {formatC(round(p_val, 3), format = 'f', digits = 3)}")
  }
  
  # Hypothesis text
  null_text <- switch(
    alt_norm,
    two.sided = glue::glue("H₀: μ[{g1}] − μ[{g2}] = {mu}"),
    less = glue::glue("H₀: μ[{g1}] − μ[{g2}] ≥ {mu}"),
    greater = glue::glue("H₀: μ[{g1}] − μ[{g2}] ≤ {mu}")
  )
  alt_text  <- switch(
    alt_norm,
    two.sided = glue::glue("H₁: μ[{g1}] − μ[{g2}] ≠ {mu}"),
    less      = glue::glue("H₁: μ[{g1}] − μ[{g2}] < {mu}"),
    greater   = glue::glue("H₁: μ[{g1}] − μ[{g2}] > {mu}")
  )
  
  test_type <- if (variance == "equal") {
    "Two-sample t-test for two independent means and equal variance:"
  } else {
    "Two-sample t-test for two independent means and unequal variance:"
  }
  
  cat(glue::glue("{test_type}\n\n"))
  cat(glue::glue("The point estimate for the difference in means is x̄[{g1}] − x̄[{g2}] = {mean_diff}\n\n"))
  cat(glue::glue("Mean of {g1}: {mean1_f}\n\n"))
  cat(glue::glue("Mean of {g2}: {mean2_f}\n\n"))
  cat(glue::glue("Null: {null_text}\n\n"))
  cat(glue::glue("Alternative: {alt_text}\n\n"))
  cat(glue::glue("Test statistic: t({df_val}) = {t_stat}\n\n"))
  cat(glue::glue("p-value: {p_text}\n\n"))
  
  # Keep existing conclusion() call from package
  conclusion(p_val, alpha)
  
  invisible(NULL)
}




#' Paired mean/median summary table
#'
#' Produces a simple printed table (not a tibble) summarizing mean (SD) and
#' median (IQR) for the "before", "after", and their difference (before - after).
#'
#' @param data Data frame or tibble.
#' @param col1 Unquoted column name for "before" values.
#' @param col2 Unquoted column name for "after" values.
#' @param accuracy Integer rounding digits for summary statistics (default 3).
#' @return None — prints a table and returns invisibly NULL.
#' @importFrom dplyr select
#' @importFrom tidyr drop_na
#' @export
dependent_mean_median <- function(data, col1, col2, accuracy = 3) {
  before_q <- rlang::enquo(col1)
  after_q  <- rlang::enquo(col2)
  
  # Basic input validation
  if (!is.numeric(accuracy) || length(accuracy) != 1 || accuracy < 0) {
    stop("`accuracy` must be a single non-negative integer.")
  }
  accuracy <- as.integer(accuracy)
  
  # Pull and keep only complete pairs
  df <- data %>%
    dplyr::select(!!before_q, !!after_q) %>%
    tidyr::drop_na()
  
  before_name <- rlang::quo_name(before_q)
  after_name  <- rlang::quo_name(after_q)
  
  n_pairs <- nrow(df)
  
  if (n_pairs == 0) {
    stop("No complete pairs available after removing missing values.")
  }
  
  # Helper: prefer a variable 'label' attribute if present, otherwise use the name
  get_var_label <- function(vec, default_name) {
    lbl <- attr(vec, "label", exact = TRUE)
    if (!is.null(lbl) && nzchar(as.character(lbl))) {
      return(as.character(lbl))
    }
    return(default_name)
  }
  
  before_label <- get_var_label(data[[before_name]], before_name)
  after_label  <- get_var_label(data[[after_name]], after_name)
  diff_label   <- paste0(before_label, " – ", after_label)
  
  before_vals <- df[[before_name]]
  after_vals  <- df[[after_name]]
  diff_vals   <- before_vals - after_vals
  
  # Compute statistics (na.rm = TRUE not needed after drop_na but kept for safety)
  m1   <- mean(before_vals, na.rm = TRUE)
  s1   <- stats::sd(before_vals, na.rm = TRUE)
  med1 <- stats::median(before_vals, na.rm = TRUE)
  iqr1 <- stats::IQR(before_vals, na.rm = TRUE)
  
  m2   <- mean(after_vals, na.rm = TRUE)
  s2   <- stats::sd(after_vals, na.rm = TRUE)
  med2 <- stats::median(after_vals, na.rm = TRUE)
  iqr2 <- stats::IQR(after_vals, na.rm = TRUE)
  
  m_diff   <- mean(diff_vals, na.rm = TRUE)
  s_diff   <- stats::sd(diff_vals, na.rm = TRUE)
  med_diff <- stats::median(diff_vals, na.rm = TRUE)
  iqr_diff <- stats::IQR(diff_vals, na.rm = TRUE)
  
  # Formatter that preserves NA as "NA" and formats numbers to fixed decimals
  fmt <- function(x) {
    if (is.na(x)) return("NA")
    formatC(round(x, accuracy), format = "f", digits = accuracy)
  }
  
  mean_sd_before <- paste0(fmt(m1), " (", fmt(s1), ")")
  mean_sd_after  <- paste0(fmt(m2), " (", fmt(s2), ")")
  mean_sd_diff   <- paste0(fmt(m_diff), " (", fmt(s_diff), ")")
  
  med_iqr_before <- paste0(fmt(med1), " (", fmt(iqr1), ")")
  med_iqr_after  <- paste0(fmt(med2), " (", fmt(iqr2), ")")
  med_iqr_diff   <- paste0(fmt(med_diff), " (", fmt(iqr_diff), ")")
  
  # Build plain data.frame in requested order: before, after, difference
  out <- data.frame(
    Variable = c(before_label, after_label, diff_label),
    `Mean (SD)` = c(mean_sd_before, mean_sd_after, mean_sd_diff),
    `Median (IQR)` = c(med_iqr_before, med_iqr_after, med_iqr_diff),
    stringsAsFactors = FALSE
  )
  
  # If too few pairs for some statistics, warn but still print the table.
  if (n_pairs < 2) {
    warning(
      sprintf(
        "Only %d complete pair(s) available. Some statistics (e.g., SD or IQR) require >= 2 observations and may be NA.",
        n_pairs
      ),
      call. = FALSE
    )
  }
  
  # Print a short heading with the number of complete pairs, then the plain table
  cat(sprintf("Number of complete pairs: %d\n\n", n_pairs))
  # Use base print to get the "plain" console-style table
  print(out, row.names = FALSE)
  
  invisible(NULL)
}

#' Paired mean difference confidence interval
#'
#' @param data Data frame or tibble.
#' @param col1 Unquoted column name for first paired measurement.
#' @param col2 Unquoted column name for second paired measurement.
#' @param confidence Numeric confidence level (default 0.95).
#' @return None. Prints point estimate, standard deviation, and confidence interval.
#' @export
dependent_mean_CI <- function(data, col1, col2, confidence = 0.95) {
  # Pull vectors
  x <- data %>% dplyr::pull({{ col1 }})
  y <- data %>% dplyr::pull({{ col2 }})
  
  # Only keep complete pairs
  valid <- complete.cases(x, y)
  x <- x[valid]
  y <- y[valid]
  
  # Group names as strings
  group1 <- rlang::as_name(rlang::enquo(col1))
  group2 <- rlang::as_name(rlang::enquo(col2))
  
  # Compute differences: always x - y (col1 - col2)
  differences <- x - y
  
  ttest <- t.test(x, y, paired = TRUE, conf.level = confidence)
  
  mean_diff <- mean(differences)
  sd_diff <- sd(differences)
  ci_lower <- round(ttest$conf.int[1], 4)
  ci_upper <- round(ttest$conf.int[2], 4)
  mean_rounded <- round(mean_diff, 4)
  sd_rounded <- round(sd_diff, 4)
  conf_percent <- round(confidence * 100)
  
  cat(glue::glue("The point estimate for the mean difference is x̄[{group1}] − x̄[{group2}] = {mean_rounded}.\n\n"))
  cat(glue::glue("The standard deviation of differences is s = {sd_rounded}.\n\n"))
  cat(glue::glue("The {conf_percent}% confidence interval for μ[{group1}] − μ[{group2}] is ({ci_lower}, {ci_upper}).\n\n"))
}

#' Paired mean difference hypothesis test
#'
#' @param data Data frame or tibble.
#' @param col1 Unquoted column name for first paired measurement.
#' @param col2 Unquoted column name for second paired measurement.
#' @param alternative Character string specifying alternative hypothesis; "two.sided", "less", or "greater" (default "two.sided").
#' @param mu Numeric hypothesized mean difference (default 0).
#' @param alpha Numeric significance level (default 0.05).
#' @return None. Prints formatted test results and conclusion.
#' @importFrom dplyr pull
#' @export
dependent_mean_HT <- function(data,
                              col1,
                              col2,
                              alternative = "two.sided",
                              mu = 0,
                              alpha = 0.05) {
  # Pull vectors
  x <- data %>% dplyr::pull({{ col1 }})
  y <- data %>% dplyr::pull({{ col2 }})
  
  # Only keep complete pairs
  complete <- complete.cases(x, y)
  x <- x[complete]
  y <- y[complete]
  
  # Get group names as strings
  group1 <- rlang::as_name(rlang::enquo(col1))
  group2 <- rlang::as_name(rlang::enquo(col2))
  
  # Paired t-test
  ttest <- t.test(x, y,
                  paired = TRUE,
                  alternative = alternative,
                  mu = mu,
                  conf.level = 1 - alpha)
  
  t_stat  <- round(ttest$statistic, 3)
  df      <- round(as.numeric(ttest$parameter), 2)
  p_val   <- ttest$p.value
  mean_d  <- round(mean(x - y), 4)
  
  # Use helpers for symbols
  null_sym <- null_symbol(alternative)
  alt_sym  <- alt_symbol(alternative)
  
  # Print hypotheses with explicit group names and correct symbols
  null_text <- glue::glue("H₀: μ[{group1}] − μ[{group2}] {null_sym} {mu}")
  alt_text  <- glue::glue("H₁: μ[{group1}] − μ[{group2}] {alt_sym} {mu}")
  
  p_text <- if (p_val < 0.001) {
    "p < 0.001"
  } else {
    glue::glue("p = {formatC(round(p_val, 3), format = 'f', digits = 3)}")
  }
  
  cat(glue::glue("Paired t-test for the mean of differences (x̄[{group1}] − x̄[{group2}]):\n\n"))
  cat(glue::glue("Null: {null_text}\n\n"))
  cat(glue::glue("Alternative: {alt_text}\n\n"))
  cat(glue::glue("Test statistic: t({df}) = {t_stat}\n\n"))
  cat(glue::glue("Sample mean difference: x̄[{group1}] − x̄[{group2}] = {mean_d}\n\n"))
  cat(glue::glue("p-value: {p_text}\n\n"))
  
  conclusion(p_val, alpha)
}

#' Cook's Distance Plot
#'
#' Returns a Cook's distance plot for the supplied linear model. Outliers can be labeled,
#' and custom thresholding options are supported. Based on `gg_cooksd` from the `lindia` package.
#'
#' @param fitted.lm A linear model created via `glm`, `lm`, or `aov`.
#' @param label Logical. Whether to label outliers on the plot. Default is TRUE.
#' @param show.threshold Logical. Whether to display threshold lines. Default is FALSE.
#' @param threshold Threshold method for outlier detection. One of `"baseR"`, `"matlab"`, or `"convention"`.
#' @param scale.factor Numeric. Scales point size and line width. Default is 0.5.
#' @param n_labels_desired Integer. Maximum number of outliers to label. Default is all.
#' @param label_height_nudge Numeric. Vertical offset for outlier labels. Default is 0.
#'
#' @return A `ggplot2` object.
#' @import ggplot2
#' @importFrom broom augment
#' @examples
#' \dontrun{
#' model <- lm(bill_length_mm ~ bill_depth_mm + year, penguins)
#' cooks(model)
#' }
#' @export
cooks <- function(fitted.lm, label = TRUE, show.threshold = FALSE, threshold = "convention",
                  scale.factor = 0.5, n_labels_desired = NULL, label_height_nudge = 0) {

  if (!inherits(fitted.lm, c("lm", "glm", "aov"))) {
    stop("Not a valid regression model. Use `glm`, `lm`, or `aov`.")
  }

  lm_matrix <- broom::augment(fitted.lm)
  lm_matrix$rowname <- seq_len(nrow(lm_matrix))

  cooksd <- lm_matrix$.cooksd
  n <- nrow(lm_matrix)

  if (is.null(n_labels_desired)) n_labels_desired <- n

  threshold_val <- switch(threshold,
                          "matlab" = mean(cooksd, na.rm = TRUE) * 3,
                          "baseR" = c(0.5, 1),
                          "convention" = c(4 / n, 1),
                          stop("Invalid threshold specified.")
  )

  max_cook <- max(cooksd, na.rm = TRUE) + round(max(cooksd, na.rm = TRUE) / 5)

  base_plot <- ggplot2::ggplot(lm_matrix, ggplot2::aes(x = rowname, y = .cooksd)) +
    ggplot2::geom_point(size = scale.factor) +
    ggplot2::geom_linerange(aes(ymin = 0, ymax = .cooksd), linewidth = scale.factor) +
    ggplot2::xlab("Observation Number") +
    ggplot2::ylab("Cook's Distance") +
    ggplot2::ylim(0, max_cook) +
    ggplot2::theme_bw()

  if (label) {
    lm_matrix$rowname[which(cooksd < min(threshold_val))] <- ""
    top_outliers <- order(lm_matrix$.cooksd, decreasing = TRUE)[seq_len(min(n_labels_desired, n))]
    lm_matrix$rowname[-top_outliers] <- ""

    lm_matrix$obs_num <- seq_len(n)  # Ensure a numeric x-axis variable

    base_plot <- base_plot +
      ggplot2::geom_text(
        data = lm_matrix,
        ggplot2::aes(
          x = obs_num,
          y = pmin(.cooksd + label_height_nudge, max_cook),
          label = rowname
        ),
        color = "black"
      )
  }


  if (show.threshold) {
    base_plot <- base_plot +
      ggplot2::geom_hline(yintercept = threshold_val, linetype = "dashed")
  }

  return(base_plot)
}

#' Count Suspected Outliers
#'
#' Counts the number of suspected outliers based on standardized residuals.
#'
#' @param df Data frame used in the model.
#' @param model A linear model created via `glm`, `lm`, or `aov`.
#'
#' @return A tibble summarizing counts of suspected and non-suspected outliers.
#' @importFrom dplyr mutate group_by summarise if_else
#' @importFrom broom augment
#' @examples
#' model <- lm(bill_length_mm ~ bill_depth_mm + year, penguins)
#' outlier_count(penguins, model)
#' @export
outlier_count <- function(df, model) {
  if (!inherits(model, c("lm", "glm", "aov"))) {
    stop("Not a valid regression model. Use `glm`, `lm`, or `aov`.")
  }

  broom::augment(model) %>%
    dplyr::mutate(outlier = dplyr::if_else(abs(.std.resid) > 2.5, "Suspected", "Not Suspected", "Missing")) %>%
    dplyr::group_by(outlier) %>%
    dplyr::summarise(count = dplyr::n(), .groups = "drop")
}

#' Outlier Scatter Plot
#'
#' Returns a scatter plot augmented with outlier classification based on standardized residuals.
#'
#' @param df Data frame used in the model.
#' @param model A linear model created via `glm`, `lm`, or `aov`.
#' @param x_var Variable to plot on the x-axis.
#' @param y_var Variable to plot on the y-axis.
#' @param x_lab Optional. Label for the x-axis. Defaults to variable name.
#' @param y_lab Optional. Label for the y-axis. Defaults to variable name.
#'
#' @return A `ggplot2` object.
#' @import ggplot2
#' @importFrom broom augment
#' @importFrom dplyr mutate filter if_else
#' @examples
#' model <- lm(bill_length_mm ~ bill_depth_mm + year, penguins)
#' outlier_graph(penguins, model, x_var = bill_depth_mm, y_var = bill_length_mm)
#' @export
outlier_graph <- function(df, model, x_var, y_var, x_lab = NULL, y_lab = NULL) {
  if (!inherits(model, c("lm", "glm", "aov"))) {
    stop("Not a valid regression model. Use `glm`, `lm`, or `aov`.")
  }

  if (is.null(x_lab)) x_lab <- deparse(substitute(x_var))
  if (is.null(y_lab)) y_lab <- deparse(substitute(y_var))

  data <- broom::augment(model, newdata = df) %>%
    dplyr::mutate(.std.resid = .resid / sd(.resid, na.rm = TRUE)) %>%
    dplyr::filter(!is.na(.std.resid)) %>%
    dplyr::mutate(outlier = dplyr::if_else(abs(.std.resid) > 2.5, "Suspected", "Not Suspected", "Missing"))

  n_suspected <- sum(data$outlier == "Suspected", na.rm = TRUE)

  ggplot2::ggplot(data, ggplot2::aes(x = {{ x_var }}, y = {{ y_var }}, color = outlier)) +
    ggplot2::geom_point() +
    ggplot2::scale_color_manual(values = c("Not Suspected" = "#999999", "Suspected" = "#000000")) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = x_lab, y = y_lab, color = "Outlier",
                  title = paste0("There are ", n_suspected, " suspected outliers."))
}

#' @title Diagnostic Plots: Q-Q Plot and Residuals Histogram
#' @description Creates a Q-Q plot of standardized residuals and a histogram of residuals for a fitted regression model object
#'              or directly from data and variables. The plots are displayed side by side.
#'
#' @param model Either a fitted regression model object of class \code{lm}, \code{glm}, or \code{aov}, 
#'              or \code{NULL} if using \code{data}, \code{continuous}, and \code{function_of}.
#' @param data (Optional) A data frame containing the data.
#' @param continuous (Optional) The numeric predictor or outcome column in \code{data}.
#' @param function_of (Optional) The grouping or explanatory column in \code{data}.
#'
#' @return A \code{ggpubr} object containing the Q-Q plot and histogram of residuals displayed side by side.
#'
#' @examples
#' \dontrun{
#' # Using an lm() object
#' fit <- lm(bill_length_mm ~ bill_depth_mm, data = palmerpenguins::penguins)
#' qq_hist_plot(fit)
#'
#' # Using data, continuous and function_of
#' qq_hist_plot(data = palmerpenguins::penguins, 
#'              continuous = bill_length_mm, function_of = bill_depth_mm)
#' }
#'
#' @import ggplot2
#' @import ggpubr
#' @export

normality_check <- function(model = NULL, data = NULL, continuous = NULL, function_of = NULL) {
  # Check for model input or data-driven input
  if (!is.null(model)) {
    # Ensure the provided model is valid
    if (!inherits(model, c("lm", "glm", "aov"))) {
      stop("The `model` argument must be a regression model of class 'lm', 'glm', or 'aov'.")
    }
  } else if (!is.null(data) && !is.null(continuous) && !is.null(function_of)) {
    # Ensure data-driven inputs are provided correctly
    outcome_q <- rlang::enquo(continuous)
    group_q <- rlang::enquo(function_of)
    
    # Prepare formula for lm() using data and columns
    outcome_chr <- rlang::as_name(outcome_q)
    group_chr <- rlang::as_name(group_q)
    formula <- as.formula(paste0(outcome_chr, " ~ ", group_chr))
    model <- lm(formula, data = data)
  } else {
    stop("Must provide either a `model` object or `data`, `continuous`, and `function_of`.")
  }
  
  # Extract residuals and standardized residuals
  res <- residuals(model)
  stdresid <- rstandard(model)  # Standardized residuals
  
  # Create data frame for plotting
  df <- data.frame(resid = res, stdresid = stdresid)
  
  # Q-Q plot of standardized residuals
  qq_res <- ggplot2::ggplot(df, ggplot2::aes(sample = stdresid)) +
    ggplot2::stat_qq(color = "#C5C4C4") +
    ggplot2::stat_qq_line(color = "black", linewidth = 1, linetype = "dashed") +
    ggplot2::labs(x = "Theoretical Quantiles", y = "Standardized Residuals",
                  title = "Q-Q Plot of Standardized Residuals") +
    ggplot2::theme_classic()
  
  # Histogram of residuals with normal curve
  res_his <- ggplot2::ggplot(df, ggplot2::aes(x = resid)) +
    ggplot2::geom_histogram(aes(y = after_stat(density)),
                            colour = "black", fill = "#C5C4C4", bins = 30) +
    ggplot2::stat_function(fun = stats::dnorm,
                           args = list(mean = mean(res), sd = sd(res)),
                           color = "black", linewidth = 1) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                        color = "black", linewidth = 1.5) +
    ggplot2::labs(x = "Residuals", y = "Density", title = "Histogram of Residuals") +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.ticks.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank())
  
  # Arrange the plots side by side
  out <- ggpubr::ggarrange(qq_res, res_his, ncol = 2, labels = c("A", "B"))
  
  return(out)
}



# Sleek and consistent version of the QQ plot functions for normality assessment

#' one_qq_plot
#'
#' Constructs a QQ plot for a single numeric variable.
#'
#' @param data A data frame.
#' @param variable A string specifying the column name of the variable.
#' @return A ggplot object representing the QQ plot.
#' @examples
#' one_qq_plot(penguins, "bill_length_mm")
one_qq_plot <- function(data, variable) {
  ggplot2::ggplot(data, ggplot2::aes(sample = .data[[variable]])) +
    ggplot2::stat_qq_line(linetype = "dashed", color = "black", linewidth = 1) +
    ggplot2::stat_qq(color = "#6A6C6E") +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "Theoretical Quantiles", y = "Sample Quantiles",
                  title = paste("QQ Plot for", variable))
}

#' independent_qq
#'
#' Constructs QQ plots for two independent samples based on a grouping variable.
#'
#' @param data A data frame in long format.
#' @param variable A string specifying the column name of the outcome variable.
#' @param group A string specifying the column name of the grouping variable (should have exactly 2 levels).
#' @return A ggpubr::ggarrange object with two QQ plots side-by-side.
#' @examples
#' independent_qq_plot(penguins, "bill_length_mm", "sex")
#' @export
independent_qq <- function(data, continuous, grouping) {
  # Capture variables using tidy evaluation
  variable_q <- rlang::enquo(continuous)
  group_q    <- rlang::enquo(grouping)
  
  # Extract column names as characters for later labeling
  variable_name <- rlang::as_name(variable_q)
  group_name    <- rlang::as_name(group_q)
  
  # Filter out missing group values and get group levels
  levels_group <- data %>%
    dplyr::filter(!is.na(!!group_q)) %>%
    dplyr::pull(!!group_q) %>%
    unique()
  
  if (length(levels_group) != 2) {
    stop(paste("Grouping variable must have exactly 2 levels. Found:", paste(levels_group, collapse = ", ")))
  }
  
  # Create QQ plots
  plots <- lapply(levels_group, function(level_val) {
    ggplot2::ggplot(dplyr::filter(data, !!group_q == level_val),
                    ggplot2::aes(sample = !!variable_q)) +
      ggplot2::stat_qq_line(linetype = "dashed", color = "black", linewidth = 1) +
      ggplot2::stat_qq(color = "#666666") +
      ggplot2::theme_bw() +
      ggplot2::labs(title = paste("QQ Plot for", variable_name, "|", group_name, "=", level_val),
                    x = "Theoretical Quantiles", y = "Sample Quantiles")
  })
  
  # Create histograms
  plots2 <- lapply(levels_group, function(level_val) {
    ggplot2::ggplot(dplyr::filter(data, !!group_q == level_val),
                    ggplot2::aes(x = !!variable_q)) +
      ggplot2::geom_histogram(color = "black", fill = "#666666", bins = 30) +
      ggplot2::theme_bw() +
      ggplot2::labs(title = paste("Histogram for", variable_name, "|", group_name, "=", level_val),
                    x = variable_name, y = "Count")
  })
  
  # Combine and arrange plots
  ggpubr::ggarrange(plotlist = c(plots, plots2))
}

#' dependent_qq
#'
#' Constructs a QQ plot for the difference scores between two paired measurements.
#'
#' @param data A data frame in wide format.
#' @param col1 A string specifying the column name of the first measurement.
#' @param col2 A string specifying the column name of the second measurement.
#' @return A ggplot object representing the QQ plot of the paired differences.
#' @importFrom stats IQR TukeyHSD aov as.formula complete.cases cor density
#'   fitted fitted.values median na.omit prop.test reformulate resid residuals
#'   rstandard sd t.test wilcox.test
#' @importFrom utils combn
#' @examples
#' dependent_qq_plot(a1c_measurements, "first_measurement", "second_measurement")
#' @export
dependent_qq <- function(data, col1, col2) {
  # Capture expressions
  var1_q <- rlang::enquo(col1)
  var2_q <- rlang::enquo(col2)
  
  # Get variable names as strings (for labeling)
  var1_name <- rlang::as_name(var1_q)
  var2_name <- rlang::as_name(var2_q)
  
  # Calculate differences
  diff_df <- data %>%
    dplyr::mutate(diff = !!var1_q - !!var2_q) %>%
    dplyr::select(diff) %>%
    dplyr::filter(!is.na(diff))
  
  # QQ Plot
  qq_plot <- ggplot2::ggplot(diff_df, ggplot2::aes(sample = diff)) +
    ggplot2::stat_qq_line(linetype = "dashed", color = "black", linewidth = 1) +
    ggplot2::stat_qq(color = "#666666") +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title = paste("QQ Plot of Paired Differences:", var1_name, "-", var2_name),
      x = "Theoretical Quantiles",
      y = "Sample Quantiles"
    )
  
  # Histogram
  hist_plot <- ggplot2::ggplot(diff_df, ggplot2::aes(x = diff)) +
    ggplot2::geom_histogram(color = "black", fill = "#666666", bins = 30) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title = paste("Histogram of Paired Differences:", var1_name, "-", var2_name),
      x = "Difference",
      y = "Count"
    )
  
  # Arrange and return both plots
  ggpubr::ggarrange(qq_plot, hist_plot, ncol = 2)
}

#' plot_residuals
#'
#' Constructs a scatterplot of residuals by groups for use in determining equality of variance.
#'
#' @param data A data frame in long format.
#' @param continuous A string specifying the column name of the outcome variable.
#' @param grouping A string specifying the column name of the grouping variable (should have exactly 2 levels).
#' @return A ggplot2 object with a scatterplot.
#' @examples
#' plot_residuals(penguins, "bill_length_mm", "sex")
#' @export
plot_residuals <- function(data, continuous, grouping) {
  grouping_q <- enquo(grouping)
  outcome_q  <- enquo(continuous)
  
  data_resid <- data %>%
    group_by(!!grouping_q) %>%
    mutate(residual = !!outcome_q - mean(!!outcome_q, na.rm = TRUE))
  
  ggplot(data_resid, aes(x = !!grouping_q, y = residual, color = !!grouping_q)) +
    geom_jitter(width = 0.2, alpha = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = "Group Residuals",
         y = "Residual",
         x = "Groups") +
    theme_minimal() +
    theme(legend.position = "none")
}


#' variances_HT
#'
#' Examines the difference between two variances.
#'
#' @param data Data frame or tibble.
#' @param grouping Unquoted column name for grouping variable (factor).
#' @param continuous Unquoted column name for continuous outcome.
#' @param alpha Numeric significance level (default 0.05).
#' @return None. Prints formatted test results and conclusion.
#' @import glue
#' @export
variances_HT <- function(data, continuous, grouping, alpha = 0.05) {

  # Capture column names
  grouping_q <- enquo(grouping)
  outcome_q  <- enquo(continuous)
  
  # Subset and drop missing
  df <- data %>%
    dplyr::select(!!grouping_q, !!outcome_q) %>%
    drop_na() %>%
    mutate(!!grouping_q := as.factor(!!grouping_q))
  
  # Convert to names for formula
  grp_name <- quo_name(grouping_q)
  out_name <- quo_name(outcome_q)
  
  # Build formula correctly (using as.name)
  fml <- reformulate(termlabels = grp_name, response = out_name)
  
  # Run Brown-Forsythe-Levene's Test (median-centered)
  levene_result <- car::leveneTest(fml, data = df, center = median)
  
  # Extract F statistic, df, and p-value
  f_stat <- levene_result[1, "F value"]
  df1 <- levene_result[1, "Df"]
  df2 <- levene_result[2, "Df"]
  p_val <- levene_result[1, "Pr(>F)"]
  
  p_text <- if (p_val < 0.001) "p < 0.001" else glue("p = {formatC(p_val, format = 'f', digits = 3)}")
  
  # Get unique groups as a character vector
  groups <- df %>%
    pull(!!grouping_q) %>%
    unique()
  
  # Build sigma text for each group number
  sigma_terms <- paste0("σ²_", groups)
  
  # Collapse with equal signs
  null_text <- paste(sigma_terms, collapse = " = ")
  
  # Print results
  cat(glue("Brown-Forsythe-Levene test for equality of variances:\n\n"))
  cat(glue("Null: {null_text} \n\n"))
  cat(glue("Alternative: At least one variance is different \n\n"))
  cat(glue("Test statistic: F({df1},{df2}) = {round(f_stat, 3)} \n\n"))
  cat(glue("p-value: {p_text}\n\n"))
  
  # Use your existing conclusion function
  conclusion(p_val, alpha)
}

#' independent_median_HT
#'
#' @param data Data frame or tibble.
#' @param grouping Unquoted column name for grouping variable (factor).
#' @param continuous Unquoted column name for continuous outcome.
#' @param alternative Character string specifying alternative hypothesis; "two.sided", "less", or "greater" (default "two.sided").
#' @param m Numeric hypothesized median difference (default 0).
#' @param alpha Numeric significance level (default 0.05).
#' @return None. Prints formatted test results and conclusion.
#' @import glue
#' @export
independent_median_HT <- function(data,
                                  continuous, 
                                  grouping,
                                  alternative = "two.sided",
                                  m = 0,
                                  alpha = 0.05,
                                  reference = NULL) {
  
  # Capture the variables
  grouping_q   <- rlang::enquo(grouping)
  continuous_q <- rlang::enquo(continuous)
  
  # Normalize and validate alternative (accept "two" as alias)
  alt_norm <- tolower(alternative)
  if (alt_norm == "two") alt_norm <- "two.sided"
  if (!alt_norm %in% c("two.sided", "less", "greater")) {
    stop('`alternative` must be one of "two", "two.sided", "less", or "greater".')
  }
  
  # Prepare the dataset
  df <- data %>%
    dplyr::select(!!grouping_q, !!continuous_q) %>%
    tidyr::drop_na()
  
  grp_name  <- rlang::quo_name(grouping_q)
  cont_name <- rlang::quo_name(continuous_q)
  
  # Force numeric outcome if it's a factor/character
  if (is.factor(df[[cont_name]]) || is.character(df[[cont_name]])) {
    df[[cont_name]] <- as.numeric(as.character(df[[cont_name]]))
  } else {
    df[[cont_name]] <- as.numeric(df[[cont_name]])
  }
  
  # Ensure exactly two groups present
  groups_present <- unique(df[[grp_name]])
  groups_chr     <- as.character(groups_present)
  if (length(groups_chr) != 2) {
    stop("Grouping variable must have exactly two levels after removing NAs.")
  }
  
  # Determine order: respect factor levels if provided, otherwise first appearance
  if (is.factor(df[[grp_name]])) {
    orig_levels  <- base::levels(df[[grp_name]])
    levels_order <- orig_levels[orig_levels %in% groups_chr]
    if (length(levels_order) != 2) {
      levels_order <- groups_chr
    }
  } else {
    levels_order <- groups_chr
  }
  
  # If reference supplied, validate and set as SECOND level
  # so the reported difference is (other − reference), matching independent_mean_HT
  if (!is.null(reference)) {
    if (!is.character(reference) || length(reference) != 1) {
      stop("`reference` must be a single quoted character string matching a group value.")
    }
    if (!(reference %in% levels_order)) {
      stop(glue::glue(
        "`reference` '{reference}' not found among grouping values: {paste(levels_order, collapse = ', ')}"
      ))
    }
    other <- setdiff(levels_order, reference)
    levels_order <- c(other, reference)  # non-reference first, reference second
  }
  
  # Relevel grouping to chosen order
  df[[grp_name]] <- factor(as.character(df[[grp_name]]), levels = levels_order)
  
  # Check sample sizes
  group_counts <- df %>%
    dplyr::group_by(!!grouping_q) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop")
  
  if (any(group_counts$n < 2)) {
    bad <- group_counts %>% dplyr::filter(n < 2)
    stop(glue::glue(
      "Each group must have at least 2 non-missing observations. Problem group(s): {paste(bad[[grp_name]], collapse = ', ')}"
    ))
  }
  
  g1 <- levels_order[1]
  g2 <- levels_order[2]
  
  # Create formula
  fml <- stats::reformulate(termlabels = grp_name, response = cont_name)
  
  # Run Wilcoxon Rank Sum test
  w_test <- stats::wilcox.test(
    formula    = fml,
    data       = df,
    alternative = alt_norm,
    conf.int   = TRUE,
    conf.level = 1 - alpha,
    mu         = m,
    exact      = FALSE
  )
  
  w_stat <- round(as.numeric(w_test$statistic), 3)
  p_val  <- as.numeric(w_test$p.value)
  
  # p-value text
  p_text <- if (p_val < 0.001) {
    "p < 0.001"
  } else {
    glue::glue("p = {formatC(round(p_val, 3), format = 'f', digits = 3)}")
  }
  
  # Hypothesis text
  null_text <- switch(
    alt_norm,
    two.sided = glue::glue("H₀: M[{g1}] − M[{g2}] = {m}"),
    less = glue::glue("H₀: M[{g1}] − M[{g2}] ≥ {m}"),
    greater = glue::glue("H₀: M[{g1}] − M[{g2}] ≤ {m}")
  )
  alt_text  <- switch(
    alt_norm,
    two.sided = glue::glue("H₁: M[{g1}] − M[{g2}] ≠ {m}"),
    less      = glue::glue("H₁: M[{g1}] − M[{g2}] < {m}"),
    greater   = glue::glue("H₁: M[{g1}] − M[{g2}] > {m}")
  )
  
  # Group medians and sample median difference
  med1 <- stats::median(df[df[[grp_name]] == g1, ][[cont_name]], na.rm = TRUE)
  med2 <- stats::median(df[df[[grp_name]] == g2, ][[cont_name]], na.rm = TRUE)
  
  med1_f <- formatC(round(med1, 4), format = "f", digits = 4)
  med2_f <- formatC(round(med2, 4), format = "f", digits = 4)
  med_diff <- formatC(round(med1 - med2, 4), format = "f", digits = 4)
  
  cat(glue::glue("Wilcoxon Rank Sum Test for two independent medians (M[{g1}] − M[{g2}]):\n\n"))
  cat(glue::glue("Sample median difference: M[{g1}] − M[{g2}] = {med_diff}\n\n"))
  cat(glue::glue("Median of {g1}: {med1_f}\n\n"))
  cat(glue::glue("Median of {g2}: {med2_f}\n\n"))
  cat(glue::glue("Null: {null_text}\n\n"))
  cat(glue::glue("Alternative: {alt_text}\n\n"))
  cat(glue::glue("Test statistic: T = {w_stat}\n\n"))
  cat(glue::glue("p-value: {p_text}\n\n"))
  
  conclusion(p_val, alpha)
  
  invisible(NULL)
}

#' Paired median difference hypothesis test
#'
#' @param data Data frame or tibble.
#' @param col1 Unquoted column name for first paired measurement.
#' @param col2 Unquoted column name for second paired measurement.
#' @param alternative Character string specifying alternative hypothesis; "two", "two.sided", "less", or "greater" (default "two.sided").
#' @param m Numeric hypothesized median difference (default 0).
#' @param alpha Numeric significance level (default 0.05).
#' @return None. Prints formatted test results and conclusion.
#' @importFrom dplyr pull
#' @export
dependent_median_HT <- function(data,
                                col1,
                                col2,
                                alternative = "two.sided",
                                m = 0,
                                alpha = 0.05) {
  # Pull paired data
  x <- data %>% dplyr::pull({{ col1 }})
  y <- data %>% dplyr::pull({{ col2 }})
  
  # Only keep complete pairs
  complete <- complete.cases(x, y)
  x <- x[complete]
  y <- y[complete]
  
  # Group names as strings
  group1 <- rlang::as_name(rlang::enquo(col1))
  group2 <- rlang::as_name(rlang::enquo(col2))
  
  # Calculate differences: col1 - col2
  differences <- x - y
  med_diff <- round(median(differences), 4)
  
  # Wilcoxon signed-rank test
  w_test <- wilcox.test(
    x, y,
    paired = TRUE,
    alternative = alternative,
    mu = m,
    conf.int = TRUE,
    conf.level = 1 - alpha,
    exact = FALSE
  )
  
  w_stat  <- round(w_test$statistic, 3)
  p_val   <- w_test$p.value
  
  # Null and alternative hypotheses using helpers
  null_sym <- null_symbol(alternative)
  alt_sym  <- alt_symbol(alternative)
  null_text <- glue::glue("H₀: M[{group1}] − M[{group2}] {null_sym} {m}")
  alt_text  <- glue::glue("H₁: M[{group1}] − M[{group2}] {alt_sym} {m}")
  
  # p-value formatting
  p_text <- if (p_val < 0.001) {
    "p < 0.001"
  } else {
    glue::glue("p = {formatC(round(p_val, 3), format = 'f', digits = 3)}")
  }
  
  # Print results
  cat(glue::glue("Wilcoxon Signed-Rank Test for the median of differences (M[{group1}] − M[{group2}]):\n\n"))
  cat(glue::glue("Sample median difference: M[{group1}] − M[{group2}] = {med_diff}\n\n"))
  cat(glue::glue("Null: {null_text}\n\n"))
  cat(glue::glue("Alternative: {alt_text}\n\n"))
  cat(glue::glue("Test statistic: T = {w_stat}\n\n"))
  cat(glue::glue("p-value: {p_text}\n\n"))
  
  conclusion(p_val, alpha)
}

#' One-way ANOVA hypothesis test
#'
#' @param data Data frame or tibble.
#' @param grouping Unquoted column name for grouping variable (factor).
#' @param continuous Unquoted column name for continuous outcome.
#' @param alpha Numeric significance level (default 0.05).
#' @export
one_way_ANOVA <- function(data, 
                          continuous, 
                          grouping,
                          alpha = 0.05) {
  # Capture column names using tidy evaluation
  outcome_q <- rlang::enquo(continuous)
  group_q   <- rlang::enquo(grouping)
  
  # Prepare formula for aov()
  outcome_chr <- rlang::as_name(outcome_q)
  group_chr   <- rlang::as_name(group_q)
  formula <- as.formula(paste0(outcome_chr, " ~ ", group_chr))
  
  # Clean dataset
  df <- data %>%
    dplyr::select(!!group_q, !!outcome_q) %>%
    dplyr::filter(!is.na(!!group_q), !is.na(!!outcome_q)) %>%
    dplyr::mutate(!!group_q := as.factor(!!group_q))
  
  # Run one-way ANOVA
  m <- aov(formula, data = df)
  anova_tbl <- broom::tidy(m)
  
  # Extract F and p-value
  f_stat <- round(anova_tbl$statistic[1], 3)
  df1 <- anova_tbl$df[1]
  df2 <- anova_tbl$df[2]
  p_val <- signif(anova_tbl$p.value[1], 4)
  
  # Extract group names
  group_levels <- levels(df[[rlang::as_name(group_q)]])
  mu_expr <- paste0("μ_", group_levels, collapse = " = ")
  
  p_text <- if (p_val < 0.001) "p < 0.001" else glue::glue("p = {formatC(round(p_val, 3), format = 'f', digits = 3)}")
  
  # Output
  cat("One-Way ANOVA: \n\n")
  cat(glue::glue("H₀: {mu_expr}\n\n"))
  cat(glue::glue("H₁: At least one group mean is different\n\n"))
  cat(glue::glue("Test Statistic: F({df1}, {df2}) = {f_stat}\n\n"))
  cat(glue::glue("p-value: {p_text}\n\n"))
  conclusion(p_val, alpha)
}

#' One-way ANOVA table
#'
#' @param data Data frame or tibble.
#' @param grouping Unquoted column name for grouping variable (factor).
#' @param continuous Unquoted column name for continuous outcome.
#' @export
one_way_ANOVA_table <- function(data, 
                                continuous, 
                                grouping) {
  
  # Capture column names using tidy evaluation
  outcome_q <- rlang::enquo(continuous)
  group_q   <- rlang::enquo(grouping)
  
  # Prepare formula for aov()
  outcome_chr <- rlang::as_name(outcome_q)
  group_chr   <- rlang::as_name(group_q)
  formula <- as.formula(paste0(outcome_chr, " ~ ", group_chr))
  
  # Clean dataset
  df <- data %>%
    dplyr::select(!!group_q, !!outcome_q) %>%
    dplyr::filter(!is.na(!!group_q), !is.na(!!outcome_q)) %>%
    dplyr::mutate(!!group_q := as.factor(!!group_q))
  
  # Run one-way ANOVA
  m <- aov(formula, data = df)
  anova_tbl <- broom::tidy(m)
  
  # Compute SS_Tot and df_Tot
  SSTot <- sum(anova_tbl$sumsq)
  dfTot <- sum(anova_tbl$df)
  
  # Extract rows
  SS_Trt <- anova_tbl$sumsq[1]
  df_Trt <- anova_tbl$df[1]
  MS_Trt <- SS_Trt / df_Trt
  F_val  <- anova_tbl$statistic[1]
  
  SS_E <- anova_tbl$sumsq[2]
  df_E <- anova_tbl$df[2]
  MS_E <- SS_E / df_E
  
  # Build display table
  result <- tibble::tibble(
    Source = c("Treatment", "Error", "Total"),
    `Sum of Squares` = c(SS_Trt, SS_E, SSTot),
    df = c(df_Trt, df_E, dfTot),
    `Mean Squares` = c(MS_Trt, MS_E, NA),
    F = c(F_val, NA, NA)
  )
  
  # Display with gt and formatting
  gt::gt(result) %>%
    gt::fmt_integer(columns = "df") %>%
    gt::fmt_number(columns = c("Sum of Squares", "Mean Squares", "F"), decimals = 2) %>%
    gt::fmt_missing(columns = everything(), missing_text = "") %>%
    gt::tab_header(title = "One-Way ANOVA Table") %>%
    gt::cols_label(
      Source = gt::md("**Source**"),
      `Sum of Squares` = gt::md("**Sum of Squares**"),
      df = gt::md("**_df_**"),
      `Mean Squares` = gt::md("**Mean Squares**"),
      F = gt::md("**_F_**")
    ) %>%
    gt::opt_align_table_header(align = "center") %>%
    gt::cols_align(align = "left", columns = "Source") %>%
    gt::cols_align(align = "right", columns = c("Sum of Squares", "Mean Squares", "F"))
}

#' Tukey's test - one-way ANOVA
#'
#' @param data Data frame or tibble.
#' @param grouping Unquoted column name for grouping variable (factor).
#' @param continuous Unquoted column name for continuous outcome.
#' @export
posthoc_tukey <- function(data, 
                          continuous,
                          grouping) {
  
  # Capture column names using tidy evaluation
  outcome_q <- rlang::enquo(continuous)
  group_q   <- rlang::enquo(grouping)
  
  # Prepare formula for aov()
  outcome_chr <- rlang::as_name(outcome_q)
  group_chr   <- rlang::as_name(group_q)
  formula <- as.formula(paste0(outcome_chr, " ~ ", group_chr))
  
  # Clean dataset
  df <- data %>%
    dplyr::select(!!group_q, !!outcome_q) %>%
    dplyr::filter(!is.na(!!group_q), !is.na(!!outcome_q)) %>%
    dplyr::mutate(!!group_q := as.factor(!!group_q))
  
  # Run one-way ANOVA
  m <- aov(formula, data = df)
  
  # Run Tukey HSD
  tukey_result <- TukeyHSD(m)
  
  # Convert Tukey result to a tidy data frame and format p-values
  tukey_tbl <- tukey_result[[group_chr]] %>%
    as.data.frame() %>%
    tibble::rownames_to_column("comparison") %>%
    dplyr::transmute(
      comparison,
      `Mean Diff.` = round(diff, 2),
      `p-value` = dplyr::case_when(
        `p adj` < 0.001 ~ "< 0.001",
        TRUE ~ formatC(`p adj`, digits = 3, format = "f")
      )
    )
  
  return(tukey_tbl)
}

#' Fisher's test - one-way ANOVA
#'
#' @param data Data frame or tibble.
#' @param grouping Unquoted column name for grouping variable (factor).
#' @param continuous Unquoted column name for continuous outcome.
#' @export
posthoc_fisher <- function(data, 
                           continuous,
                           grouping) {
  
  # Capture column names
  outcome_q <- rlang::enquo(continuous)
  group_q   <- rlang::enquo(grouping)
  
  # Extract column names as strings
  outcome_chr <- rlang::as_name(outcome_q)
  group_chr   <- rlang::as_name(group_q)
  
  # Clean and prep dataset
  df <- data %>%
    dplyr::select(!!group_q, !!outcome_q) %>%
    dplyr::filter(!is.na(!!group_q), !is.na(!!outcome_q)) %>%
    dplyr::mutate(!!group_q := as.factor(!!group_q))
  
  # All pairwise combinations
  combos <- combn(levels(df[[group_chr]]), 2, simplify = FALSE)
  
  # Run pairwise t-tests (unadjusted)
  results <- purrr::map_dfr(combos, function(pair) {
    g1 <- pair[1]
    g2 <- pair[2]
    
    x1 <- df %>% dplyr::filter(!!group_q == g1) %>% dplyr::pull(!!outcome_q)
    x2 <- df %>% dplyr::filter(!!group_q == g2) %>% dplyr::pull(!!outcome_q)
    
    ttest <- t.test(x1, x2, var.equal = TRUE)
    
    tibble::tibble(
      comparison = paste(g1, "-", g2),
      `Mean Diff.` = round(mean(x1) - mean(x2), 2),
      `p-value` = dplyr::case_when(
        ttest$p.value < 0.001 ~ "< 0.001",
        TRUE ~ formatC(ttest$p.value, digits = 3, format = "f")
      )
    )
  })
  
  return(results)
}

#' One-way ANOVA Assumptions
#'
#' @param data Data frame or tibble.
#' @param grouping Unquoted column name for grouping variable (factor).
#' @param continuous Unquoted column name for continuous outcome.
#' @export
ANOVA_assumptions <- function(data, 
                              continuous, 
                              grouping) {
  library(tidyverse)
  library(patchwork)
  
  # Capture tidy eval column names
  outcome_q <- rlang::enquo(continuous)
  group_q   <- rlang::enquo(grouping)
  
  outcome_chr <- rlang::as_name(outcome_q)
  group_chr   <- rlang::as_name(group_q)
  
  # Clean data
  df <- data %>%
    dplyr::select(!!group_q, !!outcome_q) %>%
    dplyr::filter(!is.na(!!group_q), !is.na(!!outcome_q)) %>%
    dplyr::mutate(!!group_q := as.factor(!!group_q))
  
  # Run one-way ANOVA
  formula <- as.formula(paste0(outcome_chr, " ~ ", group_chr))
  model <- aov(formula, data = df)
  
  # Extract residuals, fitted values, group
  model_df <- df %>%
    dplyr::mutate(
      fitted = fitted(model),
      residuals = residuals(model)
    )
  
  # 1. QQ plot of residuals by group
  qq_plot <- model_df %>%
    ggplot(aes(sample = residuals)) +
    stat_qq() +
    stat_qq_line() +
    facet_wrap(vars(!!sym(group_chr))) +
    theme_bw() +
    labs(title = "QQ Plots of Residuals by Group",
         y = "Sample Quantiles",
         x = "Theoretical Quantiles")
  
  # 2. Residuals vs Fitted Plot
  rvf_plot <- model_df %>%
    ggplot(aes(x = fitted, y = residuals)) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_bw() +
    labs(title = "Residuals vs Fitted Values",
         x = "Fitted Values",
         y = "Residuals")
  
  # 3. Boxplot of outcome by group (optional)
  # box_plot <- df %>%
  #   ggplot(aes(x = !!group_q, y = !!outcome_q)) +
  #   geom_boxplot() +
  #   theme_bw() +
  #   labs(title = "Boxplot of Outcome by Group",
  #        x = group_chr,
  #        y = outcome_chr)
  
  # Combine with patchwork
  combined_plot <- (qq_plot / rvf_plot) +
    patchwork::plot_annotation(title = "One-Way ANOVA Assumptions")
  
  return(combined_plot)
}

#' Kruskal–Wallis Hypothesis Test
#'
#' @param data       A data frame
#' @param continuous Numeric/continuous response variable.
#' @param grouping   Factor or grouping variable (≥ 2 levels).
#' @param alpha      Significance level for conclusion helper.
#' @export
kruskal_HT <- function(data,
                       continuous,
                       grouping,
                       alpha = 0.05) {
  
  ## capture column quosures & names
  outcome_q <- rlang::enquo(continuous)
  group_q   <- rlang::enquo(grouping)
  
  outcome_chr <- rlang::as_name(outcome_q)
  group_chr   <- rlang::as_name(group_q)
  
  ## clean data
  df <- data %>%
    dplyr::select(!!group_q, !!outcome_q) %>%
    dplyr::filter(!is.na(!!group_q), !is.na(!!outcome_q)) %>%
    dplyr::mutate(!!group_q := as.factor(!!group_q))
  
  ## KW
  kw <- stats::kruskal.test(
    formula = stats::as.formula(paste0(outcome_chr, " ~ ", group_chr)),
    data    = df
  )
  
  ## extract 
  H_stat <- round(kw$statistic, 3)
  df_kw  <- kw$parameter         
  p_val  <- signif(kw$p.value, 4)
  
  # build H0 expression with group names
  group_levels <- levels(df[[group_chr]])
  mu_expr <- paste0("M_", group_levels, collapse = " = ")  
  
  ## output
  cat("Kruskal–Wallis Rank-Sum Test\n\n")
  cat(glue::glue("H₀: {mu_expr}\n\n"))
  cat("H₁: At least one group is different\n\n")
  cat(glue::glue("Test Statistic: X({df_kw}) = {H_stat},\n\n"))
  cat(if (p_val < 0.001) " p < 0.001\n" else glue::glue(" p = {formatC(round(p_val,3), format='f', digits=3)}\n\n"))
  
  # optional conclusion() helper if you have one defined
  if (exists("conclusion") && is.function(conclusion)) {
    conclusion(p_val, alpha)
  }
  
  invisible(kw)
}



#' Dunn's posthoc test (nonparametric)
#'
#' @param data       A data frame
#' @param continuous Numeric/continuous response variable.
#' @param grouping   Factor or grouping variable (≥ 2 levels).
#' @export
posthoc_dunn <- function(data,
                         continuous,
                         grouping,
                         adjust = TRUE) {
  # Capture variable names
  outcome_q <- rlang::enquo(continuous)
  group_q   <- rlang::enquo(grouping)
  
  outcome_chr <- rlang::as_name(outcome_q)
  group_chr   <- rlang::as_name(group_q)
  
  # data prep
  df <- data %>%
    dplyr::select(!!group_q, !!outcome_q) %>%
    dplyr::filter(!is.na(!!group_q), !is.na(!!outcome_q)) %>%
    dplyr::mutate(!!group_q := as.factor(!!group_q),
                  !!outcome_q := as.numeric(!!outcome_q))
  
  # get Dunn's test
  dunn_result <- FSA::dunnTest(x = df[[outcome_chr]],
                               g = df[[group_chr]],
                               method = ifelse(adjust, "bonferroni", "none"))
  
  # print results
  tbl <- dunn_result$res %>% 
    dplyr::mutate(p = if (adjust) P.adj else P.unadj) %>%
    dplyr::mutate(p = round(p, 3),
                  p = formatC(p, format = "f", digits = 3)) %>%
    dplyr::select(Comparison, Z, p)
  
  # Output
  print(tbl)
  invisible(tbl)
}

#' Two-way ANOVA table
#'
#' @param data Data frame or tibble.
#' @param continuous Unquoted column name for continuous outcome.
#' @param A Factor A
#' @param B Factor B
#' @param interaction logical statement to indicate if an interaction term should be included
#' @export
two_way_ANOVA_table <- function(data,
                                continuous,
                                A,
                                B,
                                interaction = TRUE) {
  
  ## --- capture quosures -------------------------------------------------
  y_q <- rlang::enquo(continuous)
  A_q <- rlang::enquo(A)
  B_q <- rlang::enquo(B)
  
  y_chr <- rlang::as_name(y_q)
  A_chr <- rlang::as_name(A_q)
  B_chr <- rlang::as_name(B_q)
  
  ## --- clean data -------------------------------------------------------
  df <- data %>%
    dplyr::select(!!y_q, !!A_q, !!B_q) %>%
    dplyr::filter(!is.na(!!y_q), !is.na(!!A_q), !is.na(!!B_q)) %>%
    dplyr::mutate(
      !!A_q := as.factor(!!A_q),
      !!B_q := as.factor(!!B_q),
      !!y_q := as.numeric(!!y_q)
    )
  
  ## --- build formula + fit model ---------------------------------------
  rhs     <- if (interaction) paste0(A_chr, " * ", B_chr)
  else   paste0(A_chr, " + ", B_chr)
  formula <- stats::as.formula(paste0(y_chr, " ~ ", rhs))
  
  model <- stats::aov(formula, data = df)
  
  ## --- tidy ANOVA -------------------------------------------------------
  tbl <- broom::tidy(model) %>%
    dplyr::mutate(
      Source = dplyr::case_when(
        term == "Residuals"        ~ "Error",
        grepl(":", term)           ~ "Interaction",
        TRUE                       ~ term
      )
    ) %>%
    dplyr::select(Source, dplyr::everything(), -term) %>%
    dplyr::rename(
      `Sum of Squares` = sumsq,
      df               = df,
      F                = statistic,
      p                = p.value
    ) %>%
    dplyr::mutate(
      `Mean Squares` = `Sum of Squares` / df
    )
  
  ## --- indent model terms ----------------------------------------------
  model_terms <- tbl %>%
    dplyr::filter(Source %in% c(A_chr, B_chr, "Interaction")) %>%
    dplyr::mutate(Source = paste0("•", Source))
  
  ## --- build Regression, Error, Total rows ------------------------------
  regression_row <- model_terms %>%
    dplyr::summarise(
      Source          = "Regression",
      df              = sum(df),
      `Sum of Squares`= sum(`Sum of Squares`),
      `Mean Squares`  = NA_real_,
      F               = NA_real_,
      p               = NA_real_,
      .groups = "drop"
    )
  
  error_row <- tbl %>% dplyr::filter(Source == "Error")
  
  total_row <- tbl %>%
    dplyr::summarise(
      Source          = "Total",
      df              = sum(df),
      `Sum of Squares`= sum(`Sum of Squares`),
      `Mean Squares`  = NA_real_,
      F               = NA_real_,
      p               = NA_real_,
      .groups = "drop"
    )
  
  ## --- assemble final table in desired order ----------------------------
  final_tbl <- dplyr::bind_rows(
    regression_row,
    model_terms,
    error_row,
    total_row
  ) %>%
    dplyr::mutate(
      `Sum of Squares` = round(`Sum of Squares`, 2),
      `Mean Squares`   = round(`Mean Squares`, 2),
      F                = round(F, 2),
      p = dplyr::case_when(
        is.na(p)    ~ "",
        p < 0.001   ~ "< 0.001",
        TRUE        ~ formatC(round(p, 3), format = "f", digits = 3)
      )
    ) %>%
    dplyr::select(Source, `Sum of Squares`, df, `Mean Squares`, F, p)   # order cols
  
  ## --- print with gt ----------------------------------------------------
  gt::gt(final_tbl) %>%
    gt::sub_missing(columns = everything(), missing_text = "") %>%
    gt::tab_header(title = "Two-Way ANOVA Table") %>%
    gt::cols_label(
      Source          = gt::md("**Source**"),
      `Sum of Squares`= gt::md("**Sum of Squares**"),
      df              = gt::md("**_df_**"),
      `Mean Squares`  = gt::md("**Mean Squares**"),
      F               = gt::md("**_F_**"),
      p               = gt::md("**_p_**")
    ) %>%
    gt::opt_align_table_header(align = "center") %>%
    gt::cols_align("left",  columns = Source) %>%
    gt::cols_align("right", columns = c(`Sum of Squares`, df, `Mean Squares`, F, p))
}


#' Two-way ANOVA hypothesis test
#'
#' @param data Data frame or tibble.
#' @param continuous Unquoted column name for continuous outcome.
#' @param A Factor A
#' @param B Factor B
#' @param interaction logical statement to indicate if an interaction term should be included
#' @param alpha Numeric significance level (default 0.05).
#' @export
two_way_ANOVA <- function(data,
                          continuous,
                          A,
                          B,
                          interaction = TRUE,
                          alpha = 0.05) {
  
  # Capture variable names
  y_q <- rlang::enquo(continuous)
  A_q <- rlang::enquo(A)
  B_q <- rlang::enquo(B)
  
  y_chr <- rlang::as_name(y_q)
  A_chr <- rlang::as_name(A_q)
  B_chr <- rlang::as_name(B_q)
  
  # Clean data
  df <- data %>%
    dplyr::select(!!y_q, !!A_q, !!B_q) %>%
    dplyr::filter(!is.na(!!y_q), !is.na(!!A_q), !is.na(!!B_q)) %>%
    dplyr::mutate(
      !!A_q := as.factor(!!A_q),
      !!B_q := as.factor(!!B_q),
      !!y_q := as.numeric(!!y_q)
    )
  
  # Build formula and model
  rhs <- if (interaction) paste0(A_chr, " * ", B_chr) else paste0(A_chr, " + ", B_chr)
  model <- aov(stats::as.formula(paste0(y_chr, " ~ ", rhs)), data = df)
  anov <- broom::tidy(model)
  
  # ----------------------------
  # Interaction model
  if (interaction) {
    row_int <- anov %>% dplyr::filter(grepl(":", term))
    df1     <- row_int$df
    F_val   <- round(row_int$statistic, 2)
    p_val   <- row_int$p.value
    df2     <- anov %>% dplyr::filter(term == "Residuals") %>% dplyr::pull(df)
    
    p_txt <- ifelse(p_val < 0.001, "< 0.001",
                    formatC(round(p_val, 3), format = "f", digits = 3))
    
    cat(glue::glue("Test for Interaction ({A_chr} × {B_chr}):\n\n\n"))
    cat(glue::glue("H₀: The relationship between {y_chr} and {A_chr} does not depend on {B_chr}.\n\n"))
    cat(glue::glue("H₁: The relationship between {y_chr} and {A_chr} depends on {B_chr}.\n\n"))
    cat(glue::glue("Test Statistic: F({df1}, {df2}) = {F_val}\n\n"))
    cat(glue::glue("p-value: p = {p_txt}\n\n"))
    if (exists("conclusion") && is.function(conclusion)) conclusion(p_val, alpha)
    
    return(invisible(model))
  }
  
  # ----------------------------
  # Additive model (main effects)
  row_A <- anov %>% dplyr::filter(term == A_chr)
  row_B <- anov %>% dplyr::filter(term == B_chr)
  
  print_one <- function(row, factor_name) {
    F_val <- round(row$statistic, 2)
    df1   <- row$df
    df2   <- anov %>% dplyr::filter(term == "Residuals") %>% dplyr::pull(df)
    p_val <- row$p.value
    p_txt <- ifelse(p_val < 0.001, "< 0.001",
                    formatC(round(p_val, 3), format = "f", digits = 3))
    
    cat(glue::glue("Test Statistic: F({df1}, {df2}) = {F_val}\n\n"))
    cat(glue::glue("p-value: p = {p_txt}\n\n"))
    if (exists("conclusion") && is.function(conclusion)) conclusion(p_val, alpha)
  }
  
  # Get group levels
  A_levels <- levels(df[[A_chr]])
  B_levels <- levels(df[[B_chr]])
  mu_expr_A <- paste0("μ_", A_levels, collapse = " = ")
  mu_expr_B <- paste0("μ_", B_levels, collapse = " = ")
  
  # Output
  cat(glue::glue("Test for Main Effect {A_chr}:\n\n\n"))
  cat(glue::glue("H₀: {mu_expr_A}\n\n"))
  cat(glue::glue("H₁: At least one mean is different.\n\n"))
  print_one(row_A, A_chr)
  
  cat(glue::glue("\n\n"))
  
  cat(glue::glue("Test for Main Effect {B_chr}:\n\n\n"))
  cat(glue::glue("H₀: {mu_expr_B}\n\n"))
  cat(glue::glue("H₁: At least one mean is different.\n\n"))
  print_one(row_B, B_chr)
  
  invisible(model)
}

#' Profile plot for two-way ANOVA
#'
#' @param data Data frame or tibble.
#' @param continuous Unquoted column name for continuous outcome.
#' @param A Factor A
#' @param B Factor B
#' @param interaction logical statement to indicate if an interaction term should be included
#' @param alpha Numeric significance level (default 0.05).
#' @export
profile_plot <- function(data,
                         continuous,
                         xaxis,
                         lines) {
  
  # Tidy evaluation
  y_q  <- rlang::enquo(continuous)
  x_q  <- rlang::enquo(xaxis)
  l_q  <- rlang::enquo(lines)
  
  # Compute group means
  df_means <- data %>%
    dplyr::filter(!is.na(!!y_q), !is.na(!!x_q), !is.na(!!l_q)) %>%
    dplyr::group_by(!!x_q, !!l_q) %>%
    dplyr::summarise(mean_y = mean(!!y_q, na.rm = TRUE), .groups = "drop")
  
  # Create plot
  ggplot(df_means, aes(x = !!x_q,
                       y = mean_y,
                       group = !!l_q,
                       color = !!l_q)) +
    geom_line(size = 1.1) +
    geom_point(size = 2) +
    labs(
      x = rlang::as_name(x_q),
      y = glue::glue("Mean {rlang::as_name(y_q)}"),
      color = rlang::as_name(l_q)
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 13),
      legend.title = element_text(size = 12)
    )
}



#' Two-way ANOVA Assumptions
#'
#' @param data        Data frame or tibble.
#' @param continuous  Unquoted column name for the numeric outcome.
#' @param A           Unquoted column name for factor A.
#' @param B           Unquoted column name for factor B.
#' @param interaction Logical. If TRUE (default) fit A * B; otherwise A + B.
#' @export
ANOVA2_assumptions <- function(data,
                               continuous,
                               A,
                               B,
                               interaction = TRUE) {
  library(ggplot2)
  suppressPackageStartupMessages(library(patchwork))
  
  # --- capture names -------------------------------------------------------
  y_q <- rlang::enquo(continuous)
  A_q <- rlang::enquo(A)
  B_q <- rlang::enquo(B)
  
  y_chr <- rlang::as_name(y_q)
  A_chr <- rlang::as_name(A_q)
  B_chr <- rlang::as_name(B_q)
  
  # --- clean data ----------------------------------------------------------
  df <- data %>%
    dplyr::select(!!A_q, !!B_q, !!y_q) %>%
    dplyr::filter(!is.na(!!A_q), !is.na(!!B_q), !is.na(!!y_q)) %>%
    dplyr::mutate(
      !!A_chr := as.factor(!!sym(A_chr)),
      !!B_chr := as.factor(!!sym(B_chr))
    )
  
  # --- build & fit model ---------------------------------------------------
  rhs <- if (interaction) {
    paste(A_chr, "*", B_chr)
  } else {
    paste(A_chr, "+", B_chr)
  }
  form   <- stats::as.formula(paste(y_chr, "~", rhs))
  model  <- stats::aov(form, data = df)
  
  # --- residual & fitted values -------------------------------------------
  model_df <- df %>%
    dplyr::mutate(
      fitted    = fitted(model),
      residuals = residuals(model),
      AB_cell   = interaction(!!sym(A_chr), !!sym(B_chr), sep = " : ")
    )
  
  # --- 1. QQ plot faceted by A × B ----------------------------------------
  qq_plot <- model_df %>%
    ggplot(aes(sample = residuals)) +
    stat_qq() +
    stat_qq_line() +
    facet_wrap(vars(AB_cell)) +
    theme_bw() +
    labs(title = "QQ Plots of Residuals by Treatment Group",
         y = "Sample Quantiles",
         x = "Theoretical Quantiles")
  
  # --- 2. Residuals vs Fitted plot ----------------------------------------
  rvf_plot <- model_df %>%
    ggplot(aes(x = fitted, y = residuals,
               colour = !!sym(A_chr), shape = !!sym(B_chr))) +
    geom_point(alpha = 0.7, size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_bw() +
    labs(title = "Residuals vs Fitted Values",
         x = "Fitted Values",
         y = "Residuals",
         colour = A_chr,
         shape  = B_chr)
  
  # --- combine & return ----------------------------------------------------
  combined_plot <- (qq_plot / rvf_plot) +
    patchwork::plot_annotation(
      title = glue::glue("Two-way ANOVA Assumptions")
    )
  
  return(combined_plot)
}





#' Pairwise Correlation
#'
#' @param data A data frame or tibble.
#' @param x Optional unquoted column name for x variable.
#' @param y Optional unquoted column name for y variable.
#' @param method Correlation type: "pearson" (default) or "spearman".
#'
#' @return A tibble with var1, var2, correlation, test_statistic, p_value
#' @export
correlation <- function(data, x = NULL, y = NULL, method = "pearson") {
  library(tidyverse)
  
  if (!method %in% c("pearson", "spearman")) {
    stop("`method` must be either 'pearson' or 'spearman'.")
  }
  
  # Helper function
  extract_cor_info <- function(var1, var2, data, method) {
    test <- cor.test(data[[var1]], data[[var2]], method = method)
    
    stat_name <- if (method == "pearson") "t" else "S"
    
    tibble(
      var1 = var1,
      var2 = var2,
      correlation = round(test$estimate, 3),
      test_statistic = round(test$statistic, 3),
      p_value = ifelse(test$p.value < 0.001, "< 0.001",
                       formatC(test$p.value, digits = 3, format = "f"))
    ) %>% 
      # Attach test_stat_name as an attribute
      structure(test_stat_name = stat_name)
  }
  
  # Case 1: x and y provided
  if (!is.null(rlang::enexpr(x)) & !is.null(rlang::enexpr(y))) {
    var1 <- rlang::as_name(rlang::enexpr(x))
    var2 <- rlang::as_name(rlang::enexpr(y))
    result <- extract_cor_info(var1, var2, data, method)
    attr(result, "test_stat_name") <- if (method == "pearson") "t" else "S"
    return(result)
  }
  
  # Case 2: All pairwise correlations
  numeric_data <- data %>% select(where(is.numeric))
  var_names <- names(numeric_data)
  
  all_pairs <- expand_grid(var1 = var_names, var2 = var_names) %>%
    filter(var1 < var2)
  
  results <- map2_dfr(all_pairs$var1, all_pairs$var2,
                      ~extract_cor_info(.x, .y, numeric_data, method))
  
  attr(results, "test_stat_name") <- if (method == "pearson") "t" else "S"
  
  return(results)
}


#' Side-by-Side QQ Plots for Two Variables
#'
#' @description
#' Creates side-by-side QQ plots to assess the normality of two numeric variables.
#'
#' @param data A data frame or tibble containing the variables.
#' @param x Unquoted name of the first numeric variable.
#' @param y Unquoted name of the second numeric variable.
#'
#' @return A ggplot object showing two QQ plots side-by-side.
#' @export
correlation_qq <- function(data, x, y) {
  library(ggplot2)
  library(rlang)
  library(dplyr)
  
  # Tidy evaluation
  x_q <- enquo(x)
  y_q <- enquo(y)
  
  x_chr <- as_name(x_q)
  y_chr <- as_name(y_q)
  
  # Check for numeric
  if (!is.numeric(pull(data, !!x_q)) | !is.numeric(pull(data, !!y_q))) {
    stop("Both x and y must be numeric variables.")
  }
  
  # Reshape for facetting
  df_long <- data %>%
    select(x = !!x_q, y = !!y_q) %>%
    pivot_longer(cols = everything(), names_to = "variable", values_to = "value")
  
  ggplot(df_long, aes(sample = value)) +
    stat_qq() +
    stat_qq_line(color = "steelblue") +
    facet_wrap(~variable, scales = "free") +
    theme_minimal(base_size = 14) +
    labs(title = "QQ Plots for Normality",
         x = "Theoretical Quantiles",
         y = "Sample Quantiles")
}


#' Linear Regression Summary
#'
#' Fits a linear model and returns coefficient estimates, standard errors, test statistics,
#' confidence intervals, and formatted p-values.
#'
#' @param data A data frame or tibble.
#' @param outcome Unquoted name of the outcome (y) variable.
#' @param function_of Model terms, e.g., x1 + x2 + x1:x2.
#' @param confidence Confidence level for the confidence interval. Default is 0.95.
#'
#' @return A tibble with term, estimate, standard error, t-statistic, CI bounds, and p-value.
#' @export
linear_regression <- function(data, outcome, function_of, confidence = 0.95) {
  # Capture expressions
  outcome_expr <- rlang::enexpr(outcome)
  rhs_expr     <- substitute(function_of)
  
  # Construct formula
  fmla <- as.formula(paste0(rlang::as_name(outcome_expr), " ~ ", deparse(rhs_expr)))
  
  # Fit model
  model <- lm(fmla, data = data)
  
  # Extract and format output
  broom::tidy(model, conf.int = TRUE, conf.level = confidence) %>%
    dplyr::mutate(
      p_value = dplyr::case_when(
        p.value < 0.001 ~ "< 0.001",
        TRUE ~ formatC(p.value, format = "f", digits = 3)
      )
    ) %>%
    dplyr::select(
      term,
      estimate,
      std_error = std.error,
      statistic,
      lower_bound = conf.low,
      upper_bound = conf.high,
      p_value
    )
}

#' Test for Significant Regression Line
#'
#' @param data A data frame or tibble
#' @param outcome Unquoted outcome variable (y)
#' @param function_of Unquoted formula RHS (e.g., x or x1 + x2)
#' @param family A glm family (default is gaussian)
#' @param alpha Significance level (default = 0.05)
#'
#' @return Printed output of LRT result
#' @export
significant_line <- function(data, outcome, function_of, family = gaussian(), alpha = 0.05) {
  library(tidyverse)
  library(glue)
  library(rlang)
  
  # Capture and convert unquoted input
  y <- enquo(outcome)
  y_chr <- as_name(y)
  rhs <- enquo(function_of)
  rhs_chr <- quo_text(rhs)
  
  # Extract variable names used in the model
  vars_rhs <- all.vars(parse_expr(rhs_chr))
  vars_all <- c(y_chr, vars_rhs)
  
  # Clean data: select needed columns and drop rows with missing
  df <- data %>%
    dplyr::select(all_of(vars_all)) %>%
    tidyr::drop_na()
  
  # Fit models
  full_model <- glm(formula = as.formula(glue("{y_chr} ~ {rhs_chr}")), data = df, family = family)
  null_model <- glm(formula = as.formula(glue("{y_chr} ~ 1")), data = df, family = family)
  
  # Likelihood ratio test
  lrt <- anova(null_model, full_model, test = "LRT")
  
  # Check structure and contents
  if (!("Deviance" %in% names(lrt)) || nrow(lrt) < 2) {
    stop("LRT failed: no valid deviance results found.")
  }
  
  df_diff  <- lrt$Df[2]
  dev_diff <- lrt$Deviance[2]
  p_val    <- lrt$`Pr(>Chi)`[2]
  
  # Format p-value
  p_text <- if (is.na(p_val)) {
    "p = NA"
  } else if (p_val < 0.001) {
    "p < 0.001"
  } else {
    glue("p = {formatC(round(p_val, 3), format = 'f', digits = 3)}")
  }
  
  # ── print results ───────────────────────────────────────────────────
  cat(glue("Likelihood Ratio Test for Significant Regression Line:\n\n"))
  cat(glue("Null: H₀: β₁ = β₂ = ... = βₖ = 0\n\n"))
  cat(glue("Alternative: H₁: At least one βᵢ ≠ 0\n\n"))
  cat(glue("Test statistic: χ²({df_diff}) = {round(dev_diff, 3)}\n\n"))
  cat(glue("p-value: {p_text}\n\n"))
  
  # Optional conclusion helper
  if (exists("conclusion", mode = "function")) {
    conclusion(p_val, alpha)
  }
}



#' Adjusted R-squared for Multiple Regression
#'
#' Calculates the adjusted R-squared value for a multiple linear regression model.
#'
#' @param data A data frame or tibble.
#' @param outcome Unquoted outcome variable (e.g., y).
#' @param function_of Unquoted predictors (e.g., x1 + x2).
#'
#' @return A numeric value: the adjusted R-squared.
#' @export
r_squared <- function(data, outcome, function_of) {
  # Capture inputs
  y <- rlang::as_name(rlang::enquo(outcome))
  rhs_expr <- rlang::enquo(function_of)
  
  # Parse the RHS expression into a character vector of variables
  rhs_vars <- all.vars(rhs_expr)
  
  # Construct the formula
  f <- reformulate(rhs_vars, response = y)
  
  # Fit model and extract adjusted R-squared
  model <- lm(f, data = data)
  round(summary(model)$adj.r.squared,4)
}


#' Two-Sample Proportion Confidence Interval
#'
#' @param data A data frame or tibble
#' @param outcome Unquoted binary outcome variable (e.g., "yes"/"no", 1/0)
#' @param grouping Unquoted grouping variable with exactly 2 levels
#' @param success The value of `outcome` considered a "success"
#' @param confidence Confidence level for the interval (default is 0.95)
#'
#' @return A tibble with the estimated difference in proportions and confidence interval
#' @export
#'
#' @examples
#' two_prop_CI(data = penguins, outcome = sex, group = species, success = "female")
two_prop_CI <- function(data, binary, grouping, event, confidence = 0.95) {
  library(tidyverse)
  library(rlang)
  
  outcome_sym <- enquo(binary)
  group_sym   <- enquo(grouping)
  
  df <- data %>%
    filter(!is.na(!!outcome_sym), !is.na(!!group_sym)) %>%
    mutate(
      outcome_chr = as.character(!!outcome_sym),
      group_chr = as.character(!!group_sym),
      success_flag = outcome_chr == as.character(event)
    )
  
  # Check number of groups
  group_levels <- df %>% pull(group_chr) %>% unique()
  if (length(group_levels) != 2) {
    stop("Grouping variable must have exactly two levels.")
  }
  
  group1 <- group_levels[1]
  group2 <- group_levels[2]
  
  summary_table <- df %>%
    group_by(group_chr) %>%
    summarise(
      successes = sum(success_flag),
      n = n(),
      .groups = "drop"
    ) %>%
    arrange(match(group_chr, group_levels)) # keep consistent order
  
  # Extract counts
  x <- summary_table$successes
  n <- summary_table$n
  
  # Calculate sample proportions
  p1 <- x[1] / n[1]
  p2 <- x[2] / n[2]
  diff <- p1 - p2
  
  test <- prop.test(x = x, n = n, conf.level = confidence, correct = FALSE)
  conf_percent <- round(confidence * 100)
  ci_lower <- test$conf.int[1]
  ci_upper <- test$conf.int[2]
  
  # Print results in a clear style
  cat(glue::glue("Sample proportion ({group1}): {round(p1, 4)}\n\n"))
  cat(glue::glue("Sample proportion ({group2}): {round(p2, 4)}\n\n"))
  cat(glue::glue("Point estimate for the difference in proportions (p̂[{group1}] − p̂[{group2}]): {round(diff, 4)}\n\n"))
  cat(glue::glue("{conf_percent}% confidence interval for π[{group1}] − π[{group2}]: ({round(ci_lower, 4)}, {round(ci_upper, 4)})\n\n"))
}



#' Two-Sample Hypothesis Test for Proportions
#'
#' @param data A data frame or tibble
#' @param outcome Unquoted binary outcome variable (e.g., "yes"/"no", 1/0)
#' @param grouping Unquoted grouping variable with exactly 2 levels
#' @param success The value of `outcome` considered a "success"
#' @param alternative Type of test: "two.sided", "less", or "greater"
#' @param alpha Significance level (default = 0.05)
#'
#' @return Prints test result
#' @export
#'
#' @examples
#' two_prop_HT(data = penguins, outcome = sex, group = species, success = "female")
two_prop_HT <- function(data, binary, grouping, event, p = 0, alternative = "two.sided", alpha = 0.05) {
  library(tidyverse)
  library(rlang)
  library(glue)
  
  binary_sym   <- enquo(binary)
  group_sym    <- enquo(grouping)
  
  df <- data %>%
    filter(!is.na(!!binary_sym), !is.na(!!group_sym)) %>%
    mutate(
      outcome_chr = as.character(!!binary_sym),
      group_chr = as.character(!!group_sym),
      success_flag = outcome_chr == as.character(event)
    )
  
  group_levels <- df %>% pull(group_chr) %>% unique()
  if (length(group_levels) != 2) {
    stop("Grouping variable must have exactly two levels. Found: ",
         paste(group_levels, collapse = ", "))
  }
  
  group1 <- group_levels[1]
  group2 <- group_levels[2]
  
  summary_table <- df %>%
    group_by(group_chr) %>%
    summarise(
      successes = sum(success_flag),
      n = n(),
      .groups = "drop"
    ) %>%
    arrange(match(group_chr, group_levels))
  
  x1 <- summary_table$successes[1]
  x2 <- summary_table$successes[2]
  n1 <- summary_table$n[1]
  n2 <- summary_table$n[2]
  p1 <- x1 / n1
  p2 <- x2 / n2
  phat_diff <- p1 - p2
  
  # Standard error for the difference in proportions
  SE <- sqrt((p1 * (1 - p1)) / n1 + (p2 * (1 - p2)) / n2)
  
  # z statistic
  z_stat <- round((phat_diff - p) / SE, 2)
  
  # p-value based on alternative
  p_val <- switch(alternative,
                  "two.sided" = 2 * pnorm(-abs(z_stat)),
                  "greater"   = pnorm(-z_stat),
                  "less"      = pnorm(z_stat),
                  stop("alternative must be one of 'two.sided', 'less', or 'greater'")
  )
  
  p_text <- if (p_val < 0.001) "p < 0.001" else glue("p = {formatC(round(p_val, 3), format = 'f', digits = 3)}")
  
  # Use helper functions for hypothesis symbols
  null_sym <- null_symbol(alternative)
  alt_sym  <- alt_symbol(alternative)
  
  cat(glue("Two-sample z-test for difference in proportions:\n\n\n"))
  cat(glue("p̂[{group1} = {round(p1, 4)} ({x1}/{n1})\n\n"))
  cat(glue("p̂[{group2} = {round(p2, 4)} ({x2}/{n2})\n\n"))
  cat(glue("p̂[{group1}] − p̂[{group2}] = {round(phat_diff, 4)}\n\n\n"))
  cat(glue("Null: H₀: π[{group1}] − π[{group2}] {null_sym} {p}\n\n"))
  cat(glue("Alternative: H₁: π[{group1}] − π[{group2}] {alt_sym} {p}\n\n"))
  cat(glue("Test statistic: z = {z_stat}\n\n"))
  cat(glue("p-value: {p_text}\n\n"))
  conclusion(p_val, alpha)
}


#' Chi-Square Goodness of Fit Test
#'
#' @param data A data frame or tibble
#' @param categorical Unquoted categorical variable
#' @param expected A named vector of expected proportions (should sum to 1). If NULL, equal proportions are assumed.
#' @param alpha Significance level (default = 0.05)
#'
#' @return Printed test result
#' @export
goodness_of_fit <- function(data, categorical, expected = NULL, alpha = 0.05) {
  library(tidyverse)
  library(rlang)
  library(glue)
  
  grouping_sym <- enquo(categorical)
  
  observed <- data %>%
    filter(!is.na(!!grouping_sym)) %>%
    count(category = !!grouping_sym) %>%
    arrange(category)
  
  observed$category <- as.character(observed$category)
  
  # Use expected proportions if provided
  if (is.null(expected)) {
    k <- nrow(observed)
    expected_props <- rep(1 / k, k)
    names(expected_props) <- observed$category
  } else {
    if (!isTRUE(all.equal(sum(expected), 1))) {
      stop("Expected proportions must sum to 1.")
    }
    if (!all(sort(names(expected)) == sort(observed$category))) {
      stop("Names of expected proportions must match the observed group labels.")
    }
    expected_props <- expected[observed$category]  # reorder to match observed
  }
  
  total_n <- sum(observed$n)
  expected_counts <- total_n * expected_props
  
  test <- suppressWarnings(chisq.test(x = observed$n, p = expected_props))
  
  df <- test$parameter
  chi2 <- round(test$statistic, 2)
  p_val <- test$p.value
  p_text <- if (p_val < 0.001) "p < 0.001" else glue("p = {formatC(round(p_val, 3), format = 'f', digits = 3)}")
  
  cat(glue("Chi-square goodness-of-fit test:\n\n"))
  cat(glue("Null: H₀: Observed frequencies match expected proportions\n\n"))
  cat(glue("Alternative: H₁: Observed frequencies do not match expected proportions\n\n"))
  cat(glue("Test statistic: χ²({df}) = {chi2}\n\n"))
  cat(glue("p-value: {p_text}\n\n"))
  conclusion(p_val, alpha)
}

#' Chi-Square Test of Independence
#'
#' @param data A data frame or tibble
#' @param var1 First unquoted categorical variable
#' @param var2 Second unquoted categorical variable
#' @param alpha Significance level (default = 0.05)
#'
#' @return Printed test results
#' @export
#'
#' @examples
#' independence_test(gss_cat, marital, religion)
independence_test <- function(data, var1, var2, alpha = 0.05) {
  
  var1_sym <- enquo(var1)
  var2_sym <- enquo(var2)
  
  # Clean and tabulate
  df <- data %>%
    filter(!is.na(!!var1_sym), !is.na(!!var2_sym)) %>%
    mutate(var1_chr = as.character(!!var1_sym),
           var2_chr = as.character(!!var2_sym))
  
  tbl <- table(df$var1_chr, df$var2_chr)
  
  # Run chi-square test (no Yates continuity correction)
  test <- suppressWarnings(chisq.test(tbl, correct = FALSE))
  
  df_stat <- test$parameter
  chi2 <- round(test$statistic, 2)
  p_val <- test$p.value
  p_text <- if (p_val < 0.001) "p < 0.001" else glue("p = {formatC(round(p_val, 3), format = 'f', digits = 3)}")
  
  var1_name <- as_label(var1_sym)
  var2_name <- as_label(var2_sym)
  
  cat(glue("Chi-square test for independence:\n\n"))
  cat(glue("Null: H₀: {var1_name} and {var2_name} are independent\n\n"))
  cat(glue("Alternative: H₁: {var1_name} and {var2_name} depend on one another\n\n"))
  cat(glue("Test statistic: χ²({df_stat}) = {chi2}\n\n"))
  cat(glue("p-value: {p_text}\n\n"))
  conclusion(p_val, alpha)
}

#' Logistic Regression Summary
#'
#' @param data A data frame or tibble
#' @param outcome Unquoted binary outcome variable
#' @param function_of Unquoted formula RHS (e.g., x1 + x2)
#' @param confidence Confidence level for interval (default = 0.95)
#'
#' @return A tibble with coefficient estimates, SE, z, CI, and p-value
#' @export
#'
#' @examples
#' logistic_regression(mtcars, outcome = am, function_of = mpg + wt)
logistic_regression <- function(data, outcome, function_of, confidence = 0.95) {
  
  # Capture variables
  y <- enquo(outcome)
  y_chr <- as_name(y)
  rhs <- enquo(function_of)
  rhs_chr <- quo_text(rhs)
  
  # Formula construction
  formula <- as.formula(glue("{y_chr} ~ {rhs_chr}"))
  
  # Fit logistic regression
  model <- glm(formula, data = data, family = binomial)
  
  # Get critical z
  z_crit <- qnorm(1 - (1 - confidence) / 2)
  
  # Get tidy output and compute CI manually
  broom::tidy(model) %>%
    mutate(
      lower_bound = estimate - z_crit * std.error,
      upper_bound = estimate + z_crit * std.error,
      p_value = ifelse(p.value < 0.001, "< 0.001",
                       formatC(round(p.value, 3), format = "f", digits = 3))
    ) %>%
    transmute(
      term,
      estimate = round(estimate, 4),
      std_error = round(std.error, 4),
      z = round(statistic, 2),
      lower_bound = round(lower_bound, 4),
      upper_bound = round(upper_bound, 4),
      p_value
    )
}
