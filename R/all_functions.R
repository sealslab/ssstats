#' anova_check
#'
#' Produces diagnostic plots to assess ANOVA (and regression) assumptions
#' visually from a fitted linear model object.
#' This includes residuals vs fitted values plot, histogram of residuals
#' with normal curve overlay, and Q-Q plot of standardized residuals.
#'
#' @param model A fitted regression model object of class \code{lm}, \code{glm}, or \code{aov}.
#'
#' @return A \code{ggpubr} arranged ggplot object containing three diagnostic plots.
#'
#' @examples
#' \dontrun{
#' fit <- lm(bill_length_mm ~ bill_depth_mm, data = palmerpenguins::penguins)
#' anova_check(fit)
#' }
#'
#' @import ggplot2
#' @import ggpubr
#' @export
anova_check <- function(model) {
  # Check class of model
  if (!any(class(model) %in% c("lm", "aov", "glm"))) {
    stop("Not a valid regression model.
         Make sure object is created via `lm()`, `glm()`, or `aov()`")
  }

  # Extract residuals and fitted values
  res <- residuals(model)
  fitted <- fitted.values(model)
  stdresid <- rstandard(model)  # standardized residuals

  # Create data frame for plotting
  df <- data.frame(fitted = fitted, resid = res, stdresid = stdresid)

  # Residuals vs Fitted plot
  res_fitted <- ggplot2::ggplot(df, ggplot2::aes(x = fitted, y = resid)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                        color = "black", linewidth = 1) +
    ggplot2::geom_point(color = "#6A6C6E") +
    ggplot2::labs(y = "Residuals", x = "Fitted values",
                  title = "Residuals vs Fitted Plot") +
    ggplot2::theme_classic()

  # Histogram of residuals with normal curve
  res_his <- ggplot2::ggplot(df, ggplot2::aes(x = resid)) +
    ggplot2::geom_histogram(aes(y = after_stat(density)),
                            colour = "black", fill = "#6A6C6E", bins = 30) +
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
    ggplot2::stat_qq(color = "#6A6C6E") +
    ggplot2::stat_qq_line(color = "black", linewidth = 1, linetype = "dashed") +
    ggplot2::labs(x = "Theoretical Quantiles", y = "Standardized Residuals",
                  title = "Q-Q Plot of Standardized Residuals") +
    ggplot2::theme_classic()

  # Arrange plots together
  out <- ggpubr::ggarrange(res_fitted, res_his, qq_res, ncol = 2, nrow = 2,
                           labels = c("A", "B", "C"))

  return(out)
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
n_pct <- function(data, row_var, col_var = NULL, rows = 3, digits = 1, denom = "col") {
  row_var_enquo <- rlang::enquo(row_var)
  col_var_enquo <- rlang::enquo(col_var)

  if (rlang::quo_is_null(col_var_enquo)) {
    # One-way table
    tab_n <- janitor::tabyl(data, !!row_var_enquo)
    total_n <- sum(tab_n[[2]])

    tab_oneway <- tab_n %>%
      mutate(
        pct = .[[2]] / total_n,
        pct_fmt = scales::percent(pct, accuracy = 10^(-digits)),
        `n (pct)` = paste0(.[[2]], " (", pct_fmt, ")")
      ) %>%
      select(!!rlang::as_name(row_var_enquo), `n (pct)`) %>%
      slice_head(n = rows)

    print(tab_oneway)
    invisible(tab_oneway)

  } else {
    # Two-way table
    tab_n <- janitor::tabyl(data, !!row_var_enquo, !!col_var_enquo)
    tab_pct <- janitor::adorn_percentages(tab_n, denominator = denom)
    tab_fmt <- janitor::adorn_pct_formatting(tab_pct, digits = digits)

    row_var_name <- rlang::as_name(row_var_enquo)

    tab_n_long <- tab_n %>%
      pivot_longer(-1, names_to = "col", values_to = "n")

    tab_fmt_long <- tab_fmt %>%
      pivot_longer(-1, names_to = "col", values_to = "pct")

    tab_joined <- left_join(tab_n_long, tab_fmt_long, by = c(row_var_name, "col")) %>%
      mutate(`n (pct)` = paste0(n, " (", pct, ")")) %>%
      select(all_of(row_var_name), col, `n (pct)`) %>%
      pivot_wider(names_from = col, values_from = `n (pct)`) %>%
      slice_head(n = rows)

    print(tab_joined)
    invisible(tab_joined)
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
#' one_mean_CI(mtcars, mpg)
#' @export
one_mean_CI <- function(data, continuous, confidence = 0.95) {
  var_values <- data %>% dplyr::pull({{ continuous }})
  ttest <- t.test(var_values, conf.level = confidence)

  mean_val <- mean(var_values, na.rm = TRUE)
  sd_val <- sd(var_values, na.rm = TRUE)
  ci_lower <- ttest$conf.int[1]
  ci_upper <- ttest$conf.int[2]
  conf_percent <- confidence * 100

  tibble(
    mean = round(mean_val, 4),
    sd = round(sd_val, 4),
    conf_level = paste0(round(conf_percent), "%"),
    ci_lower = round(ci_lower, 4),
    ci_upper = round(ci_upper, 4)
  )
}

#' Calculate Proportion and Confidence Interval for a Binary Outcome
#'
#' @param data A data frame or tibble.
#' @param grouping A binary grouping variable (unquoted).
#' @param success The value in `grouping` considered a "success" (can be numeric, character, or factor).
#' @param confidence Confidence level for the interval (default 0.95).
#' @importFrom tibble tibble
#' @return A tibble with proportion estimate, number of successes, sample size, confidence level, and confidence interval bounds.
#' @examples
#' one_prop_CI(mtcars, am, success = 1)
#' @export
one_prop_CI <- function(data, grouping, success, confidence = 0.95) {
  binary_vector <- data %>% dplyr::pull({{ grouping }}) %>% na.omit()

  # Convert to character for consistent comparison
  binary_vector_chr <- as.character(binary_vector)
  success_chr <- as.character(success)

  n <- length(binary_vector_chr)
  x <- sum(binary_vector_chr == success_chr)
  p_hat <- x / n

  test <- prop.test(x = x, n = n, conf.level = confidence, correct = FALSE)
  ci_lower <- test$conf.int[1]
  ci_upper <- test$conf.int[2]
  conf_percent <- confidence * 100

  tibble(
    proportion = round(p_hat, 4),
    successes = x,
    sample_size = n,
    conf_level = paste0(round(conf_percent), "%"),
    ci_lower = round(ci_lower, 4),
    ci_upper = round(ci_upper, 4)
  )
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

#' Print Conclusion Based on p-value and Significance Level
#'
#' @param p_val Numeric p-value.
#' @param alpha Numeric significance level.
#' @return None. Prints conclusion to console.
#' @export
conclusion <- function(p_val, alpha) {
  p_text <- if (p_val < 0.001) "< 0.001" else as.character(round(p_val, 4))

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
  cat(glue::glue("Null: H0: μ = {mu}\n\n"))
  cat(glue::glue("Alternative: H1: μ {alt_symbol(alternative)} {mu}\n\n"))
  cat(glue::glue("Test statistic: t({df}) = {t_stat}\n\n"))
  cat(glue::glue("p-value: {p_text}\n\n"))
  conclusion(p_val, alpha)
}

#' One-sample z-test for a population proportion
#'
#' @param data Data frame or tibble.
#' @param grouping Unquoted column name for the binary variable.
#' @param success Character value indicating which level is considered "success".
#' @param p Numeric hypothesized proportion (default 0.5).
#' @param alternative Character string specifying alternative hypothesis; "two.sided", "less", or "greater" (default "two.sided").
#' @param alpha Numeric significance level (default 0.05).
#' @return None. Prints formatted test results and conclusion.
#' @export
one_prop_HT <- function(data, grouping, success, p = 0.5, alternative = "two.sided", alpha = 0.05) {
  binary_vector <- data %>% dplyr::pull({{ grouping }}) %>% na.omit()
  binary_vector_chr <- as.character(binary_vector)
  success_chr <- as.character(success)

  n <- length(binary_vector_chr)
  x <- sum(binary_vector_chr == success_chr)

  test <- prop.test(x = x, n = n, p = p, alternative = alternative, correct = FALSE)

  z_stat <- round(sqrt(test$statistic), 2)  # chi-sq stat is z^2
  p_val <- test$p.value
  p_text <- if (p_val < 0.001) "p < 0.001" else glue::glue("p = {formatC(round(p_val, 3), format = 'f', digits = 3)}")

  cat(glue::glue("One-sample z-test for the population proportion:\n\n"))
  cat(glue::glue("Null: H0: π = {p}\n\n"))
  cat(glue::glue("Alternative: H1: π {alt_symbol(alternative)} {p}\n\n"))
  cat(glue::glue("Test statistic: z = {z_stat}\n\n"))
  cat(glue::glue("p-value: {p_text}\n\n"))
  conclusion(p_val, alpha)
}

#' Independent samples mean confidence interval (equal variance assumed)
#'
#' @param data Data frame or tibble.
#' @param grouping Unquoted column name for grouping variable (factor).
#' @param continuous Unquoted column name for continuous outcome.
#' @param confidence Confidence level for interval (default 0.95).
#' @return None. Prints the confidence interval and point estimate.
#' @export
independent_mean_CI <- function(data, grouping, continuous, confidence = 0.95, variance = "equal") {
  grouping_q  <- rlang::enquo(grouping)
  continuous_q <- rlang::enquo(continuous)
  
  var_equal <- ifelse(variance == "equal", TRUE, FALSE)

  df <- data %>% dplyr::select(!!grouping_q, !!continuous_q) %>% tidyr::drop_na()

  grp_name <- rlang::quo_name(grouping_q)
  cont_name <- rlang::quo_name(continuous_q)

  fml <- stats::reformulate(termlabels = grp_name, response = cont_name)

  ttest <- t.test(
    formula   = fml,
    data      = data,
    conf.level= confidence,
    var.equal = var_equal
  )

  conf_pct <- round(confidence * 100)
  ci_lower <- round(ttest$conf.int[1], 4)
  ci_upper <- round(ttest$conf.int[2], 4)
  mean_diff<- round(diff(rev(ttest$estimate)), 4)

  cat(glue::glue("The point estimate for the difference in means is x̄₁ − x̄₂ = {mean_diff}\n\n"))
  cat(glue::glue("The {conf_pct}% confidence interval for μ₁ − μ₂ is ({ci_lower}, {ci_upper})\n\n"))
}

#' Independent samples t-test for mean difference
#'
#' @param data Data frame or tibble.
#' @param grouping Unquoted column name for grouping variable (factor).
#' @param continuous Unquoted column name for continuous outcome.
#' @param alternative Character string specifying alternative hypothesis; "two.sided", "less", or "greater" (default "two.sided").
#' @param mu Numeric hypothesized mean difference (default 0).
#' @param alpha Numeric significance level (default 0.05).
#' @param variance Character string; "equal" (default) or "unequal" variance assumption.
#' @return None. Prints formatted test results and conclusion.
#' @export
independent_mean_HT <- function(data,
                                grouping,
                                continuous,
                                alternative = "two.sided",
                                mu = 0,
                                alpha = 0.05,
                                variance = "equal") {
  grouping_q   <- rlang::enquo(grouping)
  continuous_q <- rlang::enquo(continuous)

  var_equal <- ifelse(variance == "equal", TRUE, FALSE)

  df <- data %>%
    dplyr::select(!!grouping_q, !!continuous_q) %>%
    tidyr::drop_na()

  grp_name  <- rlang::quo_name(grouping_q)
  cont_name <- rlang::quo_name(continuous_q)

  fml <- stats::reformulate(termlabels = grp_name, response = cont_name)

  ttest <- t.test(
    formula    = fml,
    data       = data,
    alternative= alternative,
    mu         = mu,
    conf.level = 1 - alpha,
    var.equal  = var_equal
  )

  t_stat  <- round(ttest$statistic, 3)
  df_val  <- round(as.numeric(ttest$parameter), 2)
  p_val   <- ttest$p.value
  est1    <- ttest$estimate[1]
  est2    <- ttest$estimate[2]

  p_text <- if (p_val < 0.001) {
    "p < 0.001"
  } else {
    glue::glue("p = {formatC(round(p_val, 3), format = 'f', digits = 3)}")
  }

  null_text <- glue::glue("H₀: μ₁ − μ₂ = {mu}")
  alt_text  <- switch(
    alternative,
    two.sided = glue::glue("H₁: μ₁ − μ₂ ≠ {mu}"),
    less      = glue::glue("H₁: μ₁ − μ₂ < {mu}"),
    greater   = glue::glue("H₁: μ₁ − μ₂ > {mu}"),
    stop("`alternative` must be one of \"two.sided\", \"less\", \"greater\"")
  )

  test_type <- if (variance == "equal") {
    "Two-sample t-test for two independent means and equal variance:"
  } else {
    "Two-sample t-test for two independent means and unequal variance:"
  }

  cat(glue::glue("{test_type}\n\n"))
  cat(glue::glue("Null: {null_text}\n\n"))
  cat(glue::glue("Alternative: {alt_text}\n\n"))
  cat(glue::glue("Test statistic: t({df_val}) = {t_stat}\n\n"))
  cat(glue::glue("p-value: {p_text}\n\n"))
  conclusion(p_val, alpha)
}

#' Paired mean/median summary table
#'
#' @param data Data frame or tibble.
#' @param col1 Unquoted column name for "before" values.
#' @param col2 Unquoted column name for "after" values.
#' @param accuracy Integer rounding digits for summary statistics (default 1).
#' @return A tibble summarizing mean (SD) and median (IQR) for before, after, and their differences.
#' @importFrom dplyr select
#' @importFrom tibble tibble
#' @export
dependent_mean_median <- function(data, col1, col2, accuracy = 3) {
  before_q <- rlang::enquo(col1)
  after_q  <- rlang::enquo(col2)

  df <- data %>% dplyr::select(!!before_q, !!after_q) %>% tidyr::drop_na()

  before_vals <- df %>% dplyr::pull(!!before_q)
  after_vals  <- df %>% dplyr::pull(!!after_q)
  diff_vals   <- after_vals - before_vals

  m_diff   <- mean(diff_vals,    na.rm = TRUE)
  s_diff   <- sd(diff_vals,      na.rm = TRUE)
  med_diff <- median(diff_vals,  na.rm = TRUE)
  iqr_diff <- IQR(diff_vals,     na.rm = TRUE)

  m1   <- mean(before_vals, na.rm = TRUE)
  s1   <- sd(before_vals,   na.rm = TRUE)
  med1 <- median(before_vals, na.rm = TRUE)
  iqr1 <- IQR(before_vals,    na.rm = TRUE)

  m2   <- mean(after_vals, na.rm = TRUE)
  s2   <- sd(after_vals,   na.rm = TRUE)
  med2 <- median(after_vals, na.rm = TRUE)
  iqr2 <- IQR(after_vals,    na.rm = TRUE)

  tibble::tibble(
    Variable       = c(
      paste0(rlang::as_label(after_q), " – ", rlang::as_label(before_q)),
      rlang::as_label(before_q),
      rlang::as_label(after_q)
    ),
    `Mean (SD)`    = c(
      glue::glue("{round(m_diff,  accuracy)} ({round(s_diff,  accuracy)})"),
      glue::glue("{round(m1,      accuracy)} ({round(s1,      accuracy)})"),
      glue::glue("{round(m2,      accuracy)} ({round(s2,      accuracy)})")
    ),
    `Median (IQR)` = c(
      glue::glue("{round(med_diff,accuracy)} ({round(iqr_diff,accuracy)})"),
      glue::glue("{round(med1,    accuracy)} ({round(iqr1,    accuracy)})"),
      glue::glue("{round(med2,    accuracy)} ({round(iqr2,    accuracy)})")
    )
  )
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
  x <- data %>% dplyr::pull({{ col1 }})
  y <- data %>% dplyr::pull({{ col2 }})

  valid <- complete.cases(x, y)
  x <- x[valid]
  y <- y[valid]

  differences <- x - y

  ttest <- t.test(x, y, paired = TRUE, conf.level = confidence)

  mean_diff <- mean(differences)
  sd_diff <- sd(differences)
  ci_lower <- round(ttest$conf.int[1], 4)
  ci_upper <- round(ttest$conf.int[2], 4)
  mean_rounded <- round(mean_diff, 4)
  sd_rounded <- round(sd_diff, 4)
  conf_percent <- round(confidence * 100)

  cat(glue::glue("The point estimate for the mean difference is x̄ = {mean_rounded}.\n\n"))
  cat(glue::glue("The point estimate for the standard deviation of differences is s = {sd_rounded}.\n\n"))
  cat(glue::glue("The {conf_percent}% confidence interval for the mean difference μ_d is ({ci_lower}, {ci_upper}).\n\n"))
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
  x <- data %>% dplyr::pull({{ col1 }})
  y <- data %>% dplyr::pull({{ col2 }})

  complete <- complete.cases(x, y)
  x <- x[complete]
  y <- y[complete]

  ttest <- t.test(x, y,
                  paired = TRUE,
                  alternative = alternative,
                  mu = mu,
                  conf.level = 1 - alpha)

  t_stat  <- round(ttest$statistic, 3)
  df      <- round(as.numeric(ttest$parameter), 2)
  p_val   <- ttest$p.value
  mean_d  <- round(mean(x - y), 4)

  null_text <- glue::glue("H₀: μ_d = {mu}")
  alt_text  <- switch(
    alternative,
    two.sided = glue::glue("H₁: μ_d ≠ {mu}"),
    less      = glue::glue("H₁: μ_d < {mu}"),
    greater   = glue::glue("H₁: μ_d > {mu}"),
    stop("`alternative` must be one of \"two.sided\", \"less\", \"greater\"")
  )

  p_text <- if (p_val < 0.001) "p < 0.001" else glue::glue("p = {formatC(round(p_val, 3), format = 'f', digits = 3)}")

  cat(glue::glue("Paired t-test for the mean of differences:\n\n"))
  cat(glue::glue("Null: {null_text}\n\n"))
  cat(glue::glue("Alternative: {alt_text}\n\n"))
  cat(glue::glue("Test statistic: t({df}) = {t_stat}\n\n"))
  cat(glue::glue("p-value: {p_text}\n\n"))

  conclusion(p_val, alpha)
}
#' normality_correlation
#'
#' Performs Shapiro-Wilk normality tests and generates Q-Q plots for each numeric variable
#' in a data frame. Also computes a correlation matrix using the specified method.
#'
#' @param data A data frame containing numeric variables.
#' @param method Correlation method to use; one of \code{"pearson"}, \code{"kendall"}, or \code{"spearman"}.
#' @param digits_desired Optional integer specifying the number of digits to round numeric output. Default is 5.
#'
#' @return A named list with components:
#' \describe{
#'   \item{Normality Test}{A data frame with Shapiro-Wilk test statistics and p-values for each numeric variable.}
#'   \item{Correlation Matrix}{A correlation matrix of numeric variables rounded to the specified digits.}
#'   \item{QQ Plots}{A \code{ggpubr} arranged plot object containing Q-Q plots of each numeric variable.}
#' }
#'
#' @examples
#' \dontrun{
#' normality_correlation(palmerpenguins::penguins, method = "pearson")
#' normality_correlation(mtcars, method = "kendall")
#' }
#'
#' @import ggplot2
#' @import ggpubr
#' @importFrom purrr map map_df
#' @importFrom dplyr mutate
#' @export
normality_correlation <- function(data, method = c("pearson", "kendall", "spearman"), digits_desired = 5) {
  # Internal helper function to create Q-Q plot for a variable
  one_qq_plot <- function(data, var_name) {
    ggplot2::ggplot(data, ggplot2::aes(sample = .data[[var_name]])) +
      ggplot2::stat_qq() +
      ggplot2::stat_qq_line() +
      ggplot2::labs(title = paste("Q-Q Plot:", var_name)) +
      ggplot2::theme_minimal()
  }

  # Validate inputs
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }

  method <- match.arg(method)

  if (!is.numeric(digits_desired) || digits_desired < 0) {
    stop("`digits_desired` must be a non-negative numeric value.")
  }

  # Subset numeric columns only
  numeric_subset <- data[sapply(data, is.numeric)]
  numeric_names <- colnames(numeric_subset)

  if (length(numeric_names) == 0) {
    stop("The data frame contains no numeric variables. Check variable types with `typeof()`.")
  }

  # Generate Q-Q plots for each numeric variable
  qq_plots_list <- purrr::map(numeric_names, ~one_qq_plot(numeric_subset, .x))

  # Arrange Q-Q plots in a grid layout
  cols <- ceiling(sqrt(length(qq_plots_list)))
  rows <- ceiling(length(qq_plots_list) / cols)
  qq_plots_out <- ggpubr::ggarrange(plotlist = qq_plots_list, ncol = cols, nrow = rows)

  # Compute correlation matrix with rounding
  corr_mat <- round(cor(numeric_subset, method = method), digits = digits_desired)

  # Shapiro-Wilk test on each numeric variable, tidy results
  normality_res <- purrr::map_df(numeric_subset, ~{
    test <- shapiro.test(.x)
    data.frame(
      statistic = unname(test$statistic),
      p_value = test$p.value
    )
  }, .id = "Variable") %>%
    dplyr::mutate(
      statistic = round(statistic, digits = digits_desired),
      p_value = round(p_value, digits = digits_desired)
    ) %>%
    dplyr::rename(`Test Statistic` = statistic, `p-value` = p_value)


  # Return results as a named list
  list(
    "Normality Test" = normality_res,
    "Correlation Matrix" = corr_mat,
    "QQ Plots" = qq_plots_out
  )
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
#' model <- lm(bill_length_mm ~ bill_depth_mm + year, penguins)
#' cooks(model)
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

#' independent_qq_plot
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

#' dependent_qq_plot
#'
#' Constructs a QQ plot for the difference scores between two paired measurements.
#'
#' @param data A data frame in wide format.
#' @param col1 A string specifying the column name of the first measurement.
#' @param col2 A string specifying the column name of the second measurement.
#' @return A ggplot object representing the QQ plot of the paired differences.
#' @examples
#' dependent_qq_plot(a1c_measurements, "first_measurement", "second_measurement")
#' @export
dependent_qq <- function(data, col1, col1) {
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

