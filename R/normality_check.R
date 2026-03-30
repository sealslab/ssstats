#' Normality Diagnostic Plots
#'
#' @description Produces a QQ plot and histogram of residuals for assessing
#'   the normality assumption of a regression or ANOVA model. Accepts a fitted
#'   model object or raw data with column names.
#'
#' @param model A fitted model object of class \code{lm}, \code{glm}, or
#'   \code{aov}. If \code{NULL}, \code{data}, \code{continuous}, and
#'   \code{function_of} must be supplied.
#' @param data A data frame or tibble. Used only when \code{model} is
#'   \code{NULL}.
#' @param continuous Unquoted column name for the continuous outcome. Used only
#'   when \code{model} is \code{NULL}.
#' @param function_of Unquoted column name for the grouping or predictor
#'   variable. Used only when \code{model} is \code{NULL}.
#'
#' @return A \code{ggpubr} arranged plot object containing a QQ plot and a
#'   histogram of residuals.
#'
#' @export
#' @importFrom rlang enquo as_name
#' @importFrom ggplot2 ggplot aes stat_qq stat_qq_line labs theme_classic
#'   geom_histogram after_stat stat_function geom_vline theme element_blank
#' @importFrom ggpubr ggarrange
#' @importFrom stats lm residuals rstandard dnorm sd
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
    model <- stats::lm(formula, data = data)
  } else {
    stop("Must provide either a `model` object or `data`, `continuous`, and `function_of`.")
  }

  # Extract residuals and standardized residuals
  res <- stats::residuals(model)
  stdresid <- stats::rstandard(model)  # Standardized residuals
  
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
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                            colour = "black", fill = "#C5C4C4", bins = 30) +
    ggplot2::stat_function(fun = stats::dnorm,
                           args = list(mean = mean(res), sd = stats::sd(res)),
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
