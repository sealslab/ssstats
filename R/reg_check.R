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
#' @importFrom ggplot2 ggplot aes geom_point labs geom_hline theme_bw
#'   geom_histogram after_stat stat_function geom_vline theme element_blank
#'   stat_qq stat_qq_line
#' @importFrom ggpubr ggarrange
#' @importFrom rlang enquo as_name
#' @importFrom stats lm residuals fitted.values rstandard dnorm sd
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
    fml <- as.formula(paste0(outcome_chr, " ~ ", group_chr))
    model <- stats::lm(fml, data = data)
  } else {
    stop("Must provide either a `model` object or `data`, `continuous`, and `function_of`.")
  }

  # Extract residuals and fitted values
  res <- stats::residuals(model)
  fitted_vals <- stats::fitted.values(model)
  stdresid <- stats::rstandard(model)  # Standardized residuals
  
  # Create data frame for plotting
  df <- data.frame(fitted = fitted_vals, resid = res, stdresid = stdresid)
  
  # Residuals vs Fitted plot
  res_fitted <- ggplot2::ggplot(df, ggplot2::aes(x = fitted, y = resid)) +
    ggplot2::geom_point(color = "#C5C4C4") +
    ggplot2::labs(y = "Residuals", 
                  x = "Fitted values",
                  title = "Scatterplot of Residuals") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                        color = "black", linewidth = 1) +
    ggplot2::theme_bw()
  
  # Histogram of residuals with normal curve
  res_his <- ggplot2::ggplot(df, ggplot2::aes(x = resid)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                            colour = "black", fill = "#C5C4C4", bins = 30) +
    ggplot2::stat_function(fun = stats::dnorm,
                           args = list(mean = mean(res), sd = stats::sd(res)),
                           color = "black", linewidth = 1) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                        color = "black", linewidth = 1.5) +
    ggplot2::labs(x = "Residuals", 
                  y = "Density", 
                  title = "Histogram of Residuals") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.ticks.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank())
  
  # Q-Q plot of standardized residuals
  qq_res <- ggplot2::ggplot(df, ggplot2::aes(sample = stdresid)) +
    ggplot2::stat_qq(color = "#C5C4C4") +
    ggplot2::stat_qq_line(color = "black", linewidth = 1, linetype = "dashed") +
    ggplot2::labs(x = "Theoretical Quantiles", y = "Standardized Residuals",
                  title = "Q-Q Plot of Standardized Residuals") +
    ggplot2::theme_bw()
  
  # Arrange plots together
  out <- ggpubr::ggarrange(qq_res, res_his, res_fitted, ncol = 2, nrow = 2,
                           labels = c("A", "B", "C"))
  
  return(out)
}