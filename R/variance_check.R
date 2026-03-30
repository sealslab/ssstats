#' Variance Diagnostic Plot
#'
#' @description Produces a residuals vs. fitted scatterplot for assessing the
#'   equal-variance assumption. Accepts a fitted model object or raw data with
#'   column names.
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
#' @return A \code{ggplot2} plot object.
#'
#' @export
#' @importFrom rlang enquo as_name
#' @importFrom ggplot2 ggplot aes geom_point labs geom_hline theme_classic
#' @importFrom stats lm fitted.values residuals
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
    model <- stats::lm(formula, data = data)
  } else {
    stop("Must provide either a `model` object or `data`, `continuous`, and `function_of`.")
  }
  
  # Create residuals vs fitted plot
  df <- data.frame(
    fitted = stats::fitted.values(model),
    resid = stats::residuals(model)
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
