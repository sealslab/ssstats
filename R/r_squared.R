#' Adjusted R-Squared
#'
#' @description Computes the adjusted R-squared from a fitted model object or
#'   directly from data and variable names.
#'
#' @param model A fitted model object of class \code{lm}, \code{glm} (identity
#'   link only), or \code{aov}. If \code{NULL}, \code{data}, \code{outcome},
#'   and \code{function_of} must be supplied.
#' @param data A data frame or tibble. Used only when \code{model} is
#'   \code{NULL}.
#' @param outcome Unquoted column name for the outcome variable. Used only when
#'   \code{model} is \code{NULL}.
#' @param function_of Unquoted expression for the right-hand side of the model
#'   formula. Used only when \code{model} is \code{NULL}.
#'
#' @return A numeric scalar: the adjusted R-squared, rounded to 4 decimal
#'   places.
#'
#' @export
#' @importFrom rlang enquo as_name
#' @importFrom stats lm
r_squared <- function(model = NULL, data = NULL, outcome = NULL, function_of = NULL) {
  # Check for model input or data-driven input
  if (!is.null(model)) {
    # Ensure the provided model is valid
    if (!inherits(model, c("lm", "glm", "aov"))) {
      stop("The `model` argument must be a regression model of class 'lm', 'glm', or 'aov'.")
    }
    
    # Deal with glm objects by refitting as lm, if necessary
    if (inherits(model, "glm")) {
      # Ensure the glm has an `identity` link function
      if (model$family$link != "identity") {
        stop("This function only supports glm objects with an identity link (i.e., equivalent to lm).")
      }
      
      # Refit glm as lm: Extract data and formula from the glm object
      model <- stats::lm(formula(model), data = model$model)
    }
  } else if (!is.null(data) && !is.null(outcome) && !is.null(function_of)) {
    # If no model, construct a formula from `outcome` and `function_of`
    y <- rlang::as_name(rlang::enquo(outcome))
    rhs_expr <- rlang::enquo(function_of)
    
    # Parse the RHS expression into a character vector of variables
    rhs_vars <- all.vars(rhs_expr)
    
    # Construct the formula
    f <- reformulate(rhs_vars, response = y)
    
    # Fit the model
    model <- stats::lm(f, data = data)
  } else {
    stop("You must provide either a `model` object or `data`, `outcome`, and `function_of`.")
  }
  
  # Calculate and return the adjusted R-squared value
  round(summary(model)$adj.r.squared, 4)
}
