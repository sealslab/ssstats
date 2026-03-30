#' GLM Coefficient Table
#'
#' @description Builds a table of coefficients and confidence intervals
#'   from one or more fitted model objects (\code{lm}, \code{glm}). Optionally
#'   exponentiates estimates for log-link models.
#'
#' @param ... One or more fitted model objects.
#' @param model_names Optional character vector of model names. Defaults to
#'   \code{"Model 1"}, \code{"Model 2"}, etc.
#' @param exp Logical. If \code{TRUE}, exponentiate estimates and confidence
#'   intervals. Defaults to \code{FALSE}.
#'
#' @return A tibble with one row per predictor and columns for each model's
#'   estimate (95% CI) and p-value.
#'
#' @export
#' @importFrom broom tidy
#' @importFrom dplyr filter mutate select full_join
#' @importFrom purrr map
#' @importFrom magrittr %>%
glm_table <- function(..., model_names = NULL, exp = FALSE) {

  # Get all models as a list
  models <- list(...)
  n_models <- length(models)
  
  # Create default model names if not provided
  if (is.null(model_names)) {
    model_names <- paste("Model", 1:n_models)
  }
  
  # Helper function to format estimate and CI
  format_est_ci <- function(estimate, conf.low, conf.high) {
    paste0(sprintf("%.2f", estimate), " (", 
           sprintf("%.2f", conf.low), ", ", 
           sprintf("%.2f", conf.high), ")")
  }
  
  # Process each model
  process_model <- function(model) {
    result <- model %>%
      broom::tidy(conf.int = TRUE) %>%
      dplyr::filter(term != "(Intercept)")
    
    # Exponentiate if requested
    if (exp) {
      result <- result %>%
        dplyr::mutate(
          estimate = exp(estimate),
          conf.low = exp(conf.low),
          conf.high = exp(conf.high)
        )
    }

    result <- result %>%
      dplyr::mutate(
        est_ci = format_est_ci(estimate, conf.low, conf.high),
        p_value = format.pval(p.value, digits = 3, eps = 0.001)
      ) %>%
      dplyr::select(term, est_ci, p_value)

    return(result)
  }

  # Process all models
  all_results <- purrr::map(models, process_model)

  # Combine all results
  results <- all_results[[1]]

  if (n_models > 1) {
    for (i in 2:n_models) {
      results <- results %>%
        dplyr::full_join(all_results[[i]], by = "term", suffix = c("", paste0("_m", i)))
    }
    
    # Fix column names for first model (which doesn't get suffix)
    names(results)[2:3] <- paste0(names(results)[2:3], "_m1")
  }
  
  # Rename columns to use model names
  # Use appropriate label based on exp parameter
  est_label <- if (exp) "Est (95% CI)" else "Coef (95% CI)"
  
  new_names <- c("Predictor")
  for (i in 1:n_models) {
    new_names <- c(new_names, 
                   paste0(model_names[i], " ", est_label),
                   paste0(model_names[i], " p-value"))
  }
  names(results) <- new_names
  
  return(results)
}
