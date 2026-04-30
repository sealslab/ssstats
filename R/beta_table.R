#' Beta Table
#'
#' @description Builds a table of odds ratios and confidence
#' intervals from one or more fitted beta regression model objects. 
#' Estimates and CIs are exponentiated and combined into a single tibble.
#'
#' @param ... One or more fitted beta regression model objects.
#' @param model_names Optional character vector of model names. Defaults to
#'   \code{"Model 1"}, \code{"Model 2"}, etc.
#'
#' @return A tibble with one row per predictor and columns for each model's
#'   OR (95% CI) and p-value.
#'
#' @export
#' @importFrom broom tidy
#' @importFrom dplyr filter mutate select full_join
#' @importFrom purrr map
#' @importFrom magrittr %>%
beta_table <- function(..., model_names = NULL) {

  # Get all models as a list
  models <- list(...)
  n_models <- length(models)
  
  # Create default model names if not provided
  if (is.null(model_names)) {
    model_names <- paste("Model", 1:n_models)
  }
  
  # Helper function to format OR and CI
  format_or_ci <- function(estimate, conf.low, conf.high) {
    paste0(sprintf("%.2f", estimate), " (", 
           sprintf("%.2f", conf.low), ", ", 
           sprintf("%.2f", conf.high), ")")
  }
  
  # Process each model
  process_model <- function(model) {
    model %>%
      broom::tidy(conf.int = TRUE) %>%
      dplyr::filter(term != "(Intercept)", component == "mu") %>%
      dplyr::mutate(
        estimate = exp(estimate),
        conf.low = exp(conf.low),
        conf.high = exp(conf.high),
        or_ci = format_or_ci(estimate, conf.low, conf.high),
        p_value = format.pval(p.value, digits = 3, eps = 0.001)
      ) %>%
      dplyr::select(term, or_ci, p_value)
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
  new_names <- c("Predictor")
  for (i in 1:n_models) {
    new_names <- c(new_names, 
                   paste0(model_names[i], " OR (95% CI)"),
                   paste0(model_names[i], " p-value"))
  }
  names(results) <- new_names
  
  return(results)
}
