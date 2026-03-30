#' Count Suspected Outliers from a Regression Model
#'
#' Classifies observations from a fitted model as suspected outliers based on
#' their standardized residuals, then returns a summary count. Observations
#' with \eqn{|\hat{\varepsilon}_i| > 2.5} are flagged as \code{"Suspected"};
#' all others are \code{"Not Suspected"}. Missing standardized residuals are
#' labeled \code{"Missing"}.
#'
#' @param model A fitted model object of class \code{lm}, \code{glm}, or \code{aov}.
#'
#' @return A tibble with columns \code{outlier} (classification) and
#'   \code{count} (number of observations in each category).
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom broom augment
#' @importFrom dplyr mutate if_else group_by summarise n
#' 
outlier_count <- function(model) {
  if (!inherits(model, c("lm", "glm", "aov"))) {
    stop("Not a valid regression model. Use `glm`, `lm`, or `aov`.")
  }

  broom::augment(model) %>%
    dplyr::mutate(outlier = dplyr::if_else(abs(.std.resid) > 2.5, "Suspected", "Not Suspected", "Missing")) %>%
    dplyr::group_by(outlier) %>%
    dplyr::summarise(count = dplyr::n(), .groups = "drop")
}
