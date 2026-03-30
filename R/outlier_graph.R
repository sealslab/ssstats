#' Outlier Plot from a Regression Model
#'
#' Produces a scatterplot of two variables from a fitted model's data, coloring
#' observations by suspected outlier status based on standardized residuals.
#' Observations with \eqn{|\hat{\varepsilon}_i| > 2.5} are flagged as
#' \code{"Suspected"} outliers; all others are \code{"Not Suspected"}. The plot
#' title reports the total number of suspected outliers.
#'
#' @param df A dataframe or tibble containing the original data passed to the model.
#' @param model A fitted model object of class \code{lm}, \code{glm}, or \code{aov}.
#' @param x_var Unquoted column name for the x-axis variable.
#' @param y_var Unquoted column name for the y-axis variable.
#' @param x_lab Character string for the x-axis label. Defaults to the name of
#'   \code{x_var}.
#' @param y_lab Character string for the y-axis label. Defaults to the name of
#'   \code{y_var}.
#'
#' @return A \code{ggplot} object.
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom broom augment
#' @importFrom dplyr mutate filter if_else
#' @importFrom ggplot2 ggplot aes geom_point scale_color_manual theme_bw labs
#' @importFrom rlang enquo as_name `!!`
outlier_graph <- function(df, model, x_var, y_var, x_lab = NULL, y_lab = NULL) {
  if (!inherits(model, c("lm", "glm", "aov"))) {
    stop("Not a valid regression model. Use `glm`, `lm`, or `aov`.")
  }

  x_q   <- rlang::enquo(x_var)
  y_q   <- rlang::enquo(y_var)
  x_chr <- rlang::as_name(x_q)
  y_chr <- rlang::as_name(y_q)

  if (is.null(x_lab)) x_lab <- x_chr
  if (is.null(y_lab)) y_lab <- y_chr

  data <- broom::augment(model, newdata = df) %>%
    dplyr::mutate(.std.resid = .resid / sd(.resid, na.rm = TRUE)) %>%
    dplyr::filter(!is.na(.std.resid)) %>%
    dplyr::mutate(outlier = dplyr::if_else(abs(.std.resid) > 2.5, "Suspected", "Not Suspected", "Missing"))

  n_suspected <- sum(data$outlier == "Suspected", na.rm = TRUE)

  ggplot2::ggplot(data, ggplot2::aes(x = !!x_q, y = !!y_q, color = outlier)) +
    ggplot2::geom_point() +
    ggplot2::scale_color_manual(values = c("Not Suspected" = "#999999", "Suspected" = "#000000")) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = x_lab, y = y_lab, color = "Outlier",
                  title = paste0("There are ", n_suspected, " suspected outliers."))
}
