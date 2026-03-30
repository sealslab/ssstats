#' Within-Group Residuals Plot
#'
#' @description Produces a jitter plot of within-group residuals for each
#'   group level, useful for visually assessing the equal-variance assumption.
#'
#' @param data A data frame or tibble.
#' @param continuous Unquoted column name for the continuous outcome (numeric).
#' @param grouping Unquoted column name for the grouping variable.
#'
#' @return A \code{ggplot2} plot object.
#'
#' @export
#' @importFrom rlang enquo `!!`
#' @importFrom dplyr group_by mutate
#' @importFrom ggplot2 ggplot aes geom_jitter geom_hline labs theme_minimal theme
#' @importFrom magrittr %>%
plot_residuals <- function(data, continuous, grouping) {
  grouping_q <- rlang::enquo(grouping)
  outcome_q  <- rlang::enquo(continuous)

  data_resid <- data %>%
    dplyr::group_by(!!grouping_q) %>%
    dplyr::mutate(residual = !!outcome_q - mean(!!outcome_q, na.rm = TRUE))

  ggplot2::ggplot(data_resid, ggplot2::aes(x = !!grouping_q, y = residual, color = !!grouping_q)) +
    ggplot2::geom_jitter(width = 0.2, alpha = 0.7) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::labs(title = "Group Residuals",
                  y = "Residual",
                  x = "Groups") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")
}
