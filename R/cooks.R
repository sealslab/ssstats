#' Cook's Distance Plot
#'
#' Produces a Cook's Distance plot for a fitted \code{lm}, \code{glm}, or
#' \code{aov} model. Each observation is shown as a point with a drop line from
#' zero. Optionally, influential observations can be labelled by their row
#' number and one or more threshold reference lines can be overlaid.
#'
#' @param fitted.lm A fitted model object of class \code{lm}, \code{glm}, or \code{aov}.
#' @param label Logical. If \code{TRUE} (default), labels observations that
#'   exceed the minimum threshold value with their row number.
#' @param show.threshold Logical. If \code{TRUE}, adds dashed horizontal line(s)
#'   at the threshold value(s). Default is \code{FALSE}.
#' @param threshold Character string specifying the threshold convention.
#'   One of \code{"convention"} (default; \eqn{4/n} and 1),
#'   \code{"baseR"} (0.5 and 1), or \code{"matlab"} (3 × mean Cook's distance).
#' @param scale.factor Numeric. Controls the size of points and drop-line width.
#'   Default is \code{0.5}.
#' @param n_labels_desired Integer or \code{NULL}. Maximum number of observations
#'   to label, selected from those with the largest Cook's distances. If
#'   \code{NULL} (default), all observations exceeding the threshold are eligible.
#' @param label_height_nudge Numeric. Vertical offset applied to observation
#'   labels, in the same units as Cook's distance. Default is \code{0}.
#'
#' @return A \code{ggplot} object.
#'
#' @export
#' @importFrom broom augment
#' @importFrom ggplot2 ggplot aes geom_point geom_linerange geom_text geom_hline xlab ylab ylim theme_bw
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
