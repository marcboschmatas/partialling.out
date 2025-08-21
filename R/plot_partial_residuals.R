#' Function for plotting partial residuals
#' Uses `tinyplot` as backend

# nolint start
#' @title plot_partial_residuals: scatterplot of partial residuals
#' @param x a partial_residuals objects from `partialling_out()`
#' @param add_lm if TRUE, a `lm` will be plotted
#' @param quantile if TRUE, will plot only the mean values of the quantiles of the mean explanatory variable specified by `probs`
#' @param probs numeric vector of length one that specifies the number of quantiles to be computed if `quantile` is TRUE.
#' by default, 0.02, which will give 50 quantiles.
#' @param ... Any other `tinyplot::plt()` params
#' @returns invisibly, x
#' @importFrom tinyplot plt
#' @importFrom tinyplot plt_add
#' @importFrom stats aggregate
#' @examples
#' \donttest{library(palmerpenguins)
#' library(fixest)
#' model <- feols(bill_length_mm ~ bill_depth_mm | species + island,
#'                data = penguins)
#' partial_df <- partialling_out(model, penguins, both = TRUE)
#' plot_partial_residuals(partial_df)
#' }
#' @export
# nolint end
plot_partial_residuals <- function(
  x,
  add_lm = TRUE,
  quantile = FALSE,
  probs = .02,
  ...
) {
  fml <- as.formula(paste(colnames(x)[1], "~", colnames(x)[2]))
  if (!quantile) {
    tinyplot::plt(fml, data = x, type = "p")
  } else {
    x$qnt <- cut(
      x[[2]],
      breaks = quantile(x[[2]], probs = seq(0, 1, probs)),
      include.lowest = TRUE
    )

    fml_agg <- as.formula(paste0(
      "cbind(",
      colnames(x)[1],
      ", ",
      colnames(x)[2],
      ")",
      " ~ ",
      "qnt"
    ))

    res_qnt <- aggregate(
      fml_agg,
      data = x,
      FUN = mean
    )
    tinyplot::plt(fml, data = res_qnt, type = "p")
  }

  if (add_lm) {
    tinyplot::plt_add(fml, data = x, type = "lm")
  }
  invisible(x)
}
