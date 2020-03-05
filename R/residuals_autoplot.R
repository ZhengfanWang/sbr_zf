#' residuals_autoplot 
#'
#' @param data
#' @param y
#' @param yhat
#'
#' @return
#' @export
#'
#' @examples
residuals_autoplot <- function(data,
                     y,
                     yhat) {
  y <- rlang::ensym(y)
  yhat <- rlang::ensym(yhat)
  exclude <- c("2.5%",
               "10%",
               "50%",
               "90%",
               "97.5%")
  data <- data %>%
    dplyr::mutate(residual = !!y-!!yhat)
  predictors <- data %>%
    dplyr::summarize_all(.funs=c(is.numeric)) %>%
    dplyr::select(-!!y, -exclude, -residual)
  ids <- predictors %>% unlist %>% as.vector()
  x_names <- names(predictors[ids])
  pl <- list()
  for (x_name in x_names) {
    pl[[x_name]] <- data %>%
      ggplot2::ggplot() +
      ggplot2::geom_point(
        ggplot2::aes_string(
          x = x_name,
          y = "residual"
          # color = "data_series_type",
          # shape = "group_type_relative_to_baseline"
        )
      ) +
      ggplot2::xlab(x_name)
  }
  return(pl)
}
