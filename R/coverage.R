#' coverage
#'
#' @param data \emph{\sQuote{Tibble}} formated as our example package data \code{\link[rvalidate:data]{rvalidate:data}}
#' @param y \emph{\sQuote{Character}} columm name of y in your data
#' @param lower \emph{\sQuote{Character}} columm name of lower bound in your data
#' @param upper \emph{\sQuote{Character}} columm name of upper bound in your data
#'
#' @return \emph{\sQuote{Tibble}} as a table with coverage
#' @export
coverage <- function(data,
                     y,
                     lower,
                     upper,
                     subset = NULL
                     ) {
  y <- rlang::ensym(y)
  lower <- rlang::ensym(lower)
  upper <- rlang::ensym(upper)
  subset <- rlang::syms(subset)
  data <- data %>%
    dplyr::mutate(out = as.numeric(y < lower | y > upper)) %>%
    dplyr::select(!!!subset, out) %>%
    dplyr::group_by(!!!subset) %>%
    dplyr::summarise("coverage" = 1 - (sum(out)/nrow(.)))
  return(data)
}
