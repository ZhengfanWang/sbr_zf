#' residuals
#'
#' \describe{
#'    \item{error (denoted \eqn{\epsilon}) = \eqn{y - \hat{y}}}
#'    \item{standard error = \eqn{\epsilon / sd(\epsilon)}}
#'    \item{adjusted error = \eqn{\epsilon / (1 / total standard error )}}
#' }
#'
#' @param data \emph{\sQuote{Tibble}} formated as our example package data \code{\link[rvalidate:rv_data]{rvalidate:rv_data}}
#' @param y \emph{\sQuote{Character}} columm name of y in your data
#' @param yhat  \emph{\sQuote{Character}} column name of yhat in your data
#' @param total_standard_error \emph{\sQuote{Character}} column name of total standard error in your data
#'
#' @return \emph{\sQuote{Tibble}} as a table with errors
#' @export
#'
residuals <- function(data, y, yhat, total_standard_error, subset = NULL)
{
  y <- rlang::ensym(y)
  yhat <- rlang::ensym(yhat)
  total_standard_error <- rlang::ensym(total_standard_error)
  subset <- rlang::syms(subset)
  exclude <- purrr::map(rlang::syms(subset), function(x) {dplyr::expr(-!!x)})
  edata <- data %>%
    dplyr::mutate(residual = !!y - !!yhat) %>%
    dplyr::mutate(standardized_residual = residual/sd(residual)) %>%
    dplyr::mutate(adjusted_residual = residual/(1-!!total_standard_error)) %>%
    dplyr::select(!!!subset, residual, standardized_residual, adjusted_residual) %>%
    tidyr::gather("residual", "value",  !!!exclude) %>%
    dplyr::group_by(residual, !!!subset) %>%
    dplyr::summarise_all(.funs=c("mean" = mean,
                                 "mean_absolute" = function(x) { x %>% abs() %>% mean() },
                                 "median"= median,
                                 "median_absolute" = function(x) { x %>% abs() %>% median()}))
  return(edata)
}






















# Archivied, version with less tidyverse
#
# residual <- function(data, y, yhat, total_standard_error, subset = NULL)
# {
#   if (is.null(subset)) {
#     resid <- data[[y]] - data[[yhat]]
#     e <- matrix(c(resid,
#                   resid / sd(resid),
#                   resid / (1 / data[[total_standard_error]])),
#                 ncol = 3)
#     rownames<- c("error", "standard error", "adjusted error")
#     edata <- tibble::tibble(
#                             "." = rownames,
#                             mean = e %>% apply(2, mean),
#                             absolute_mean = e %>% apply(2, abs) %>% apply(2, mean),
#                             median = e %>% apply(2, median),
#                             absolute_median = e %>% apply(2, abs) %>% apply(2, median)
#     )
#   } else {
#     sets <- unique(data[[subset]])
#     symsubset <- rlang::sym(subset)
#     colname <- rlang::quo_name(subset)
#     rownames <- c("error", "standard error", "adjusted error")
#     edata <- tibble::tibble(!!rlang::quo_name(colname) := NA,
#                             "." = rownames,#rep(rownames, length(sets)),
#                             mean = NA,
#                             absolute_mean = NA,
#                             median = NA,
#                             absolute_median = NA
#                             )
#     elist <- list()
#     for(i in 1:length(sets)) {
#       set <- sets[i]
#       tempdata <- data %>% dplyr::filter(!!symsubset == set)
#       resid <- tempdata[[y]] - tempdata[[yhat]]
#       e <- matrix(c(resid,
#                     resid / sd(resid),
#                     resid / (1 / tempdata[[total_standard_error]])),
#                   ncol = 3)
#       edata <- tibble::tibble(!!rlang::quo_name(colname) := set,
#                               "." = rownames,#rep(rownames, length(sets)),
#                               mean = e %>% apply(2, mean),
#                               absolute_mean = e %>% apply(2, abs) %>% apply(2, mean),
#                               median = e %>% apply(2, median),
#                               absolute_median = e %>% apply(2, abs) %>% apply(2, median)
#       )
#       elist[[i]] <- edata
#     }
#     edata <- do.call(rbind, elist)
#   }
#   edata <- edata %>%
#     dplyr::mutate(error_numeric_code = as.numeric(as.factor(.))) %>%
#     dplyr::arrange(error_numeric_code) %>%
#     dplyr::select(".", !!rlang::quo_name(colname), dplyr::everything(), -error_numeric_code)
#   return(edata)
# }
