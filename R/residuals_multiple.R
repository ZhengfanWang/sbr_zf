#' residuals_multiple
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
residuals_multiple <-
  function(data,
           y,
           yhat,
           total_standard_error,
           subset,
           group_to_check)
  {
  y <- rlang::ensym(y)
  yhat <- rlang::ensym(yhat)
  total_standard_error <- rlang::ensym(total_standard_error)
  subset <- rlang::syms(subset)
  exclude <- purrr::map(rlang::syms(subset), function(x) {dplyr::expr(-!!x)})
  sets <- data %>%
    permutation_set_finder(group_to_check)
  permutations <- sets$permutations
  non_duplicate_rownum <- sets$non_duplicate_rownum
  e_list <- list()
  for (j in 1:length(permutations)) {
    dsamp <- data[c(non_duplicate_rownum, permutations[[j]]),]
    edata <- dsamp %>%
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
    e_list[[j]] <- edata
  }
  ld <- do.call(rbind, e_list)
  ld <- ld %>%
    dplyr::group_by(residual, !!!subset) %>%
    dplyr::summarise_all(.funs = c(meansd)) #%>%
    # dplyr::mutate(error_numeric_code = as.numeric(as.factor(statistic))) %>%
    # dplyr::arrange(error_numeric_code) %>%
    # dplyr::select(statistic, !!subset, dplyr::everything(),-error_numeric_code)


  return(ld)
}
