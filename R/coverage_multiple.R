#' coverage_multiple
#'
#' when there are multiple obervations from same group eg. two observations
#' with the same country and year
#'
#' @param data
#' @param y
#' @param lower
#' @param upper
#' @param group_to_check {\emph{\sQuote{Character}} A vector of column names in which the multiple observations occur eg. c("country", "year")
#'
#' @return
#' @export
coverage_multiple <- function(data,
                              y,
                              lower,
                              upper,
                              subset = NULL,
                              group_to_check) {
  y <- rlang::ensym(y)
  lower <- rlang::ensym(lower)
  upper <- rlang::ensym(upper)
  subset <- rlang::syms(subset)
  sets <- data %>%
    permutation_set_finder(group_to_check)
  permutations <- sets$permutations
  non_duplicate_rownum <- sets$non_duplicate_rownum
  cov_list <- list()
  for (j in 1:length(permutations)) {
    dsamp <- data[c(non_duplicate_rownum, permutations[[j]]), ]
    dsamp <- dsamp %>%
      dplyr::mutate(out = as.numeric(y < lower | y > upper)) %>%
      dplyr::select(!!!subset, out) %>%
      dplyr::group_by(!!!subset) %>%
      dplyr::summarise("coverage" = 1 - (sum(out)/nrow(.)))
    cov_list[[j]] <- dsamp
  }
  ld <- do.call(rbind, cov_list)
  ld <- ld %>%
    dplyr::group_by(!!!subset) %>%
    dplyr::summarise_all(.funs = c(meansd))
  return(ld)
}
