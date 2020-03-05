
bin_markup <- function(data, groups) {
  t <- c()
  for (group in groups) {
    t1 <- data %>% dplyr::select(group) %>% unlist() %>% as.vector
    t <- paste0(t, t1)
  }
  
  dup1 <- t %>% duplicated(fromLast = TRUE)
  dup2 <- t %>% duplicated(fromLast = FALSE)
  dups <- dup1 | dup2
  t <- t %>% as.factor() %>% as.numeric()
  t[!dups] <- NA
  data <- data %>% 
    dplyr::mutate(duplicates = t) %>% 
    tibble::rowid_to_column("rownum") #add row numbers 
  data <- data %>% 
    dplyr::mutate(non_duplicate = as.numeric(is.na(duplicates)))
  return(data)
}





permutation_number <- function(data, duplicates_vec) {
  bin_size <- rep(NA, length(duplicates_vec))
  for(i in 1:length(duplicates_vec)) {
    bin_size[i] <- data %>% 
      dplyr::filter(duplicates == duplicates_vec[i]) %>%
      nrow()
  }
  return(prod(bin_size))
}


sample_picker <- function(data, duplicates_vec) {
  sample_set <- rep(NA, length(duplicates_vec))
  for(i in 1:length(duplicates_vec)) {
    rows <- data %>% 
      dplyr::filter(duplicates == duplicates_vec[i]) %>%
      dplyr::select("rownum") %>%
      unlist() %>%
      as.vector()
    sample_set[i] <- sample(rows, 1)
  }
  return(sample_set)
}


permutation_set_finder <- function(data, groups) {
  data <- data %>% bin_markup(groups)
  non_duplicate_rownum <- data %>% 
    dplyr::filter(non_duplicate == 1) %>% 
    dplyr::select(rownum) %>% 
    unlist() %>%
    as.vector()
  duplicates_vec <- data %>% 
    dplyr::filter(complete.cases(.)) %>% 
    dplyr::select("duplicates") %>% 
    unique() %>% 
    unlist() %>% 
    as.vector()
  perm_num <- permutation_number(data, duplicates_vec)
  i <- 0
  permutations <- list()
  repeat {
    i <- i + 1 
    permutations[[i]] <- sample_picker(data, duplicates_vec)
    if (permutations %>% lapply(is.null) %>% unlist %>% any) {
      toremove <- permutations %>% lapply(is.null) %>% unlist %>% which
      permutations[[toremove]] <- NULL
    }
    if (length(permutations) == perm_num) {
      break
    }
  }
  return(list(permutations = permutations, 
              non_duplicate_rownum = non_duplicate_rownum)
  )
}


