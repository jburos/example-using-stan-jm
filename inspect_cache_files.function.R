
library(rstanarm)
library(ggplot2)
source('analyze_simulated_irAE_data.function.R')

#' Inspect cache files in cache_dir
#' return list of filenames matching given parameters provided
#' (any parameters not provided are matched as wildcards)
get_cache_files <- function(cache_dir = CACHE_DIR, 
                            prefix = 'binomial_data_sim',
                            filetypes = c('stan_jm_fit'),
                            max_treedepth = 15,
                            n = c(200, 500),
                            iter = c(2000, 5000),
                            ...) {
  argg <- c(as.list(environment()), list(...))
  argg[['cache_dir']] <- NULL
  argg[['prefix']] <- NULL
  argg[['filetypes']] <- NULL
  
  all_files <- dir(cache_dir, pattern = '.rda')
  all_files <- stringr::str_subset(all_files, pattern = stringr::str_c('\\b', prefix, '\\.'))
  all_files <- stringr::str_subset(all_files, pattern = stringr::str_c('\\b', filetypes, '\\.rda'))
  
  all_patterns <- purrr::map2(names(argg), argg, ~ stringr::str_c('\\.', .x, '-', .y, '\\.'))

  filter_results <- function(file, patterns = all_patterns) {
    res <- file
    for (i in seq_len(length(patterns))) {
      res2 <- unlist(unique(stringr::str_subset(res, pattern = patterns[[i]])))
      res <- res2
    }
    res
  }
  
  res <- purrr::map(all_files, filter_results) %>%
    purrr::compact() %>%
    unique() %>%
    unlist()

}
                            
                            