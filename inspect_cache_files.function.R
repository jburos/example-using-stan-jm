
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
                            iter = c(2000, 3000, 5000, 8000),
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

summarize_cached_result <- function(stanfit_cache, cache_dir = CACHE_DIR) {
  
  simdata_cache <- stringr::str_replace(stanfit_cache, pattern = 'stan_jm_fit', replacement = 'simulated_data')
  pscheck_cache <- stringr::str_replace(stanfit_cache, pattern = 'stan_jm_fit', replacement = 'stan_jm_ps_check')
  
  # load data
  simdat <- readRDS(file.path(cache_dir, simdata_cache))

  dataLong <- simdat %>% 
    dplyr::select(id, Z1, Z2, tij, starts_with('Yij'))
  
  dataEvent <- simdat %>%
    dplyr::select(id, Z1, Z2, event, eventtime) %>%
    dplyr::distinct(id, .keep_all = TRUE)
  
  # load stan_jm_fit
  stan_jm_fit <- readRDS(file.path(cache_dir, stanfit_cache))
  
  # ps_check
  ps_check <- readRDS(file.path(cache_dir, pscheck_cache))

  # recover params
  recovery_plot <- function() {
    trueparams <- c('betaLong_binary', 'betaLong_continuous', 'betaLong_slope', 'betaEvent_assoc')
    params <- c('Long1|Z1', 'Long1|Z2', 'Long1|tij', 'Assoc|Long1|etavalue')
    
    paramvals <- as.array(stan_jm_fit)[, , params, drop = FALSE]
    truevals <- as.numeric(attr(simdat, 'params')[trueparams])
    bayesplot::mcmc_recover_hist(paramvals, truevals, facet_args = list(scales = 'free', ncol = 1))
  }
  
  rstanarm:::nlist(stan_jm_fit,
                   simdat = simdat,
                   ps_check = ps_check,
                   recovery_plot = recovery_plot,
                   dataLong = dataLong,
                   dataEvent = dataEvent,
                   beta_assoc = as.numeric(attr(simdat, 'params')['betaEvent_assoc']),
                   beta_binary = as.numeric(attr(simdat, 'params')['betaLong_binary']),
                   n = nrow(dataEvent)
                   )
}
                            
                            