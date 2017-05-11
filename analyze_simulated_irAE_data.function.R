
#devtools::load_all('../rstanarm-sam/')
library(rstanarm)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(stringr)
library(purrr)
options(mc.cores = min(parallel::detectCores(), 6))
source('with_filecache.function.R')

#' generate cache filename
#' @param filetype string describing file
#' @param prefix prefix for cached files
#' @param namedparams named list of parameters used to generate filename
#' 
#' @return string containing cached filename
cache_filename <- function(filetype, 
                           prefix = 'binomial_data_sim',
                           named_params = params) {
  ## reorder named_params alphabetically by name
  paramlist <- named_params[sort(names(named_params), decreasing = TRUE)]
  paramstrings <- purrr::map2_chr(paramlist, names(paramlist), ~ stringr::str_c(.y, .x, sep='-'))
  paramstring <- stringr::str_c(paramstrings, collapse = '.')
  stringr::str_c(prefix, paramstring, filetype, 'rda', sep = '.')
}

#' simulate irAE data using joint model, then fit model to simulated data
#' @param n sample size of data to simulate
#' @param seed data-simulation seed 
#' @param iter number of iterations
#' @param stan_seed seed used to set stan initial values
#' @param init either 'model_based' or 'random'
#' @param max_treedepth control parameter to stan fit 
#' @param basehaz either 'weibull', 'bs' or 'piecewise'
#' @param betaLong_binary true parameter value to use for simulation
#' @param betaEvent_assoc true parameter value to use for simulation
#' 
#' @return object containing simulated data (simdat), stan_jm_fit, ps_check & recovery_plot
analyze_simulated_irae_data <- function(max_treedepth = 15,
                                        n = 200,
                                        seed = 12345,
                                        stan_seed = 1234,
                                        iter = 5000,
                                        init = 'model_based',
                                        adapt_delta = 0.9999,
                                        basehaz = 'weibull',
                                        betaLong_binary = -0.6,
                                        betaEvent_assoc = 0.05
                                        ) 
  {
  set.seed(seed)
  params <- rstanarm:::nlist(max_treedepth, n, init, seed, iter, adapt_delta, stan_seed, basehaz,
                             betaLong_binary, betaEvent_assoc)
  
  get_cachefile <- purrr::partial(cache_filename, named_params = params)
  
  simdat <- with_filecache(
    rstanarm::simjm(M = 1, 
                    n = n,
                    betaLong_intercept = log(0.1),     ## 10% base rate 
                    betaLong_binary = betaLong_binary, ## true beta for Z1
                    betaLong_continuous = -0.1,        ## true beta for Z2
                    betaLong_slope = 0.1,              ## true beta for tij (slope)
                    b_sd = c(1, 0.05),
                    betaEvent_intercept = -9,
                    betaEvent_assoc = betaEvent_assoc, ## increased risk among ppl with irAE
                    family = binomial()),
    filename = get_cachefile(filetype = 'simulated_data'))
  
    dataLong <- simdat %>% 
      dplyr::select(id, Z1, Z2, tij, starts_with('Yij'))
    
    dataEvent <- simdat %>%
      dplyr::select(id, Z1, Z2, event, eventtime) %>%
      dplyr::distinct(id, .keep_all = TRUE)
    
    stan_jm_fit <- with_filecache(
      stan_jm(formulaLong = Yij_1 ~ Z1 + Z2 + tij + (1 + tij | id),
              dataLong = dataLong,
              family = list(binomial),
              time_var = 'tij',
              formulaEvent = Surv(eventtime, event) ~ Z1 + Z2,
              dataEvent = dataEvent,
              assoc = 'etavalue',
              basehaz = basehaz,
              init = init,
              init_r = 1,
              iter = iter,
              seed = stan_seed,
              adapt_delta = adapt_delta,
              control = list(max_treedepth = max_treedepth)
      ),
      filename = get_cachefile(filetype = 'stan_jm_fit'))
    
    ps_check <- with_filecache(ps_check(stan_jm_fit),
                        file = get_cachefile(filetype = 'stan_jm_ps_check'))
    
    trueparams <- c('betaLong_binary', 'betaLong_continuous', 'betaLong_slope', 'betaEvent_assoc')
    params <- c('Long1|Z1', 'Long1|Z2', 'Long1|tij', 'Assoc|Long1|etavalue')
    
    paramvals <- as.array(stan_jm_fit)[, , params, drop = FALSE]
    truevals <- as.numeric(attr(simdat, 'params')[trueparams])
    recovery_plot <- bayesplot::mcmc_recover_hist(paramvals, truevals, facet_args = list(scales = 'free', ncol = 1))
    
    structure(simdat = simdat,
              stan_jm_fit = stan_jm_fit,
              ps_check = ps_check,
              recovery_plot = recovery_plot,
              dataLong = dataLong,
              dataEvent = dataEvent
    )
    }

analyze_simulated_irae_data_given_params <- purrr::lift_dl(analyze_simulated_irae_data)
