devtools::load_all('../rstanarm-sam/')
#library(rstanarm)
library(dplyr)
library(tidyr)
library(ggplot2)
library(survminer)
library(survival)
library(lme4)
options(mc.cores = min(parallel::detectCores(), 4))
set.seed(12345)
source('with_filecache.function.R')

## ---- data preparation ---- 

simdat <- with_filecache(
  rstanarm::simjm(M = 1, 
                  betaLong_intercept = log(0.1),  ## 10% base rate 
                  betaLong_binary = -0.6,         
                  betaLong_continuous = -0.1,
                  betaLong_slope = 0.1,
                  b_sd = c(1, 0.05),
                  betaEvent_intercept = -9,
                  betaEvent_assoc = 0.05, ## increased risk among ppl with irAE
                  family = binomial()),
  filename = 'binomial_data_sim.simdat.rda')

  
dataLong <- simdat %>% 
  dplyr::select(id, Z1, Z2, tij, starts_with('Yij'))

dataEvent <- simdat %>%
  dplyr::select(id, Z1, Z2, event, eventtime) %>%
  dplyr::distinct(id, .keep_all = TRUE)

## ---- data inspection ---- 

## plot frequency of binary events
ggplot(dataLong %>% gather('var', 'value', starts_with('Yij')),
       aes(x = tij, y = value, group = id)) + geom_line()

## plot survival curves
survfit(Surv(eventtime, event) ~ Z1, data = dataEvent) %>%
  ggsurvplot(fit = .)

## ---- fit naive models to long data ---- 

## glm on binary event outcome
(glm_fit <- glm(Yij_1 ~ Z1 + Z2 + tij,
               data = dataLong,
               family = binomial()
               ))

## similar results using glmer, accounting for clustering by id
(glmer_fit1 <- glmer(Yij_1 ~ Z1 + Z2 + tij + (1 | id),
                    data = dataLong,
                    family = binomial()
                    ))
(glmer_fit2 <- glmer(Yij_1 ~ Z1 + Z2 + tij + (1 + tij |id),
                    data = dataLong,
                    family = binomial()
                    ))

## has bug; come back to research later
# stan_glmer_fit1 <- stan_glmer(Yij_1 ~ Z1 + Z2 + tij + (1 + tij | id),
#                               data = dataLong,
#                               family = binomial(),
#                               prior_intercept = cauchy(0,10), 
#                               prior_aux = cauchy(0, 10)
#                               )

## ---- fit joint model ----

stan_jm_fit <- readRDS(file.path(CACHE_DIR, 'binomial_data_sim.stan_jm_fit.rda'))

## ---- review model fit ----

ps_check(stan_jm_fit)

## ---- summarize parameter recovery ----

trueparams <- c('betaLong_binary', 'betaLong_continuous', 'betaLong_slope')
params <- c('Long1|Z1', 'Long1|Z2', 'Long1|tij')

paramvals <- as.array(stan_jm_fit)[, , params, drop = FALSE]
truevals <- as.numeric(attr(simdat, 'params')[trueparams])
bayesplot::mcmc_recover_hist(paramvals, truevals, facet_args = list(scales = 'free', ncol = 1))
