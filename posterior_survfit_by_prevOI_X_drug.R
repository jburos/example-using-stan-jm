
library(rstanarm)
library(dplyr)
library(tidyr)
library(ggplot2)
# library(JM)
options(mc.cores = min(4, parallel::detectCores()))
source('with_filecache.function.R')

data('aids', package = 'JM')
data('aids.id', package = 'JM')

aids2 <- aids %>%
  dplyr::mutate(sqrt_cd4 = sqrt(CD4))

model_name <- 'f7'
model_cache_file <- paste0(model_name, '.rds')

# ---- load fit model ---- 

jmfit <- readRDS(file.path(CACHE_DIR, model_cache_file))

# ---- prep data for survfit ---- 

# ---- ... among AIDS subgroup ----

with_aids <- aids2 %>% 
  dplyr::filter(obstime == min(obstime)) %>%
  dplyr::filter(prevOI == 'AIDS')

with_aids_ddI <- with_aids %>%
  dplyr::filter(drug == 'ddI')
with_aids_ddI.id <- aids.id %>%
  dplyr::semi_join(with_aids_ddI, by='patient')

with_aids_ddC <- with_aids %>%
  dplyr::filter(drug == 'ddC')
with_aids_ddC.id <- aids.id %>%
  dplyr::semi_join(with_aids_ddC, by='patient')

# ---- ... among noAIDS subgroup ----

no_aids <- aids2 %>% 
  dplyr::filter(obstime == min(obstime)) %>%
  dplyr::filter(prevOI == 'noAIDS')

no_aids_ddI <- no_aids %>%
  dplyr::filter(drug == 'ddI')
no_aids_ddI.id <- aids.id %>%
  dplyr::semi_join(no_aids_ddI, by='patient')

no_aids_ddC <- no_aids %>%
  dplyr::filter(drug == 'ddC')
no_aids_ddC.id <- aids.id %>%
  dplyr::semi_join(no_aids_ddC, by='patient')

# ---- posterior_survfit by drug*AIDS subgroups ---- 

with_aids_ddI_ppsurv <- with_filecache(
  rstanarm::posterior_survfit(
    jmfit,
    newdataLong = with_aids_ddI,
    newdataEvent = with_aids_ddI.id,
    standardise = TRUE,
    times = 0,
    extrapolate = TRUE,
    control = list(condition = FALSE)
  ), filename = paste0(model_name, '.posterior_survfit.with_aids_ddI.rds'))

with_aids_ddC_ppsurv <- with_filecache(
  rstanarm::posterior_survfit(
    jmfit,
    newdataLong = with_aids_ddC,
    newdataEvent = with_aids_ddC.id,
    standardise = TRUE,
    times = 0,
    extrapolate = TRUE,
    control = list(condition = FALSE)
  ), filename = paste0(model_name, '.posterior_survfit.with_aids_ddC.rds'))

no_aids_ddI_ppsurv <- with_filecache(
  rstanarm::posterior_survfit(
    jmfit,
    newdataLong = no_aids_ddI,
    newdataEvent = no_aids_ddI.id,
    standardise = TRUE,
    times = 0,
    extrapolate = TRUE,
    control = list(condition = FALSE)
  ), filename = paste0(model_name, '.posterior_survfit.no_aids_ddI.rds'))

no_aids_ddC_ppsurv <- with_filecache(
  rstanarm::posterior_survfit(
    jmfit,
    newdataLong = no_aids_ddC,
    newdataEvent = no_aids_ddC.id,
    standardise = TRUE,
    times = 0,
    extrapolate = TRUE,
    control = list(condition = FALSE)
  ), filename = paste0(model_name, '.posterior_survfit.no_aids_ddC.rds'))

