
library(rstanarm)
library(dplyr)
library(tidyr)
library(ggplot2)
# library(JM)
options(mc.cores = min(4, parallel::detectCores()))
source('with_filecache.function.R')

data('aids', package = 'JM')
data('aids.id', package = 'JM')

str(aids)
str(aids.id)

aids2 <- aids %>%
  dplyr::mutate(sqrt_cd4 = sqrt(CD4))

# ---- fit longitudinal models ----

long0 <- with_filecache(stan_glmer(sqrt(CD4) ~ obstime + (1 |patient),
                   data = aids),
                   filename = 'long0.rds')

long1 <- with_filecache(stan_glmer(sqrt(CD4) ~ obstime + (1 + obstime |patient),
                   data = aids),
                   filename = 'long1.rds')

long2 <- with_filecache(stan_glmer(sqrt(CD4) ~ obstime + drug + obstime:drug + (1 + obstime | patient),
                    data = aids),
                    filename = 'long2.rds')

long3 <- with_filecache(stan_glmer(sqrt(CD4) ~ obstime + drug + obstime:drug + gender + prevOI + AZT + (1 + obstime | patient),
                                   data = aids),
                        filename = 'long3.rds')

long4 <- with_filecache(stan_glmer(sqrt(CD4) ~ obstime + drug + obstime:drug + prevOI + obstime:prevOI + gender + AZT + (1 + obstime | patient),
                                   data = aids),
                        filename = 'long4.rds')

long5 <- with_filecache(stan_glmer(sqrt(CD4) ~ obstime + drug + obstime:drug + prevOI + obstime:prevOI + drug:prevOI + gender + AZT + (1 + obstime | patient),
                                   data = aids),
                        filename = 'long5.rds')

long6 <- with_filecache(stan_glmer(sqrt(CD4) ~ obstime + drug + obstime:drug + prevOI + obstime:prevOI + gender + AZT + (1 + obstime | patient),
                                   data = aids),
                        filename = 'long6.rds')

# ---- use loo for model comparison ----

long0.loo <- with_filecache(loo(long0),
                            filename = 'long0.loo.rds')
long1.loo <- with_filecache(loo(long1),
                            filename = 'long1.loo.rds')
long2.loo <- with_filecache(loo(long2),
                            filename = 'long2.loo.rds')
long3.loo <- with_filecache(loo(long3),
                            filename = 'long3.loo.rds')
long4.loo <- with_filecache(loo(long4),
                            filename = 'long4.loo.rds')
long5.loo <- with_filecache(loo(long5),
                            filename = 'long5.loo.rds')
long6.loo <- with_filecache(loo(long6),
                            filename = 'long6.loo.rds')


loo_comp <- loo::compare(long0.loo, long1.loo, long2.loo, long3.loo, long4.loo, long5.loo, long6.loo)

model_list <- list(long0 = long0, long1 = long1, long2 = long2, long3 = long3, long4 = long4, long5 = long5, long6 = long6)


model_descriptions <- tbl_df(list(model_name = names(model_list),
                                  RHS = lapply(model_list, FUN = function(x) as.character(x$formula)[3])))

loo_comp_table <- as.data.frame(loo_comp) %>% 
  dplyr::mutate(model_name = gsub(rownames(.), pattern = '.loo', replacement = '')) %>%
  dplyr::left_join(model_descriptions, by = 'model_name')
  



