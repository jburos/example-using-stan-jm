
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




# ---- joint model ---- 

# eta-value only
f0 <- with_filecache(stan_jm(formulaLong = sqrt_cd4 ~ obstime*drug + (1 + obstime | patient), 
                             dataLong = aids2,
                             time_var = "obstime",
                             formulaEvent = Surv(Time, death) ~ gender + drug + AZT + prevOI, 
                             dataEvent = aids.id,
                             chains = 4,
                             assoc = c("etavalue"),
                             basehaz = 'bs',
                             adapt_delta = 0.999
),
filename = 'f0.rds')

f1 <- with_filecache(stan_jm(formulaLong = sqrt_cd4 ~ obstime*drug + (1 + obstime | patient), 
                             dataLong = aids2,
                             time_var = "obstime",
                             formulaEvent = Surv(Time, death) ~ gender + drug + AZT + prevOI, 
                             dataEvent = aids.id,
                             chains = 4,
                             assoc = c("etavalue", "etaslope"),
                             basehaz = 'bs',
                             adapt_delta = 0.999
), filename = 'f1.rds')

f2 <- with_filecache(stan_jm(formulaLong = sqrt_cd4 ~ obstime*drug + (1 + obstime | patient), 
                             dataLong = aids2,
                             time_var = "obstime",
                             formulaEvent = Surv(Time, death) ~ gender + drug + AZT + prevOI, 
                             dataEvent = aids.id,
                             chains = 4,
                             assoc = c("etavalue", "etaslope", 'etavalue_data(~ drug)'),
                             basehaz = 'bs',
                             adapt_delta = 0.999
), 
filename = 'f2.rds')

# ---- same long submodel as in paper ----


f3 <- with_filecache(stan_jm(formulaLong = sqrt_cd4 ~ obstime + drug + obstime:drug + gender + prevOI + AZT + (1 + obstime | patient), 
                             dataLong = aids2,
                             time_var = "obstime",
                             formulaEvent = Surv(Time, death) ~ gender + drug + prevOI + AZT, 
                             dataEvent = aids.id,
                             chains = 4,
                             assoc = c("etavalue", "etaslope"),
                             basehaz = 'bs',
                             adapt_delta = 0.999
), 
filename = 'f3.rds')


f4 <- with_filecache(stan_jm(formulaLong = sqrt_cd4 ~ obstime + drug + obstime:drug + gender + prevOI + AZT + (1 + obstime | patient), 
                             dataLong = aids2,
                             time_var = "obstime",
                             formulaEvent = Surv(Time, death) ~ gender + drug + prevOI + strata(AZT), 
                             dataEvent = aids.id,
                             chains = 4,
                             assoc = c("etavalue", "etaslope"),
                             basehaz = 'bs',
                             adapt_delta = 0.999
), 
filename = 'f4.rds')

# .. skipped models f5 & f6

f7 <- with_filecache(stan_jm(formulaLong = sqrt_cd4 ~ obstime + drug + obstime:drug + (1 + obstime | patient),
                             dataLong = aids2,
                             time_var = 'obstime',
                             formulaEvent = Surv(Time, death) ~ drug + prevOI + drug:prevOI + gender + AZT,
                             dataEvent = aids.id,
                             chains = 4,
                             assoc = c("etavalue", "etaslope"),
                             basehaz = 'bs',
                             adapt_delta = 0.999
), filename = 'f7.rds')

f8 <- with_filecache(stan_jm(formulaLong = sqrt_cd4 ~ obstime + drug + obstime:drug + (1 + obstime | patient),
                             dataLong = aids2,
                             time_var = 'obstime',
                             formulaEvent = Surv(Time, death) ~ drug + prevOI + drug:prevOI + gender + AZT + CD4,
                             dataEvent = aids.id,
                             chains = 4,
                             assoc = c("etavalue", "etaslope"),
                             basehaz = 'bs',
                             adapt_delta = 0.999
), filename = 'f8.rds')

f0.loo <- with_filecache(loo(f0),
                         filename = 'f0.loo.rds')
f1.loo <- with_filecache(loo(f1),
                         filename = 'f1.loo.rds')
f2.loo <- with_filecache(loo(f2),
                         filename = 'f2.loo.rds')
f3.loo <- with_filecache(loo(f3),
                         filename = 'f3.loo.rds')
f4.loo <- with_filecache(loo(f4),
                         filename = 'f4.loo.rds')
f7.loo <- with_filecache(loo(f7),
                         filename = 'f7.loo.rds')
f8.loo <- with_filecache(loo(f8),
                         filename = 'f8.loo.rds')

loo_comp <- loo::compare(f0.loo, f1.loo, f2.loo, f3.loo, f4.loo, f7.loo, f8.loo)
