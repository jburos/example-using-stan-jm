
"usage: analyze_simulated_irAE_data.args.R [options]
options:
 --n=<int>               sample size [default: 200]
 --seed=<int>            data-sim seed [default: 12345]
 --stan_seed=<int>       stan_seed [default: 1234]
 --iter=<int>            number of iterations [default: 5000]
 --init=<chr>            init type (random or model_based) [default: model_based]
 --max_treedepth=<int>   max_treedepth [default: 15]
 --adapt_delta=<float>   adapt_delta parameter [default: 0.9999]
 --basehaz=<chr>         basehaz parameter (weibull, bs, or piecewise) [default: weibull]
 --beta_binary=<float>   parameter value for Z1 assoc with irAE [default: -0.6]
 --beta_assoc=<float>    parameter value for irAE assoc with hazard [default: 0.05]" -> doc

# load analysis function
source('analyze_simulated_irAE_data.function.R', chdir = T)

# load the docopt library
library(docopt)
# retrieve the command-line arguments
opts <- docopt(doc)

# save parameters for simulation & model fit
max_treedepth <- as.integer(opts['max_treedepth'])
n <- as.integer(opts['n'])
seed <- as.integer(opts['seed'])
stan_seed <- as.integer(opts['stan_seed'])
iter <- as.integer(opts['iter'])
init <- as.character(opts['init'])
adapt_delta <- as.numeric(opts['adapt_delta'])
basehaz <- as.character(opts['basehaz'])
betaLong_binary <- as.numeric(opts['beta_binary'])
betaEvent_assoc <- as.numeric(opts['beta_assoc'])

params <- rstanarm:::nlist(max_treedepth, n, init, seed, iter, adapt_delta, stan_seed, basehaz, betaLong_binary, betaEvent_assoc)

res <- analyze_simulated_irae_data_given_params(params)
summary(res$stan_jm_fit)
