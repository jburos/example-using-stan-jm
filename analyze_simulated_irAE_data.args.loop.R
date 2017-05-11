
"usage: analyze_simulated_irAE_data.args.R [options]
options:
 --n=<int>               sample size [default: sequence]
 --seed=<int>            data-sim seed [default: random]
 --runs=<int>            how many runs to execute [default: 20]
 --stan_seed=<int>       stan_seed [default: 1234]
 --iter=<int>            number of iterations [default: 8000]
 --init=<chr>            init type (random or model_based) [default: random]
 --max_treedepth=<int>   max_treedepth [default: 15]
 --adapt_delta=<float>   adapt_delta parameter [default: 0.9999]
 --basehaz=<chr>         basehaz parameter (weibull, bs, or piecewise) [default: weibull]
 --beta_binary=<float>   parameter value for Z1 assoc with irAE [default: -0.6]
 --beta_assoc=<float>    parameter value for irAE assoc with hazard [default: 0.05]" -> doc

# load analysis function
source('analyze_simulated_irAE_data.function.R', chdir = T)
set.seed(12345)

# load the docopt library
library(docopt)
# retrieve the command-line arguments
opts <- docopt(doc)

# save parameters for simulation & model fit
runs <- as.integer(opts['runs'])
max_treedepth <- as.integer(opts['max_treedepth'])
stan_seed <- as.integer(opts['stan_seed'])
iter <- as.integer(opts['iter'])
init <- as.character(opts['init'])
adapt_delta <- as.numeric(opts['adapt_delta'])
basehaz <- as.character(opts['basehaz'])
betaLong_binary <- as.numeric(opts['beta_binary'])
betaEvent_assoc <- as.numeric(opts['beta_assoc'])
if (opts['seed'] == 'random') {
  seedlist <- as.integer(runif(n = runs, min = 0, max = 1)*100000)
} else {
  seedlist <- as.integer(opts['seed'])
}
if (opts['n'] == 'sequence') {
  nlist <- c(200, 500)
} else {
  nlist <- as.integer(opts['n'])
}


for (seed in seedlist) {
  for (n in nlist) {
    params <- rstanarm:::nlist(max_treedepth, adapt_delta, 
                               n, init, iter, seed,
                               stan_seed, basehaz,
                               betaLong_binary, betaEvent_assoc)
    res <- analyze_simulated_irae_data_given_params(params)
    summary(res$stan_jm_fit)
  }
}
