library(tidyverse)
library (gridExtra)
#library(patchwork)

setwd("/Users/sblumberg/Google Drive/Research/COVID-19/Upper CI/")
PRIM_ARR <- 5:30
SEC_ARR <- 1:50
K_DEFAULT <- .2
K_ARR <- c(0.2, 0.3, 0.5, 1)
R_ARR <- seq(0.5,1,0.1)
Np_LIMIT <- 10

INTRO_FOR_TRIALS <- 30
NUM_TRIALS <- 1000
R_TRIAL <- c(0.5,.75)
K_TRIAL <- c(0.2,1)
POBS_TRIAL <- tribble(
  ~p_passive, ~p_active, ~leg_text,
  1,1, 'Perfect\nobservation\n\n',
  0.5,1, 'Perfect\ncontact\ntracing\n',
  0.5,0, 'Passive\nobservation\nonly\n',
  0.5,0.5, 'Partial\ncontact\ntracing\n'
)
# POBS_TRIAL <- tribble(
#   ~p_passive, ~p_active, ~leg_text,
#   1,1, '1.0 : 1.0',
#   0.5,1, '0.5 : 1.0',
#   0.5,0, '0.5 : 0.0',
#   0.5,0.5, '0.5 : 0.5'
# )

############################################################

# log_prob that i cases cause j cases in a single generation
log_g_ij <- function(i,j,rval,kval) {
  lgamma(j +kval*i) - lgamma(j+1) - lgamma(kval*i) + 
    kval * i * log(kval/(rval+kval)) +
    j * log(rval/(rval+kval))
}

# log_prob that m primaries causes n cases in totality
log_r_mn <- function (m,n,rval,kval) {
  log(m/n) + log_g_ij(i=n, j = n-m, rval = rval, kval = kval)
}

R_inference <- function(num_primary, num_transmissions, kval = K_DEFAULT) {
  opt_r <- num_transmissions/(num_primary+num_transmissions)
  opt_val <- log_r_mn(m = num_primary, n = num_primary+num_transmissions, rval = opt_r, kval = kval)
  zero_par <- uniroot(function (x) opt_val -1.92 - log_r_mn(m = num_primary, n = num_primary+num_transmissions, rval = x, kval = kval),
                      interval = c(opt_r,20))
  tibble(opt_r = opt_r,r_upper = zero_par[[1]])
}

sim_observation <- function (num_intro, R, k, p_passive, p_active) {
  
  obs_list <- tibble(intro = 1:num_intro) %>% group_by(intro) %>% do({
    chain_seen <- 0
    while(chain_seen == 0) {
      
      # True size
      total_cases <- 1
      cur_cases <- 1
      while (cur_cases > 0) {
        cur_cases <- sum(rnbinom(n = cur_cases, size = k, mu = R))
        total_cases <- total_cases + cur_cases
      }
      
      # Is it noticed?
      num_passive_obs <- rbinom(n = 1,prob = p_passive, size = total_cases)
      if (num_passive_obs > 0) {
        chain_seen <- 1
        # How many are seen?
        num_active_obs <- rbinom(n = 1, prob = p_active, size = total_cases - num_passive_obs)
        cases_obs <- num_passive_obs + num_active_obs
      }
    }
    tibble(cases_obs = cases_obs)
  })
  sum(obs_list$cases_obs)
}
