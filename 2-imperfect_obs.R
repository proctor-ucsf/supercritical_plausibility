rm(list = ls())

setwd("/Users/sblumberg/Google Drive/Research/COVID-19/Upper CI/")
source('0-library.R')

# Generate trial of xxx introductions, for multiple scenarios

trial_res <-  expand_grid(POBS_TRIAL,R = R_TRIAL, k = K_TRIAL, trial = 1:NUM_TRIALS) %>% group_by(trial,R,k,leg_text) %>% do({
  total_obs <- sim_observation(num_intro = INTRO_FOR_TRIALS,R = .$R, k = .$k,p_passive = .$p_passive, p_active = .$p_active)
  tibble(p_passive = .$p_passive, p_active = .$p_active, total_obs = total_obs)
})


inference_res <- tibble(total_obs = unique(trial_res$total_obs)) %>% group_by(total_obs) %>% do({
  R_inference(num_primary = INTRO_FOR_TRIALS, num_transmissions = max(0.5,.$total_obs - INTRO_FOR_TRIALS))
})
trial_res <- right_join(trial_res,inference_res,by = "total_obs")
write_csv(trial_res,'Results/trial_res_103121')
trial_res <- read_csv('Results/trial_res_103121')
#trial_res <- read_csv('Results/trial_res_070821')

# vert <- expand_grid(R = R_TRIAL,k = K_TRIAL, y = c(0,1))
# g_imperfect <- ggplot(trial_res) +
#   stat_ecdf(aes(x=r_upper, col = leg_text),geom = "step",linetype = 'dashed') +
#   stat_ecdf(aes(x=opt_r, col = leg_text),geom = "step") +
#   geom_line(data = vert, aes(x=R, y = y), linetype = 'dotted') +
#   geom_line(data = vert, aes(x=1, y = y)) +
#   labs(x = 'Reproduction number',y = 'CDF of inferred reproduction number',col = 'Observation\nscenario') +
#   #  labs(x = 'Reproduction number',y = 'CDF of inferred reproduction number',col = 'Passive : active\nobservation\nprobabilities') +
#   facet_grid(vars(paste('k = ',k)),vars(paste('R = ',R)))
# (g_imperfect)
# 
# ggsave(filename = 'Figs/Imperfect_obs.jpg',plot = g_imperfect)

subset_res <- trial_res %>% filter (R == 0.5, k == 0.2) %>%
  mutate(num_trans = total_obs - INTRO_FOR_TRIALS) %>%
  select(-total_obs) %>%
  pivot_longer(cols = c('num_trans','opt_r','r_upper'))

facet.labs <- c('Maximum likelihood value for R', 'Upper confidence limit for R','Number of observed transmissions')
names(facet.labs) <- c('opt_r', 'r_upper','num_trans')

subset_res$leg_text <- factor(subset_res$leg_text, levels = c('Passive\nobservation\nonly\n','Partial\ncontact\ntracing\n','Perfect\ncontact\ntracing\n','Perfect\nobservation\n\n'))

g_imperfect_demo <- ggplot(subset_res) +
#  stat_ecdf(aes(x=value, col = leg_text)) +
  geom_density(aes(x=value, fill = leg_text), alpha = 0.5) +
  facet_wrap(~name,scales = 'free', nrow = 3, labeller = labeller(name = facet.labs)) +
  labs(x = '',y = 'Probaility distribution',fill = 'Observation\nscenario') +
  theme(text = element_text(size=24))
(g_imperfect_demo)

ggsave(filename = 'Figs/Imperfect_obs_demo.jpg',plot = g_imperfect_demo)

subset_res %>% group_by(leg_text, name) %>%
  summarize(mean = mean(value), median = median(value), lower_ci = quantile(value,probs = 0.025), upper_ci = quantile(value,probs = 0.975))

subset_res %>% filter(name == 'r_upper') %>% group_by(leg_text) %>% summarize(fraction = sum(value > 1)/n())