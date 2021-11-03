rm(list = ls())

setwd("/Users/sblumberg/Google Drive/Research/COVID-19/Upper CI/")
source('0-library.R')
init_frame <- expand_grid(num_prim = PRIM_ARR, num_sec = SEC_ARR)
res_inference <- init_frame %>% group_by(num_prim, num_sec) %>% do({
  R_inference(num_primary = .$num_prim, num_transmissions = .$num_sec)
})


res_R <- expand_grid(r_thresh = R_ARR,num_prim = PRIM_ARR) %>% group_by(r_thresh,num_prim) %>% do({
  upper_ci <- 0
  thresh <- 0
  while (upper_ci < .$r_thresh){
    thresh <- thresh + 1
    temp_inf_res <- R_inference(num_primary = .$num_prim, num_transmissions = thresh)
    upper_ci <- temp_inf_res$r_upper
  }
  tibble(thresh = thresh)
})

res_k <- expand_grid(kval = K_ARR,num_prim = PRIM_ARR) %>% group_by(kval,num_prim) %>% do({
  upper_ci <- 0
  thresh <- 0
  while (upper_ci < 1){
    thresh <- thresh + 1
    temp_inf_res <- R_inference(num_primary = .$num_prim, num_transmissions = thresh, k = .$kval)
    upper_ci <- temp_inf_res$r_upper
  }
  tibble(thresh = thresh)
})

# res_k_old <- expand_grid(kval = K_ARR,num_prim = PRIM_ARR) %>% group_by(kval,num_prim) %>% do({
#   upper_ci <- 0
#   thresh <- 0
#   while (upper_ci < 1){
#     thresh <- thresh + 1
#     opt_r <- thresh/(.$num_prim+thresh)
#     opt_val <- log_r_mn(m = .$num_prim, n = .$num_prim + thresh, rval = opt_r, kval = .$kval)
#     zero_par <- uniroot(function (x) opt_val -1.92 - log_r_mn(m = .$num_prim, n = .$num_prim + thresh, rval = x, kval = .$kval),
#                         interval = c(opt_r,20))
#     upper_ci <- zero_par[[1]]
#   }
#   tibble(thresh = thresh)
# })

res_inference_sh <- res_inference %>% filter(num_sec <= Np_LIMIT)

g_mle <- ggplot(data = res_inference_sh, aes(x=num_prim, y = opt_r, col = as.factor(num_sec))) + geom_line() + geom_hline(yintercept = 1) +
  labs(tag = "A)", x ='Number of introductions',y = expression(paste("Maximum likelihood estimate of ", italic("R"))), col = 'Number of\ntransmissions') +
  theme(text = element_text(size=18)) + scale_x_continuous(breaks=seq(5,30,5)) + coord_cartesian(ylim = c(0,1.5)) +
  theme(legend.position=c(0.5,0.85),legend.direction = 'horizontal',legend.title = element_text(size = 12), legend.text=element_text(size=9))
(g_mle)

g_upper<-ggplot(data = res_inference_sh, aes(x=num_prim, y = r_upper, col = as.factor(num_sec))) + geom_line() + geom_hline(yintercept = 1) +
  labs(tag = "B)", x = 'Number of introductions', y = expression(paste("Upper limit of ", italic("R"), ' estimate')), col = 'Number of\ntransmissions') +
  theme(text = element_text(size=18)) + scale_x_continuous(breaks=seq(5,30,5)) + coord_cartesian(ylim = c(0,1.5)) +
  theme(legend.position = 'none')
#  theme(legend.position=c(0.6,0.85),legend.direction = 'horizontal',legend.title = element_text(size = 14), legend.text=element_text(size=10))
(g_upper)

g_thresh_R <- ggplot(data = res_R)+
  geom_point(aes(x=num_prim, y = thresh, pch = as.factor(r_thresh))) + 
  labs(tag = "C)",x = 'Number of introductions', y = 'Threshold transmissions', pch = expression(paste('Maximum\nallowable ', italic(R)))) +
  theme(text = element_text(size=18)) + scale_x_continuous(breaks=seq(5,30,5)) +
  theme(legend.position=c(0.2,0.675),legend.direction = 'vertical',legend.title = element_text(size = 12), legend.text=element_text(size=9))
(g_thresh_R)

g_thresh_k <- ggplot(data = res_k) +
  geom_line(aes(x=num_prim, y = thresh, linetype = as.factor(kval))) + 
  labs(tag = "D)",x = 'Number of introductions', y = 'Threshold transmissions', linetype = 'Dispersion\nparameter') +
  theme(text = element_text(size=18)) + scale_x_continuous(breaks=seq(5,30,5)) +
  theme(legend.position=c(0.2,0.7),legend.direction = 'vertical',legend.title = element_text(size = 12), legend.text=element_text(size=9))
(g_thresh_k)

(g_all<-grid.arrange(g_mle, g_upper, g_thresh_R, g_thresh_k,nrow =2))
ggsave("Figs/Res_perfect_obs.jpg",g_all)

#######
# Case example Hawaii

#March: R_upper = 0.46
R_inference(num_primary = 250,num_transmissions = 134)

#April: R_upper = 1.35
R_inference(num_primary = 15,num_transmissions = 145)

#May: R_upper = 1.76
R_inference(num_primary = 10,num_transmissions = 30)

#June: R_upper = 1.24
R_inference(num_primary = 24,num_transmissions = 226)
