#######Gamma-Poisson Update Function#####
update_gamma <- function(prior_alpha,prior_beta,observed_sum,n){
  posterior_alpha <- prior_alpha + observed_sum
  posterior_beta <- prior_beta  + n
  return(list(alpha=posterior_alpha, beta=posterior_beta))
}

#Flat prior parameters
prior_alpha_flat <- 1
prior_beta_flat <- 1

#Informed prior parameters
prior_alpha_inf <- 6
prior_beta_inf <- 2

#Retrieve Wimbledon Data
wimbledon_top8 <- match_stats %>% 
  filter(tourney_name=="Wimbledon" & rank <=8) %>%
  dplyr::select(name,ace)

head(wimbledon_top8)
wimbledon_top8[wimbledon_top8$name=="Taylor Fritz","ace"]
#Observed Data For Aces
observed_aces <- wimbledon_top8[wimbledon_top8$name=="Taylor Fritz","ace"]
sum_aces <- sum(observed_aces)
n <- length(observed_aces)

#Update Aces
posterior_flat <- update_gamma(prior_alpha_flat,prior_beta_flat,sum_aces,n)
posterior_inf <- update_gamma(prior_alpha_inf,prior_beta_inf,sum_aces,n)

cat("Flat Prior:  Alpha=", posterior_flat$alpha,
    "Beta=",posterior_flat$beta,"\n")

cat("Informed Prior:  Alpha=", posterior_inf$alpha,
    "Beta=",posterior_inf$beta,"\n") 

#Calculate credible intervals
n_sim <- 1000
shape <- posterior_flat$alpha
rate <-  posterior_flat$beta
lambda_sim <- rgamma(shape,rate)
credible_interval <- quantile(lambda_sim,c(0.025,0.975))

#Build the function into a loop
head(wimbledon_top8)
dim(wimbledon_top8)
player_ids <- unique(wimbledon_top8$name)
total_aces <- vector(mode="numeric")
total_games <- vector(mode="numeric")
alpha <- vector(mode="numeric")
beta <- vector(mode="numeric") 

for (i in seq_along(player_ids)){
   total_aces[i] <- sum(wimbledon_top8[wimbledon_top8$name==player_ids[i],"ace"])
   total_games[i] <- length(wimbledon_top8[wimbledon_top8$name==player_ids[i],"ace"])
   alpha[i]  <- update_gamma(prior_alpha_flat,prior_beta_flat,total_aces[i],total_games[i])$alpha
   beta[i]  <- update_gamma(prior_alpha_flat,prior_beta_flat,total_aces[i],total_games[i])$beta
}

#Combine into dataframe
wimbledon_top8_aces_df <- data.frame(cbind(player_name=player_ids,total_aces=total_aces,
                                           total_games=total_games,alpha=alpha,beta=beta))

