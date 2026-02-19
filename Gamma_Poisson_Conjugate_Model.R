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
str(wimbledon_top8_aces_df)
wimbledon_top8_aces_df$alpha <- as.numeric(wimbledon_top8_aces_df$alpha)
wimbledon_top8_aces_df$beta <- as.numeric(wimbledon_top8_aces_df$beta)

wimbledon_top8_aces_gamma <- wimbledon_top8_aces_df %>%
  crossing(x = seq(1,20, 1)) %>%
  ungroup() %>%
  mutate(density = dgamma(x, shape=alpha, rate=beta))

ggplot(wimbledon_top8_aces_gamma , aes(x, density, color = player_name)) +
  geom_line() +
  
  stat_function(fun = function(x) dbeta(x, alpha, beta),
                lty = 2, color = "black") +
  xlim(c(0,20)) +
  labs(x = "Aces Per Game at Wimbledon",
       color = "Player") +
      ggtitle("Aces Per Game at Wimbledon- Uniformed Prior") +
  

#Retrieve US Open Data
us_open_top8 <- match_stats %>% 
  filter(tourney_name=="US Open" & rank <=8) %>%
  na.omit() %>%
  dplyr::select(name,df) 

#Build the function into a loop
head(us_open_top8)
dim(us_open_top8)
player_ids <- unique(us_open_top8$name)
total_df <- vector(mode="numeric")
total_games <- vector(mode="numeric")
alpha <- vector(mode="numeric")
beta <- vector(mode="numeric")

for (i in seq_along(player_ids)){
  total_df[i] <- sum(us_open_top8[us_open_top8$name==player_ids[i],"df"])
  total_games[i] <- length(us_open_top8[us_open_top8$name==player_ids[i],"df"])
  alpha[i]  <- update_gamma(prior_alpha_flat,prior_beta_flat,total_df[i],total_games[i])$alpha
  beta[i]  <- update_gamma(prior_alpha_flat,prior_beta_flat,total_df[i],total_games[i])$beta
}


#Combine into dataframe  
us_open_top8_df <- data.frame(cbind(player_name=player_ids,total_df=total_df,
                                           total_games=total_games,alpha=alpha,beta=beta))
str(us_open_top8_df)
us_open_top8_df$alpha <- as.numeric(us_open_top8_df$alpha)
us_open_top8_df$beta <- as.numeric(us_open_top8_df$beta)

us_open_top8_df_gamma <- us_open_top8_df %>%
  crossing(x = seq(1,20, 1)) %>%
  ungroup() %>%
  mutate(density = dgamma(x, shape=alpha, rate=beta))

head(us_open_top8_df_gamma)

ggplot(us_open_top8_df_gamma , aes(x, density, color = player_name)) +
  geom_line() +
  stat_function(fun = function(x) dbeta(x, alpha, beta),
                lty = 2, color = "black") +
  xlim(c(0,10)) +
  labs(x = "Double Faults Per Game at US Open",
       color = "Player") +
  ggtitle("Double Faults Per Game at US Open- Uniformed Prior")
