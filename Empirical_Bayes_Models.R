#Conjugate Beta- Binomial Function
#Load libraries
library(dplyr)
library(ggplot2)
library(stats4)
library(tidyr) 

update_beta <- function(prior_alpha,prior_beta,sucesses,trials){
  posterior_alpha <- prior_alpha + sucesses
  posterior_beta <- prior_beta + (trials-sucesses)
  return(list(alpha=posterior_alpha,beta=posterior_beta))
}

#Test with flat priors
prior_alpha_flat <- 1
prior_beta_flat <- 1

#observed data
successes <- 6
trials <- 10

#Update the posterior
posterior_flat <- update_beta(prior_alpha_flat,prior_beta_flat,successes,trials)
#Print the updated parameters
cat("Flat Prior:  Alpha=", posterior_flat$alpha,
    "Beta=",posterior_flat$beta,"\n")

#Run Example with Australian Open Data
#Read data
atp_2025 <- read.csv("C:/Applied_Stats_Project/Data/2025.csv")

#Prepare the data
winners_df <-atp_2025 %>%
  dplyr::select(surface,tourney_date,tourney_name, starts_with("w")) %>%
  dplyr::rename_with(~ sub("^[^_]*_", "", .x), starts_with("w"))

#Create win column
winners_df$win_game <- "Yes"

losers_df <- atp_2025 %>%
  dplyr::select(surface,tourney_date,tourney_name, starts_with("l")) %>%
  dplyr::rename_with(~ sub("^[^_]*_", "", .x), starts_with("l"))

losers_df$win_game <- "No"


#Quality checks before stacking both datasets
all(dim(winners_df) ==dim(losers_df))
all(names(winners_df)==names(losers_df))

match_stats <- data.frame(rbind(losers_df,winners_df))
head(match_stats)
str(match_stats)
match_stats$win_game  <- as.factor(match_stats$win_game)

#Extract info for Novak Djokovic in the Australian Open
novak_trials <- sum(match_stats[match_stats$tourney_name=="Australian Open" & 
  match_stats$name=="Novak Djokovic","X1stIn"])
novak_sucesses <- sum(match_stats[match_stats$tourney_name=="Australian Open" & 
                                match_stats$name=="Novak Djokovic","X1stWon"])

novak_posterior <- update_beta(prior_alpha_flat,prior_beta_flat,
                               novak_sucesses,novak_trials)

cat("Flat Prior:  Alpha=", novak_posterior$alpha,
    "Beta=",novak_posterior$beta,"\n")

#Empirical Bayes Approach for Australian Open
aus_open_top8 <- match_stats %>% 
  dplyr::filter(tourney_name=="Australian Open" & rank <=8) %>%
  select(name,X1stIn,X1stWon) %>%
  dplyr::group_by(name)%>%
  dplyr::summarise(First_Serve=sum(X1stIn),First_Won=sum(X1stWon))%>%
  mutate(First_Serve_Won_Avg=First_Won/First_Serve)

# log-likelihood function
ll <- function(alpha, beta) {
  x <- aus_open_top8$First_Won
  total <- aus_open_top8$First_Serve
  -sum(VGAM::dbetabinom.ab(x, total, alpha, beta, log = TRUE))
}

# maximum likelihood estimation
m <- mle(ll, start = list(alpha = 1, beta = 1), method = "L-BFGS-B",
         lower = c(0.0001, .1))
ab <- coef(m)
alpha0 <- ab[1]
beta0 <- ab[2]

#Update Empirical Bayes
aus_open_top8 <- aus_open_top8 %>%
  mutate(eb_estimate = (First_Won + alpha0) / (First_Serve + alpha0 + beta0),
    alpha1 = alpha0 + First_Won,
         beta1 = beta0 + First_Serve- First_Won)
head(aus_open_top8)

#Plotting Credible Intervals
aus_beta <- aus_open_top8 %>%
  crossing(x = seq(.6, .9, .001)) %>%
  ungroup() %>%
  mutate(density = dbeta(x, alpha1, beta1))

ggplot(aus_beta, aes(x, density, color = name)) +
  geom_line() +
  stat_function(fun = function(x) dbeta(x, alpha0, beta0),
                lty = 2, color = "black") +
  labs(x = "First Serve Win Percentage",
       color = "Player") 

#Apply to French Open to evaluate second serve
french_open_top8 <- match_stats %>% 
  filter(tourney_name=="Roland Garros" & rank <=8) %>%
  na.omit() %>%
  dplyr::select(name,X1stIn,svpt,df,X2ndWon) %>%
  group_by(name)%>%
  summarise(Second_Serve=sum(svpt - X1stIn -df),Second_Won=sum(X2ndWon))%>%
  mutate(Second_Serve_Won_Avg=Second_Won/Second_Serve)

# log-likelihood function
ll <- function(alpha, beta) {
  x <- french_open_top8$Second_Won
  total <- french_open_top8$Second_Serve
  -sum(VGAM::dbetabinom.ab(x, total, alpha, beta, log = TRUE))
}

# maximum likelihood estimation
m <- mle(ll, start = list(alpha = 1, beta = 1), method = "L-BFGS-B",
         lower = c(0.0001, .1))
ab <- coef(m)
alpha0 <- ab[1]
beta0 <- ab[2]  

#Update Empirical Bayes
french_open_top8 <- french_open_top8 %>%
  mutate(eb_estimate = (Second_Won + alpha0) / (Second_Serve + alpha0 + beta0),
         alpha1 = alpha0 + Second_Won,
         beta1 = beta0 + Second_Serve - Second_Won)
head(french_open_top8)

french_open_beta <- french_open_top8 %>%
  crossing(x = seq(.5, .7, .001)) %>%
  ungroup() %>%
  mutate(density = dbeta(x, alpha1, beta1))

ggplot(french_open_beta, aes(x, density, color = name)) +
  geom_line() +
  stat_function(fun = function(x) dbeta(x, alpha0, beta0),
                lty = 2, color = "black") +
  labs(x = "Second Serve Win Percentage",
       color = "Player") 

#US Open it requires further feature engineering to derive
#first return winning percentage

#Add return statistics and enforce data integrity constraints
atp_2025_cleaned <- atp_2025%>%
  filter((l_1stIn>=l_1stWon) & (w_svpt - w_1stIn -w_df >=0) & (l_svpt - l_1stIn -l_df >0)
         & (w_bpFaced>=w_bpSaved) & (l_bpFaced>=l_bpSaved)
         & (w_svpt - w_1stIn -w_df >= w_2ndWon) & (l_svpt - l_1stIn -l_df >l_2ndWon) ) %>% 
  mutate(w_1stRetIn=l_1stIn,
         w_1stRetWon=l_1stIn-l_1stWon,
         w_2ndRetIn=l_svpt - l_1stIn -l_df,
         w_2ndRetWon=w_2ndRetIn-l_2ndWon,
         w_bpIn =l_bpFaced,
         w_bpConverted=l_bpFaced-l_bpSaved,
         l_1stRetIn=w_1stIn,
         l_1stRetWon=w_1stIn-w_1stWon,
         l_2ndRetIn=w_svpt - w_1stIn -w_df,
         l_2ndRetWon=l_2ndRetIn-w_2ndWon,
         l_bpIn =w_bpFaced,
         l_bpConverted=w_bpFaced-w_bpSaved)
summary(atp_2025_cleaned)

#Prepare the data
winners_df <-atp_2025_cleaned %>%
  dplyr::select(surface,tourney_date,tourney_name, starts_with("w")) %>%
  dplyr::rename_with(~ sub("^[^_]*_", "", .x), starts_with("w"))

#Create win column
winners_df$win_game <- "Yes"

losers_df <- atp_2025_cleaned %>%
  dplyr::select(surface,tourney_date,tourney_name, starts_with("l")) %>%
  dplyr::rename_with(~ sub("^[^_]*_", "", .x), starts_with("l"))

losers_df$win_game <- "No"  

#Quality checks before stacking both datasets
all(dim(winners_df) ==dim(losers_df))
all(names(winners_df)==names(losers_df))

match_stats <- data.frame(rbind(losers_df,winners_df))
head(match_stats)
match_stats$win_game <- as.factor(match_stats$win_game)
colnames(match_stats)

#Explore distribution
summary(match_stats$X1stRetWon/match_stats$X1stRetIn)

#Empirical Bayes Approach for US Open
us_open_top8 <- match_stats %>% 
  filter(tourney_name=="US Open" & rank <=8) %>%
  select(name,X1stRetIn,X1stRetWon) %>%
  group_by(name)%>%
  dplyr::summarise(First_Ret=sum(X1stRetIn),First_Ret_Won=sum(X1stRetWon))%>%
  mutate(First_Ret_Won_Avg=First_Ret_Won/First_Ret)

# log-likelihood function
ll <- function(alpha, beta) {
  x <- us_open_top8$First_Ret_Won
  total <- us_open_top8$First_Ret
  -sum(VGAM::dbetabinom.ab(x, total, alpha, beta, log = TRUE))
} 

# maximum likelihood estimation
#Tweak optimisation parameters to avoid excessive shrinkage where all
#players had the same average
m <- mle(ll, start = list(alpha = 1, beta = 1), method = "BFGS")
ab <- coef(m)
alpha0 <- ab[1]
beta0 <- ab[2]  

#Update Empirical Bayes
us_open_top8 <- us_open_top8 %>%
  mutate(eb_estimate = (First_Ret_Won + alpha0) / (First_Ret + alpha0 + beta0),
         alpha1 = alpha0 + First_Ret_Won,
         beta1 = beta0 + First_Ret- First_Ret_Won)
head(us_open_top8)

#Plotting Credible Intervals
us_open_beta <- us_open_top8 %>%
  crossing(x = seq(.2, .4, .001)) %>%
  ungroup() %>%
  mutate(density = dbeta(x, alpha1, beta1))

ggplot(us_open_beta, aes(x, density, color = name)) +
  geom_line() +
  stat_function(fun = function(x) dbeta(x, alpha0, beta0),
                lty = 2, color = "black") +
  labs(x = "First Serve Return Win Percentage",
       color = "Player")



 
