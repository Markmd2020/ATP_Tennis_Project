#Calculate shape and rate parameters for Wimbledon 2024 top 8 players
#Read data
atp_2024 <- read.csv("C:/Applied_Stats_Project/Data/2024.csv")

#Prepare the data
winners_df <-atp_2024 %>%
  dplyr::select(surface,tourney_date,tourney_name, starts_with("w")) %>%
  dplyr::rename_with(~ sub("^[^_]*_", "", .x), starts_with("w"))

#Create win column
winners_df$win_game <- "Yes"

losers_df <- atp_2024 %>%
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

#Retrieve Wimbledon Data for 2024
wimbledon24_top8 <- match_stats %>% 
  filter(tourney_name=="Wimbledon" & rank <=8) %>%
  dplyr::select(name,ace)

mean_aces <- mean(wimbledon24_top8$ace,na.rm=TRUE)
var_aces <- var(wimbledon24_top8$ace,na.rm=TRUE)

#Function to extract shape and rate parameters from mean and st dev
parms_from_mean_var <- function(mean, variance) {
  shape <- mean^2 / variance
  rate  <- mean / variance
  list(shape = shape, rate = rate)
}

gamma_prior <- parms_from_mean_var(mean=mean_aces,variance=var_aces)

#Sanity check to see that it worked
mean(rgamma(1000,shape=gamma_prior$shape,rate=gamma_prior$rate))
sd(rgamma(1000,shape=gamma_prior$shape,rate=gamma_prior$rate))
