######Feature Engineering####

#Load libraries
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(reshape2)
library(lubridate)
library(GGally)
library(scales)
library(naniar)
library(MASS)
library(leaps)

#Set seed for reproducibility
set.seed(135)

#Read data
atp_2025 <- read.csv("C:/Applied_Stats_Project/Data/2025.csv")

#Initial Summary
str(atp_2025)

#Descriptive statistics
summary(atp_2025)
head(atp_2025)

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
atp_2025_cleaned%>%filter(w_2ndRetWon<0)
atp_2025%>%filter(w_1stRetWon==-3)
atp_2025%>% filter(l_1stIn<l_1stWon) %>% tally()#Constraint 1
atp_2025%>% filter(l_2ndRetIn <0) %>% tally()
atp_2025%>% filter(w_svpt - w_1stIn -w_df <0) %>% tally()#Constraint 2
atp_2025%>% filter(l_svpt - l_1stIn -l_df <0) %>% tally()#Constraint 3
atp_2025%>% filter(w_bpFaced<w_bpSaved) %>% tally()#Constraint 4
atp_2025%>% filter(l_bpFaced<l_bpSaved) %>% tally()#Constraint 5
atp_2025%>% filter(w_svpt - w_1stIn -w_df < w_2ndWon) %>% tally()#Constraint 6
atp_2025%>% filter(l_svpt - l_1stIn -l_df < l_2ndWon) %>% tally()#Constraint 7


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

match_stats1 <-match_stats %>%
  mutate(first_point_serve_win_pct=X1stWon/X1stIn,
         second_point_serve_win_pct=X2ndWon/(svpt - X1stIn -df),
         first_ret_win_pct = X1stRetWon/X1stRetIn,
         second_ret_win_pct = X2ndRetWon/X2ndRetIn,
         bp_conversion_pct=bpConverted/bpIn,
         aces_df_ratio=ace/pmax(df,1),
         bp_saved_pct=bpSaved/bpFaced)
head(match_stats1)
summary(match_stats1)

match_stats1 %>%
  filter(X2ndRetIn<0)

match_stats1%>% filter(X1stRetWon==-3)

fit <- glm(win_game~second_point_serve_win_pct,family="binomial",data=match_stats1)
summary(fit)
w_2ndWon/(w_svpt-w_1stIn)
