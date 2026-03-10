############Upsets Model Data Prep######

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

#Read data from 2022 till 2025
years <- 2022:2025
path <- "C:/Applied_Stats_Project/Data"
for (y in years) {
  assign(paste0("atp_", y),
         read.csv(file.path(path, paste0(y, ".csv"))))
}

#Check that the code worked and dimensions are consistent
head(atp_2022)
head(atp_2023)
head(atp_2024)
head(atp_2025)
dim(atp_2022)
dim(atp_2023)
dim(atp_2024)
dim(atp_2025)

#Combine all datasets together
atp_data <- data.frame(rbind(atp_2022,atp_2023,atp_2024,atp_2025))
head(atp_data)

atp_data_cleaned <- atp_data%>%
  filter((l_1stIn>=l_1stWon) & (w_1stIn>=w_1stWon) & (w_svpt - w_1stIn -w_df >=0) & (l_svpt - l_1stIn -l_df >0)
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
summary(atp_data_cleaned)

#Prepare the data
winners_df <-atp_data_cleaned %>%
  dplyr::select(surface,tourney_date,tourney_name, starts_with("w")) %>%
  dplyr::rename_with(~ sub("^[^_]*_", "", .x), starts_with("w"))

#Create win column
winners_df$win_game <- "Yes"

losers_df <- atp_data_cleaned %>%
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
  na.omit() %>%
  mutate(first_point_serve_win_pct=X1stWon/X1stIn,
         second_point_serve_win_pct=X2ndWon/(svpt - X1stIn -df),
         first_ret_win_pct = X1stRetWon/X1stRetIn,
         second_ret_win_pct = X2ndRetWon/X2ndRetIn,
         bp_conversion_pct=bpConverted/bpIn,
         aces_df_ratio=ace/pmax(df,1),
         bp_saved_pct=bpSaved/bpFaced)
head(match_stats1)
summary(match_stats1) 

#Setup tournament date
match_stats1$tourney_date <- ymd(match_stats1$tourney_date)

#Create dataset to compute rankings
years <- 2018:2025
path <- "C:/Applied_Stats_Project/Data"
for (y in years) {
  assign(paste0("atp_rank_", y),
         read.csv(file.path(path, paste0(y, ".csv"))))
}

atp_rank_data <- data.frame(rbind(atp_rank_2018,atp_rank_2019,atp_rank_2020,
                             atp_rank_2021,atp_rank_2022,atp_rank_2023,
                             atp_rank_2024,atp_rank_2025))

winners_rank_df <- atp_rank_data %>%
  dplyr::select(winner_id,tourney_date,winner_name,winner_rank) %>%
  dplyr::rename_with(~ sub("^[^_]*_", "", .x), starts_with("w"))

losers_rank_df <- atp_rank_data %>%
  dplyr::select(loser_id,tourney_date,loser_name,loser_rank) %>%
  dplyr::rename_with(~ sub("^[^_]*_", "", .x), starts_with("l"))

#Quality checks before stacking both datasets
all(dim(winners_rank_df) ==dim(losers_rank_df))
all(names(winners_rank_df)==names(losers_rank_df))

rank_stats <- data.frame(rbind(losers_rank_df,winners_rank_df))
rank_stats$tourney_date <- ymd(rank_stats$tourney_date)

##US 2025 Data Prep##

#Retrieve tourney start data
match_stats1 %>%
  filter(tourney_name=="US Open" & year(tourney_date)==2025)%>%
  summarise(min(tourney_date))

min_tourney_start_date <- as.Date("2025-08-24")

#Retrieve age and height for each player
us_2025_df <- match_stats1 %>%
  filter(tourney_name=="US Open" & year(tourney_date)==2025)%>%
  arrange(name,tourney_date)%>%
  group_by(name)%>%
  slice(1)%>%
  ungroup() %>%
  dplyr::select(name,id,age,ht,rank)

#Tag IQR statistical summaries for the last year
source("C:/Users/MarkM/OneDrive/Documents/ATP_Tennis_Project1/match_stats_iqr_func.R")

us_open_2025_iqr_df <- stats_iqr_func(data=match_stats1,ref_date = min_tourney_start_date,
                                      window_days = 365,bySurface = FALSE)

us_2025_df1 <- us_2025_df %>%
               left_join(us_open_2025_iqr_df,by="name",suffix = c("",""))
head(us_2025_df1)
us_2025_df1 <- data.frame(us_2025_df1)

#Tag  statistical summaries for warm up tournaments
#The way to do it is filter by surface and cover 3 months before tournament
source("C:/Users/MarkM/OneDrive/Documents/ATP_Tennis_Project1/match_stats_average_func.R")
us_open_2025_avg_df <- stats_avg_func(data=match_stats1,ref_date = min_tourney_start_date,
               window_days = 90,bySurface = TRUE)
head(us_open_2025_avg_df)

us_2025_df2 <- us_2025_df1 %>%
  left_join(us_open_2025_avg_df%>%filter(surface=="Hard"),by="name",suffix = c("",""))
us_2025_df2 <- data.frame(us_2025_df2)

head(us_2025_df2)
str(us_2025_df2)

#Compute target variable
#First step is to filter out
us_2025_df3 <-us_2025_df2 %>%
  mutate(rank_bin=case_when(rank<9 ~"ATP Rank 1-8",
                            rank<17 ~"ATP Rank 9-16",
                            rank<33 ~"ATP Rank 17-32",
                            rank<65 ~"ATP Rank 33-64",
                            rank<129 ~"ATP Rank 65-128",
                            TRUE~"129+"))%>%
  filter(rank_bin != "129+")

head(us_2025_df3)
atp_2025[atp_2025$tourney_name=="US Open","tourney_id"]

us2025_exp_wins <- tourney_df(atp_2025,"2025-560")
us2025_exp_wins <- data.frame(us2025_exp_wins)
dim(us2025_exp_wins)

us_2025_df4 <- us_2025_df3 %>%
  left_join(us2025_exp_wins%>% dplyr::select(id,bonus_round),
            by="id",suffix = c("",""))
us_2025_df4 <- data.frame(us_2025_df4)

#Add Historical Rankings
source("C:/Users/MarkM/OneDrive/Documents/ATP_Tennis_Project1/top_rank_func.R")
#Retrieve highest ranking from the last five years which 1825 days
max_rank_df <- top_rank_func(rank_stats,min_tourney_start_date,1825)

us_2025_df5 <- us_2025_df4 %>%
  left_join(max_rank_df,
            by="name",suffix = c("",""))
colnames(us_2025_df5)
dim(us_2025_df5)
head(us_2025_df5)
us_2025_df5$name

#Save dataset
saveRDS(us_2025_df5,
file="C:/Users/MarkM/OneDrive/Documents/ATP_Tennis_Project1/GrandSlams_Datasets/us_2025_df5.rds")

##Wimbledon 2025 Data Prep##

#Retrieve tourney start data
match_stats1 %>%
  filter(tourney_name=="Wimbledon" & year(tourney_date)==2025)%>%
  summarise(min(tourney_date))

min_tourney_start_date <- as.Date("2025-06-30")

#Retrieve age and height for each player
wimbledon_2025_df <- match_stats1 %>%
  filter(tourney_name=="Wimbledon" & year(tourney_date)==2025)%>%
  arrange(name,tourney_date)%>%
  group_by(name)%>%
  slice(1)%>%
  ungroup() %>%
  dplyr::select(name,id,age,ht,rank)

#Tag IQR statistical summaries for the last year
source("C:/Users/MarkM/OneDrive/Documents/ATP_Tennis_Project1/match_stats_iqr_func.R")

wimbledon_2025_iqr_df <- stats_iqr_func(data=match_stats1,ref_date = min_tourney_start_date,
                                      window_days = 365,bySurface = FALSE)

wimbledon_2025_df1 <- wimbledon_2025_df %>%
  left_join(wimbledon_2025_iqr_df,by="name",suffix = c("",""))
head(wimbledon_2025_df1)
wimbledon_2025_df1 <- data.frame(wimbledon_2025_df1)

#Tag  statistical summaries for warm up tournaments
#The way to do it is filter by surface and cover 3 months before tournament
source("C:/Users/MarkM/OneDrive/Documents/ATP_Tennis_Project1/match_stats_average_func.R")
wimbledon_2025_avg_df <- stats_avg_func(data=match_stats1,ref_date = min_tourney_start_date,
                                      window_days = 90,bySurface = TRUE)
head(wimbledon_2025_avg_df)

wimbledon_2025_df2 <- wimbledon_2025_df1 %>%
  left_join(wimbledon_2025_avg_df%>%filter(surface=="Grass"),by="name",suffix = c("",""))
wimbledon_2025_df2 <- data.frame(wimbledon_2025_df2)

head(wimbledon_2025_df2)
str(wimbledon_2025_df2)

#Compute target variable
#First step is to filter out
wimbledon_2025_df3 <- wimbledon_2025_df2 %>%
  mutate(rank_bin=case_when(rank<9 ~"ATP Rank 1-8",
                            rank<17 ~"ATP Rank 9-16",
                            rank<33 ~"ATP Rank 17-32",
                            rank<65 ~"ATP Rank 33-64",
                            rank<129 ~"ATP Rank 65-128",
                            TRUE~"129+"))%>%
  filter(rank_bin != "129+")

head(wimbledon_2025_df3)
atp_2025[atp_2025$tourney_name=="Wimbledon","tourney_id"]
source("C:/Users/MarkM/OneDrive/Documents/ATP_Tennis_Project1/grand_slam_expected_wins.R")

wimbledon_exp_wins <- tourney_df(atp_2025,"2025-540")
wimbledon_exp_wins <- data.frame(wimbledon_exp_wins)
dim(wimbledon_exp_wins)

wimbledon_2025_df4 <- wimbledon_2025_df3 %>%
  left_join(wimbledon_exp_wins%>% dplyr::select(id,bonus_round),
            by="id",suffix = c("",""))
wimbledon_2025_df4 <- data.frame(wimbledon_2025_df4)

#Add Historical Rankings
source("C:/Users/MarkM/OneDrive/Documents/ATP_Tennis_Project1/top_rank_func.R")
#Retrieve highest ranking from the last five years which 1825 days
max_rank_df <- top_rank_func(rank_stats,min_tourney_start_date,1825)

wimbledon_2025_df5 <- wimbledon_2025_df4 %>%
  left_join(max_rank_df,
            by="name",suffix = c("",""))
colnames(wimbledon_2025_df5)
dim(wimbledon_2025_df5)
head(wimbledon_2025_df5)
table(wimbledon_2025_df5$bonus_round)

#Save dataset 
saveRDS(wimbledon_2025_df5,
        file="C:/Users/MarkM/OneDrive/Documents/ATP_Tennis_Project1/GrandSlams_Datasets/wimbledon_2025_df5.rds")

##French Open 2025 Data Prep##

#Retrieve tourney start data
match_stats1 %>%
  filter(tourney_name=="Roland Garros" & year(tourney_date)==2025)%>%
  summarise(min(tourney_date))

min_tourney_start_date <- as.Date("2025-05-26")

#Retrieve age and height for each player
french_open_2025_df <- match_stats1 %>%
  filter(tourney_name=="Roland Garros" & year(tourney_date)==2025)%>%
  arrange(name,tourney_date)%>%
  group_by(name)%>%
  slice(1)%>%
  ungroup() %>%
  dplyr::select(name,id,age,ht,rank)

#Tag IQR statistical summaries for the last year
source("C:/Users/MarkM/OneDrive/Documents/ATP_Tennis_Project1/match_stats_iqr_func.R")

french_open_2025_iqr_df <- stats_iqr_func(data=match_stats1,ref_date = min_tourney_start_date,
                                        window_days = 365,bySurface = FALSE)

french_open_2025_df1 <- french_open_2025_df %>%
  left_join(french_open_2025_iqr_df,by="name",suffix = c("",""))
head(french_open_2025_df1)
french_open_2025_df1 <- data.frame(french_open_2025_df1)

#Tag  statistical summaries for warm up tournaments
#The way to do it is filter by surface and cover 3 months before tournament
source("C:/Users/MarkM/OneDrive/Documents/ATP_Tennis_Project1/match_stats_average_func.R")
french_open_2025_avg_df <- stats_avg_func(data=match_stats1,ref_date = min_tourney_start_date,
                                        window_days = 90,bySurface = TRUE)
head(french_open_2025_avg_df)

french_open_2025_df2 <- french_open_2025_df1 %>%
  left_join(french_open_2025_avg_df%>%filter(surface=="Clay"),by="name",suffix = c("",""))
french_open_2025_df2 <- data.frame(french_open_2025_df2)

head(french_open_2025_df2)
str(french_open_2025_df2)

#Compute target variable
#First step is to filter out
french_open_2025_df3 <- french_open_2025_df2 %>%
  mutate(rank_bin=case_when(rank<9 ~"ATP Rank 1-8",
                            rank<17 ~"ATP Rank 9-16",
                            rank<33 ~"ATP Rank 17-32",
                            rank<65 ~"ATP Rank 33-64",
                            rank<129 ~"ATP Rank 65-128",
                            TRUE~"129+"))%>%
  filter(rank_bin != "129+")

head(french_open_2025_df3)
atp_2025[atp_2025$tourney_name=="Roland Garros","tourney_id"]
source("C:/Users/MarkM/OneDrive/Documents/ATP_Tennis_Project1/grand_slam_expected_wins.R")

french_open_exp_wins <- tourney_df(atp_2025,"2025-520")
french_open_exp_wins <- data.frame(french_open_exp_wins)
dim(french_open_exp_wins)

french_open_2025_df4 <- french_open_2025_df3 %>%
  left_join(wimbledon_exp_wins%>% dplyr::select(id,bonus_round),
            by="id",suffix = c("",""))
french_open_2025_df4 <- data.frame(french_open_2025_df4)

#Add Historical Rankings 
source("C:/Users/MarkM/OneDrive/Documents/ATP_Tennis_Project1/top_rank_func.R")
#Retrieve highest ranking from the last five years which 1825 days
max_rank_df <- top_rank_func(rank_stats,min_tourney_start_date,1825)

french_open_2025_df5 <- french_open_2025_df4 %>%
  left_join(max_rank_df,
            by="name",suffix = c("",""))
colnames(french_open_2025_df5)
dim(french_open_2025_df5)
head(french_open_2025_df5)
table(french_open_2025_df5$bonus_round)

#Save dataset 
saveRDS(french_open_2025_df5,
        file="C:/Users/MarkM/OneDrive/Documents/ATP_Tennis_Project1/GrandSlams_Datasets/french_open_2025_df5.rds")
