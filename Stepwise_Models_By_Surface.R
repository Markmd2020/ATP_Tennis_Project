########Stepwise Exploratory Models For 3 Surfaces###### 

#The idea is extract variable importance#

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

#Feature Engineering
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

vars_to_remove <- c("tourney_date","id","seed","entry","name","hand","ioc",
                    "rank","rank_points","X1stIn","X1stWon","X2ndWon",
                    "SvGms","svpt","X1stRetWon","X1stRetIn","X2ndRetIn",
                    "X2ndRetWon","bpIn","bpConverted")

#Service Games and Service Points have been removed to target data leakage potential
match_stats2 <-  match_stats1[,setdiff(names(match_stats1),vars_to_remove)]
str(match_stats2)

#Create a hard court model
hard_court <- match_stats2%>%
  filter(surface=="Hard") %>%
  dplyr::select(-surface) %>%
  na.omit()
head(hard_court)
summary(hard_court)

#Keep numeric vars
hard_court_num <- hard_court%>%
  select_if(is.numeric)

#Data Visualisation

#calculate correlation coefficients, rounded to 2 decimal places
cor_df <- round(cor(hard_court_num), 2)

#melt the data frame to enable plotting
melted_cor <- melt(cor_df)

#view head of melted data frame
head(melted_cor)

#create correlation heatmap
ggplot(data = melted_cor, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value), size = 5) +
  scale_fill_gradient2(low = "blue", high = "red",
                       limit = c(-1,1), name="Correlation") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank())

hard_vars_to_remove <- c("bpSaved")

hard_court1 <- hard_court[,setdiff(c("win_game",names(hard_court_num)),hard_vars_to_remove)]
head(hard_court1)
hard_court_full_model <- glm(win_game~.,data=hard_court1,family = "binomial")
#Stepwise selection
step(hard_court_full_model,scope=~.,direction = "both")

#Repeat the analysis for grass
grass_court <- match_stats2%>%
  filter(surface=="Grass") %>%
  dplyr::select(-surface) %>%
  na.omit()
head(grass_court)
summary(grass_court)

#Keep numeric vars
grass_court_num <- grass_court%>%
  select_if(is.numeric)

head(grass_court_num)

#Data Visualisation

#calculate correlation coefficients, rounded to 2 decimal places
cor_df <- round(cor(grass_court_num), 2)
#melt the data frame to enable plotting
melted_cor <- melt(cor_df)
#view head of melted data frame
head(melted_cor) 

#create correlation heatmap
ggplot(data = melted_cor, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value), size = 5) +
  scale_fill_gradient2(low = "blue", high = "red",
                       limit = c(-1,1), name="Correlation") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank())

grass_vars_to_remove <- c("bpSaved")

grass_court1 <- grass_court[,setdiff(c("win_game",names(grass_court_num)),grass_vars_to_remove)]
head(grass_court1)
grass_court_full_model <- glm(win_game~.,data=grass_court1,family = "binomial")
#Stepwise selection
step(grass_court_full_model,scope=~.,direction = "both")

#Repeat the analysis for clay
clay_court <- match_stats2%>%
  filter(surface=="Clay") %>%
  dplyr::select(-surface) %>%
  na.omit()
head(clay_court)
summary(clay_court)

#Keep numeric vars
clay_court_num <- clay_court%>%
  select_if(is.numeric)

head(clay_court_num)

#Data Visualisation

#calculate correlation coefficients, rounded to 2 decimal places
cor_df <- round(cor(clay_court_num), 2)
#melt the data frame to enable plotting
melted_cor <- melt(cor_df)
#view head of melted data frame
head(melted_cor)

#create correlation heatmap
ggplot(data = melted_cor, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value), size = 5) +
  scale_fill_gradient2(low = "blue", high = "red",
                       limit = c(-1,1), name="Correlation") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank())

clay_vars_to_remove <- c("bpSaved")   

clay_court1 <- clay_court[,setdiff(c("win_game",names(clay_court_num)),clay_vars_to_remove)]
head(clay_court1)
clay_court_full_model <- glm(win_game~.,data=clay_court1,family = "binomial")
#Stepwise selection
step(clay_court_full_model,scope=~.,direction = "both") 

