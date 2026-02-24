##########Hierarchical Clustering Models########

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
library(Epi)
library(lattice)
library(MASS)
library(cluster)
library(factoextra)

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

#Select players whose maximum ranking is at least 32
top_players <- match_stats1 %>%
  group_by(name)%>%
  summarise(max_rank= max(rank))%>%
  filter(max_rank<=32)%>%
  distinct(name) 
top_players <- as.vector(unlist(top_players))

#Create a dataframe of top players
top_players_stats <- match_stats1[match_stats1$name %in% top_players,]
head(top_players_stats)

vars_to_remove <- c("id","seed","entry","hand","ioc",
                    "rank","rank_points","X1stIn","X1stWon","X2ndWon",
                    "X1stRetWon","X1stRetIn","X2ndRetIn",
                    "X2ndRetWon","bpIn","bpConverted")

#Service Games and Service Points have been removed to target data leakage potential
top_players_stats1 <-  top_players_stats[,setdiff(names(match_stats1),
                                                  vars_to_remove)]

head(top_players_stats1)
#Retrieve maximum and minimum date
max(top_players_stats1$tourney_date)
min(top_players_stats1$tourney_date)

top_players_stats1$tourney_date <- ymd(top_players_stats1$tourney_date)
class(top_players_stats1$tourney_date)

#Create vector of vars to summarise
vars_summary <- c("name","tourney_date","surface","age", "ace", "df","bpSaved","bpFaced","first_point_serve_win_pct",
                  "second_point_serve_win_pct","first_ret_win_pct","second_point_serve_win_pct",
                  "first_ret_win_pct", "second_ret_win_pct", "bp_conversion_pct","svpt","SvGms", 
                  "aces_df_ratio","bp_saved_pct")

#Aggregate statistics
source("C:/Users/MarkM/OneDrive/Documents/ATP_Tennis_Project1/match_stats_average_func.R")
stats_by_surface_df <- stats_avg_func(data=top_players_stats1[,vars_summary],group_id=name,ref_date="20251110",
               window_days=300,bySurface=TRUE)

stats_by_surface_df <- data.frame(stats_by_surface_df)
str(stats_by_surface_df)


#Retrieve athletic attribute statistics such as age  and height
top_players_data <- top_players_stats1 %>%
  dplyr::select(name,tourney_date,age,ht)%>%
  group_by(name) %>%
  slice_min(order_by = tourney_date, n = 1,with_ties = FALSE) %>%
  ungroup() %>%
  dplyr::select(name,age,ht)

#Duplicate check
any(duplicated(top_players_data$name))


#Merge to stats file
stats_by_surface_df1 <- stats_by_surface_df %>%
                      merge(.,top_players_data,by="name")
head(stats_by_surface_df1) 

#Calculate win rate by surface
win_rate_surface <- top_players_stats1%>%
  group_by(name,surface)%>%
  summarise(win_rate=mean(win_game=="Yes"))

#Build a Clay Model
clay_stats <- stats_by_surface_df1[stats_by_surface_df1$surface=="Clay",]
str(clay_stats)
plot_labels <- clay_stats$name

#Scale the data
clay_stats_scaled <- scale(clay_stats[,3:14])

#Try different hierarchical clustering methods with euclidean distance
#Average
clay_hclust1  <- agnes(clay_stats_scaled,diss=FALSE,metric="euclidean",method="average")
plot(clay_hclust1,main="Average Link",which.plot=2,labels=plot_labels)	## dendrogram

#Single
clay_hclust2  <- agnes(clay_stats_scaled,diss=FALSE,metric="euclidean",method="single")
plot(clay_hclust2,main="Single Link",which.plot=2,labels=plot_labels)	## dendrogram

#Complete
clay_hclust3  <- agnes(clay_stats_scaled,diss=FALSE,metric="euclidean",method="complete")
plot(clay_hclust3,main="Complete Link",which.plot=2,labels=plot_labels)	## dendrogram

#Ward
clay_hclust4  <- agnes(clay_stats_scaled,diss=FALSE,metric="euclidean",method="ward")
plot(clay_hclust4,main="Ward",which.plot=2,labels=plot_labels)	## dendrogram

fviz_nbclust(clay_stats_scaled, FUN=hcut, method = "wss",hc.method="ward")

# re-draw dendrogram with red borders around the clusters 
plot(clay_hclust4,main="Ward, 3 Clusters For Clay Matches",which.plot=2,labels=plot_labels)
rect.hclust(clay_hclust4, k=3, border="red") 

# Explore the clusters
clay.groups.3 <- cutree(clay_hclust4,3) # store the results
clay.groups.3
aggregate(clay_stats[,3:14],list(clay.groups.3),median)