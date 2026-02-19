#######ATP 2025 EDA######

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

#Split into categorical and numerical variables
atp_2025_cat_vars <- atp_2025 %>% select(where(~ is.character(.x) || is.factor(.x)))
atp_2025_num_vars   <- atp_2025 %>% select(where(is.numeric))
any(is.na(atp_2025_cat_vars)) #  There are no instances of categorical variables
any(atp_2025_num_vars)

#Visualisation of missing values
vis_miss(atp_2025_num_vars)

#Test if the data are missing completely at random
mcar_test(atp_2025_num_vars)  

#Summary of winners
players_win <- as.data.frame(table(atp_2025$winner_name))

###Change the name of the columns
names(players_win)[names(players_win) =="Var1"] <- "player_name"
names(players_win)[names(players_win) =="Freq"] <- "wins"
str(players_win)
summary(players_win)

#Change threshold to 10 to make it less crowded
F = 10
p <- ggplot(subset(players_win, wins > F), aes(x = player_name, y = wins)) +
  geom_bar(stat="identity", color = I('black'), fill = I('#099DD9'))
p <- p + theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.2, 
                                          family="Times",  colour="darkred"))
p <- p + scale_y_continuous(breaks = seq(0, 80, 5))

## Add winner country and show nationality 

players_ioc <- atp_2025 %>% 
  select(winner_name,winner_ioc) %>%
  distinct(winner_name,winner_ioc) %>%
  filter(winner_name %in% players_win$player_name) 

names(players_ioc)[names(players_ioc) =="winner_name"] <-"player_name"
###Merge the Data frame
players_win_ioc <-  merge(players_ioc,players_win,
                          by ="player_name", all.y= TRUE)


F = 20
p <- ggplot(subset(players_win_ioc, wins > F),
            aes(x = reorder(player_name, wins), y = wins))
p <- p + geom_bar(width=1, colour="white",stat="identity")
p <- p + geom_text(aes(label=wins), color="white", hjust=1, size=3)
p <- p + theme(axis.text.x = element_text(hjust=1, vjust=0.2), 
               legend.position="none")
p <- p + coord_flip()

#Win Ratio
## select the winner id and names
w_count <- atp_2025 %>% select(winner_id,winner_name)
names(w_count)[names(w_count) =="winner_id"] <- "player_id"
names(w_count)[names(w_count) =="winner_name"] <- "player_name"

## Do the same for loser id and names
l_count <- atp_2025 %>% select(loser_id,loser_name)
names(l_count)[names(l_count) =="loser_id"] <- "player_id"
names(l_count)[names(l_count) =="loser_name"] <- "player_name"

## Merge the two dataframes together 
merged_count <- rbind(w_count,l_count)
merged_count <- as.data.frame(merged_count)

## group by player_id and sum the matches.
count_player <- group_by(merged_count, player_id, player_name)
matches_by_player <- summarize(count_player, total_matches = n())

## merge with earlier daatset on player_name
merged_count_wins <- merge(matches_by_player,players_win, 
                           by ="player_name", all.x= TRUE)
## replace NA's with 0 and add win ratio column
merged_count_wins$wins[is.na(merged_count_wins$wins)] <- 0 
merged_count_wins <- merged_count_wins %>% 
  mutate(win_ratio = ((wins/total_matches)*100 ))

p <- ggplot(subset(merged_count_wins, wins > F), 
            aes(x = reorder(player_name, wins), 
                y = win_ratio, fill = win_ratio))
p <- p + geom_bar(width=1, colour="white",stat="identity")
p <- p + geom_text(aes(label=wins),
                   color="black",
                   hjust=0.6,vjust=0.2,  size=2)
p <- p + theme(axis.text.x = element_text(angle=90,hjust=1, vjust=0.2), 
               legend.position="none")
p <- p + scale_y_continuous(breaks = seq(0, 100, 10)) +xlab("Player") +ylab("Win Ratio")

#tournament Wins
tour_wins <- atp_2025 %>% select(tourney_id,tourney_name,tourney_level, 
                                 surface,winner_name,loser_name, 
                                 winner_seed,round) %>%
  filter(round == 'F')

p <- ggplot(tour_wins, aes(x = winner_name)) + 
  geom_bar(aes(y = ..count..), stat = "count", color = I('black'), 
           fill = I('#099DD9'))
p <- p + theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.2, 
                                          family="Times", colour="darkred"))
p <- p + scale_y_continuous(breaks = seq(0, 10, 1))  

qplot(x = winner_name, 
      data = tour_wins, 
      xlab ='winner_name', color = I('black'), fill = I('#099DD9')) + 
  scale_y_continuous(breaks = seq(0, 26, 2)) +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.2, 
                                   family="Times", colour="darkred")) +
  facet_wrap(~ surface, scales="free")

qplot(x = winner_name, 
      data = tour_wins, 
      xlab ='winner_name', color = I('black'), fill = I('#099DD9')) + 
  scale_y_continuous(breaks = seq(0, 26, 2)) +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.2, 
                                   family="Times", colour="darkred")) +
  facet_wrap(~ tourney_level, scales="free")

#### Players win ratio per surface for the tournament winners
pl_win_by_surface <- as.data.frame(table(atp_2025$winner_name, 
                                         atp_2025$surface))
names(pl_win_by_surface)[names(pl_win_by_surface) =="Var1"] <- "player_name"
names(pl_win_by_surface)[names(pl_win_by_surface) =="Var2"] <- "Surface"
names(pl_win_by_surface)[names(pl_win_by_surface) =="Freq"] <- "Surface_wins"

## Change dataframe to wide format

players_win_by_surface_gp <- dcast(data=pl_win_by_surface, 
                                   formula = player_name ~ Surface)
players_win_by_surface_gp$total <- rowSums(players_win_by_surface_gp[-1])

## get percentage_win ratio for each surface
players_win_by_surface_gp <- players_win_by_surface_gp %>% 
  mutate(w_pc_clay = ((Clay/total) *100)) %>% 
  mutate(w_pc_Grass = ((Grass/total) * 100), w_pc_Hard = ((Hard/total) * 100))


#### change back to Long and look at players with more than 15 wins on tour

players_win_by_surface_15 <- players_win_by_surface_gp %>% filter(total > 15)
players_win_by_surface_gp.long <- gather(players_win_by_surface_15,
                                         surface_type, pc_s_wins, -player_name,
                                         -Clay, -Grass, -Hard, -total)

tours_wins_surface <- players_win_by_surface_gp.long %>% 
  filter(player_name %in% tour_wins$winner_name)


tours_wins_surface <- tours_wins_surface %>% 
  filter(pc_s_wins != 0)

ggplot(tours_wins_surface,aes(x=factor(player_name),
                              y=pc_s_wins,label=paste(round(pc_s_wins),"%"),fill=factor(surface_type))) + 
  geom_bar(stat="identity") + geom_text(position="stack",
                                        aes(ymax=1),vjust=1.2, size=2.5) + 
  theme(axis.text.x = element_text(angle=90, hjust=1, 
                                   vjust=0.2, family="Times",  colour="darkred")) +
  xlab("Player") +
  ylab("Win Percentage")

## select the winner id and names
ws_count <- atp_2025 %>% select(winner_id,winner_name,surface)
names(ws_count)[names(ws_count) =="winner_id"] <- "player_id"
names(ws_count)[names(ws_count) =="winner_name"] <- "player_name"

## Do the same for loser id and names
ls_count <- atp_2025 %>% select(loser_id,loser_name, surface)
names(ls_count)[names(ls_count) =="loser_id"] <- "player_id"
names(ls_count)[names(ls_count) =="loser_name"] <- "player_name"

## Merge the two dataframes together 
merged_scount <- rbind(ws_count,ls_count)
merged_scount <- as.data.frame(merged_scount)

## Count the matches for each player
m_by_surface <- as.data.frame(table(merged_scount$player_name,
                                    merged_scount$surface))
names(m_by_surface )[names(m_by_surface) =="Var1"] <- "player_name"
names(m_by_surface )[names(m_by_surface) =="Var2"] <- "Surface"
names(m_by_surface )[names(m_by_surface)=="Freq"]<-"Surface_Matches"

## Change the data to wide foramt
m_by_surface_gp <- dcast(data=m_by_surface , 
                         formula = player_name ~ Surface)
m_by_surface_gp$total <- rowSums(m_by_surface_gp[-1])

### rename so we can merge with wins_per_surafce data frame
names(m_by_surface_gp)[names(m_by_surface_gp)=="Grass"] <- "Grass_Total"
names(m_by_surface_gp)[names(m_by_surface_gp) =="Clay"] <- "Clay_Total"
names(m_by_surface_gp)[names(m_by_surface_gp) =="Hard"] <- "Hard_Total"
names(m_by_surface_gp)[names(m_by_surface_gp) =="total"] <- "Matches_Total"

combined_surface <- merge(m_by_surface_gp,players_win_by_surface_gp, 
                          by ="player_name", all.x= TRUE)

## perform win_ratio calculations
wr_by_surface <- combined_surface %>% 
  mutate(wr_clay = ((Clay/Clay_Total)), 
        wr_Grass = ((Grass/Grass_Total)), 
         wr_Hard = ((Hard/Hard_Total)))

## Change Nan's to 0
wr_by_surface$wr_clay[is.nan(wr_by_surface$wr_clay)] <- 0
wr_by_surface$wr_Grass[is.nan(wr_by_surface$wr_Grass)] <- 0
wr_by_surface$wr_Hard[is.nan(wr_by_surface$wr_Hard)] <- 0

wr_by_surface_updated <- wr_by_surface %>% 
  select(wr_clay,wr_Grass,wr_Hard,player_name,Matches_Total,total)

tours_wr_surface <- wr_by_surface_updated %>% 
  filter(player_name %in% tour_wins$winner_name)


tours_wr_surface.long <- gather(tours_wr_surface, 
                                surface_type, wr_by_surface, 
                                -player_name,-Matches_Total, -total)
## rename wins per surface
names(tours_wr_surface.long)[names(tours_wr_surface.long)
                             =="wr_by_surface"] <- "surface_win_ratio" 

## I removed every row with 0 because they were causing issue withe the graph display

tours_wr_surface.long <- tours_wr_surface.long %>% 
  filter(surface_win_ratio != 0)

ggplot(tours_wr_surface.long,
       aes(x=factor(player_name),
           y=surface_win_ratio,label=paste(round(surface_win_ratio * 100),"%"),
           fill=factor(surface_type))) + 
  geom_bar(stat="identity") + 
  geom_text(position="stack", vjust=1.2, size = 2.5) + 
  theme(axis.text.x = element_text(angle=90, 
                                   hjust=1, vjust=0.2, 
                                   family="Times",  colour="darkred"))+
  scale_y_continuous(labels = scales::percent) +
  xlab("Player") +
  ylab("Surface Win Ratio")

players_tours_win <- as.data.frame(table(tour_wins$winner_name))

##Change the name of the column
names(players_tours_win)[names(players_tours_win) =="Var1"] <- "player_name"
names(players_tours_win)[names(players_tours_win) =="Freq"] <-"tournament_wins"
###Merge the Data frame
atp_player_data <-  merge(merged_count_wins,players_tours_win,
                          by ="player_name", all.x= TRUE)
### replace NA's of tournament wins with 0
atp_player_data$tournament_wins[is.na(atp_player_data$tournament_wins)] <- 0 
str(atp_player_data)

#### Tournament Types Analysis
summary(atp_2025$tourney_level)

players_win_by_tour <- as.data.frame(table(atp_2025$winner_name, 
                                           atp_2025$tourney_level))
names(players_win_by_tour)[names(players_win_by_tour) 
                           =="Var1"] <- "player_name"
names(players_win_by_tour)[names(players_win_by_tour) 
                           =="Var2"] <- "Tournament_Level"
names(players_win_by_tour)[names(players_win_by_tour) 
                           =="Freq"] <- "Total_wins"

## Have a look at the data
str(players_win_by_tour)

p <- ggplot(aes(x = player_name, y=Total_wins, fill = Tournament_Level),
            data=subset(players_win_by_tour,Total_wins > 5)) +
  geom_bar(stat="Identity")
p <- p + theme(axis.text.x = element_text(angle=90, 
                                          hjust=1, vjust=0.2, 
                                          family="Times", colour="darkred"))
p <- p + scale_y_continuous(breaks = seq(0, 80, 5))

#Influence of player seed
qplot(x = winner_seed, 
      data = subset(atp_2025,!is.na(winner_seed)), 
      xlab ='winner_seed', binwidth = 1,color = I('black'), 
      fill = I('#099DD9')) +
  scale_x_continuous(breaks = seq(1, 32, 1))

tour_wins$winner_seed[is.na(tour_wins$winner_seed)] <- "Unseeded"
qplot(x = winner_seed, 
      data = tour_wins, 
      xlab ='winner_seed', color = I('black'), fill = I('#099DD9')) + 
  scale_y_continuous(breaks = seq(0, 26, 2))

qplot(x = winner_seed, 
      data = tour_wins, 
      xlab ='winner_seed', color = I('black'), fill = I('#099DD9')) + 
  scale_y_continuous(breaks = seq(0, 26, 2)) +
  facet_wrap(~ tourney_level, scales="free")

##### First and Second Serve percentage - Is there a difference between Match winners and Losers
atp_2025 <- atp_2025 %>% 
  mutate(w_2ndIn = (w_svpt - w_1stIn - w_df ), 
         l_2ndIn = (l_svpt - l_1stIn - l_df )) %>%
  mutate(w_1stpwon = ((w_1stWon/w_1stIn) * 100), 
         l_1stpwon = ((l_1stWon/l_1stIn) * 100)) %>%
  mutate(w_2ndpwon = ((w_2ndWon/w_2ndIn) * 100), 
         l_2ndpwon = ((l_2ndWon/l_2ndIn) * 100))

fs_serve_won <- atp_2025 %>% select(w_1stpwon,l_1stpwon)
fs_serve_won_long <- gather(fs_serve_won, player_type, 
                            fs_percentage_won, w_1stpwon:l_1stpwon)

summary(fs_serve_won_long)
ggplot(fs_serve_won_long, aes(fs_percentage_won, fill = player_type)) + 
  geom_histogram(alpha = 0.5, binwidth = 1, aes(y = ..count..), 
                 position = 'identity') +
  scale_x_continuous(breaks = seq(10, 100, 10)) +
 xlab("First Service Percentage Wins")

qplot(x = player_type,y = fs_percentage_won,
      data = fs_serve_won_long, 
      xlab ='player_type',
      ylab = 'First server percentage points won',
      geom = 'boxplot', color = player_type) +
  scale_y_continuous(breaks = seq(0, 100, 5))

##### First serve percentage points - Match wins
high_fsperc_won <-fs_serve_won %>% select(w_1stpwon,l_1stpwon) %>%
  mutate(player_type = ifelse(w_1stpwon > l_1stpwon, "Winner", "Loser"))


summary(high_fsperc_won)

qplot(x = player_type, 
      data = subset(high_fsperc_won,!is.na(player_type)), 
      xlab ='Player_type',color = I('black'), fill = I('#099DD9'))

##### Second Serve Percentage points
### select the second serve variables for winners and losers and change to long format

ss_serve_won <- atp_2025 %>% select(w_2ndpwon,l_2ndpwon)
ss_serve_won_long <- gather(ss_serve_won, player_type, 
                            ss_percentage_won, w_2ndpwon:l_2ndpwon)


summary(ss_serve_won_long)

ggplot(ss_serve_won_long, aes(ss_percentage_won, fill = player_type)) + 
  geom_histogram(alpha = 0.5, binwidth = 1, aes(y = ..count..), 
                 position = 'identity') +
  scale_x_continuous(breaks = seq(10, 100, 10)) +
  xlim(0,100) +
  xlab("Second Service Percentage Wins")


qplot(x = player_type,y = ss_percentage_won,
      data = ss_serve_won_long, 
      xlab ='player_type',
      ylab = 'Second Serve Percentage points won',
      geom = 'boxplot', color = player_type) +
  scale_y_continuous(breaks = seq(0, 100, 5))

###### Second Serve Percenatage - Match Wins
high_ssperc_won <- ss_serve_won %>% select(w_2ndpwon,l_2ndpwon) %>%
  mutate(player_type = ifelse(w_2ndpwon > l_2ndpwon, "Winner", "Loser"))


summary(high_ssperc_won)

qplot(x = player_type, 
      data = subset(high_ssperc_won,!is.na(player_type)), 
      xlab ='Player_type',color = I('black'), fill = I('#099DD9'))

###### Break points faced
break_points_faced <- atp_2025 %>% select(w_bpFaced,l_bpFaced)
bp_faced_long <- gather(break_points_faced, player_type, 
                        bp_faced, w_bpFaced:l_bpFaced)

summary(bp_faced_long)

ggplot(bp_faced_long, aes(bp_faced, fill = player_type)) + 
  geom_histogram(alpha = 0.5, binwidth = 0.5,aes(y = ..count..), 
                 position = 'identity') +
  scale_x_continuous(breaks = seq(0, 30, 1))

qplot(x = player_type,y = bp_faced,
      data = bp_faced_long, 
      xlab ='player_type',
      ylab = 'bp_faced',
      geom = 'boxplot', color = player_type) +
  ##coord_cartesian(ylim = c(0, 35))
  scale_y_continuous(breaks = seq(0, 35, 1))

#####  Percentage break points saved
atp_2025 <- atp_2025 %>% 
  mutate(w_per_bpsaved = ((w_bpSaved/w_bpFaced) * 100), 
         l_per_bpsaved = ((l_bpSaved/l_bpFaced) * 100))
per_bpSaved <- atp_2025 %>% select(w_per_bpsaved,l_per_bpsaved)
per_bpSaved_long <- gather(per_bpSaved, player_type, 
                           percentage_bpSaved, w_per_bpsaved:l_per_bpsaved)

atp_2025 <- atp_2025 %>% 
  mutate(w_per_bpsaved = ((w_bpSaved/w_bpFaced) * 100), 
         l_per_bpsaved = ((l_bpSaved/l_bpFaced) * 100))
per_bpSaved <- atp_2025 %>% select(w_per_bpsaved,l_per_bpsaved)
per_bpSaved_long <- gather(per_bpSaved, player_type, 
                           percentage_bpSaved, w_per_bpsaved:l_per_bpsaved)

summary(per_bpSaved_long)

ggplot(per_bpSaved_long, aes(percentage_bpSaved, fill = player_type)) + 
  geom_histogram(alpha = 0.5, aes(y = ..count..), position = 'identity') +
  xlim(0,100) +
  xlab("Percentage of Break Points Saved")

#Correlation between match win stats
match_win_stats <- ggpairs(atp_player_data, c(3:6))

#### Look at players with more than 13 wins, 
F = 13

p <- ggplot(subset(merged_count_wins, wins > F), 
            aes(x = reorder(player_name, wins), y = win_ratio, fill=win_ratio))
p <- p + geom_bar(width=1, colour="white",stat="identity")
p <- p + geom_text(aes(label=wins),
                   color="black",
                   hjust=0.6,vjust=0.2,  size=2)
p <- p + theme(axis.text.x = element_text(angle=90,hjust=1, vjust=0.2), 
               legend.position="none")
p <- p + labs(x = "Player name", y = "Win Ratio (%)")
p <- p + labs(title = "Ratio of wins to Matches")

tours_wr_surface <- wr_by_surface_updated %>% 
  filter(player_name %in% tour_wins$winner_name)


tours_wr_surface.long <- gather(tours_wr_surface, 
                                surface_type, wr_by_surface, 
                                -player_name,-Matches_Total, -total)

## rename wins per surface
names(tours_wr_surface.long)[names(tours_wr_surface.long)
                             =="wr_by_surface"] <- "surface_win_ratio"

tours_wr_surface.long <- tours_wr_surface.long %>% 
  filter(surface_win_ratio != 0)

p <- ggplot(tours_wr_surface.long,
            aes(x=factor(player_name),y=surface_win_ratio,
                label=paste(round(surface_win_ratio * 100),"%"),
                fill=factor(surface_type)))
p <- p + geom_bar(stat="identity") + 
  geom_text(position="stack", aes(ymax=1),vjust=1.2, size = 2.5) 
p <- p + theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.2, 
                                          family="Times",  colour="darkred"))
p <- p + scale_y_continuous(labels = scales::percent)
p <- p + labs(x = "Player name", y = "Surface_win_Ratio(%)")
p <- p + labs(title = "Win ratio for Tournament winners by Surface")
p <- p + labs(fill ='Surface Type') 

p <- ggplot(fs_serve_won_long, aes(fs_percentage_won, fill = player_type)) 
p <- p + geom_histogram(alpha = 0.5, binwidth = 1, aes(y = ..count..), 
                        position = 'identity') 
p <- p + scale_x_continuous(breaks = seq(10, 100, 10))
p <- p + labs(x = "Percentage points won on first serve")
p <- p + labs(title = "Percentage points won on first serve by winners 
              and Losers")

#Effect of age,height and hand
#Prepare the data
winners_df <-atp_2025 %>%
  dplyr::select(surface,tourney_date, starts_with("w")) %>%
  dplyr::rename_with(~ sub("^[^_]*_", "", .x), starts_with("w"))

#Create win column
winners_df$win_game <- "Yes"

losers_df <- atp_2025 %>%
  dplyr::select(surface,tourney_date, starts_with("l")) %>%
  dplyr::rename_with(~ sub("^[^_]*_", "", .x), starts_with("l"))

losers_df$win_game <- "No"

#Quality checks before stacking both datasets
all(dim(winners_df) ==dim(losers_df))
all(names(winners_df)==names(losers_df))

match_stats <- data.frame(rbind(losers_df,winners_df))
head(match_stats)
str(match_stats)
match_stats$win_game  <- as.factor(match_stats$win_game)

#Investigate the relationship between aces and double faults
#check correlation
stats::cor(x=match_stats$df,y=match_stats$ace,use = "complete.obs")

#Assess visually
ggplot(data=match_stats,aes(x=df, y=ace,col=win_game)) +
  geom_point() +
  facet_wrap(~surface) +
  xlab("Double Faults") +
  ylab("Aces")

sum(match_stats$win_game=="Yes")/nrow(match_stats)#Sanity check to see we have the
#right number of winners and losers

#Assess visually the relationship between breakpoints faced and saved
ggplot(data=match_stats,aes(x=df, y=ace,col=win_game)) +
  geom_point() +
  facet_wrap(~surface) +
  xlab("Double Faults") +
  ylab("Aces")

match_stats %>%
  group_by(win_game) %>%
  summarise(avg_break_points_faced=mean(bpFaced,na.rm=TRUE))

#Influence of Age
ggplot(match_stats, aes(x = age, colour = win_game)) +
  geom_density() +
  labs(title = "Age Distribution by Match OutCome", x = "Age", y = "Density") +
  facet_wrap(~surface) +
  labs(colour="Game Winner")

#KS Tests
clay_winners <- match_stats %>% filter(surface=="Clay"& win_game=="Yes")%>%
                select(age) 
clay_winners1 <-  unlist(as.vector(clay_winners))


clay_losers <- match_stats %>% filter(surface=="Clay"& win_game=="No")%>%
  select(age)
clay_losers1 <-  unlist(as.vector(clay_losers))

ks.test(x=as.vector(clay_winners1),y=as.vector(clay_losers1))


grass_winners <- match_stats %>% filter(surface=="Grass"& win_game=="Yes")%>%
  select(age) 
grass_winners1 <-  unlist(as.vector(grass_winners))

grass_losers <- match_stats %>% filter(surface=="Grass"& win_game=="No")%>%
  select(age)
grass_losers1 <-  unlist(as.vector(grass_losers))

ks.test(x=as.vector(grass_winners1),y=as.vector(grass_losers1))


hard_winners <- match_stats %>% filter(surface=="Hard"& win_game=="Yes")%>%
  select(age) 
hard_winners1 <-  unlist(as.vector(hard_winners))

hard_losers <- match_stats %>% filter(surface=="Hard"& win_game=="No")%>%
  select(age)
hard_losers1 <-  unlist(as.vector(hard_losers))

ks.test(x=as.vector(hard_winners1),y=as.vector(hard_losers1))

#Influence of Height
ggplot(match_stats, aes(x = ht, colour = win_game)) +
  geom_density() +
  labs(title = "Height Distribution by Match OutCome", x = "Height", y = "Density") +
  facet_wrap(~surface) +
  labs(colour="Game Winner")

#Carry out KS Tests
clay_winners <- match_stats %>% filter(surface=="Clay"& win_game=="Yes")%>%
  select(ht) 
clay_winners1 <-  unlist(as.vector(clay_winners))


clay_losers <- match_stats %>% filter(surface=="Clay"& win_game=="No")%>%
  select(ht)
clay_losers1 <-  unlist(as.vector(clay_losers))

ks.test(x=as.vector(clay_winners1),y=as.vector(clay_losers1))

grass_winners <- match_stats %>% filter(surface=="Grass"& win_game=="Yes")%>%
  select(ht) 
grass_winners1 <-  unlist(as.vector(grass_winners))

grass_losers <- match_stats %>% filter(surface=="Grass"& win_game=="No")%>%
  select(ht)
grass_losers1 <-  unlist(as.vector(grass_losers))

ks.test(x=as.vector(grass_winners1),y=as.vector(grass_losers1))

hard_winners <- match_stats %>% filter(surface=="Hard"& win_game=="Yes")%>%
  select(ht) 
hard_winners1 <-  unlist(as.vector(hard_winners))

hard_losers <- match_stats %>% filter(surface=="Hard"& win_game=="No")%>%
  select(ht)
hard_losers1 <-  unlist(as.vector(hard_losers))

ks.test(x=as.vector(hard_winners1),y=as.vector(hard_losers1))

head(match_stats)
#Influence of Dominant Hand
match_stats %>%
  filter(hand !="")%>%
  dplyr::mutate(game_winner=ifelse(win_game=="Yes",1,0)) %>%
  group_by(surface,hand) %>%
  summarise(win_rate=mean(game_winner)) 

match_stats %>%
  filter(hand !="")%>%
  dplyr::mutate(game_winner=ifelse(win_game=="Yes",1,0)) %>%
  group_by(surface,hand) %>%
  summarise(win_rate=mean(game_winner))  

clay_stats1 <- match_stats %>%
  filter(hand !="" & surface=="Clay")

chisq.test(table(clay_stats1$win_game,clay_stats1$hand)) 

grass_stats1 <- match_stats %>%
  filter(hand !="" & surface=="Grass")

chisq.test(table(grass_stats1$win_game,grass_stats1$hand))


hard_stats1 <- match_stats %>%
  filter(hand !="" & surface=="Hard")

chisq.test(table(hard_stats1$win_game,hard_stats1$hand))

#Expected Wins Analysis

#Put the data together
unique(atp_2025$tourney_id)
unique(atp_2025$tourney_name)
#Extract IDs
atp_2025[atp_2025$tourney_name=="Australian Open","tourney_id"]
atp_2025[atp_2025$tourney_name=="Roland Garros","tourney_id"]
atp_2025[atp_2025$tourney_name=="Wimbledon","tourney_id"]
atp_2025[atp_2025$tourney_name=="US Open","tourney_id"]

#Dataset by tournament
aus_open_df <- tourney_df(atp_2025,"2025-580")
french_open_df <- tourney_df(atp_2025,"2025-520")
wimbledon_df <- tourney_df(atp_2025,"2025-540")
us_open_df <- tourney_df(atp_2025,"2025-560")

#Compare number of bonus round by tournaments
table(aus_open_df$bonus_round)
table(french_open_df$bonus_round)
table(wimbledon_df$bonus_round)
table(us_open_df$bonus_round)

#Plot in Bar Chart
summary_df <- data.frame(tournament=c("Australian Open","French Open",
                                      "Wimbledon","US Open"),
                         overachievers=c(20,22,26,21))
head(summary_df)

ggplot(summary_df, aes(x = tournament, y = overachievers,fill=tournament)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("purple","orange","blue","darkgreen")) +
  geom_text(aes(label=overachievers),vjust=-0.5) +
  ggtitle("Number of players that went further than suggested by their seed")

#Distribution of expected wins by rank
rank_seq <- seq(1,200)
expected_wins <- ceiling(7 -log(rank_seq,base=2))
expected_wins_df <- data.frame(cbind(player_rank=rank_seq,expected_wins))
head(expected_wins_df)

ggplot(expected_wins_df, aes(x = player_rank, y = expected_wins)) +
  geom_point() +
  stat_smooth() +
  xlab("Player Rank") +
  ylab("Grand Slam Expected Games Win Total") +
  ggtitle("Expected Grand Slam Wins By ATP Ranking")

#Check which ATP ranking bands yields the most upsets
grand_slams_df <- data.frame(rbind(aus_open_df,french_open_df,
                                   wimbledon_df,us_open_df))
head(grand_slams_df)
players_rank <- data.frame(atp_rank=grand_slams_df[grand_slams_df$bonus_round==1,"player_rank"])
players_rank1 <- players_rank  %>%
  filter(atp_rank<257)%>%
  na.omit()

custom_breaks <-c(1,9,17,33,65,129,257)
hist(players_rank1$atp_rank,breaks=custom_breaks,
     main="Histogram of ATP rank for Grand Slam Overchievers",
     xlab="ATP Rank")

#Group Rankings Into Bins
grand_slams_df%>%
                mutate(rank_bin=case_when(player_rank<9 ~"ATP Rank 1-8",
                player_rank<17 ~"ATP Rank 9-16",
                player_rank<33 ~"ATP Rank 17-32",
                player_rank<65 ~"ATP Rank 33-64",
                player_rank<129 ~"ATP Rank 65-128",
                TRUE~"129+")) %>%
                group_by(rank_bin) %>%
                summarise(avg_bonus=mean(bonus_round,na.rm=TRUE))

#Exclude players with a rank lower than 128 
grand_slams_df%>%
  filter(player_rank <=128)%>%
  summarise(avg_bonus=mean(bonus_round,na.rm=TRUE),
            total_bonus=sum(bonus_round))

#Exploratory model to investigate the metrics with highest predictive value in a model
#Use stepwise technique based on AIC
#Add feature engineering techniques
#Feature Engineering
head(match_stats) 
match_stats1 <-match_stats %>%
  mutate(first_point_serve_win_pct=X1stWon/X1stIn,
         second_point_serve_win_pct=X2ndWon/pmax((svpt - X1stIn -df),1),#Double check this metric
         aces_df_ratio=ace/pmax(df,1),
         bp_saved_pct=bpSaved/pmax(bpFaced,1))

names(match_stats1)
vars_to_remove <- c("tourney_date","id","seed","entry","name","hand","ioc",
                    "rank","rank_points","X1stIn","X1stWon","X2ndWon",
                    "SvGms","svpt")
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

hard_court1 <- hard_court[,setdiff(names(hard_court),hard_vars_to_remove)]
head(hard_court1)
full_model <- glm(win_game~.,data=hard_court1,family = "binomial")
#Stepwise selection
step(full_model,scope=~.,direction = "both")

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

grass_court1 <- grass_court[,setdiff(names(grass_court),grass_vars_to_remove)]
head(grass_court1)
full_model <- glm(win_game~.,data=grass_court1,family = "binomial")
#Stepwise selection
step(full_model,scope=~.,direction = "both")

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

clay_court1 <- clay_court[,setdiff(names(clay_court),clay_vars_to_remove)]
head(clay_court1)
full_model <- glm(win_game~.,data=clay_court1,family = "binomial")
#Stepwise selection
step(full_model,scope=~.,direction = "both")
