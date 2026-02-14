tourney_df <- function(data,tourney_id){
  
  
  
  #Returns a dataframe withe the number of actual versus expected wins in the rank
  
  arguments <- as.list(match.call())
  
  id <- as.character(eval(arguments$tourney_id,data))
  
  df <- data[data$tourney_id==id,]
  
  
  
  progress_past_first_id <-unique(df$winner_id)
  
  first_round_ids <- setdiff(unique(df$loser_id),progress_past_first_id)
  
  stopifnot(length(progress_past_first_id)==length(first_round_ids))
  
  
  
  
  
  num_wins <- vector(mode="numeric",length(progress_past_first_id))
  
  counter <- 1
  
  
  
  for (i in progress_past_first_id ){
    
    num_wins[counter]  <- nrow(df[df$winner_id==i,])
    
    counter = counter +1
    
  }
  
  
  
  winners_df <- data.frame(id=progress_past_first_id,num_wins=num_wins)
  
  losers_df <-  data.frame(id=first_round_ids,num_wins=0)
  
  losers_df1 <- losers_df %>% merge(.,df[,c("loser_id","loser_rank","loser_hand", "loser_ht", "loser_age")],
                                    
                                    by.x="id",by.y="loser_id") %>%
    
    dplyr::rename(player_rank=loser_rank, player_hand=loser_hand,
                  
                  player_ht=loser_ht, player_age=loser_age)
  
  
  
  winners_metrics <- df %>%
    
    arrange(tourney_date) %>%
    
    group_by(winner_id,tourney_date) %>%
    
    filter(row_number() == 1) %>%
    
    select(winner_id,winner_rank,winner_hand,winner_ht,winner_age)
  
  
  
  winners_df1 <- winners_df %>% merge(.,winners_metrics[,c("winner_id","winner_rank","winner_hand", "winner_ht", "winner_age")],
                                      
                                      by.x="id",by.y="winner_id") %>%
    
    dplyr::rename(player_rank=winner_rank,player_hand=winner_hand,
                  
                  player_ht=winner_ht, player_age=winner_age)
  
  
  
  players_df1 <- data.frame(rbind(winners_df1,losers_df1))
  
  
  
  players_df2 <- players_df1%>% mutate(expected_wins=ceiling(7 -log(player_rank,base=2)),
                                       
                                       bonus_round=ifelse(num_wins>expected_wins,1,0))
  
  
  
  players_df2
  
}