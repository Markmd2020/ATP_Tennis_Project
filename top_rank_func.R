top_rank_func <- function(data,ref_date,window_days){
  
  ref_date <- ymd(ref_date)      # parse the input date
  ref_date1 <- ymd(ref_date - days(window_days))
  
  if (!inherits(data$tourney_date, "Date")) {
    stop(sprintf("Tourney Date Column must be of type Date, but is '%s'.",
                 class(data$tourney_date)[1]),
         call. = FALSE)
  } 
  
  top_rank_df <- data %>%
    filter(between(tourney_date,ref_date1,ref_date))%>%  
    group_by(name)%>%
    summarise(max_rank= max(rank))
  
}