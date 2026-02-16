#This function calculates average for key match metrics for a time window
stats_avg_func <- function(data,group_id,ref_date,window_days,bySurface=TRUE) {
  
  arguments <- as.list(match.call())
  player_id <- as.character(eval(arguments$group_id,data))
  ref_date <- ymd(ref_date)      # parse the input date
  ref_date1 <- ymd(ref_date - days(window_days))
  
  summary_df <- if(bySurface) { data%>%
      filter(between(tourney_date,ref_date1,ref_date))%>%
      select({{group_id}},surface,ace,df,svpt,SvGms,bpSaved,bpFaced,first_point_serve_win_pct,
             second_point_serve_win_pct)%>%
      group_by({{group_id}},surface)%>%
      summarise(avg_ace=mean(ace,na.rm=TRUE),
                avg_df=mean(df,na.rm=TRUE),
                avg_bpSaved=mean(bpSaved,na.rm=TRUE),
                avg_bpFaced=mean(bpFaced,na.rm=TRUE),
                avg_first_point_serve_win_pct=mean(first_point_serve_win_pct,na.rm=TRUE),
                avg_second_point_serve_win_pct=mean(second_point_serve_win_pct,na.rm=TRUE))
  } else {
    data%>%
      filter(between(tourney_date,ref_date1,ref_date))%>%
      select({{group_id}},surface,ace,df,svpt,SvGms,bpSaved,bpFaced,first_point_serve_win_pct,
             second_point_serve_win_pct)%>%
      group_by({{group_id}})%>%
      summarise(avg_ace=mean(ace,na.rm=TRUE),
                avg_df=mean(df,na.rm=TRUE),
                avg_bpSaved=mean(bpSaved,na.rm=TRUE),
                avg_bpFaced=mean(bpFaced,na.rm=TRUE),
                avg_first_point_serve_win_pct=mean(first_point_serve_win_pct,na.rm=TRUE),
                avg_second_point_serve_win_pct=mean(second_point_serve_win_pct,na.rm=TRUE)) 
  }
  
  return(summary_df)               
  
}