#This function calculates IQR values for key match metrics for a time window
stats_iqr_func <- function(data,group_id,ref_date,window_days,bySurface=TRUE) {
  
  arguments <- as.list(match.call())
  player_id <- as.character(eval(arguments$group_id,data))
  ref_date <- ymd(ref_date)      # parse the input date
  ref_date1 <- ymd(ref_date - days(window_days))
  
  summary_df <- if(bySurface) { data%>%
      filter(between(tourney_date,ref_date1,ref_date))%>%
      select({{group_id}},surface,ace,df,svpt,SvGms,bpSaved,bpFaced,first_point_serve_win_pct,
             second_point_serve_win_pct)%>%
      group_by({{group_id}},surface)%>%
      summarise(ace_q25=quantile(ace,0.25,na.rm=TRUE),
                ace_q50=quantile(ace,0.5,na.rm=TRUE),
                ace_q75=quantile(ace,0.75,na.rm=TRUE),
                df_q25=quantile(df,0.25,na.rm=TRUE),
                df_q50=quantile(df,0.5,na.rm=TRUE),
                df_q75=quantile(df,0.75,na.rm=TRUE),
                bpSaved_q25=quantile(bpSaved,0.25,na.rm=TRUE),
                bpSaved_q50=quantile(bpSaved,0.5,na.rm=TRUE),
                bpSaved_q75=quantile(bpSaved,0.75,na.rm=TRUE),
                bpFaced_q25=quantile(bpFaced,0.25,na.rm=TRUE),
                bpFaced_q50=quantile(bpFaced,0.5,na.rm=TRUE),
                bpFaced_q75=quantile(bpFaced,0.75,na.rm=TRUE),
                first_point_serve_win_pct_q25=quantile(first_point_serve_win_pct,0.25,na.rm=TRUE),
                first_point_serve_win_pct_q50=quantile(first_point_serve_win_pct,0.5,na.rm=TRUE),
                first_point_serve_win_pct_q75=quantile(first_point_serve_win_pct,0.75,na.rm=TRUE),
                second_point_serve_win_pct_q25=quantile(second_point_serve_win_pct,0.25,na.rm=TRUE),
                second_point_serve_win_pct_q50=quantile(second_point_serve_win_pct,0.5,na.rm=TRUE),
                second_point_serve_win_pct_q75=quantile(second_point_serve_win_pct,0.75,na.rm=TRUE),
                )
  } else {
    data%>%
      filter(between(tourney_date,ref_date1,ref_date))%>%
      select({{group_id}},surface,ace,df,svpt,SvGms,bpSaved,bpFaced,first_point_serve_win_pct,
             second_point_serve_win_pct)%>%
      group_by({{group_id}})%>%
      summarise(ace_q25=quantile(ace,0.25,na.rm=TRUE),
                ace_q50=quantile(ace,0.5,na.rm=TRUE),
                ace_q75=quantile(ace,0.75,na.rm=TRUE),
                df_q25=quantile(df,0.25,na.rm=TRUE),
                df_q50=quantile(df,0.5,na.rm=TRUE),
                df_q75=quantile(df,0.75,na.rm=TRUE),
                bpSaved_q25=quantile(bpSaved,0.25,na.rm=TRUE),
                bpSaved_q50=quantile(bpSaved,0.5,na.rm=TRUE),
                bpSaved_q75=quantile(bpSaved,0.75,na.rm=TRUE),
                bpFaced_q25=quantile(bpFaced,0.25,na.rm=TRUE),
                bpFaced_q50=quantile(bpFaced,0.5,na.rm=TRUE),
                bpFaced_q75=quantile(bpFaced,0.75,na.rm=TRUE),
                first_point_serve_win_pct_q25=quantile(first_point_serve_win_pct,0.25,na.rm=TRUE),
                first_point_serve_win_pct_q50=quantile(first_point_serve_win_pct,0.5,na.rm=TRUE),
                first_point_serve_win_pct_q75=quantile(first_point_serve_win_pct,0.75,na.rm=TRUE),
                second_point_serve_win_pct_q25=quantile(second_point_serve_win_pct,0.25,na.rm=TRUE),
                second_point_serve_win_pct_q50=quantile(second_point_serve_win_pct,0.5,na.rm=TRUE),
                second_point_serve_win_pct_q75=quantile(second_point_serve_win_pct,0.75,na.rm=TRUE),
      ) 
  }
  
  return(summary_df)               
  
}
test1 <- stats_iqr_func(match_stats1,name,"20251022",150,bySurface = TRUE)
test1[test$name=="Novak Djokovic",]