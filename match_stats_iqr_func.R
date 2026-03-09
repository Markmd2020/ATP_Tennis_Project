#This function calculates IQR values for key match metrics for a time window
stats_iqr_func <- function(data,ref_date,window_days,bySurface=TRUE) {
  
  arguments <- as.list(match.call())
  ref_date <- ymd(ref_date)      # parse the input date
  ref_date1 <- ymd(ref_date - days(window_days))
  
  if (!inherits(data$tourney_date, "Date")) {
    stop(sprintf("Tourney Date Column must be of type Date, but is '%s'.",
                 class(data$tourney_date)[1]),
         call. = FALSE)
  }
  
  summary_df <- if(bySurface) { data%>%
      filter(between(tourney_date,ref_date1,ref_date))%>%
      dplyr::select(name,surface,ace,df,svpt,SvGms,bpSaved,bpFaced,first_point_serve_win_pct,
             second_point_serve_win_pct,first_ret_win_pct,second_ret_win_pct,bp_saved_pct,
             bp_conversion_pct)%>%
      group_by(name,surface)%>%
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
                first_ret_win_pct_pct_q25=quantile(first_ret_win_pct,0.25,na.rm=TRUE),
                first_ret_win_pct_win_pct_q50=quantile(first_ret_win_pct,0.5,na.rm=TRUE),
                first_ret_win_pct_win_pct_q75=quantile(first_ret_win_pct,0.75,na.rm=TRUE),
                second_ret_win_pct_q25=quantile(second_ret_win_pct,0.25,na.rm=TRUE),
                second_ret_win_pct_q50=quantile(second_ret_win_pct,0.5,na.rm=TRUE),
                second_ret_win_pct_q75=quantile(second_ret_win_pct,0.75,na.rm=TRUE),
                bp_saved_pct_q25=quantile(bp_saved_pct,0.25,na.rm=TRUE),
                bp_saved_pct_q50=quantile(bp_saved_pct,0.5,na.rm=TRUE),
                bp_saved_pct_q75=quantile(bp_saved_pct,0.75,na.rm=TRUE),
                bp_conversion_pct_q25=quantile(bp_conversion_pct,0.25,na.rm=TRUE),
                bp_conversion_pct_q50=quantile(bp_conversion_pct,0.5,na.rm=TRUE),
                bp_conversion_pct_q75=quantile(bp_conversion_pct,0.75,na.rm=TRUE)
                )
  } else {
    data%>%
      filter(between(tourney_date,ref_date1,ref_date))%>%
      dplyr::select(name,surface,ace,df,svpt,SvGms,bpSaved,bpFaced,first_point_serve_win_pct,
             second_point_serve_win_pct,first_ret_win_pct,second_ret_win_pct,bp_saved_pct,
             bp_conversion_pct)%>%
      group_by(name)%>%
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
                first_ret_win_pct_pct_q25=quantile(first_ret_win_pct,0.25,na.rm=TRUE),
                first_ret_win_pct_win_pct_q50=quantile(first_ret_win_pct,0.5,na.rm=TRUE),
                first_ret_win_pct_win_pct_q75=quantile(first_ret_win_pct,0.75,na.rm=TRUE),
                second_ret_win_pct_q25=quantile(second_ret_win_pct,0.25,na.rm=TRUE),
                second_ret_win_pct_q50=quantile(second_ret_win_pct,0.5,na.rm=TRUE),
                second_ret_win_pct_q75=quantile(second_ret_win_pct,0.75,na.rm=TRUE),
                bp_saved_pct_q25=quantile(bp_saved_pct,0.25,na.rm=TRUE),
                bp_saved_pct_q50=quantile(bp_saved_pct,0.5,na.rm=TRUE),
                bp_saved_pct_q75=quantile(bp_saved_pct,0.75,na.rm=TRUE),
                bp_conversion_pct_q25=quantile(bp_conversion_pct,0.25,na.rm=TRUE),
                bp_conversion_pct_q50=quantile(bp_conversion_pct,0.5,na.rm=TRUE),
                bp_conversion_pct_q75=quantile(bp_conversion_pct,0.75,na.rm=TRUE)
      ) 
  }
  
  return(summary_df)               
  
}
