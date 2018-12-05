#event: reach 90 days, one-time, conditional on "start warfarin", switch: "sWarfarinEvents"
days_till_90d <- function(attrs, inputs)
{ 
  switch = attrs[["sWarfarinEvents"]]
  if(switch==1)  {t2e <- 90 }
  else           {t2e <- inputs$vHorizon*365+2}
  return(t2e)
}  


reach_90d <- function(traj, inputs)
{
  traj %>%
    mark("pass_90d") %>%
    set_attribute("sWarfarinEvents", 2) %>% #switch off warfarin events
    set_attribute("sINRMonitor", 2) %>% # stop monitoring 
    
    stop_monitor_INR() %>%
    adj_clock()
}




