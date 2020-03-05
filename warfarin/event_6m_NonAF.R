days_till_6m <- function(inputs)
{ 
  on = get_attribute(env, "aOnWarfarin")
  indi = get_attribute(env, "aWarfarinIndication")
  if(on==1 & indi==2)  {t2e <- 30*3 } # only Non-AF patients can trigger this event (redraw at 90 days)
  else {t2e <- inputs$vHorizon*365+1}
  return(t2e)
}  


reach_6m_NonAF <- function(traj, inputs)
{
  traj %>%
    release("warfarin") %>% set_attribute("aOnWarfarin", 2) 
}
