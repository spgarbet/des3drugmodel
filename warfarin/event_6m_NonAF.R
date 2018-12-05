days_till_6m <- function(attrs, inputs)
{ 
  on = attrs[["aOnWarfarin"]]
  indi = attrs[["aWarfarinIndication"]]
  if(on==1 & indi==2)  {t2e <- 30*3 } # only Non-AF patients can trigger this event (redraw at 90 days)
  else {t2e <- inputs$vHorizon*365+2}
  return(t2e)
}  


reach_6m_NonAF <- function(traj, inputs)
{
  traj %>%
    release("warfarin") %>% set_attribute("aOnWarfarin", 2) 
}
