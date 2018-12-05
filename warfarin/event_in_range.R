
days_till_in_range <- function(attrs, inputs)
{
  switch = attrs[["sWarfarinEvents"]]
  cat = attrs[["aInRange"]]
  PGx =  attrs[["aGenotyped_Warfarin"]]
  ava =  attrs[["aWTestAvail"]]
  read = attrs[["aControlWar2"]]
  
  if (switch==1 & cat==2) #must be on warfarin and currently out of range
  {
    if (PGx==1 & ava==1 & read==2)             {t2e <- rexp(1, inputs$warfarin$vMedianTimetoINR_PGx)} #genotype-guided dosing: no delay (test available at hand)
    else if (PGx==1 & ava==2)        {t2e <- rexp(1, inputs$warfarin$vMedianTimetoINR_PGx_delay)} #genotype-guided dosing: with 3-day delay (reactive test this time) 
    else                                      {t2e <- rexp(1, inputs$warfarin$vMedianTimetoINR)} #usual care (not tested at all, or physicians do not read)
  }
  else {t2e <- inputs$vHorizon*365+2} 
  return(t2e)
}


get_in_range <- function(traj,inputs)
{
  traj %>%
    release("out_of_range") %>%
    seize("in_range")%>%
    set_attribute("aINR", 2.5) %>% 
    set_attribute("aInRange", 1)
}



