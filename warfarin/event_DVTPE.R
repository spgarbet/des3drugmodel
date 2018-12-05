#t2e_rexp function defined in event_bleed
days_till_DVTPE <- function(attrs, inputs)
{ 
  switch = attrs[["sWarfarinEvents"]]
  ind = attrs[["aWarfarinIndication"]]
  if(switch==1 & ind==2)  #Non-AF
  { 
    x = attrs[["aINR"]]
    if(x<2)                return(t2e_rexp(inputs$warfarin$vNonAF_Risk_DVTPE_2,inputs$warfarin$vRRDVTPE_NonAF,inputs$warfarin$vTimeDurDVTPE))
    else                   return(t2e_rexp(inputs$warfarin$vNonAF_Risk_DVTPE_Over2,inputs$warfarin$vRRDVTPE_NonAF,inputs$warfarin$vTimeDurDVTPE))
  }
  else {return(inputs$vHorizon*365+1)} 
}

vDVTPE_freq <- c(inputs$warfarin$vR_DVT, inputs$warfarin$vR_PE, inputs$warfarin$vR_DVTPE_Fatal)

switch_drug <- function(x)
{
  if(x>inputs$warfarin$vPrSwitchDrug) y=1
  else                                y=2
  return(y)
}  

DVTPE_event <- function(traj, inputs)
{
  traj %>%
    branch(
      function() sample(1:3, 1, prob=vDVTPE_freq),
      continue=c(TRUE,TRUE,FALSE),
      trajectory("DVT") %>% set_attribute("aTypeofDVTPE", 1) %>%
        mark("DVT") %>% mark("DVTPE_event"),
      trajectory("PE") %>% set_attribute("aTypeofDVTPE", 2) %>%
        mark("PE") %>% mark("DVTPE_event"),
      trajectory("DVTPE_Fatal") %>% 
        set_attribute("aTypeofDVTPE", 3) %>% 
        mark("DVTPE_Fatal") %>% mark("DVTPE_event") %>% cleanup_on_termination()
    )
}

