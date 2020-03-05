t2e_rexp <- function(risk,rr,time) #used in bleed, stroke, DVTPE
{
  rate = -log(1-risk)*rr/time
  t2e <- rexp(1, rate)
  return(t2e)
}

###major bleed
days_till_major_bleed <- function(inputs)
{ 
  switch = get_attribute(env, "sWarfarinEvents")
  if(switch==1) {
    x = get_attribute(env, "aINR")
    if(get_attribute(env, "aWarfarinIndication")==1) #AF
    { 
      if(x<3)                return(t2e_rexp(inputs$warfarin$vAF_Risk_Major_Bleed_3,inputs$warfarin$vRRMajorBleed_AF,inputs$warfarin$vTimeDurBleed))
      else if(x>=3 & x<=4)   return(t2e_rexp(inputs$warfarin$vAF_Risk_Major_Bleed_3to4,inputs$warfarin$vRRMajorBleed_AF,inputs$warfarin$vTimeDurBleed))
      else                   return(t2e_rexp(inputs$warfarin$vAF_Risk_Major_Bleed_Over4,inputs$warfarin$vRRMajorBleed_AF,inputs$warfarin$vTimeDurBleed))
    }
    else #Non-AF
    { 
      if(x<3)                return(t2e_rexp(inputs$warfarin$vNonAF_Risk_Major_Bleed_3,inputs$warfarin$vRRMajorBleed_NonAF,inputs$warfarin$vTimeDurBleed))
      else if(x>=3 & x<=4)   return(t2e_rexp(inputs$warfarin$vNonAF_Risk_Major_Bleed_3to4,inputs$warfarin$vRRMajorBleed_NonAF,inputs$warfarin$vTimeDurBleed))
      else                   return(t2e_rexp(inputs$warfarin$vNonAF_Risk_Major_Bleed_Over4,inputs$warfarin$vRRMajorBleed_NonAF,inputs$warfarin$vTimeDurBleed))
    }
  }
  else {return(inputs$vHorizon*365+1)}
}

vMajorBleedfreq <- c(inputs$warfarin$vR_Bleed_ICH, inputs$warfarin$vR_Bleed_ICH_Fatal, inputs$warfarin$vR_Bleed_GI, 
                     inputs$warfarin$vR_Bleed_GI_Fatal, inputs$warfarin$vR_Bleed_Other, inputs$warfarin$vR_Bleed_Other_Fatal)

major_bleed_event <- function(traj, inputs)
{
  traj %>%
        branch(
          function() sample(1:6, 1, prob=vMajorBleedfreq),
          continue=rep(c(TRUE,FALSE),3),
          trajectory("ICH") %>% 
            set_attribute("aTypeofBleed",1) %>% mark("MajorBleed_ICH") %>% mark("MajorBleed_event"),
          trajectory("ICH_Fatal") %>% 
            set_attribute("aTypeofBleed",2) %>% mark("MajorBleed_ICH_Fatal") %>% mark("MajorBleed_event") %>% cleanup_on_termination(),
          trajectory("GI") %>%
            set_attribute("aTypeofBleed",3) %>% mark("MajorBleed_GI") %>% mark("MajorBleed_event"),
          trajectory("GI_Fatal") %>%
            set_attribute("aTypeofBleed",4) %>% mark("MajorBleed_GI_Fatal") %>% mark("MajorBleed_event") %>% cleanup_on_termination(),
          trajectory("Other") %>%
            set_attribute("aTypeofBleed",5) %>% mark("MajorBleed_Other") %>% mark("MajorBleed_event"),
          trajectory("Other_Fatal") %>%
            set_attribute("aTypeofBleed",6) %>% mark("MajorBleed_Other_Fatal") %>% mark("MajorBleed_event") %>% cleanup_on_termination()
        )
}  


###minor bleed
days_till_minor_bleed <- function(inputs)
{ 
  switch = get_attribute(env, "sWarfarinEvents")
  if(switch==1) {
    x = get_attribute(env, "aINR")
    if(get_attribute(env, "aWarfarinIndication")==1) #AF
    { 
      if(x<3)                return(t2e_rexp(inputs$warfarin$vAF_Risk_Minor_Bleed_3,inputs$warfarin$vRRMinorBleed_AF,inputs$warfarin$vTimeDurBleed))
      else if(x>=3 & x<=4)   return(t2e_rexp(inputs$warfarin$vAF_Risk_Minor_Bleed_3to4,inputs$warfarin$vRRMinorBleed_AF,inputs$warfarin$vTimeDurBleed))
      else                   return(t2e_rexp(inputs$warfarin$vAF_Risk_Minor_Bleed_Over4,inputs$warfarin$vRRMinorBleed_AF,inputs$warfarin$vTimeDurBleed))
    }
    else #Non-AF
    { 
      if(x<3)                return(t2e_rexp(inputs$warfarin$vNonAF_Risk_Minor_Bleed_3,inputs$warfarin$vRRMinorBleed_NonAF,inputs$warfarin$vTimeDurBleed))
      else if(x>=3 & x<=4)   return(t2e_rexp(inputs$warfarin$vNonAF_Risk_Minor_Bleed_3to4,inputs$warfarin$vRRMinorBleed_NonAF,inputs$warfarin$vTimeDurBleed))
      else                   return(t2e_rexp(inputs$warfarin$vNonAF_Risk_Minor_Bleed_Over4,inputs$warfarin$vRRMinorBleed_NonAF,inputs$warfarin$vTimeDurBleed))
    }
  }
  else {return(inputs$vHorizon*365+1)}
}

minor_bleed_event <- function(traj, inputs)
{
  traj %>%
  mark("MinorBleed")
}  


