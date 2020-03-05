days_till_warfarin <- function(inputs)
{
  on = get_attribute(env, "aOnWarfarin")
  if (inputs$vDrugs$vWarfarin == TRUE & on==2)
    {t2e <- rweibull(1,inputs$warfarin$vshape_timetowarfarin,inputs$warfarin$vscale_timetowarfarin)} 
  else {t2e <- inputs$vHorizon*365+1}
  return(t2e)
} 

#for all genotyped patients through preemptive strategies (Panel or PREDICT), physician can choose to use or ignore the test results
#under reactive strategies, physician can also choose to order test for those not genotyped
warfarin_reactive_strategy <- function(traj, inputs)
{
  if(inputs$vReactive == "None") 
  {
    traj #
  } else if (inputs$vReactive == "Single")
  {
    traj %>%
      branch(
        function() get_attribute(env, 'aGenotyped_Warfarin'),
        continue=c(TRUE, TRUE),
        trajectory("have test results") %>%  timeout(0),
        trajectory("not have") %>% 
          branch(
            function() get_attribute(env, 'aControlWar1'),
            continue=c(TRUE,TRUE),
            trajectory() %>% timeout(0),
            trajectory() %>% set_attribute("aGenotyped_Warfarin", 1) %>% mark("single_test") 
          )
      )
  } else if (inputs$vReactive == "Panel")
  {
    traj %>%
      branch(
        function() all_genotyped()+1,
        continue=c(TRUE, TRUE),
        trajectory("not panel tested") %>% 
          branch(
            function() get_attribute(env, 'aControlWar1'),
            continue=c(TRUE,TRUE),
            trajectory() %>% timeout(0),
            trajectory() %>% panel_test() 
          ), # Not all genotyped, then do it
        trajectory("panel tested") %>% timeout(0)
      )
  } else stop("Unhandled Reactive Warfarin Strategy")
}

initial_INR <- function(x) 
{
  set.seed(x) 
  sample(inputs$warfarin$vINRvalue, 1, prob=inputs$warfarin$vINRfreq)
}

assign_indication <- function(x) 
{
  set.seed(x) 
  sample(1:2, 1, prob=c(inputs$warfarin$vpct_afib, 1-inputs$warfarin$vpct_afib))
}


INR_status <- function(x) {
  if (x>=2 & x<= 3) {return(1)} 
  else {return(2)}
}  

start_warfarin <- function(traj, inputs)
{
  traj %>%
    seize("warfarin") %>% 
    set_attribute("aTimeToStartWarfarin", inputs$vHorizon*365+1) %>% # cannot trigger "start warfarin" again
    set_attribute("sWarfarinEvents", 1) %>%  #switch on warfarin events
    set_attribute("aOnWarfarin", 1) %>% # start on warfarin 
    set_attribute("sINRMonitor", 1) %>% # start monitoring INR in 90 days
    
    #assign indication
    set_attribute("aWarfarinIndication", function() assign_indication(get_attribute(env, "aSeed"))) %>%
    
    #assign initial INR
    set_attribute("aINRInitial", function() initial_INR(get_attribute(env, "aSeed"))) %>%
    set_attribute("aINR", function() get_attribute(env, "aINRInitial")) %>%
    set_attribute("aInRange", function() INR_status(get_attribute(env, "aINRInitial"))) %>%
    
    #mark initial INR cat 
    branch(
      function() get_attribute(env, "aInRange"),
      continue=rep(TRUE, 2),
      trajectory("Initial In Range") %>% mark("Initial_InRange") %>%
        seize("in_range"),
      trajectory("Initial Out of Range") %>% mark("Initial_OutRange") %>%
        seize("out_of_range") 
    )
}

prescribe_warfarin <- function(traj, inputs)
{
  traj %>%
    set_attribute("aControlWar1",function() sample(1:2,1,prob=c(1- inputs$warfarin$vProbabilityReactive,  inputs$warfarin$vProbabilityReactive))) %>%
    set_attribute("aControlWar2",function() sample(1:2,1,prob=c(1- inputs$warfarin$vProbabilityRead,  inputs$warfarin$vProbabilityRead))) %>%
    mark("warfarin_start") %>%
    set_attribute("aWTestAvail", function() get_attribute(env, "aGenotyped_Warfarin")) %>% #before reactive strategy, know whether test available
    warfarin_reactive_strategy(inputs) %>%
    start_warfarin(inputs) %>%
    #downstream events
    adj_clock()
}


