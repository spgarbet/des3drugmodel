
####
## Assign Time to Simvastatin therapy
days_till_statin <- function(inputs) 
{
  if (inputs$vDrugs$vSimvastatin & get_attribute(env, 'aStatinRxHx')==0)
  {
    rweibull(1, inputs$simvastatin$vShape, inputs$simvastatin$vScale)
  } else 
  {
    inputs$vHorizon*365+1
  }
}

#for all genotyped patients through preemptive strategies (Panel or PREDICT), physician can choose to use or ignore the test results
#under reactive strategies, physician can also choose to order test for those not genotyped
statin_reactive_strategy <- function(traj, inputs)
{
  if(inputs$vReactive == "None") 
  {
    traj %>% timeout(0)
  } else if (inputs$vReactive == "Single")
  {
    traj %>%
      branch(
        function() get_attribute(env, 'aGenotyped_CVD'),
        continue=c(TRUE, TRUE),
        trajectory("have test results") %>%  timeout(0),
        trajectory("not have") %>% # Use probability of ordering test
          branch(
            function() get_attribute(env, 'aControlSim1'),
              continue=c(TRUE,TRUE),
              trajectory() %>% timeout(0),
              trajectory() %>% set_attribute("aGenotyped_CVD", 1) %>% mark("single_test") %>% set_attribute("aOrdered_test", 2)
              )
      )
  } else if (inputs$vReactive == "Panel")
  {
    traj %>%
    branch(
      function() all_genotyped()+1,
      continue=c(TRUE, TRUE),
      trajectory("not panel tested") %>% # Use probability of ordering test
        branch(
          function() get_attribute(env, 'aControlSim1'),          
          continue=c(TRUE,TRUE),
          trajectory() %>% timeout(0),
          trajectory() %>% panel_test() %>% set_attribute("aOrdered_test", 2)
        ),
      trajectory("panel tested") %>% timeout(0)
    )
  } else stop("Unhandled Reactive Statin Strategy")
}

assign_statin <- function(traj, inputs)
{
  traj %>%
    branch(
      function() min(get_attribute(env, "aStatinRxHx")+1, 2), # 0 = No history, 1+ prior history
      continue=c(TRUE,TRUE),
      trajectory() %>%
        mark("statin_any") %>% 
        set_attribute("aStatinRxHx", 1) %>% # Now they have a history of statin RX
        branch(
          function() 
          {
            # If not genotyped, return 1 for simvastatin
            if(get_attribute(env, 'aGenotyped_CVD') != 1) return(1)
            else if(get_attribute(env, 'aCVDgenotype' ) == 1 && 
                   (get_attribute(env, 'aOrdered_test') == 2 || get_attribute(env, 'aControlSim2')==2) )
            # If known to be wildtype (reactive test or use previous test), use simvastatin
            {
              return(1)
            }
            
            # Otherwise, run probability of prescribing alternate
            sample(1:2, 1, prob=c(1-inputs$simvastatin$vProbSimvastatinAlt, inputs$simvastatin$vProbSimvastatinAlt))
          },  
          continue = rep(TRUE,2),
          trajectory("Simvastatin") %>%
            seize("simvastatin") %>% 
            set_attribute("aCVDdrug", 1),
          trajectory("Alt. Simvastatin") %>%
            seize("alt_simvastatin") %>% 
            set_attribute("aCVDdrug", 2)
        ) %>% 
        branch(
          function()
          {
            if (get_attribute(env, 'aCVDdrug')!=1 & get_attribute(env, 'aGenotyped_CVD') == 1) return(1)
            return(2)
          },
          continue = c(TRUE,TRUE),
          trajectory() %>% mark("statin_switched_PGx"),
          trajectory() %>% timeout(0)
        ),
      trajectory() %>% timeout(0) # Due to prior history, don't do anything, as this has already been dealt with
    )
}

prescribe_statin <- function(traj, inputs)
{
  traj %>%
    set_attribute("aControlSim1",function() sample(1:2,1,prob=c(1- inputs$simvastatin$vProbabilityReactive,  inputs$simvastatin$vProbabilityReactive))) %>%
    set_attribute("aControlSim2",function() sample(1:2,1,prob=c(1- inputs$simvastatin$vProbabilityRead,  inputs$simvastatin$vProbabilityRead))) %>% #used in physician behavior
    statin_reactive_strategy(inputs) %>%
    assign_statin(inputs) %>%
    set_attribute("aOrdered_test", 1)
}
