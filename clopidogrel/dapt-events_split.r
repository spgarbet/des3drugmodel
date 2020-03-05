

####
## Assign Time to DAPT
days_till_dapt <- function(inputs)
{
  if (inputs$vDrugs$vClopidogrel)
  {
    aRandUnif = runif(n = 1, min = 0, max = 1)
    aLPEvent = get_attribute(env, 'aRRDAPT')
    inputs$clopidogrel$vDAPTScale * (-log(aRandUnif) * exp(-log(aLPEvent))) ^
      (1 / inputs$clopidogrel$vDAPTShape)
  } else
  {
    inputs$vHorizon * 365 + 1
  }
}



######
## Assign DAPT Medication
#for all genotyped patients through preemptive strategies (Panel or PREDICT), physician can choose to use or ignore the test results
#under reactive strategies, physician can also choose to order test for those not genotyped
clopidogrel_reactive_strategy <- function(traj, inputs)
{
  if (inputs$vReactive == "None")
  {
    traj %>% timeout(0)
  } else if (inputs$vReactive == "Single")
  {
    traj %>%
      branch(
        function()
          get_attribute(env, 'aGenotyped_CYP2C19'),
        continue = c(TRUE, TRUE),
        trajectory("have test results") %>%  timeout(0),
        trajectory("not have") %>%
          branch(
            function()
              get_attribute(env, 'aControlClo1'),
            continue = c(TRUE, TRUE),
            trajectory() %>% timeout(0),
            trajectory() %>% set_attribute("aGenotyped_CYP2C19", 1) %>% mark("single_test") %>% set_attribute("aOrdered_test", 2)
          )
      )
  } else if (inputs$vReactive == "Panel")
  {
    traj %>%
      branch(
        function()
          all_genotyped() + 1,
        continue = c(TRUE, TRUE),
        trajectory("not panel tested") %>%
          branch(
            function()
              get_attribute(env, 'aControlClo1'),
            continue = c(TRUE, TRUE),
            trajectory() %>% timeout(0),
            trajectory() %>% panel_test() %>% set_attribute("aOrdered_test", 2)
          ),
        trajectory("panel tested") %>% timeout(0)
      )
  } else
    stop("Unhandled Reactive Clopidogrel Strategy")
}

assign_DAPT_medication <- function(traj, inputs)
{
  traj %>%
    clopidogrel_reactive_strategy(inputs) %>%
    set_attribute("aDAPT.Rx", 1) %>%
    set_attribute("aDAPT.SecondLine",
                  function() {
                    if (inputs$clopidogrel$vDAPT.SecondLine == "Ticagrelor")       {
                      return(2)
                    } else
                      if (inputs$clopidogrel$vDAPT.SecondLine == "Prasugrel")        {
                        return(3)
                      }
                    # Something went very wrong
                    stop("Invalid Logic in assigning DAPT medication")
                  }) %>%
    branch(
      function() {
        # The genotyped patients are switched with some probability.
        if (get_attribute(env, 'aGenotyped_CYP2C19') == 1 &&
            get_attribute(env, 'aCYP2C19') == 1 &&
            (
              get_attribute(env, 'aOrdered_test') == 2 ||
              get_attribute(env, 'aControlClo2') == 2
            )) {
          return(sample(
            c(1, get_attribute(env, 'aDAPT.SecondLine')),
            1,
            prob = c(
              1 - inputs$clopidogrel$vProbabilityDAPTSwitch,
              inputs$clopidogrel$vProbabilityDAPTSwitch
            )
          ))
        } else if (get_attribute(env, 'aDAPT.Rx.Hx') != 0) {
          return(get_attribute(env, 'aDAPT.Rx.Hx'))
        }
        return(1) # Default is to Clopidogrel, hence return 1 if no Hx of alternative drug, or if not switched.
      },
      continue = rep(TRUE, 3),
      trajectory("sClopidogrel") %>%
        seize("clopidogrel") %>%
        set_attribute("aDAPT.Rx", 1) %>% set_attribute("aTest", inputs$clopidogrel$vProbabilityDAPTSwitch),
      trajectory("sTicagrelor")  %>%
        seize('ticagrelor') %>%
        set_attribute("aDAPT.Rx", 2) %>% set_attribute("aSwitchedDAPT", 1) %>% set_attribute("aTest", inputs$clopidogrel$vProbabilityDAPTSwitch) %>%
        mark("dapt_switched"),
      trajectory("sPrasugrel") %>%
        seize('prasugrel') %>%
        set_attribute("aDAPT.Rx", 3) %>% set_attribute("aSwitchedDAPT", 1) %>% set_attribute("aTest", inputs$clopidogrel$vProbabilityDAPTSwitch) %>%
        mark("dapt_switched")
    ) %>%
    
    # Set DAPT Rx History to Whatever Drug You Were Put On
    set_attribute("aDAPT.Rx.Hx", function()
      get_attribute(env, 'aDAPT.Rx')) %>%
    
    #Initiate Aspirin if Not Already Started
    branch(
      function()
        ifelse(
          get_attribute(env, 'aAspirin') == 2 &
            get_attribute(env, "aDAPT.Rx") %in% c(1, 2, 3) ,
          1,
          2
        ),
      continue = c(TRUE, TRUE),
      trajectory() %>% seize("aspirin") %>% set_attribute("aAspirin", 1),
      trajectory() %>% timeout(0)
    )
}

dapt <- function(traj, inputs)
{
  traj %>%
    set_attribute("aControlClo1", function()
      sample(
        1:2,
        1,
        prob = c(
          1 - inputs$clopidogrel$vProbabilityReactive,
          inputs$clopidogrel$vProbabilityReactive
        )
      )) %>%
    set_attribute("aControlClo2", function()
      sample(
        1:2,
        1,
        prob = c(
          1 - inputs$clopidogrel$vProbabilityRead,
          inputs$clopidogrel$vProbabilityRead
        )
      )) %>%
    branch(
      function()
        ifelse(
          get_attribute(env, 'aNumDAPT') < inputs$clopidogrel$vMaxDAPT,
          1,
          2
        ),
      continue = c(TRUE, TRUE),
      trajectory() %>%
        mark("dapt_start")  %>%
        set_attribute("aRRDAPT", inputs$clopidogrel$vRRRepeat.DAPT)  %>%
        set_attribute("aNumDAPT", function()
          get_attribute(env, 'aNumDAPT') + 1) %>%
        set_attribute("aOnDAPT", 1) %>%
        set_attribute("aDAPTRxHx", 1) %>%
        assign_DAPT_medication(inputs) %>%
        set_attribute("aOrdered_test", 1) %>%
        set_attribute("t_clo", function() now(env)) %>%
        
        ####
        ##
        # Downstream Events Go here
        ##
        ####
        
        # End of Therapy
        set_attribute("aDAPTEnded", function()
          now(env) + dapt_end_time(inputs)) %>%
        
        adj_dapt()
      ,
      trajectory() %>% set_attribute("aRRDAPT", epsilon)
    )
}

### downstream events redraw
adj_dapt <- function(traj)
{
  traj %>%
    # Stent Thrombosis
    set_attribute("aST", function()
      now(env) + time_to_ST(inputs)) %>%
    
    # Myocardial Infarction (Non-Fatal)
    set_attribute("aMI", function()
      now(env) + time_to_MI(inputs)) %>%
    
    # Revascularizaiton
    set_attribute("aRV", function()
      now(env) + time_to_RV(inputs)) %>%
    
    # Extracranial (TIMI major and nonfatal)
    set_attribute("aExtBleed", function()
      now(env) + time_to_ExtBleed(inputs)) %>%
    
    # Intracranial (TIMI major and nonfatal)
    set_attribute("aIntBleed", function()
      now(env) + time_to_IntBleed(inputs)) %>%
    
    # TIMI minor
    set_attribute("aTIMIMinor", function()
      now(env) + time_to_TIMIMinor(inputs)) %>%
    
    # Fatal Bleed
    set_attribute("aFatalBleed", function()
      now(env) + time_to_FatalBleed(inputs)) %>% 
  
  # CABG related bleed
  set_attribute("aCABGBleed", function()
    now(env) + time_to_CABGBleed(inputs))
}


####
##
# DAPT Treatment Course Ends
##
####
dapt_end_time <- function(inputs) {
  if (get_attribute(env, "aOnDAPT") == 1)
  {
    return(inputs$clopidogrel$vDAPT.Tx.Duration)
  } else
    return(inputs$vHorizon * 365 + 1)
}

dapt_end <- function(traj, inputs)
{
  traj %>%
    branch(
      function()
        ifelse(
          get_attribute(env, 'aDAPT.Rx') %in% c(1:3),
          get_attribute(env, 'aDAPT.Rx'),
          4
        ),
      continue = rep(TRUE, 4),
      trajectory() %>% release("clopidogrel") ,
      trajectory() %>% release("ticagrelor") ,
      trajectory() %>% release("prasugrel") ,
      trajectory() %>% timeout(0)
    ) %>%
    
    set_attribute("aDAPT.Rx", 4) %>%
    set_attribute("aOnDAPT", 2) %>%
    adj_dapt()
  
}

####
##
# Stent Thrombosis Events
##
####
# From Annals Paper:
# We assumed that all stent thromboses resulted in a myocardial infarction (MI), 
# and 20% were fatal (Appendix Table). Based on expert opinion and a review of the literature, 
# we assumed that 10% (range: 5-15%) of those who survived the episode of ST underwent emergent 
# CABG, and, as a simplifying assumption, the others underwent a repeat PCI with a drug-eluting stent.


time_to_ST <- function(inputs)
{
  if (get_attribute(env, "aOnDAPT") ==2)
    return(inputs$vHorizon * 365 + 1)
  else
  {
    # Relative Risk
    rr = get_attribute(env, "aRR.DAPT.ST")
    # Need to add in loss of function and gain of function RRs here too.
    if (get_attribute(env, 'aCYP2C19') == 1 &
        get_attribute(env, 'aDAPT.Rx') == 1)
      rr = inputs$clopidogrel$vRR.ST.LOF
    if (get_attribute(env, 'aDAPT.Rx') == 2)
      rr = inputs$clopidogrel$vRR.ST.Ticagrelor
    if (get_attribute(env, 'aDAPT.Rx') == 3)
      rr = inputs$clopidogrel$vRR.ST.Prasugrel
    if (get_attribute(env, 'aDAPT.Rx') == 4)
      rr = inputs$clopidogrel$vRR.ST.Aspirin
    
    # Baseline Risk
    rates = c(
      inputs$clopidogrel$vRiskST30,
      inputs$clopidogrel$vRiskST365,
      inputs$clopidogrel$vRiskSTgt365
    )
    days = c(30, 335, 365)
    
    # Convert To Exponential Rate from percentage
    rates2 = (-(log (1 - rates) * rr) / days)
    rates2 <- c(rates2, epsilon)
    
    # decide which time frame
    ageOfTherapy <- now(env) - get_attribute(env, "t_clo")
    times  <- c(0, 30, 365, 4 * 365) - ageOfTherapy
    
    if (ageOfTherapy >= 30 & ageOfTherapy < 365) {
      rates2 <- rates2[2:4]
      times  <- times[2:4]
    } else if (ageOfTherapy >= 365) {
      rates2 <- rates2[3:4]
      times <- times[3:4]
    }
    times[1] <- 0
    
    timeST = rpexp(1, rate = rates2, t = times)
    return(timeST)
    
  }
}

ST_event = function(traj, inputs)
{
  traj %>%
    branch(
      function()
        ifelse(get_attribute(env, "aOnDAPT") == 1, 1, 2),
      continue = c(TRUE, TRUE),
      trajectory()  %>%  mark("st_event") %>%
        # Case Fatatliy
        branch(
          function()
            sample(
              1:2,
              1,
              prob = c(
                inputs$clopidogrel$vSt.Case.Fatality,
                1 - inputs$clopidogrel$vSt.Case.Fatality
              )
            ),
          continue = c(FALSE, TRUE),
          trajectory() %>% mark("st_fatal") %>% cleanup_on_termination(),
          trajectory() %>%
            branch(
              function()
                sample(
                  1:2,
                  1,
                  prob = c(
                    inputs$clopidogrel$vPrCABG.ST,
                    1 - inputs$clopidogrel$vPrCABG.ST
                  )
                ),
              continue = c(TRUE, TRUE),
              # Discontinue DAPT Therapy if CABG, Continue With Aspirin
              trajectory() %>% mark("st_cabg") %>% cleanup_clopidogrel() %>% set_attribute("aOnDAPT", 2) %>% set_attribute("aDAPT.Rx", 4) %>%
                set_attribute("sCABG", 1) %>% set_attribute("aCABGBleed", function()
                  now(env) + time_to_CABGBleed(inputs)),
              # Reset Tx Duration to 1 year if PCI
              trajectory() %>%  mark("st_pci") %>%
                set_attribute("aRRDAPT", inputs$clopidogrel$vRRRepeat.DAPT)  %>%
                set_attribute("aNumDAPT", function()
                  get_attribute(env, 'aNumDAPT') + 1) %>%
                set_attribute("aOnDAPT", 1) %>% set_attribute("aDAPTEnded", function()
                  now(env) + dapt_end_time(inputs))
              
              #* TO DO: Add in Brief 7 Day Utility Decrement
            )
        ),
      trajectory() %>% timeout(0)
    )
}


####
##
# Myocardial Infarction Events
##
####

#Patients were also at risk for MI unrelated to stent thrombosis at a base-case rate of 3.5%/year
#in the clopidogrel arm (16,19,62). Eight percent of these patients underwent coronary artery bypass
#grafting (CABG) and 55% underwent a percutaneous coronary intervention (PCI) during the index
#hospitalization for the nonfatal MI (17,72). Patients who had one or more nonfatal MIs experienced
#a 30% increase in long-term cardiovascular mortality and recurrent MI (72).

time_to_MI = function(inputs)
{
  if (get_attribute(env, "aOnDAPT") ==2)
    return(inputs$vHorizon * 365 + 1)
  else
  {
    # Relative Risk
    rr = get_attribute(env, "aRR.DAPT.MI")
    if (get_attribute(env, 'aCYP2C19') == 1 &
        get_attribute(env, 'aDAPT.Rx') == 1)
      rr = inputs$clopidogrel$vRR.MI.LOF
    if (get_attribute(env, 'aDAPT.Rx') == 2)
      rr = inputs$clopidogrel$vRR.MI.Ticagrelor
    if (get_attribute(env, 'aDAPT.Rx') == 3)
      rr = inputs$clopidogrel$vRR.MI.Prasugrel
    if (get_attribute(env, 'aDAPT.Rx') == 4)
      rr = inputs$clopidogrel$vRR.MI.Aspirin
    
    # Baseline Risk
    rates = inputs$clopidogrel$vRiskMI
    days  = 365
    
    # Convert To Probability
    rates2 = (-(log (1 - rates) * rr) / days)
    
    timeMI = rpexp(1,
                   rate = c(rates2, epsilon),
                   t = c(0, 4 * 365))
    
    return(timeMI)
    
  }
}



MI_event = function(traj, inputs)
{
  traj %>%
    branch(
      function()
        ifelse(get_attribute(env, "aOnDAPT") == 1, 1, 2),
      continue = c(TRUE, TRUE),
      trajectory() %>% mark("mi_event") %>%
        branch(
          function()
            sample(
              1:3,
              1,
              prob = c(
                inputs$clopidogrel$vPrCABG.MI,
                inputs$clopidogrel$vPrPCI.MI,
                1 - inputs$clopidogrel$vPrCABG.MI - inputs$clopidogrel$vPrPCI.MI
              )
            ),
          continue = c(TRUE, TRUE, TRUE),
          
          # CABG
          trajectory() %>% mark("mi_cabg") %>% cleanup_clopidogrel() %>% set_attribute("aOnDAPT", 2) %>% set_attribute("aDAPT.Rx", 4) %>%
            set_attribute("sCABG", 1) %>% set_attribute("aCABGBleed", function()
              now(env) + time_to_CABGBleed(inputs)),
          #* TO DO: Add in Brief 14 Day Utility Decrement
          #* TO DO: Confirm DAPT Therapy shut off with CABG.
          
          # Repeat PCI
          trajectory() %>% mark("mi_pci") %>%
            set_attribute("aRRDAPT", inputs$clopidogrel$vRRRepeat.DAPT)  %>%
            set_attribute("aNumDAPT", function()
              get_attribute(env, 'aNumDAPT') + 1) %>%
            set_attribute("aOnDAPT", 1) %>% set_attribute("aDAPTEnded", function()
              now(env) + dapt_end_time(inputs)) ,
          #* TO DO: Add in Brief 7 Day Utility Decrement
          
          trajectory() %>%  mark("mi_med_manage")
        ),
      trajectory() %>% timeout(0)
    )
}



####
##
# Revascularization
##
####

# We estimated rates of repeat revascularization after PCI for acute coronary syndrome (ACS)
# from Medicare claims data (2001-2006; Appendix Table) (57, 58). Because we modeled revascularizations
# related to stent thrombosis and MI separately, we subtracted these from the total observed revascularizations
# to avoid double counting.

time_to_RV = function(inputs)
{
  if (get_attribute(env, "aOnDAPT") ==2)
    return(inputs$vHorizon * 365 + 1)
  else
  {
    # Relative Risk
    rr = get_attribute(env, "aRR.DAPT.RV")
    
    # Baseline Risk
    rates = c(inputs$clopidogrel$vRiskRV365,
              inputs$clopidogrel$vRiskRVgt365)
    days  = c(365, 365)
    
    # Convert To Probability
    rates2 = (-(log (1 - rates) * rr) / days)
    rates2 <- c(rates2, epsilon)
    
    # decide which time frame
    ageOfTherapy <- now(env) - get_attribute(env, "t_clo")
    times  <- c(0, 365, 4 * 365) - ageOfTherapy
    
    if (ageOfTherapy >= 365) {
      rates2 <- rates2[2:3]
      times <- times[2:3]
    }
    times[1] <- 0
    
    timeRV = rpexp(1, rate = rates2, t = times)
    
    return(timeRV)
    
  }
}



RV_event = function(traj, inputs)
{
  traj %>%
    branch(
      function()
        ifelse(get_attribute(env, "aOnDAPT") == 1, 1, 2),
      continue = c(TRUE, TRUE),
      trajectory() %>% mark("revasc_event") %>%
        branch(
          function()
            sample(
              1:2,
              1,
              prob = c(
                inputs$clopidogrel$vPrCABG.RV,
                1 - inputs$clopidogrel$vPrCABG.RV
              )
            ),
          continue = c(TRUE, TRUE),
          
          # CABG
          trajectory() %>% mark("revasc_cabg") %>% cleanup_clopidogrel() %>% set_attribute("aOnDAPT", 2) %>% set_attribute("aDAPT.Rx", 4) %>%
            set_attribute("sCABG", 1) %>% set_attribute("aCABGBleed", function()
              now(env) + time_to_CABGBleed(inputs)),
          #* TO DO: Add in Brief 14 Day Utility Decrement
          #* TO DO: Confirm DAPT Therapy shut off with CABG.
          
          # Repeat PCI
          trajectory() %>%  mark("revasc_pci") %>%
            set_attribute("aRRDAPT", inputs$clopidogrel$vRRRepeat.DAPT)  %>%
            set_attribute("aNumDAPT", function()
              get_attribute(env, 'aNumDAPT') + 1) %>%
            set_attribute("aOnDAPT", 1) %>% set_attribute("aDAPTEnded", function()
              now(env) + dapt_end_time(inputs))
          #* TO DO: Add in Brief 7 Day Utility Decrement
          
        ),
      trajectory() %>% timeout(0)
    )
}


####
##
# Bleeding Events
##
####

# Hemorrhage
# Based on Thrombolysis in Myocardial Infarction (TIMI) criteria (36), bleeds were classified
# as minor or major; major bleeds were further divided into extracranial and intracranial.
# Half of all non-CABG-related major bleeding observed in the first year occurred in the first month
# after PCI and the hazard was constant over the remaining duration of drug exposure. We also modeled
# CABG-related TIMI major bleeds and assumed that all excess CABG-related bleeds were extracranial;
# these were associated with increased costs and decreased quality-adjusted life years (QALYs), but
# did not increase perioperative mortality.

time_to_ExtBleed = function(inputs)
{
  if (get_attribute(env, "aOnDAPT") == 2)
    return(inputs$vHorizon * 365 + 1)
  else
  {
    # Relative Risk
    rr = get_attribute(env, "aRR.DAPT.ExtBleed")
    if (get_attribute(env, 'aCYP2C19') == 1 &
        get_attribute(env, 'aDAPT.Rx') == 1)
      rr = inputs$clopidogrel$vRR.Bleed.LOF
    if (get_attribute(env, 'aDAPT.Rx') == 2)
      rr = inputs$clopidogrel$vRR.ExtBleed.Ticagrelor
    if (get_attribute(env, 'aDAPT.Rx') == 3)
      rr = inputs$clopidogrel$vRR.ExtBleed.Prasugrel
    if (get_attribute(env, 'aDAPT.Rx') == 4)
      rr = inputs$clopidogrel$vRR.ExtBleed.Aspirin
    
    # Baseline Risk
    rates = inputs$clopidogrel$vRiskExtBleed
    days = c(365)
    
    # Convert To Probability
    rates2 = (-(log (1 - rates) * rr) / days)
    
    timeExtBleed = rpexp(1, rate = c(rates2, epsilon), t = c(0, days))
    
    return(timeExtBleed)
    
  }
}

ExtBleed_event = function(traj, inputs)
{
  traj %>%
    branch(
      function()
        ifelse(get_attribute(env, "aOnDAPT") == 1, 1, 2),
      continue = c(TRUE, TRUE),
      trajectory() %>% mark("bleed_event") %>% mark("bleed_ext_maj_nonfatal"),
      trajectory() %>% timeout(0)
    )
}


##
# Intracranial (TIMI Major and Non-Fatal)
##
time_to_IntBleed = function(inputs)
{
  if (get_attribute(env, "aOnDAPT") != 1)
    return(inputs$vHorizon * 365 + 1)
  else
  {
    # Relative Risk
    rr = get_attribute(env, "aRR.DAPT.IntBleed")
    if (get_attribute(env, 'aDAPT.Rx') == 2)
      rr = inputs$clopidogrel$vRR.IntBleed.Ticagrelor
    if (get_attribute(env, 'aDAPT.Rx') == 3)
      rr = inputs$clopidogrel$vRR.IntBleed.Prasugrel
    if (get_attribute(env, 'aDAPT.Rx') == 4)
      rr = inputs$clopidogrel$vRR.IntBleed.Aspirin
    
    # Baseline Risk
    rates = inputs$clopidogrel$vRiskIntBleed
    days = c(365)
    
    # Convert To Probability
    rates2 = (-(log (1 - rates) * rr) / days)
    
    timeIntBleed = rpexp(1, rate = c(rates2, epsilon), t = c(0, days))
    
    return(timeIntBleed)
    
  }
}

IntBleed_event = function(traj, inputs)
{
  traj %>%
    branch(
      function()
        ifelse(get_attribute(env, "aOnDAPT") == 1, 1, 2),
      continue = c(TRUE, TRUE),
      trajectory() %>% mark("bleed_event") %>% mark("bleed_int_maj_nonfatal"),
      trajectory() %>% timeout(0)
    )    # Make sure to add this to counters
}

##
# TIMI Minor Bleed
##
time_to_TIMIMinor = function(inputs)
{
  if (get_attribute(env, "aOnDAPT") == 2)
    return(inputs$vHorizon * 365 + 1)
  else
  {
    # Relative Risk
    rr = get_attribute(env, "aRR.DAPT.TIMIMinor")
    if (get_attribute(env, 'aDAPT.Rx')==2) rr = inputs$clopidogrel$vRR.TIMIMinor.Ticagrelor 
    if (get_attribute(env, 'aDAPT.Rx')==3) rr = inputs$clopidogrel$vRR.TIMIMinor.Prasugrel
    if (get_attribute(env, 'aDAPT.Rx')==4) rr = inputs$clopidogrel$vRR.TIMIMinor.Aspirin
    
    # Baseline Risk
    rates = inputs$clopidogrel$vRiskTIMIMinor
    days = c(365)
    
    # Convert To Probability
    rates2 = (-(log (1 - rates) * rr) / days)
    
    timeTIMIMinor = rpexp(1, rate = c(rates2, epsilon), t = c(0, days))
    
    return(timeTIMIMinor)
    
  }
}

TIMIMinor_event = function(traj, inputs)
{
  traj %>%
    branch(
      function()
        ifelse(get_attribute(env, "aOnDAPT") == 1, 1, 2),
      continue = c(TRUE, TRUE),
      trajectory() %>% mark("bleed_event") %>% mark("bleed_min_nonfatal"),
      trajectory() %>% timeout(0)
    )    # Make sure to add this to counters
}


##
# Fatal Bleed
##
time_to_FatalBleed = function(inputs)
{
  if (get_attribute(env, "aOnDAPT") == 2)
    return(inputs$vHorizon * 365 + 1)
  else
  {
    # Relative Risk
    rr = get_attribute(env, "aRR.DAPT.FatalBleed")
    if (get_attribute(env, 'aDAPT.Rx')==2) rr = inputs$clopidogrel$vRR.FatalBleed.Ticagrelor 
    if (get_attribute(env, 'aDAPT.Rx')==3) rr = inputs$clopidogrel$vRR.FatalBleed.Prasugrel
    if (get_attribute(env, 'aDAPT.Rx')==4) rr = inputs$clopidogrel$vRR.FatalBleed.Aspirin
    
    # Baseline Risk
    rates = inputs$clopidogrel$vRiskFatalBleed
    days = c(365)
    
    # Convert To Probability
    rates2 = (-(log (1 - rates) * rr) / days)
    
    timeFatalBleed =  rpexp(1, rate = c(rates2, epsilon), t = c(0, days))
    
    return(timeFatalBleed)
    
  }
}

FatalBleed_event = function(traj, inputs)
{
  traj %>%
    branch(
      function()
        1,
      continue = c(FALSE),
      # False is patient death
      trajectory("Fatal Bleed") %>% mark("bleed_event") %>% mark("bleed_fatal") %>% cleanup_on_termination()
    )
}


###############
######
# Added CABG-related TIMI major bleeding
##
time_to_CABGBleed = function(inputs)
{
  if (get_attribute(env, "sCABG")!=1) return(inputs$vHorizon * 365 + 1) else
  {
    # Relative Risk
    rr = 1
    if (get_attribute(env, 'aDAPT.Rx')==2) rr = inputs$clopidogrel$vRR.RiskCABGTIMImajor.Ticagrelor
    if (get_attribute(env, 'aDAPT.Rx')==3) rr = inputs$clopidogrel$vRR.RiskCABGTIMImajor.Prasugrel
    if (get_attribute(env, 'aDAPT.Rx')==4) rr = inputs$clopidogrel$vRR.RiskCABGTIMImajor.Aspirin
    
    # Baseline Risk
    rates = inputs$clopidogrel$vRiskCABGTIMImajor
    days = c(365)
    
    # Convert To Probability
    rates2 = (-(log (1 - rates) * rr) / days)
    
    timeCABGBleed =  rpexp(1, rate = c(rates2, epsilon), t = c(0, days))
    
    return(timeCABGBleed)
    
  }
}

CABGBleed_event = function(traj, inputs)
{
  traj %>%
    set_attribute("sCABG", 2) %>% #switch off until next CABG
    set_attribute("aCABGBleed",function() now(env) + time_to_CABGBleed(inputs)) %>% 
    mark("bleed_event") %>% mark("cabg_bleed")
}
