

assign_initial_clopidogrel_attributes <- function(traj,inputs) 
{
  traj %>%
    set_attribute("aAspirin",2) %>%
    set_attribute("aOnDAPT",2) %>%
    set_attribute("aGenotyped_CYP2C19",2) %>%
    set_attribute("aRR.DAPT.ST",1) %>%
    set_attribute("aRR.DAPT.MI",1) %>%
    set_attribute("aRR.DAPT.RV",1) %>%
    set_attribute("aRR.DAPT.ExtBleed",1) %>%
    set_attribute("aRR.DAPT.IntBleed",1) %>%
    set_attribute("aRR.DAPT.TIMIMinor",1) %>%
    set_attribute("aRR.DAPT.FatalBleed",1) %>% 
    set_attribute("aDAPT.Rx",5) %>%
    set_attribute("sCABG", 2) %>% #switch for cabg-related bleed
                #init drug time tracker
                set_attribute("t_clo",0)
}

assign_dapt_attributes <- function(traj,inputs=list()) 
{
  traj %>%
    set_attribute("aRRDAPT",1) %>%
    set_attribute("aNumDAPT",0) %>%
    set_attribute("aDAPT.Rx.Hx",0)
}

# These are the probabilitie of being a poor, rapid, uknown, or normal metabolizer. Just arranging into a vector to facilitate sampling below. 
assign_CYP2C19_status <- function(traj,inputs)
{
  vCYP2C19.Probs = c(inputs$clopidogrel$vCYP2C19.Poor,
                     inputs$clopidogrel$vCYP2C19.Rapid,
                     inputs$clopidogrel$vCYP2C19.Unknown,
                     1-inputs$clopidogrel$vCYP2C19.Poor-inputs$clopidogrel$vCYP2C19.Rapid-inputs$clopidogrel$vCYP2C19.Unknown )
  
  traj %>%
    branch(
    function() sample(1:4,1,prob=vCYP2C19.Probs),
    continue= rep(TRUE,4),
    trajectory() %>% set_attribute("aCYP2C19",1), # Poor (currently only one used)
    trajectory() %>% set_attribute("aCYP2C19",2), # Rapid
    trajectory() %>% set_attribute("aCYP2C19",3), # Unknown (heterozygous)
    trajectory() %>% set_attribute("aCYP2C19",4)  # Wildtype
  )
}

assign_clopidogrel_attributes <- function(traj, inputs)
{
  traj %>%
    assign_initial_clopidogrel_attributes(inputs) %>%
    assign_dapt_attributes(inputs) %>%
    assign_CYP2C19_status(inputs) %>%
    set_attribute("aOrdered_test", 1) %>%     # Did a physician order a test this time 1=NO, 2= YES
    set_attribute("aControlClo1", 0) %>% #control ordering test
    set_attribute("aControlClo2",0) #control reading test
}
