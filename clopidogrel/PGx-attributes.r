

####################################################################################################################
####################################################################################################################



# This is where the "preemptive" startegy is dealt with

predict_clopidogrel_draw <- function(traj, inputs)
{
  # First, Get the Probability That the Risk Score Returns a Value Above the Threshold
  # TO DO: Fix teh Sensitivity and Specificity to reflect what was done for PREDICT (e.g., 5 years)
  traj %>%
    set_attribute("aPrDAPT.Score.Eq1",  function(attrs)
      inputs$clopidogrel$vPREDICTsens * (attrs[['aTimeDAPTInitialized']] <= 365*inputs$vHorizon) + 
      (1 - inputs$clopidogrel$vPREDICTspec) * (1 - (attrs[["aTimeDAPTInitialized"]] < 365*inputs$vHorizon))) %>%
    set_attribute("aGenotyped_CYP2C19_PREDICT", function(attrs)
        sample(1:2, 1, prob = c(attrs[['aPrDAPT.Score.Eq1']], 1 - attrs[['aPrDAPT.Score.Eq1']])))
}

predict_clopidogrel <- function(traj, inputs)
{
  traj %>% set_attribute("aGenotyped_CYP2C19", function(attrs) attrs[["aGenotyped_CYP2C19_PREDICT"]])
}

