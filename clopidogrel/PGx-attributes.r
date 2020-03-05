

####################################################################################################################
####################################################################################################################



# This is where the "preemptive" startegy is dealt with

predict_clopidogrel_draw <- function(traj, inputs)
{
  # First, Get the Probability That the Risk Score Returns a Value Above the Threshold
  # TO DO: Fix teh Sensitivity and Specificity to reflect what was done for PREDICT (e.g., 5 years)
  traj %>%
    set_attribute("aPrDAPT.Score.Eq1",  function()
      inputs$clopidogrel$vPREDICTsens * (get_attribute(env, 'aTimeDAPTInitialized') <= 365*inputs$vHorizon) + 
      (1 - inputs$clopidogrel$vPREDICTspec) * (1 - (get_attribute(env, "aTimeDAPTInitialized") < 365*inputs$vHorizon))) %>%
    set_attribute("aGenotyped_CYP2C19_PREDICT", function()
        sample(1:2, 1, prob = c(get_attribute(env, 'aPrDAPT.Score.Eq1'), 1 - get_attribute(env, 'aPrDAPT.Score.Eq1'))))
}

predict_clopidogrel <- function(traj, inputs)
{
  traj %>% set_attribute("aGenotyped_CYP2C19", function() get_attribute(env, "aGenotyped_CYP2C19_PREDICT"))
}

