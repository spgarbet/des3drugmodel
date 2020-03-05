
predict_simvastatin_draw <- function(traj, inputs)
{
  # First, Get the Probability That the Risk Score Returns a Value Above the Threshold
  # Note: the sensitivity and specificty are defined over a fixed horizon (I think three years), not the model horizon.
  # Need to confirm the true horizon from Jonatan and Yaping. 
  traj %>%
    set_attribute("aPrCVD.Score.Eq1",  function()
      inputs$simvastatin$vPREDICTsens * (get_attribute(env, 'aStartStatin') <= 365*inputs$vHorizon) + 
      (1 - inputs$simvastatin$vPREDICTspec) * (1 - (get_attribute(env, "aStartStatin") < 365*inputs$vHorizon))) %>%
    
    # All this routine needs to do is set the genotyped attribute correctly
    # The main loop code will pick this up and triggers a "panel_test" if needed.
    set_attribute("aGenotyped_CVD_PREDICT", function()
        sample(1:2, 1, prob = c(get_attribute(env, 'aPrCVD.Score.Eq1'), 1 - get_attribute(env, 'aPrCVD.Score.Eq1'))))
}

predict_simvastatin <- function(traj, inputs)
{
  traj %>% set_attribute("aGenotyped_CVD", function() get_attribute(env, "aGenotyped_CVD_PREDICT"))
}



