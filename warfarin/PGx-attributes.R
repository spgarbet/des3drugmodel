
predict_warfarin_draw <- function(traj, inputs)
{
  # First, Get the Probability That the Risk Score Returns a Value Above the Threshold
  # Note: the sensitivity and specificty are defined over a fixed horizon (I think three years), not the model horizon.
  # Need to confirm the true horizon from Jonatan and Yaping. 
  traj %>%
    set_attribute("aPrWarfarin.Score.Eq1",  function()
      inputs$warfarin$vPREDICTsens * (get_attribute(env, 'aTimeToStartWarfarin') <= 365*inputs$vHorizon) + 
        (1 - inputs$warfarin$vPREDICTspec) * (1 - (get_attribute(env, "aTimeToStartWarfarin") < 365*inputs$vHorizon))) %>%
    
    # All this routine needs to do is set the genotyped attribute correctly
    # The main loop code will pick this up and triggers a "panel_test" if needed.
    set_attribute("aGenotyped_Warfarin_PREDICT", function()
      sample(1:2, 1, prob = c(get_attribute(env, 'aPrWarfarin.Score.Eq1'), 1 - get_attribute(env, 'aPrWarfarin.Score.Eq1'))))
}


predict_warfarin <- function(traj, inputs)
{
  traj %>% set_attribute("aGenotyped_Warfarin", function() get_attribute(env, "aGenotyped_Warfarin_PREDICT"))
}



