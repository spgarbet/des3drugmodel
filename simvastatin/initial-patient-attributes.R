
assign_simvastatin_attributes <- function(traj, inputs)
{
  # Assign Genotype
  traj %>%
  branch(
    function() sample(1:3, 1, prob=c(1-inputs$simvastatin$vMedMetabolizer-inputs$simvastatin$vPoorMetabolizer, 
                                     inputs$simvastatin$vMedMetabolizer,
                                     inputs$simvastatin$vPoorMetabolizer)),
    continue=rep(TRUE,3),
    trajectory("TT") %>% set_attribute("aCVDgenotype", 1),
    trajectory("TC") %>% set_attribute("aCVDgenotype", 2),
    trajectory("CC") %>% set_attribute("aCVDgenotype", 3)
  ) %>%
  set_attribute("aCVDdrug", 0)       %>% # Not prescribed anything
  set_attribute("aGenotyped_CVD", 2) %>% # Initially not genotyped
  set_attribute("aStatinRxHx", 0)    %>% # No history of prescription
  set_attribute("aOrdered_test", 1)  %>%    # Did a physician order a test this time 1=NO, 2= YES
  set_attribute("aControlSim1", 0) %>% #control ordering test
  set_attribute("aControlSim2", 0) #control reading test
}
