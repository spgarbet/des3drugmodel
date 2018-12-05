
stop_simvastatin_treatment <- function(traj)
{
  traj %>% branch(
    function(attrs) attrs[["aCVDdrug"]]+1,
    continue=rep(TRUE,3),
    trajectory() %>% timeout(0),
    trajectory() %>% release("simvastatin"),
    trajectory() %>% release("alt_simvastatin")
  ) #%>%
  #set_attribute("aCVDdrug", 0)
}


# Cleanup on termination function, called for any form of trajectory exiting
# This is needed for use in any event that results in a
# death. One must closeout "in use" counters, otherwise they won't
# appear in statistics
cleanup_simvastatin <- function(traj)
{
  traj %>%
  stop_simvastatin_treatment()
}
