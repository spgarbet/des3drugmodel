####
## Cleanup 
cleanup_clopidogrel <- function(traj)
{
  traj %>% 
  branch(
    function() ifelse(get_attribute(env, 'aDAPT.Rx') %in% c(1:3),get_attribute(env, 'aDAPT.Rx'),4),
    continue=rep(TRUE,4),
    trajectory() %>% release("clopidogrel") ,
    trajectory() %>% release("ticagrelor") ,
    trajectory() %>% release("prasugrel") ,
    trajectory() %>% timeout(0)
  ) 
}

cleanup_aspirin <- function(traj)
{
  traj %>% 
    branch(
      function() ifelse(get_attribute(env, "aAspirin")==1,1,2),
      continue = c(TRUE,TRUE),
      trajectory() %>% release("aspirin"),
      trajectory() %>% timeout(0)
    ) 
}
