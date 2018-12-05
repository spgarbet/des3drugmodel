#adjust clock to redraw warfarin events
adj_clock <- function(traj)
{
  traj %>%
    set_attribute("aTimeToInRange",function(attrs) now(env) + days_till_in_range(attrs,inputs)) %>% # event: get in range
    set_attribute("aTimeTo90d",function(attrs) now(env) + days_till_90d(attrs,inputs)) %>% # event: reach 90 days
    set_attribute("aTimeToMajorBleed",function(attrs) now(env) + days_till_major_bleed(attrs,inputs)) %>% # event: major bleeding
    set_attribute("aTimeToMinorBleed",function(attrs) now(env) + days_till_minor_bleed(attrs,inputs)) %>% # event: minor bleeding
    set_attribute("aTimeToStroke",function(attrs) now(env) + days_till_stroke(attrs,inputs)) %>% # event: stroke
    set_attribute("aTimeToDVTPE",function(attrs) now(env) + days_till_DVTPE(attrs,inputs)) %>% # event: DVTPE
    set_attribute("aTimeTo6m",function(attrs) now(env) + days_till_6m(attrs,inputs)) # event: 6m stop warfarin for Non-AF patients
}  

#stop accumulating in/out range time, call it when pass 90 days or death
stop_monitor_INR <- function(traj)
{
  traj %>% 
    branch(
      function(attrs) attrs[["aInRange"]], 
      continue=rep(TRUE, 2),
      trajectory("In Range") %>% release("in_range"),
      trajectory("Out of Range") %>% release("out_of_range")
    ) %>%
    set_attribute("sINRMonitor",2)
}

stop_warfarin_treatment <- function(traj)
{
  traj %>% 
    branch(
      function(attrs) attrs[["aOnWarfarin"]],
      continue=rep(TRUE,2),
      trajectory("On") %>% 
        release("warfarin") %>% set_attribute("aOnWarfarin", 2), 
      trajectory("Off") %>% timeout(0)
    )
}

# Cleanup on termination function, called for any form of trajectory exiting
# This is needed for use in any event that results in a
# death. One must closeout "in use" counters, otherwise they won't
# appear in statistics
cleanup_warfarin <- function(traj)
{
  traj %>%
    stop_warfarin_treatment() %>%
    branch(
      function(attrs) attrs[["sINRMonitor"]], 
      continue=rep(TRUE, 2),
      trajectory("being monitored") %>% stop_monitor_INR(),
      trajectory("not") %>% timeout(0) #do nothing
    )
}
