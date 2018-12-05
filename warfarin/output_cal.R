warfarin_out <- function(raw,inputs) 
{
  li <- unique(raw$resource)
  if(any(li=="warfarin")) {
  t1 <- raw %>% filter(resource %in% c("in_range","out_of_range")) %>% 
    select(name, preemptive, reactive, resource, activity_time) %>%
    arrange(name) #in/out time among warfarin users
  t2 <- t1 %>% spread(resource, activity_time, fill=0) #long to wide
  r1 <- t2 %>% mutate(tot=out_of_range+in_range) %>% mutate(pctp=in_range/tot*100) %>% 
    select(name,preemptive,reactive,pctp)
  r2 <- t2 %>% mutate(test=out_of_range/3+in_range/7) %>%
    select(name,preemptive,reactive,test)
  warfarin_sup <- merge(r1,r2,by=c("name","preemptive","reactive"))
  return(warfarin_sup)
  }
  else{print("Warfarin model's off.")}
}



