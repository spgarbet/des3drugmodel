####### Costs & QALYs
#If run after the main run, no need to reload inputs & counters below.
# source('inputs.R')
# source('./simvastatin/counters.R')
# source("./clopidogrel/counters.R")
# source('./warfarin/counters.R')
# source("./main/counters.R")
# counters <- c(counters.gen, counters.dapt, counters.simvastatin, counters.warfarin)
# inputs$vN <- 200000

###########
#library(plyr)
#library(dplyr)
#library(tidyr)
#library(reshape2)
options("scipen"=100, "digits"=6)
annual_discount_rate <- 0.03

cont_discount_rate   <- -log(1-annual_discount_rate) # Yearly Time Scale
discounted_cost <- function(start_day, end_day, base_yearly_cost, rate = cont_discount_rate)
{
  #base_yearly_cost*(exp(-rate*start_day/365) - exp(-rate*end_day/365))/rate 
  
  base_yearly_cost / (exp(-rate*((end_day-start_day)/365.25)))
  
}

discount_value = function(value,ar=annual_discount_rate,A,B)
{
  r <- (1 + ar)^(1/365.25)-1
  (value/r)*(exp(-r*A)-exp(-r*B))
}

discount = function(value,ar=annual_discount_rate,A) value / (1+ar)^(A/365.25)

cost_cat <- data.frame(resource=c("panel_test","single_test",
                                  "clopidogrel","ticagrelor","prasugrel","aspirin","warfarin","simvastatin","alt_simvastatin",
                                  "revasc_event","revasc_pci","revasc_cabg","bleed_ext_maj_nonfatal","bleed_int_maj_nonfatal","bleed_min_nonfatal","bleed_fatal",
                                  "st_fatal","st_pci","st_cabg","mi_cabg","mi_pci","mi_med_manage","mild_myopathy","mod_myopathy","sev_myopathy","rahbdo_death",
                                  "cvd","cvd_death","out_of_range","in_range","MajorBleed_ICH","MajorBleed_ICH_Fatal","MajorBleed_GI","MajorBleed_GI_Fatal","MajorBleed_Other",
                                  "MajorBleed_Other_Fatal","MinorBleed","Stroke_MinorDeficit","Stroke_MajorDeficit","Stroke_Fatal","DVTPE_Fatal","DVT","PE", "cabg_bleed","dapt_stroke"),
                       cat=c(rep(1,2),rep(2,7),rep(3,36))
)

cost.qaly <- function(raw,inputs) 
{
  arrivals <- raw #%>%  mutate(name = paste0(name,"_",replication))
  #arrivals <- results %>%  mutate(name = paste0(name,"_",replication))
  # Make all resources a factor (this allows for null events to still get summaries)
  arrivals$resource <- factor(arrivals$resource, counters)
  
  # Adjust all event end times from the inputs$durations
  mapply(function(value, name){
    arrivals[arrivals$resource == name,]$end_time <<-arrivals[arrivals$resource == name,]$start_time + value
  }, value=inputs$durations, name=names(inputs$durations) )
  
  # Truncate to end of study or life
  end_times <- arrivals[arrivals$resource == 'time_in_model',]
  arrivals$end_time <- pmin(arrivals$end_time, 
                            plyr::join(arrivals[,c("name","end_time")], end_times[,c("name","end_time")], by="name", match="first")[,3])
  
  # Compute total activity times
  arrivals$activity_time <- arrivals$end_time - arrivals$start_time
  
  
  # Computes discounted rate of time
  arrivals$discounted_time <- discount_value(value=1,A=arrivals$start_time,B=arrivals$end_time)
  
  # Compute Event base cost map
  idx <- function(str) {as.numeric(factor(str, levels=levels(arrivals$resource)))}
  base_cost_map <- rep(0, nlevels(arrivals$resource))
  sapply(names(inputs$costs), FUN=function(name){
    base_cost_map[idx(name)] <<- inputs$costs[[name]]    
  })
  
  # Compute Disutility cost map
  base_disutility_map <- rep(0, nlevels(arrivals$resource))
  sapply(names(inputs$disutilities), FUN=function(name){
    base_disutility_map[idx(name)] <<- inputs$disutilities[[name]]    
  })
  names(base_disutility_map) = levels(arrivals$resource)
  
  arrivals$cost <- base_cost_map[as.numeric(arrivals$resource)]
  arrivals$discounted_cost <- ifelse(arrivals$activity_time>0,
                                     discount_value(value=arrivals$cost,A=arrivals$start_time,B=arrivals$end_time),
                                     discount(value = arrivals$cost,A=arrivals$start_time))
  
  arrivals$disutility = base_disutility_map[arrivals$resource]
  
  type <- data.frame(resource=names(inputs$type),type=unlist(inputs$type),row.names=NULL)
  qaly1 <- arrivals %>% group_by(name) %>% 
    arrange(start_time,desc(end_time)) %>% dplyr::mutate(utility = ifelse(row_number()==1,1,NA)) %>% filter(disutility>0 | utility>0) %>% #cross out events that have no impact on utility
    select(name,resource,start_time,end_time,activity_time,disutility) %>%
    merge(type,by="resource",all.x=TRUE) %>% #attach type of events: temp vs. permanent disutility
    dplyr::mutate(us=disutility,ue=disutility*(-type)) %>%  #us/ue stand for disutility at start/end time: temp event will add back disutility at end time
    select(name,start_time,end_time,us,ue,resource,type) %>% melt(id.vars=c("name","resource","us","ue","type")) %>% arrange(value) %>% #separate and spread start/end time
    dplyr::mutate(disutility=ifelse(variable=="start_time",us,ue)) %>% arrange(name,value,desc(variable)) %>% #match disutility with start/end time
    group_by(name) %>% mutate(time=lead(value)) %>% dplyr::mutate(dtime=ifelse(row_number()>1,time-lag(time),time)) %>% filter(!is.na(dtime)) %>% 
    filter(!(type==0 & dtime==0)) #For events that permanently reduce utility, this deletes double counts of the event and prevent double counting of disutility 
  #For temp event, we need to keep two records (start & end) in the datasets in order to adding back disutility at end time
  
  qaly2 <- qaly1 %>% mutate(cum1=ifelse(type==1 | is.na(type),0,disutility)) %>% #For permanent events (type==0), pass disutility to accumulate
    group_by(name) %>% mutate(temp_u=1-cumsum(cum1)) %>% 
    dplyr::mutate(cum2=ifelse(type==0 | is.na(type),0,disutility)) %>% mutate(utility=temp_u-cumsum(cum2)) %>% #For temp events, deduct accumulative disutility from temp_u
    filter(utility>0) #do not count negative/zero utility in qaly computation
  
  qaly.i <- qaly2 %>% select(name, value, time, utility) %>%
    dplyr::mutate(qaly.d = discount_value(utility,A=value,B=time)) #discounted QALY for each period of time 
  
  QALY = qaly.i %>% group_by(name) %>% dplyr::summarise(dQALY = sum(qaly.d)/365.25)
  COST = arrivals %>% filter(discounted_cost>0) %>% group_by(name,resource) %>% dplyr::summarise(dCOST = sum(discounted_cost)) %>% merge(cost_cat,by="resource",all.x = TRUE)
  c1 <- COST %>% group_by(name) %>% summarize(total=sum(dCOST))
  c2 <- COST %>% group_by(name) %>% filter(cat==1) %>% summarize(test=sum(dCOST))
  c3 <- COST %>% group_by(name) %>% filter(cat==2) %>% summarize(drug=sum(dCOST))
  c4 <- COST %>% group_by(name) %>% filter(cat==3) %>% summarize(event=sum(dCOST))
  out <- QALY %>% left_join(c1,by="name") %>% left_join(c2,by="name") %>% left_join(c3,by="name") %>% left_join(c4,by="name")
  return(out)
  #avgsum <- data.frame(    dQALY = sum(QALY$dQALY)/inputs$vN,
   #                        dCOST = sum(COST$dCOST)/inputs$vN,
    #                       dCOST.test = sum(COST$dCOST[COST$cat==1])/inputs$vN,
     #                      dCOST.drug = sum(COST$dCOST[COST$cat==2])/inputs$vN,
      #                     dCOST.event = sum(COST$dCOST[COST$cat==3])/inputs$vN) %>%
    #mutate(NMB10k=dQALY*10000-dCOST,NMB50k=dQALY*50000-dCOST,NMB100k=dQALY*100000-dCOST) #add NMB calcuation
  #return(avgsum)
}



#######ICER & C/E Plane
#library(ggplot2)
#library(ggrepel)
#library(dplyr)
#library(tidyr)

icer <- function(results) 
{
  x <- results %>% arrange(dCOST) %>% mutate(ICER = (lag(dCOST)-dCOST)/(lag(dQALY)-dQALY)) 

  #strong dominance (cross out strategies with a negative ICER)
  str.dom <- NULL
  if(any(x$ICER[-1]<0)==FALSE) {
    x$dominated[2:(nrow(x))] = 0
  } 
  while(any(x$ICER[-1]<0))
    {
      y <- x %>% filter(ICER<0)
      x <- x %>% filter(ICER>0 | is.na(ICER)) %>% arrange(dCOST) %>% mutate(ICER = (dCOST-lag(dCOST))/(dQALY-lag(dQALY)))
      x$dominated[2:(nrow(x))] = 0
      str.dom <- rbind.fill(str.dom, y)
    }
  if(is.null(str.dom)==FALSE) {str.dom <- str.dom %>% mutate(ICER=NA, dominated=1)}
  
  #extended dominance (cross out weakly dominated strategies until ICERs always increase with costs)
  ext.dom <- NULL
  while(any(order(x$ICER[-1])!=1:(nrow(x)-1))) 
  {
    r <- nrow(x)
    x$ext.dominated <- NA
    for (i in 2:(r-1)) {
      x$ext.dominated[i] = as.integer(x$ICER[i] > x$ICER[i+1])   
    }
    y <- x %>% filter(ext.dominated==1)
    x <- x %>% filter(ext.dominated==0 | is.na(ext.dominated)) %>% arrange(dCOST) %>% mutate(ICER = (dCOST-lag(dCOST))/(dQALY-lag(dQALY)))
    ext.dom <- rbind.fill(ext.dom, y)
  }
  if(is.null(ext.dom)==FALSE) {ext.dom <- ext.dom %>% mutate(ICER=NA, ext.dominated=1) }

  out = plyr::rbind.fill(x, str.dom, ext.dom) %>% arrange(dCOST)
  out
}

### need to adjust labels for strategies
ce_plane <- function(sum, title) {
  dt <- sum
  dt$line <- 1
  if (any(names(dt) %in% "dominated")==TRUE)
  {dt$line[dt$dominated==1] <- 0 }
  if (any(names(dt) %in% "ext.dominated")==TRUE)
  {dt$line[dt$ext.dominated==1] <- 0}
  if (any(names(dt) %in% "Y")==TRUE) 
  {dt <- dt %>% mutate(label=paste0(preemptive,"-",reactive,"-",X,"-",Y))} # With sensitivity analyses
  if (any(names(dt) %in% "Y")==FALSE) 
  {dt <- dt %>% mutate(label=paste0(preemptive,"-",reactive))}
  ggplot(dt, aes(x=dQALY,y=dCOST)) + geom_point() + 
    geom_line(data=dt[dt$line==1,]) + 
    geom_label_repel(aes(label=label)) +
    ggtitle(title)
}

