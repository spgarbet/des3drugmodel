library(data.table)
library(plyr)
library(dplyr)
library(tidyr)
library(reshape2)

#sum up event counts
for(dr in c("clo","sim","war")) {
  
  costi <- NULL
  for(i in 0:9) {
    load(file=paste0("/gpfs23/data/h_imph/gravesj/right/jonathan/cost_",dr,"_newS_",i,"_1y.rda"))
    out <- out %>%  mutate(name = paste0(name,"_b",i))
    if(is.null(costi)) { costi <- out } else  {costi <- rbind(costi, out)}
    rm(out)
  }

  sum1 <- costi %>% select(name,dQALY,total,test,drug,event,strategy) %>% replace(is.na(.), 0) %>% 
    group_by(strategy) %>% summarise_at(c("dQALY","total","test","drug","event"),mean) %>%
    arrange(strategy) %>% mutate(ICER = (total-total[1])/(dQALY-dQALY[1]))
  sum2 <- costi %>% select(-dQALY,-total,-test,-drug,-event,-aAgeInitial,-aGender) %>% replace(is.na(.), 0) %>% 
    group_by(strategy) %>% summarise_if(is.numeric,sum) 
  
  sum_costsi <- inner_join(sum1,sum2,by="strategy")
  rm(list=c("sum1","sum2","costi"))
  
  # costi <- costi %>% mutate(total=ifelse(is.na(total),0,total)) %>%
  #         mutate(test=ifelse(is.na(test),0,test)) %>%
  #         mutate(drug=ifelse(is.na(drug),0,drug)) %>%
  #         mutate(event=ifelse(is.na(event),0,event)) 
  # 
  # sum_costsi <- costi %>% group_by(strategy) %>%
  #         summarise(QALY=mean(dQALY),total=mean(total),test=mean(test),drug=mean(drug),event=mean(event)) %>%
  #         arrange(strategy) %>%
  #         mutate(ICER = (total-total[1])/(QALY-QALY[1]))
  
  save(sum_costsi,file=paste0("/gpfs23/data/h_imph/gravesj/right/jonathan/icer_",dr,"10_newS_1y.rda"))
  rm("sum_costsi")
}
  
###event counts
# for(dr in c("clo","sim","war")) {
#         
#         event <- NULL
#         for(i in 0:9) {
#                 load(file=paste0("/gpfs23/data/h_imph/gravesj/right/jonathan/sum_",dr,"_newS_",i,"_1y.rda"))
#                 if(is.null(event)) { event <- summ } else  {event <- rbind(event, summ)}
#         }
#         
#         event <- event %>% group_by(resource,preemptive,reactive) %>% summarise(N=sum(N))
#         save(event,file=paste0("/gpfs23/data/h_imph/gravesj/right/jonathan/summ_",dr,"10_newS_1y.rda"))
# }