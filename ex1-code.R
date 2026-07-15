library(survey)
data(api)

apipop$instrat<- apipop$snum %in% apisrs$snum
dsrs2ph<-twophase(id=list(~1,~1), strata=list(NULL,~stype), 
                     subset=~instrat,data=apipop)
dcal1 <- calibrate(dsrs2ph, ~api99, phase=2)                     
dcal2 <- calibrate(dsrs2ph, ~api99+api.stu, phase=2)                     
dcal3 <- calibrate(dsrs2ph, ~stype, phase=2)                     
dcal4 <- calibrate(dsrs2ph, ~stype+api.stu+api99, phase=2)                     

svymean(~enroll+api00, dsrs2ph)         
svymean(~enroll+api00, dcal1)
svymean(~enroll+api00, dcal2)
svymean(~enroll+api00, dcal3)
svymean(~enroll+api00, dcal4)

                     
apipop$instrat<- apipop$snum %in% apiclus1$snum
dclus2ph<-twophase(id=list(~dnum,~dnum), strata=list(NULL,NULL), 
                     subset=~instrat,data=apipop)
                     
 dcal1 <- calibrate(dclus2ph, ~api99, phase=2)                     
dcal2 <- calibrate(dclus2ph, ~api99+api.stu, phase=2)                     
dcal3 <- calibrate(dclus2ph, ~stype, phase=2)                     
dcal4 <- calibrate(dclus2ph, ~stype+api.stu+api99, phase=2)                     

svymean(~enroll+api00, dclus2ph)         
svymean(~enroll+api00, dcal1)
svymean(~enroll+api00, dcal2)
svymean(~enroll+api00, dcal3)
svymean(~enroll+api00, dcal4)
                    
                    
tuyns<-read.table("~/TWOPHASE-SISCER/tuynsc.txt", col.names=c("case","age","agegp","tobgp","tobacco","logtb","beer","cider",'wine',
"aperitif","digestif","alcohol","logalc"))             
summary(tuyns)

tuyns$id<-1:NROW(tuyns)
tuyns$expand<-with(tuyns, ifelse(case==1,1,450))
pop<-tuyns[rep(tuyns$id,tuyns$expand),]
summary(pop)
pop$insubset<-!duplicated(pop$id)

d2pop<-twophase(id=list(~1,~1),strata=list(NULL,~case),
   subset=~insubset,data=pop)
svymean(~factor(agegp),d2pop)
svyhist(~beer, d2pop)          
svyboxplot(beer~factor(tobgp),d2pop)