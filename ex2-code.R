library(survey)
library(survival)
data(nwtco)
dcchs<-twophase(id=list(~seqno,~seqno), strata=list(NULL,~rel),
         subset=~I(in.subcohort | rel), data=nwtco)
svycoxph(Surv(edrel,rel)~factor(stage)+factor(histol)+I(age/12), design=dcchs)


d_instit<-calibrate(dcchs,~instit,phase=2,calfun="raking")
svycoxph(Surv(edrel,rel)~factor(stage)+factor(histol)+I(age/12), design=d_instit)

d_rel<-calibrate(dcchs,~rel*edrel,phase=2,calfun="raking")
svycoxph(Surv(edrel,rel)~factor(stage)+factor(histol)+I(age/12), design=d_rel)


mphase1<-coxph(Surv(edrel,rel)~factor(stage)+factor(instit)+I(age/12),data=nwtco)
d_inf<-calibrate(dcchs, formula=mphase1,phase=2, calfun="raking")
svycoxph(Surv(edrel,rel)~factor(stage)+factor(histol)+I(age/12), design=d_inf)

## WRONG, but for educational purposes
nwtco$franken_hist<-with(nwtco, ifelse(in.subcohort | rel, histol, instit))
BADphase1<-coxph(Surv(edrel,rel)~factor(stage)+factor(franken_hist)+I(age/12),data=nwtco)
d_BAD<-calibrate(dcchs, formula=BADphase1,phase=2, calfun="raking")
svycoxph(Surv(edrel,rel)~factor(stage)+factor(histol)+I(age/12), design=d_BAD)
