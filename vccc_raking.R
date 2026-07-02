
## mock.vccc impute

library(survey)
library(sleev)
data(mock.vccc)
mock.vccc$validated <- !is.na(mock.vccc$VL_val)

dvccc<-twophase(id=list(~1,~1), strata=list(NULL,NULL),
  subset=~validated, data=mock.vccc, method="simple")


## Imputation models fitted to phase 2 data
imp_ADE<-svyglm(ADE_val~ADE_unval+VL_unval+sqrt(CD4_unval)+Prior_ART+Sex+Age, design=dvccc, family=quasibinomial)
imp_CD4<-svyglm(CD4_val~ADE_unval+VL_unval+CD4_unval+sqrt(CD4_unval)+Prior_ART+Sex+Age, design=dvccc)

## phase 1 imputations
mock.vccc$ADE_hat<-predict(imp_ADE, newdata=mock.vccc, type="response")
mock.vccc$CD4_hat<-predict(imp_CD4, newdata=mock.vccc, type="response")

## phase-1 model

mphase1<-glm(ADE_hat~CD4_hat+Prior_ART, data=mock.vccc, family=quasibinomial)



h<- survey:::estfuns(mphase1)  ## or model.matrix(mphase1)*resid(mphase1,type="response")
mock.vccc<-cbind(mock.vccc,h=h)
dvccc_aug<-twophase(id=list(~1,~1), strata=list(NULL,NULL),
  subset=~validated, data=mock.vccc, method="simple")
dvccc_cal<-calibrate(dvccc_aug, formula=~`h.(Intercept)`+h.CD4_hat+h.Prior_ART,phase=2,calfun="raking")


## Now do it

coef(summary(svyglm(ADE_val~CD4_val+Prior_ART,design=dvccc_cal, family=quasibinomial)))
