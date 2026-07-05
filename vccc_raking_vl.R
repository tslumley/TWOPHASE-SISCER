
## mock.vccc impute

library(survey)
library(sleev)
data(mock.vccc)
mock.vccc$validated <- !is.na(mock.vccc$VL_val)

dvccc<-twophase(id=list(~1,~1), strata=list(NULL,NULL),
  subset=~validated, data=mock.vccc, method="simple")


## Imputation models fitted to phase 2 data
imp_lVL<-svyglm(log10(ADE_val)~ADE_unval+log10(VL_unval)+sqrt(CD4_unval)+Prior_ART+Sex+Age, design=dvccc)
imp_CD4<-svyglm(CD4_val~ADE_unval+VL_unval+CD4_unval+sqrt(CD4_unval)+Prior_ART+Sex+Age, design=dvccc)

## phase 1 imputations
mock.vccc$lVL_hat<-predict(imp_lVL, newdata=mock.vccc, type="response")
mock.vccc$CD4_hat<-predict(imp_CD4, newdata=mock.vccc, type="response")

## phase-1 model

mphase1<-glm(CD4_hat~lVL_hat+Sex, data=mock.vccc)



h<- survey:::estfuns(mphase1)  ## or model.matrix(mphase1)*resid(mphase1,type="response")
mock.vccc<-cbind(mock.vccc,h=h)
dvccc_aug<-twophase(id=list(~1,~1), strata=list(NULL,NULL),
  subset=~validated, data=mock.vccc, method="simple")
dvccc_cal<-calibrate(dvccc_aug, formula=~`h.(Intercept)`+h.lVL_hat+h.Sex,phase=2,calfun="raking")


## Now do it

coef(summary(svyglm(CD4_val~log10(VL_val)+Sex,design=dvccc_cal)))
