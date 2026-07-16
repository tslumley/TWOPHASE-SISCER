library(survey)
library(survival)
data(nwtco)

nwtco$strata<-with(nwtco,interaction(rel,instit))

model_ph1<-coxph(Surv(edrel,rel)~factor(stage)+factor(instit)+I(age/12),data=nwtco)

h<-survey:::estfuns(model_ph1)
h_histol<-h[,4]

sigma_k<-with(nwtco, by(h_histol,strata,sd))
N_k<-with(nwtco, by(h_histol,strata,length))
n<-1154
round(n*sigma_k*N_k/sum(sigma_k*N_k))

nleft<-n-406
round(nleft*sigma_k[1:2]*N_k[1:2]/sum(sigma_k[1:2]*N_k[1:2]))




## just based on one-way error

table(nwtco$instit)

## we should have another roughly 100 unfavorable histology

nwtco$histol_hat<-with(nwtco, ifelse(instit==1, rbinom(NROW(nwtco),1,.03)+1,instit))
with(nwtco,table(instit,histol_hat))

model_ph1<-coxph(Surv(edrel,rel)~factor(stage)+factor(histol_hat)+I(age/12),data=nwtco)

h<-survey:::estfuns(model_ph1)
h_histol<-h[,4]

nwtco$strata2<-with(nwtco,interaction(rel,histol_hat))


sigma_k<-with(nwtco, by(h_histol,strata2,sd))
N_k<-with(nwtco, by(h_histol,strata2,length))
n<-1154
round(n*sigma_k*N_k/sum(sigma_k*N_k))


nleft<-n-337
round(nleft*sigma_k[-3]*N_k[-3]/sum(sigma_k[-3]*N_k[-3]))




## In fact, though, the extra unfavorable histology are probably relapses

with(nwtco, table(rel,instit))

nwtco$histol_hat2<-with(nwtco, ifelse(instit==1 & rel==1, rbinom(NROW(nwtco),1,100/415)+1,instit))
with(nwtco,table(histol_hat2,rel))

model_ph1<-coxph(Surv(edrel,rel)~factor(stage)+factor(histol_hat2)+I(age/12),data=nwtco)

h<-survey:::estfuns(model_ph1)
h_histol<-h[,4]

nwtco$strata3<-with(nwtco,interaction(rel,histol_hat2))

sigma_k<-with(nwtco, by(h_histol,strata3,sd))
N_k<-with(nwtco, by(h_histol,strata3,length))
n<-1154
round(n*sigma_k*N_k/sum(sigma_k*N_k))


nleft<-n-337
round(nleft*sigma_k[-3]*N_k[-3]/sum(sigma_k[-3]*N_k[-3]))



## With optimall

library(optimall)
nwtco$h<-h_histol
optimum_allocation(nwtco,strata="strata3",y="h",
  nsample=1154,method="Neyman")
## Neyman-Wright allocation
optimum_allocation(nwtco,strata="strata3",y="h",nsample=1154)


## more different than I'd expect 