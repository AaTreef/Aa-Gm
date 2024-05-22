##Package install + load##
install.packages(c("survival", "survminer"))
install.packages("rmarkdown")
library("survival")
library("survminer")

##Data entry: In each case the treatment type is force-fed unless denoted by "I" for injected. The doses are numbered in order of highest to lowest concentration:c=control 1=original dose, 2=10-fold dilution etc...##
##GFP for injected dose: time is given as dpi when an event occurs, event 1= systematic GFP expression##

igdat1<-data.frame(
  time=c(1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,3,3,7,7),
  event=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0)
)
igdat2<-data.frame(
  time=c(2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,7,7,7,7),
  event=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0)
)
igdat3<-data.frame(
  time=c(4,4,4,4,4,4,4,4,4,4,4,5,5,7,7,7,7,7,7,7,7,7,7,7),
  event=c(1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0)
)
igdat4<-data.frame(
  time=c(4,4,4,4,4,4,4,6,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7),
  event=c(1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
)
igdat5<-data.frame(
  time=c(5,5,5,5,5,5,5,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7),
  event=c(1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
)
igdatc<-data.frame(
  time=c(7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7),
  event=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
)

##GFP for forcefed dose : time is given as dpi when an event occurs, event 1= systematic GFP expression##

gdat1<-data.frame(
  time=c(1,1,1,1,1,1,2,2,2,2,3,3,3,3,3,3,4,8,8,8,8,8,8,8),
  event=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0)
)
gdat2<-data.frame(
  time=c(2,2,2,2,2,3,3,3,3,3,3,3,3,3,4,4,4,4,8,8,8,8,8,8),
  event=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0)
)
gdat3<-data.frame(
  time=c(4,4,4,5,5,5,5,5,5,5,5,5,5,8,8,8,8,8,8,8,8,8,8,8),
  event=c(1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0)
)
gdat4<-data.frame(
  time=c(5,5,5,6,6,6,6,6,6,6,6,8,8,8,8,8,8,8,8,8,8,8,8,8),
  event=c(1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0)
)
gdat5<-data.frame(
  time=c(7,7,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8),
  event=c(1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
)
gdatc<-data.frame(
  time=c(8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8),
  event=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
)

##survival data for force-fed dose: time is given as dpi when an event occurs, event 1= death##

dat1<-data.frame(
  time=c(8, 8, 8, 8, 8, 8, 8, 8, 8, 8,8,8,8,8,8,3,3,3,3,3,4,4,8,8),
  event = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1)
)
datc<-data.frame(
  time=c(8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,1,1),
  event=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1)
)
dat2<-data.frame(
  time=c(3,3,3,3,4,4,7,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8),
  event=c(1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
)
dat3<-data.frame(
  time=c(6,6,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8),
  event=c(1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
)
dat4<-data.frame(
  time=c(8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8),
  event=c(1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
)
dat5<-data.frame(
  time=c(8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8),
  event=c(1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
)

##survival data for injected dose: time is given as dpi when an event occurs, event 1= death##

idat1<-data.frame(
  time=c(3,3,3,3,3,3,3,4,5,5,5,6,6,6,7,7,7,7,7,7,7,7,7,7),
  event=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0)
)
idat2<-data.frame(
  time=c(3,3,3,5,5,6,6,6,6,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7),
  event=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0)
)
idat3<-data.frame(
  time=c(7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7),
  event=c(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
)
idat4<-data.frame(
  time=c(5,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7),
  event=c(1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
)
idat5<-data.frame(
  time=c(7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7),
  event=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
)
idatc<-data.frame(
  time=c(7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7),
  event=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
)

##melanisation data for force-fed : time is given as dpi when an event occurs, event 1= systematic melanisation##

mdat1<-data.frame(
  time=c(2,2,2,2,2,3,3,3,3,3,4,4,4,5,8,8,8,8,8,8,8,8,8,8),
  event=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0))

mdat2<-data.frame(
  time=c(2,2,2,2,3,3,3,4,4,4,4,4,5,5,8,8,8,8,8,8,8,8,8,8),
  event=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0)
)
mdat3<-data.frame(
  time=c(4,4,4,4,4,5,5,5,5,5,8,8,8,8,8,8,8,8,8,8,8,8,8,8),
  event=c(1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0)
)
mdat4<-data.frame(
  time=c(4,4,4,5,5,5,5,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8),
  event=c(1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
)
mdat5<-data.frame(
  time=c(5,5,5,5,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8),
  event=c(1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
)
mdatc<-data.frame(
  time=c(4,4,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8),
  event=c(1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
)

#melanisation data for injected : time is given as dpi when an event occurs, event 1= systematic melanisation#

imdat1<-data.frame(
  time=c(1,1,2,2,2,2,2,2,2,2,3,4,4,4,4,6,7,7,7,7,7,7,7,7),
  event=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0)
)
imdat2<-data.frame(
  time=c(2,2,2,2,2,2,2,2,2,2,3,4,4,4,4,7,7,7,7,7,7,7,7,7),
  event=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0)
)
imdat3<-data.frame(
  time=c(5,6,6,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7),
  event=c(1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
)
imdat4<-data.frame(
  time=c(1,4,5,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7),
  event=c(1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
)
imdat5<-data.frame(
  time=c(7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7),
  event=c(1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
)
imdatc<-data.frame(
  time=c(7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7),
  event=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
)

#pupation data for forcefed: time is given as dpi when an event occurs, event 1= succesful pupation#

pdat1<-data.frame(
  time=c(6,7,7,7,7,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8),
  event=c(1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
)
pdat2<-data.frame(
  time=c(6,6,6,6,6,7,7,7,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8),
  event=c(1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
)
pdat3<-data.frame(
  time=c(3,5,5,6,6,7,7,7,7,7,7,8,8,8,8,8,8,8,8,8,8,8,8,8),
  event=c(1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0)
)
pdat4<-data.frame(
  time=c(5,5,6,6,7,7,7,7,7,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8),
  event=c(1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
)
pdat5<-data.frame(
  time=c(6,6,6,6,6,6,7,7,7,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8),
  event=c(1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0)
)
pdatc<-data.frame(
  time=c(5,5,6,6,6,6,6,6,6,7,7,7,7,8,8,8,8,8,8,8,8,8,8,8),
  event=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0)
)

#pupation data for injected: time is given as dpi when an event occurs, event 1= successful pupation#

ipdat1<-data.frame(
  time=c(7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7),
  event=c(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
)
ipdat2<-data.frame(
  time=c(7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7),
  event=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
)
ipdat3<-data.frame(
  time=c(5,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7),
  event=c(1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
)
ipdat4<-data.frame(
  time=c(6,6,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7),
  event=c(1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
)
ipdat5<-data.frame(
  time=c(5,5,5,6,6,6,6,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7),
  event=c(1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0)
)
ipdatc<-data.frame(
  time=c(5,6,6,6,6,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7),
  event=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0)
)

##compiled data table of time-to-outcome for each treatment and create survival plots for each treatment##

##GFP-injected##

CompGInj<-rbind((cbind(igdatc,type='control dose')),
                (cbind(igdat1,type='original dose')),
                (cbind(igdat2,type='10 fold dilution')),
                (cbind(igdat3,type='100 fold dilution')),
                (cbind(igdat4, type='1,000 fold dilution')),
                (cbind(igdat5,type='10,000 fold dilution')))
sf.CompGInj<-survfit(Surv(time,event)~type,data=CompGInj)
ggsurvplot(sf.CompGInj,data=CompGInj,conf.int = TRUE,ylim=c(0.0,1),break.x.by=1,xlim=c(0,7),ylab="Infection Probability",xlab="Days Post Treatment",fun="event")

##GFP-force fed##

CompGFf<-rbind((cbind(gdatc,type='control dose')),
               (cbind(gdat1,type='original dose')),
               (cbind(gdat2,type='10 fold dilution')),
               (cbind(gdat3,type='100 fold dilution')),
               (cbind(gdat4, type='1,000 fold dilution')),
               (cbind(gdat5,type='10,000 fold dilution')))
sf.CompGFf<-survfit(Surv(time,event)~type,data=CompGFf)
ggsurvplot(sf.CompGFf,data=CompGFf,conf.int = TRUE,ylim=c(0.0,1),break.x.by=1,xlim=c(0,8),ylab="Infection Probability",xlab="Days Post Treatment",fun="event")

##survival- injected##

CompInj<-rbind((cbind(idatc,type='control dose')),
               (cbind(idat1,type='original dose')),
               (cbind(idat2,type='10 fold dilution')),
               (cbind(idat3,type='100 fold dilution')),
               (cbind(idat4, type='1,000 fold dilution')),
               (cbind(idat5,type='10,000 fold dilution')))
sf.CompInj<-survfit(Surv(time,event)~type,data=CompInj)
ggsurvplot(sf.CompInj,data=CompInj,conf.int = TRUE,ylab="Survival Probability", xlab="Days Post Treatment",ylim=c(0.0,1),xlim=c(0,7),break.x.by=1)

##survival-force fed##

CompFf<-rbind((cbind(datc,type='control dose')),
              (cbind(dat1,type='original dose')),
              (cbind(dat2,type='10 fold dilution')),
              (cbind(dat3,type='100 fold dilution')),
              (cbind(dat4, type='1,000 fold dilution')),
              (cbind(dat5, type= '10,000 fold dilution')))
sf.CompFf<-survfit(Surv(time,event)~type,data=CompFf)
ggsurvplot(sf.CompFf,data=CompFf,conf.int=TRUE,ylab="Survival Probability", xlab="Days Post Treatment",ylim=c(0.0,1),xlim=c(0,8),break.x.by=1)

##melanisation-injected##

CompMInj<-rbind((cbind(imdatc,type='control dose')),
              (cbind(imdat1,type='original dose')),
              (cbind(imdat2,type='10 fold dilution')),
              (cbind(imdat3,type='100 fold dilution')),
              (cbind(imdat4, type='1,000 fold dilution')),
              (cbind(imdat5, type= '10,000 fold dilution')))
sf.CompMInj<-survfit(Surv(time,event)~type,data=CompMInj)
ggsurvplot(sf.CompMInj,data=CompMInj,conf.int=TRUE,ylab="Melanisation Rate", xlab="Days Post Treatment",ylim=c(0.0,1),xlim=c(0,7),break.x.by=1,fun="event")


##melanisation-forcefed##

CompMFf<-rbind((cbind(mdatc,type='control dose')),
              (cbind(mdat1,type='original dose')),
              (cbind(mdat2,type='10 fold dilution')),
              (cbind(mdat3,type='100 fold dilution')),
              (cbind(mdat4, type='1,000 fold dilution')),
              (cbind(mdat5, type= '10,000 fold dilution')))
sf.CompMFf<-survfit(Surv(time,event)~type,data=CompMFf)
ggsurvplot(sf.CompMFf,data=CompMFf,conf.int=TRUE,ylab="Melanisation Rate", xlab="Days Post Treatment",ylim=c(0.0,1),xlim=c(0,8),break.x.by=1,fun="event")

##pupation-injected##

CompPInj<-rbind((cbind(ipdatc,type='control dose')),
                (cbind(ipdat1,type='original dose')),
                (cbind(ipdat2,type='10 fold dilution')),
                (cbind(ipdat3,type='100 fold dilution')),
                (cbind(ipdat4, type='1,000 fold dilution')),
                (cbind(ipdat5, type= '10,000 fold dilution')))
sf.CompPInj<-survfit(Surv(time,event)~type,data=CompPInj)
ggsurvplot(sf.CompPInj,data=CompPInj,conf.int=TRUE,ylab="Pupation Rate", xlab="Days Post Treatment",ylim=c(0.0,1),xlim=c(0,7),break.x.by=1,fun="event")

##pupation-forcefed##

CompPFf<-rbind((cbind(pdatc,type='control dose')),
                (cbind(pdat1,type='original dose')),
                (cbind(pdat2,type='10 fold dilution')),
                (cbind(pdat3,type='100 fold dilution')),
                (cbind(pdat4, type='1,000 fold dilution')),
                (cbind(pdat5, type= '10,000 fold dilution')))
sf.CompPFf<-survfit(Surv(time,event)~type,data=CompPFf)
ggsurvplot(sf.CompPFf,data=CompPFf,conf.int=TRUE,ylab="Pupation Rate", xlab="Days Post Treatment",ylim=c(0.0,1),xlim=c(0,8),break.x.by=1,fun="event")


##Overall heterogeneity analysis by likelyhood ratio test using cox proportional hazard model##

##GFP-injected

coxCompGInj<-coxph(Surv(time,event)~type,data=CompGInj)
summary(coxCompGInj)

##GFP-forcefed

coxCompGFf<-coxph(Surv(time,event)~type,data=CompGFf)
summary(coxCompGFf)

##survival-injected

coxCompInj<-coxph(Surv(time,event)~type,data=CompInj)
summary(coxCompInj)

##survival-forcefed

coxCompFf<-coxph(Surv(time,event)~type,data=CompFf)
summary(coxCompFf)

##melanisation-injected

coxCompMInj<-coxph(Surv(time,event)~type,data=CompMInj)
summary(coxCompMInj)

##melanisation-forcefed

coxCompMFf<-coxph(Surv(time,event)~type,data=CompMFf)
summary(coxCompMFf)

##pupation-injection

coxCompPInj<-coxph(Surv(time,event)~type,data=CompPInj)
summary(coxCompPInj)

##pupation-forcefed

coxCompPFf<-coxph(Surv(time,event)~type,data=CompPFf)
summary(coxCompPFf)

##Pair-wise heterogeneity and hazard proportion analysis
##for CompFf data, 6 categories so 6 pairs (and bonferroni corrected a would be 0.05/12=0.004)
##the data is split into individual doses##

FfC<-CompFf[CompFf$type=="control dose",]
FfO<-CompFf[CompFf$type=="original dose",]
Ff10<-CompFf[CompFf$type=="10 fold dilution",]
Ff100<-CompFf[CompFf$type=="100 fold dilution",]
Ff1000<-CompFf[CompFf$type=="1,000 fold dilution",]
Ff10000<-CompFf[CompFf$type=="10,000 fold dilution",]

##merge by pair

FfCvO<-rbind(FfC,FfO)
FfCv10<-rbind(FfC,Ff10)
FfCv100<-rbind(FfC,Ff100)
FfCv1000<-rbind(FfC,Ff1000)
FfCv10000<-rbind(FfC,Ff10000)
Ff10000vO<-rbind(Ff10000,FfO)
Ff10000v10<-rbind(Ff10000,Ff10)
Ff10000v100<-rbind(Ff10000,Ff100)
Ff10000v1000<-rbind(Ff10000,Ff1000)
Ff1000vO<-rbind(Ff1000,FfO)
Ff1000v10<-rbind(Ff1000,Ff10)
Ff1000v100<-rbind(Ff1000,Ff100)
Ff100vO<-rbind(Ff100,FfO)
Ff100v10<-rbind(Ff100,Ff10)
Ff10vO<-rbind(Ff10,FfO)

##coxph individual pairs to check heterogeneity (likelihood ratio) and hazard proportion (exp coef)

coxFf10000v1000<-coxph(Surv(time,event)~type,data=c10000v1000)
summary(coxFF10000v1000)

coxFfCvO<-coxph(Surv(time,event)~type,data=FfCvO)
summary(coxFfCvO)
coxFfCv10<-coxph(Surv(time,event)~type,data=FfCv10)
summary(coxFfCv10)
coxFfCv100<-coxph(Surv(time,event)~type,data=FfCv100)
summary(coxFfCv100)
coxFfCv1000<-coxph(Surv(time,event)~type,data=FfCv1000)
summary(coxFfCv1000)
coxFfCv10000<-coxph(Surv(time,event)~type,data=FfCv10000)
summary(coxFfCv10000)
coxFf10000vO<-coxph(Surv(time,event)~type,data=Ff10000vO)
summary(coxFf10000vO)
coxFf10000v10<-coxph(Surv(time,event)~type,data=Ff10000v10)
summary(coxFf10000v10)
coxFf10000v100<-coxph(Surv(time,event)~type,data=Ff10000v100)
summary(coxFf10000v100)
coxFf10000v1000<-coxph(Surv(time,event)~type,data=Ff10000v1000)
summary(coxFf10000v1000)
coxFf1000vO<-coxph(Surv(time,event)~type,data=Ff1000vO)
summary(coxFf1000vO)
coxFf1000v10<-coxph(Surv(time,event)~type,data=Ff1000v10)
summary(coxFf1000v10)
coxFf1000v100<-coxph(Surv(time,event)~type,data=Ff1000v100)
summary(coxFf1000v100)
coxFf100vO<-coxph(Surv(time,event)~type,data=Ff100vO)
summary(coxFf100vO)
coxFf100v10<-coxph(Surv(time,event)~type,data=Ff100v10)
summary(coxFf100v10)
coxFf10vO<-coxph(Surv(time,event)~type,data=Ff10vO)
summary(coxFf10vO)



##Pair-wise heterogeneity and hazard proportion analysis##
##for CompGFf data, 6 categories so 6 pairs (and bonferroni corrected a would be 0.05/12=0.004)
##split data into individual doses

GFfC<-CompGFf[CompGFf$type=="control dose",]
GFfO<-CompGFf[CompGFf$type=="original dose",]
GFf10<-CompGFf[CompGFf$type=="10 fold dilution",]
GFf100<-CompGFf[CompGFf$type=="100 fold dilution",]
GFf1000<-CompGFf[CompGFf$type=="1,000 fold dilution",]
GFf10000<-CompGFf[CompGFf$type=="10,000 fold dilution",]
##merge by pair
GFfCvO<-rbind(GFfC,GFfO)
GFfCv10<-rbind(GFfC,GFf10)
GFfCv100<-rbind(GFfC,GFf100)
GFfCv1000<-rbind(GFfC,GFf1000)
GFfCv10000<-rbind(GFfC,GFf10000)
GFf10000vO<-rbind(GFf10000,GFfO)
GFf10000v10<-rbind(GFf10000,GFf10)
GFf10000v100<-rbind(GFf10000,GFf100)
GFf10000v1000<-rbind(GFf10000,GFf1000)
GFf1000vO<-rbind(GFf1000,GFfO)
GFf1000v10<-rbind(GFf1000,GFf10)
GFf1000v100<-rbind(GFf1000,GFf100)
GFf100vO<-rbind(GFf100,GFfO)
GFf100v10<-rbind(GFf100,GFf10)
GFf10vO<-rbind(GFf10,GFfO)

##coxph individual pairs to check heterogeneity (likelihood ratio) and hazard proportion (exp coef)

coxFf10000v1000<-coxph(Surv(time,event)~type,data=c10000v1000)
summary(coxFF10000v1000)

coxGFfCvO<-coxph(Surv(time,event)~type,data=GFfCvO)
summary(coxGFfCvO)
coxGFfCv10<-coxph(Surv(time,event)~type,data=GFfCv10)
summary(coxGFfCv10)
coxGFfCv100<-coxph(Surv(time,event)~type,data=GFfCv100)
summary(coxGFfCv100)
coxGFfCv1000<-coxph(Surv(time,event)~type,data=GFfCv1000)
summary(coxGFfCv1000)
coxGFfCv10000<-coxph(Surv(time,event)~type,data=GFfCv10000)
summary(coxGFfCv10000)
coxGFf10000vO<-coxph(Surv(time,event)~type,data=GFf10000vO)
summary(coxGFf10000vO)
coxGFf10000v10<-coxph(Surv(time,event)~type,data=GFf10000v10)
summary(coxGFf10000v10)
coxGFf10000v100<-coxph(Surv(time,event)~type,data=GFf10000v100)
summary(coxGFf10000v100)
coxGFf10000v1000<-coxph(Surv(time,event)~type,data=GFf10000v1000)
summary(coxGFf10000v1000)
coxGFf1000vO<-coxph(Surv(time,event)~type,data=GFf1000vO)
summary(coxGFf1000vO)
coxGFf1000v10<-coxph(Surv(time,event)~type,data=GFf1000v10)
summary(coxGFf1000v10)
coxGFf1000v100<-coxph(Surv(time,event)~type,data=GFf1000v100)
summary(coxGFf1000v100)
coxGFf100vO<-coxph(Surv(time,event)~type,data=GFf100vO)
summary(coxGFf100vO)
coxGFf100v10<-coxph(Surv(time,event)~type,data=GFf100v10)
summary(coxGFf100v10)
coxGFf10vO<-coxph(Surv(time,event)~type,data=GFf10vO)
summary(coxGFf10vO)


##Pair-wise heterogeneity and hazard proportion analysis##
##for CompInj data, 6 categories so 6 pairs (and bonferroni corrected a would be 0.05/12=0.004)
##split data into individual doses

InjC<-CompInj[CompInj$type=="control dose",]
InjO<-CompInj[CompInj$type=="original dose",]
Inj10<-CompInj[CompInj$type=="10 fold dilution",]
Inj100<-CompInj[CompInj$type=="100 fold dilution",]
Inj1000<-CompInj[CompInj$type=="1,000 fold dilution",]
Inj10000<-CompInj[CompInj$type=="10,000 fold dilution",]
##merge by pair
InjCvO<-rbind(InjC,InjO)
InjCv10<-rbind(InjC,Inj10)
InjCv100<-rbind(InjC,Inj100)
InjCv1000<-rbind(InjC,Inj1000)
InjCv10000<-rbind(InjC,Inj10000)
Inj10000vO<-rbind(Inj10000,InjO)
Inj10000v10<-rbind(Inj10000,Inj10)
Inj10000v100<-rbind(Inj10000,Inj100)
Inj10000v1000<-rbind(Inj10000,Inj1000)
Inj1000vO<-rbind(Inj1000,InjO)
Inj1000v10<-rbind(Inj1000,Inj10)
Inj1000v100<-rbind(Inj1000,Inj100)
Inj100vO<-rbind(Inj100,InjO)
Inj100v10<-rbind(Inj100,Inj10)
Inj10vO<-rbind(Inj10,InjO)

##coxph individual pairs to check heterogeneity (likelihood ratio) and hazard proportion (exp coef)

coxFf10000v1000<-coxph(Surv(time,event)~type,data=c10000v1000)
summary(coxFF10000v1000)

coxInjCvO<-coxph(Surv(time,event)~type,data=InjCvO)
summary(coxInjCvO)
coxInjCv10<-coxph(Surv(time,event)~type,data=InjCv10)
summary(coxInjCv10)
coxInjCv100<-coxph(Surv(time,event)~type,data=InjCv100)
summary(coxInjCv100)
coxInjCv1000<-coxph(Surv(time,event)~type,data=InjCv1000)
summary(coxInjCv1000)
coxInjCv10000<-coxph(Surv(time,event)~type,data=InjCv10000)
summary(coxInjCv10000)
coxInj10000vO<-coxph(Surv(time,event)~type,data=Inj10000vO)
summary(coxInj10000vO)
coxInj10000v10<-coxph(Surv(time,event)~type,data=Inj10000v10)
summary(coxInj10000v10)
coxInj10000v100<-coxph(Surv(time,event)~type,data=Inj10000v100)
summary(coxInj10000v100)
coxInj10000v1000<-coxph(Surv(time,event)~type,data=Inj10000v1000)
summary(coxInj10000v1000)
coxInj1000vO<-coxph(Surv(time,event)~type,data=Inj1000vO)
summary(coxInj1000vO)
coxInj1000v10<-coxph(Surv(time,event)~type,data=Inj1000v10)
summary(coxInj1000v10)
coxInj1000v100<-coxph(Surv(time,event)~type,data=Inj1000v100)
summary(coxInj1000v100)
coxInj100vO<-coxph(Surv(time,event)~type,data=Inj100vO)
summary(coxInj100vO)
coxInj100v10<-coxph(Surv(time,event)~type,data=Inj100v10)
summary(coxInj100v10)
coxInj10vO<-coxph(Surv(time,event)~type,data=Inj10vO)
summary(coxInj10vO)

##Pair-wise heterogeneity and hazard proportion analysis##
##for CompGInj data, 6 categories so 6 pairs (and bonferroni corrected a would be 0.05/12=0.004)
##split data into individual doses

GInjC<-CompGInj[CompGInj$type=="control dose",]
GInjO<-CompGInj[CompGInj$type=="original dose",]
GInj10<-CompGInj[CompGInj$type=="10 fold dilution",]
GInj100<-CompGInj[CompGInj$type=="100 fold dilution",]
GInj1000<-CompGInj[CompGInj$type=="1,000 fold dilution",]
GInj10000<-CompGInj[CompGInj$type=="10,000 fold dilution",]
##merge by pair
GInjCvO<-rbind(GInjC,GInjO)
GInjCv10<-rbind(GInjC,GInj10)
GInjCv100<-rbind(GInjC,GInj100)
GInjCv1000<-rbind(GInjC,GInj1000)
GInjCv10000<-rbind(GInjC,GInj10000)
GInj10000vO<-rbind(GInj10000,GInjO)
GInj10000v10<-rbind(GInj10000,GInj10)
GInj10000v100<-rbind(GInj10000,GInj100)
GInj10000v1000<-rbind(GInj10000,GInj1000)
GInj1000vO<-rbind(GInj1000,GInjO)
GInj1000v10<-rbind(GInj1000,GInj10)
GInj1000v100<-rbind(GInj1000,GInj100)
GInj100vO<-rbind(GInj100,GInjO)
GInj100v10<-rbind(GInj100,GInj10)
GInj10vO<-rbind(GInj10,GInjO)

##coxph individual pairs to check heterogeneity (likelihood ratio) and hazard proportion (exp coef)

coxGInjCvO<-coxph(Surv(time,event)~type,data=GInjCvO)
summary(coxGInjCvO)
coxGInjCv10<-coxph(Surv(time,event)~type,data=GInjCv10)
summary(coxGInjCv10)
coxGInjCv100<-coxph(Surv(time,event)~type,data=GInjCv100)
summary(coxGInjCv100)
coxGInjCv1000<-coxph(Surv(time,event)~type,data=GInjCv1000)
summary(coxGInjCv1000)
coxGInjCv10000<-coxph(Surv(time,event)~type,data=GInjCv10000)
summary(coxGInjCv10000)
coxGInj10000vO<-coxph(Surv(time,event)~type,data=GInj10000vO)
summary(coxGInj10000vO)
coxGInj10000v10<-coxph(Surv(time,event)~type,data=GInj10000v10)
summary(coxGInj10000v10)
coxGInj10000v100<-coxph(Surv(time,event)~type,data=GInj10000v100)
summary(coxGInj10000v100)
coxGInj10000v1000<-coxph(Surv(time,event)~type,data=GInj10000v1000)
summary(coxGInj10000v1000)
coxGInj1000vO<-coxph(Surv(time,event)~type,data=GInj1000vO)
summary(coxGInj1000vO)
coxGInj1000v10<-coxph(Surv(time,event)~type,data=GInj1000v10)
summary(coxGInj1000v10)
coxGInj1000v100<-coxph(Surv(time,event)~type,data=GInj1000v100)
summary(coxGInj1000v100)
coxGInj100vO<-coxph(Surv(time,event)~type,data=GInj100vO)
summary(coxGInj100vO)
coxGInj100v10<-coxph(Surv(time,event)~type,data=GInj100v10)
summary(coxGInj100v10)
coxGInj10vO<-coxph(Surv(time,event)~type,data=GInj10vO)
summary(coxGInj10vO)


##Pair-wise heterogeneity and hazard proportion analysis##
##for ComMGFf data, 6 categories so 6 pairs (and bonferroni corrected a would be 0.05/12=0.004)
##split data into individual doses

MFfC<-CompMFf[CompMFf$type=="control dose",]
MFfO<-CompMFf[CompMFf$type=="original dose",]
MFf10<-CompMFf[CompMFf$type=="10 fold dilution",]
MFf100<-CompMFf[CompMFf$type=="100 fold dilution",]
MFf1000<-CompMFf[CompMFf$type=="1,000 fold dilution",]
MFf10000<-CompMFf[CompMFf$type=="10,000 fold dilution",]

##merge by pair

MFfCvO<-rbind(MFfC,MFfO)
MFfCv10<-rbind(MFfC,MFf10)
MFfCv100<-rbind(MFfC,MFf100)
MFfCv1000<-rbind(MFfC,MFf1000)
MFfCv10000<-rbind(MFfC,MFf10000)
MFf10000vO<-rbind(MFf10000,MFfO)
MFf10000v10<-rbind(MFf10000,MFf10)
MFf10000v100<-rbind(MFf10000,MFf100)
MFf10000v1000<-rbind(MFf10000,MFf1000)
MFf1000vO<-rbind(MFf1000,MFfO)
MFf1000v10<-rbind(MFf1000,MFf10)
MFf1000v100<-rbind(MFf1000,MFf100)
MFf100vO<-rbind(MFf100,MFfO)
MFf100v10<-rbind(MFf100,MFf10)
MFf10vO<-rbind(MFf10,MFfO)

##coxph individual pairs to check heterogeneity (likelihood ratio) and hazard proportion (exp coef)

coxMFfCvO<-coxph(Surv(time,event)~type,data=MFfCvO)
summary(coxMFfCvO)
coxMFfCv10<-coxph(Surv(time,event)~type,data=MFfCv10)
summary(coxMFfCv10)
coxMFfCv100<-coxph(Surv(time,event)~type,data=MFfCv100)
summary(coxMFfCv100)
coxMFfCv1000<-coxph(Surv(time,event)~type,data=MFfCv1000)
summary(coxMFfCv1000)
coxMFfCv10000<-coxph(Surv(time,event)~type,data=MFfCv10000)
summary(coxMFfCv10000)
coxMFf10000vO<-coxph(Surv(time,event)~type,data=MFf10000vO)
summary(coxMFf10000vO)
coxMFf10000v10<-coxph(Surv(time,event)~type,data=MFf10000v10)
summary(coxMFf10000v10)
coxMFf10000v100<-coxph(Surv(time,event)~type,data=MFf10000v100)
summary(coxMFf10000v100)
coxMFf10000v1000<-coxph(Surv(time,event)~type,data=MFf10000v1000)
summary(coxMFf10000v1000)
coxMFf1000vO<-coxph(Surv(time,event)~type,data=MFf1000vO)
summary(coxMFf1000vO)
coxMFf1000v10<-coxph(Surv(time,event)~type,data=MFf1000v10)
summary(coxMFf1000v10)
coxMFf1000v100<-coxph(Surv(time,event)~type,data=MFf1000v100)
summary(coxMFf1000v100)
coxMFf100vO<-coxph(Surv(time,event)~type,data=MFf100vO)
summary(coxMFf100vO)
coxMFf100v10<-coxph(Surv(time,event)~type,data=MFf100v10)
summary(coxMFf100v10)
coxMFf10vO<-coxph(Surv(time,event)~type,data=MFf10vO)
summary(coxMFf10vO)


##Pair-wise heterogeneity and hazard proportion analysis##
##for CompMInj data, 6 categories so 6 pairs (and bonferroni corrected a would be 0.05/12=0.004)
##split data into individual doses

MInjC<-CompMInj[CompMInj$type=="control dose",]
MInjO<-CompMInj[CompMInj$type=="original dose",]
MInj10<-CompMInj[CompMInj$type=="10 fold dilution",]
MInj100<-CompMInj[CompMInj$type=="100 fold dilution",]
MInj1000<-CompMInj[CompMInj$type=="1,000 fold dilution",]
MInj10000<-CompMInj[CompMInj$type=="10,000 fold dilution",]
##merge by pair
MInjCvO<-rbind(MInjC,MInjO)
MInjCv10<-rbind(MInjC,MInj10)
MInjCv100<-rbind(MInjC,MInj100)
MInjCv1000<-rbind(MInjC,MInj1000)
MInjCv10000<-rbind(MInjC,MInj10000)
MInj10000vO<-rbind(MInj10000,MInjO)
MInj10000v10<-rbind(MInj10000,MInj10)
MInj10000v100<-rbind(MInj10000,MInj100)
MInj10000v1000<-rbind(MInj10000,MInj1000)
MInj1000vO<-rbind(MInj1000,MInjO)
MInj1000v10<-rbind(MInj1000,MInj10)
MInj1000v100<-rbind(MInj1000,MInj100)
MInj100vO<-rbind(MInj100,MInjO)
MInj100v10<-rbind(MInj100,MInj10)
MInj10vO<-rbind(MInj10,MInjO)

##coxph individual pairs to check heterogeneity (likelihood ratio) and hazard proportion (exp coef)

coxMInjCvO<-coxph(Surv(time,event)~type,data=MInjCvO)
summary(coxMInjCvO)
coxMInjCv10<-coxph(Surv(time,event)~type,data=MInjCv10)
summary(coxMInjCv10)
coxMInjCv100<-coxph(Surv(time,event)~type,data=MInjCv100)
summary(coxMInjCv100)
coxMInjCv1000<-coxph(Surv(time,event)~type,data=MInjCv1000)
summary(coxMInjCv1000)
coxMInjCv10000<-coxph(Surv(time,event)~type,data=MInjCv10000)
summary(coxMInjCv10000)
coxMInj10000vO<-coxph(Surv(time,event)~type,data=MInj10000vO)
summary(coxMInj10000vO)
coxMInj10000v10<-coxph(Surv(time,event)~type,data=MInj10000v10)
summary(coxMInj10000v10)
coxMInj10000v100<-coxph(Surv(time,event)~type,data=MInj10000v100)
summary(coxMInj10000v100)
coxMInj10000v1000<-coxph(Surv(time,event)~type,data=MInj10000v1000)
summary(coxMInj10000v1000)
coxMInj1000vO<-coxph(Surv(time,event)~type,data=MInj1000vO)
summary(coxMInj1000vO)
coxMInj1000v10<-coxph(Surv(time,event)~type,data=MInj1000v10)
summary(coxMInj1000v10)
coxMInj1000v100<-coxph(Surv(time,event)~type,data=MInj1000v100)
summary(coxMInj1000v100)
coxMInj100vO<-coxph(Surv(time,event)~type,data=MInj100vO)
summary(coxMInj100vO)
coxMInj100v10<-coxph(Surv(time,event)~type,data=MInj100v10)
summary(coxMInj100v10)
coxMInj10vO<-coxph(Surv(time,event)~type,data=MInj10vO)
summary(coxMInj10vO)


##Pair-wise heterogeneity and hazard proportion analysis##
##for CompPInj data, 6 categories so 6 pairs (and bonferroni corrected a would be 0.05/12=0.004)
##split data into individual doses

PInjC<-CompPInj[CompPInj$type=="control dose",]
PInjO<-CompPInj[CompPInj$type=="original dose",]
PInj10<-CompPInj[CompPInj$type=="10 fold dilution",]
PInj100<-CompPInj[CompPInj$type=="100 fold dilution",]
PInj1000<-CompPInj[CompPInj$type=="1,000 fold dilution",]
PInj10000<-CompPInj[CompPInj$type=="10,000 fold dilution",]

##merge by pair

PInjCvO<-rbind(PInjC,PInjO)
PInjCv10<-rbind(PInjC,PInj10)
PInjCv100<-rbind(PInjC,PInj100)
PInjCv1000<-rbind(PInjC,PInj1000)
PInjCv10000<-rbind(PInjC,PInj10000)
PInj10000vO<-rbind(PInj10000,PInjO)
PInj10000v10<-rbind(PInj10000,PInj10)
PInj10000v100<-rbind(PInj10000,PInj100)
PInj10000v1000<-rbind(PInj10000,PInj1000)
PInj1000vO<-rbind(PInj1000,PInjO)
PInj1000v10<-rbind(PInj1000,PInj10)
PInj1000v100<-rbind(PInj1000,PInj100)
PInj100vO<-rbind(PInj100,PInjO)
PInj100v10<-rbind(PInj100,PInj10)
PInj10vO<-rbind(PInj10,PInjO)

#coxph individual pairs to check heterogeneity (likelihood ratio) and hazard proportion (exp coef)

coxPInjCvO<-coxph(Surv(time,event)~type,data=PInjCvO)
summary(coxPInjCvO)
coxPInjCv10<-coxph(Surv(time,event)~type,data=PInjCv10)
summary(coxPInjCv10)
coxPInjCv100<-coxph(Surv(time,event)~type,data=PInjCv100)
summary(coxPInjCv100)
coxPInjCv1000<-coxph(Surv(time,event)~type,data=PInjCv1000)
summary(coxPInjCv1000)
coxPInjCv10000<-coxph(Surv(time,event)~type,data=PInjCv10000)
summary(coxPInjCv10000)
coxPInj10000vO<-coxph(Surv(time,event)~type,data=PInj10000vO)
summary(coxPInj10000vO)
coxPInj10000v10<-coxph(Surv(time,event)~type,data=PInj10000v10)
summary(coxPInj10000v10)
coxPInj10000v100<-coxph(Surv(time,event)~type,data=PInj10000v100)
summary(coxPInj10000v100)
coxPInj10000v1000<-coxph(Surv(time,event)~type,data=PInj10000v1000)
summary(coxPInj10000v1000)
coxPInj1000vO<-coxph(Surv(time,event)~type,data=PInj1000vO)
summary(coxPInj1000vO)
coxPInj1000v10<-coxph(Surv(time,event)~type,data=PInj1000v10)
summary(coxPInj1000v10)
coxPInj1000v100<-coxph(Surv(time,event)~type,data=PInj1000v100)
summary(coxPInj1000v100)
coxPInj100vO<-coxph(Surv(time,event)~type,data=PInj100vO)
summary(coxPInj100vO)
coxPInj100v10<-coxph(Surv(time,event)~type,data=PInj100v10)
summary(coxPInj100v10)
coxPInj10vO<-coxph(Surv(time,event)~type,data=PInj10vO)
summary(coxPInj10vO)


##Pair-wise heterogeneity and hazard proportion analysis##
##for CompPFf data, 6 categories so 6 pairs (and bonferroni corrected a would be 0.05/12=0.004)
##split data into individual doses

PFfC<-CompPFf[CompPFf$type=="control dose",]
PFfO<-CompPFf[CompPFf$type=="original dose",]
PFf10<-CompPFf[CompPFf$type=="10 fold dilution",]
PFf100<-CompPFf[CompPFf$type=="100 fold dilution",]
PFf1000<-CompPFf[CompPFf$type=="1,000 fold dilution",]
PFf10000<-CompPFf[CompPFf$type=="10,000 fold dilution",]
##merge by pair
PFfCvO<-rbind(PFfC,PFfO)
PFfCv10<-rbind(PFfC,PFf10)
PFfCv100<-rbind(PFfC,PFf100)
PFfCv1000<-rbind(PFfC,PFf1000)
PFfCv10000<-rbind(PFfC,PFf10000)
PFf10000vO<-rbind(PFf10000,PFfO)
PFf10000v10<-rbind(PFf10000,PFf10)
PFf10000v100<-rbind(PFf10000,PFf100)
PFf10000v1000<-rbind(PFf10000,PFf1000)
PFf1000vO<-rbind(PFf1000,PFfO)
PFf1000v10<-rbind(PFf1000,PFf10)
PFf1000v100<-rbind(PFf1000,PFf100)
PFf100vO<-rbind(PFf100,PFfO)
PFf100v10<-rbind(PFf100,PFf10)
PFf10vO<-rbind(PFf10,PFfO)

#coxph individual pairs to check heterogeneity (likelihood ratio) and hazard proportion (exp coef)

coxPFfCvO<-coxph(Surv(time,event)~type,data=PFfCvO)
summary(coxPFfCvO)
coxPFfCv10<-coxph(Surv(time,event)~type,data=PFfCv10)
summary(coxPFfCv10)
coxPFfCv100<-coxph(Surv(time,event)~type,data=PFfCv100)
summary(coxPFfCv100)
coxPFfCv1000<-coxph(Surv(time,event)~type,data=PFfCv1000)
summary(coxPFfCv1000)
coxPFfCv10000<-coxph(Surv(time,event)~type,data=PFfCv10000)
summary(coxPFfCv10000)
coxPFf10000vO<-coxph(Surv(time,event)~type,data=PFf10000vO)
summary(coxPFf10000vO)
coxPFf10000v10<-coxph(Surv(time,event)~type,data=PFf10000v10)
summary(coxPFf10000v10)
coxPFf10000v100<-coxph(Surv(time,event)~type,data=PFf10000v100)
summary(coxPFf10000v100)
coxPFf10000v1000<-coxph(Surv(time,event)~type,data=PFf10000v1000)
summary(coxPFf10000v1000)
coxPFf1000vO<-coxph(Surv(time,event)~type,data=PFf1000vO)
summary(coxPFf1000vO)
coxPFf1000v10<-coxph(Surv(time,event)~type,data=PFf1000v10)
summary(coxPFf1000v10)
coxPFf1000v100<-coxph(Surv(time,event)~type,data=PFf1000v100)
summary(coxPFf1000v100)
coxPFf100vO<-coxph(Surv(time,event)~type,data=PFf100vO)
summary(coxPFf100vO)
coxPFf100v10<-coxph(Surv(time,event)~type,data=PFf100v10)
summary(coxPFf100v10)
coxPFf10vO<-coxph(Surv(time,event)~type,data=PFf10vO)
summary(coxPFf10vO)



