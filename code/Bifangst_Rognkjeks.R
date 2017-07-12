################################################################
#                     Rognkjeks-Bifangst                       #
################################################################

library(glmmADMB)

library(data.table)

setwd( "C:/Users/kim.barum/Documents/Bifangst")

Rognkjeks_data<-read.csv("Rognkjeks_raadata.csv", header=T,sep=";", na.strings = c("","-99"))
names(Rognkjeks_data)
#klargjør data
Rognkjeks_data$Bifangst.totalt.teist<-as.numeric(Rognkjeks_data$Bifangst.totalt.teist)

dt <- data.table(Rognkjeks_data)

#sum of bird bycatch per trip per year
Rognkjeks_colapsed_date<-as.data.table(Rognkjeks_data)[,.(Fishing_depth=mean(Fiskedyp.Min..favn.),Meter_of_net=max(Total.garnlengde..meter.),Teist=sum(Bifangst.totalt.teist),Skarv=sum(Bifangst.totalt.skarv),Fishing_time=max(Ståtid..timer.), Bifangst_tot=sum(c(Bifangst.totalt.teist,Bifangst.totalt.skarv, Bifangst.andre.arter))), by = .(Fartøy, Dag,Måned,År)]
#sum_data<-dt[, lapply(.SD,sum), by = list(Fartøy, Dag,Måned,År)]

#sum(Rognkjeks_colapsed_date$V1,na.rm = TRUE)

#with(sum_data, tapply(Fartøy, År, function(x) length(unique(x))))#antal båter

#with(sum_data, tapply(Fartøy, År, function(x) length(x)))#antall turer

#as.data.table(sum_data)[, sum(Total.garnlengde..meter.), by = .(År)]#lengde garn

#sum_data$bifangst_sjofugl<-ifelse(sum_data$Bifangst.totalt.teist>0,"1","0")
#sum_data$bifangst_sjofugl<-ifelse(sum_data$Bifangst.totalt.skarv>0,"1",sum_data$bifangst_sjofugl)
#table(sum_data[sum_data$År=="2 015"]$bifangst_sjofugl)
#sum(sum_data$Bifangst.totalt.teist,na.rm = T)
#sum(sum_data$Bifangst.totalt.skarv,na.rm = T)
#sum(sum_data$Bifangst.andre.arter,na.rm = T)
#sum(sum_data[sum_data$År=="2 015"]$Total.garnlengde..meter.,na.rm = T)
#bifangst pr tur 15/46
#bifangst pr 1000-meter garn 15/94.845
#Total.garnlengde..meter.

#table(sum_data$bifangst_sjofugl)
#sum(sum_data$Total.garnlengde..meter.,na.rm = T)


#Modeling
##Gamma Hurdle##

Rognkjeks_colapsed_date$bifangst_bin<-ifelse(Rognkjeks_colapsed_date$Bifangst_tot>0,1,0)
Rognkjeks_colapsed_date$effort<-Rognkjeks_colapsed_date$Meter_of_net/(Rognkjeks_colapsed_date$Fishing_time/24) #Netday=meters of net per day?
# Exclude trips wich have either negative effort or NA
Rognkjeks_colapsed_sub<-Rognkjeks_colapsed_date[Rognkjeks_colapsed_date$effort>0, ]
Rognkjeks_colapsed_sub$bycatch<-Rognkjeks_colapsed_sub$Bifangst_tot/Rognkjeks_colapsed_sub$effort

hist(Rognkjeks_colapsed_sub$bycatch)

m1 <- glm(bifangst_bin ~ 1, data=Rognkjeks_colapsed_sub,family=binomial(link=logit))
summary(m1)
m2 <- glm(bycatch ~ 1, data=subset(Rognkjeks_colapsed_sub, bifangst_bin == 1), family=Gamma(link=log))
summary(m2)

(bin_coef <- plogis(coef(m1)[[1]]))  # prob of bycatch occurring
(bin_CI <- plogis(confint(m1)))
(bin_SD <- mean(c((bin_coef-bin_CI[1]) / 1.96), (bin_CI[2]-bin_coef) / 1.96))

(gamma_coef <- exp(coef(m2)[[1]]))   # given bycatch, the mean meammals/netnights
(gamma_CI <- exp(confint(m2)))
(gamma_SD <- mean(c((gamma_coef-gamma_CI[1]) / 1.96), (gamma_CI[2]-gamma_coef) / 1.96))


#When including nested-struckture
library(lme4)
mm1 <- glmer(bifangst_bin ~ 1+(1|Fartøy), data=Rognkjeks_colapsed_sub,family=binomial(link=logit))
summary(mm1)
mm2 <- glmer(bycatch ~ 1+(1|Fartøy), data=subset(Rognkjeks_colapsed_sub, bifangst_bin == 1), family=Gamma(link=log))
summary(mm2)

(bin_coef_mm <- plogis(mean(coef(mm1)[[1]][,1]))) # prob of bycatch occurring
(bin_CI_mm <- plogis(confint(mm1)))
(bin_SD_mm <- mean(c((bin_coef_mm-bin_CI_mm[2,1]) / 1.96), (bin_CI_mm[2,2]-bin_coef_mm) / 1.96))

(gamma_coef_mm <- exp(mean(coef(mm2)[[1]][,1])))   # given bycatch, the mean meammals/netnights
#quick and dirt CI
(coeftbl <- as.data.frame(coef(summary(mm2))))
(gamma_CI_mm <- exp(with(coeftbl, Estimate + outer(`Std. Error`, c(lower=-1, upper=1)) * sqrt(qchisq(0.95, 1)))))
(gamma_SD_mm <- mean(c((gamma_coef_mm-gamma_CI_mm[1]) / 1.96), (gamma_CI_mm[2]-gamma_coef_mm) / 1.96))

gamma_coef_mm*bin_coef_mm#0.005011863

#ADMB-approach
#Check dist
fit <- goodfit(Rognkjeks_colapsed_sub$Bifangst_tot) 
summary(fit) 
rootogram(fit)
Ord_plot(Rognkjeks_colapsed_sub$Bifangst_tot)
distplot(Rognkjeks_colapsed_sub$Bifangst_tot, type="poisson")
distplot(Rognkjeks_colapsed_sub$Bifangst_tot, type="nbinom")

library(glmmADMB)
library(bbmle)
mod.1<-glmmadmb(Bifangst_tot ~ 1+(1|Fartøy),data=Rognkjeks_colapsed_sub,zeroInflation=F,family="poisson")
mod.2<-glmmadmb(Bifangst_tot ~ 1+(1|Fartøy),data=Rognkjeks_colapsed_sub,zeroInflation=T,family="poisson")
mod.3<-glmmadmb(Bifangst_tot ~ 1+(1|Fartøy),data=Rognkjeks_colapsed_sub,zeroInflation=F,family="nbinom")
mod.4<-glmmadmb(Bifangst_tot ~ 1+(1|Fartøy),data=Rognkjeks_colapsed_sub,zeroInflation=T,family="nbinom")

#per effort
(exp(coef(mod.3)[[1]]))/mean(Rognkjeks_colapsed_sub$effort)#0.003156534
(exp(coef(mod.4)[[1]]))/mean(Rognkjeks_colapsed_sub$effort)
AICtab(mod.1,mod.2,mod.3,mod.4)
mod.3.mcmc<-glmmadmb(Bifangst_tot ~ 1+(1|Fartøy),data=Rognkjeks_colapsed_sub,zeroInflation=F,family="nbinom",mcmc=TRUE, mcmc.opts=mcmcControl(mcmc=2000))
