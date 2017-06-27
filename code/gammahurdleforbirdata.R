library(readr)
library(tidyverse)

## ICELAND 2016 ##
## Change directory to whatever location before you begin, they are in the data folder on github ##

birddataMFRIall <- read_csv("E:/medafli/lumpsucker/birddataMFRI.csv")
lumpsuckereffortIceland2016 <- read_csv("E:/medafli/lumpsucker/lumpsuckereffortIceland2016.csv")

birddataMFRIall <- birddataMFRIall %>% 
  left_join(lumpsuckereffortIceland2016) 

birddataMFRIall<-birddataMFRIall %>% 
  dplyr::select(vessel_id = skip, U.aalge,U.lomvia,F.arctica,C.grylle,Phalacro,S.mollissima,`C. hyemalis`, 
         landings = landanir, netdays = netanaetur)

birddataMFRIall$effort<-birddataMFRIall$netdays/birddataMFRIall$landings
## perhaps join the birdlife data here? ##
#birdlifedata <- read.csv("E:/medafli/lumpsucker/Iceland gillnets.csv")

birddataMFRIall$birds<-birddataMFRIall$U.aalge+birddataMFRIall$U.lomvia+
  birddataMFRIall$F.arctica+birddataMFRIall$C.grylle+birddataMFRIall$Phalacro+
  birddataMFRIall$S.mollissima+birddataMFRIall$`C. hyemalis`

birddataMFRIall$bycatch <- birddataMFRIall$birds/birddataMFRIall$effort
birddataMFRIall$non.zero <- ifelse(birddataMFRIall$bycatch > 0, 1, 0)

## GAMMA HURDLE MODEL ###
hist(birddataMFRIall$bycatch)

m1 <- glm(non.zero ~ 1, data=birddataMFRIall, family=binomial(link=logit))
summary(m1)
m2 <- glm(bycatch ~ 1, data=subset(birddataMFRIall, non.zero == 1), family=Gamma(link=log))
summary(m2)

library(ggplot2)
library(lmtest)
library(multcomp)

# Get some coefficients

(bin_coef <- plogis(coef(m1)[[1]]))  # prob of bycatch occurring
(bin_CI <- plogis(confint(m1)))
(bin_SD <- mean(c((bin_coef-bin_CI[1]) / 1.96), (bin_CI[2]-bin_coef) / 1.96))

(gamma_coef <- exp(coef(m2)[[1]]))   # given bycatch, the mean meammals/netnights
(gamma_CI <- exp(confint(m2)))
(gamma_SD <- mean(c((gamma_coef-gamma_CI[1]) / 1.96), (gamma_CI[2]-gamma_coef) / 1.96))

## Raise for the fleet ##

Totalnetdays <- sum(lumpsuckereffortIceland2016$netanaetur,na.rm = TRUE)

Est.bycatch.modelapproach <- Totalnetdays * sample(rnorm(244, bin_coef, bin_SD)) * sample(rnorm(244, gamma_coef, gamma_SD))
summary(Est.bycatch.modelapproach)
quantile(Est.bycatch.modelapproach, c(0.025, 0.975))

Est.bycatch.simple <- sum(birddataMFRIall$birds)*(Totalnetdays/sum(birddataMFRIall$effort,na.rm = TRUE))

##################################################################
# By Species
###################################################################

# Guillemot
birddataMFRIall$u.aalge.bycatch <- birddataMFRIall$U.aalge / birddataMFRIall$effort
birddataMFRIall$U.aalge.non.zero <- ifelse(birddataMFRIall$U.aalge.bycatch > 0, 1, 0)
m1 <- glm(U.aalge.non.zero ~ 1, data=birddataMFRIall, family=binomial(link=logit))
summary(m1)
m2 <- glm(U.aalge.bycatch ~ 1, data=subset(birddataMFRIall, U.aalge.non.zero == 1), family=Gamma(link=log))
summary(m2)

# Get some coefficients
(bin_coef <- plogis(coef(m1)[[1]]))  # prob of bycatch occurring
(bin_CI <- plogis(confint(m1)))
(bin_SD <- mean(c((bin_coef-bin_CI[1]) / 1.96), (bin_CI[2]-bin_coef) / 1.96))
(gamma_coef <- exp(coef(m2)[[1]]))   # given bycatch, the mean birds/m/day
(gamma_CI <- exp(confint(m2)))
(gamma_SD <- mean(c((gamma_coef-gamma_CI[1]) / 1.96), (gamma_CI[2]-gamma_coef) / 1.96))

Est.U.aalge.bycatch.modelapproach <- rep.int(Totalnetdays * sample(rnorm(244, bin_coef, bin_SD)) * sample(rnorm(244, gamma_coef, gamma_SD)),1000)
summary(Est.U.aalge.bycatch.modelapproach)
quantile(Est.U.aalge.bycatch.modelapproach, c(0.025, 0.975))

Est.U.aalge.bycatch.simple <- sum(birddataMFRIall$U.aalge)*(Totalnetdays/sum(birddataMFRIall$effort,na.rm = TRUE))

## Brunnichs guillemot ##

birddataMFRIall$U.lomvia.bycatch <- birddataMFRIall$U.lomvia / birddataMFRIall$effort
birddataMFRIall$harbseal.non.zero <- ifelse(birddataMFRIall$U.lomvia.bycatch > 0, 1, 0)
m1 <- glm(harbseal.non.zero ~ 1, data=birddataMFRIall, family=binomial(link=logit))
summary(m1)
m2 <- glm(U.lomvia.bycatch ~ 1, data=subset(birddataMFRIall, harbseal.non.zero == 1), family=Gamma(link=log))
summary(m2)

# Get some coefficients
(bin_coef <- plogis(coef(m1)[[1]]))  # prob of bycatch occurring
(bin_CI <- plogis(confint(m1)))
(bin_SD <- mean(c((bin_coef-bin_CI[1]) / 1.96), (bin_CI[2]-bin_coef) / 1.96))
(gamma_coef <- exp(coef(m2)[[1]]))   # given bycatch, the mean birds/m/day
(gamma_CI <- exp(confint(m2)))
(gamma_SD <- mean(c((gamma_coef-gamma_CI[1]) / 1.96), (gamma_CI[2]-gamma_coef) / 1.96))

Est.U.lomvia.bycatch.modelapproach <- rep.int(Totalnetdays * sample(rnorm(244, bin_coef, bin_SD)) * sample(rnorm(244, gamma_coef, gamma_SD)),1000)
summary(Est.U.lomvia.bycatch.modelapproach)
quantile(Est.U.lomvia.bycatch.modelapproach, c(0.025, 0.975))

Est.U.lomvia.bycatch.simple <- sum(birddataMFRIall$U.lomvia)*(Totalnetdays/sum(birddataMFRIall$effirt,na.rm = TRUE))

## Puffin
birddataMFRIall$F.arctica.bycatch <- birddataMFRIall$F.arctica / birddataMFRIall$effort
birddataMFRIall$F.arctica.non.zero <- ifelse(birddataMFRIall$F.arctica.bycatch > 0, 1, 0)
m1 <- glm(F.arctica.non.zero ~ 1, data=birddataMFRIall, family=binomial(link=logit))
summary(m1)
m2 <- glm(F.arctica.bycatch ~ 1, data=subset(birddataMFRIall, F.arctica.non.zero == 1), family=Gamma(link=log))
summary(m2)

# Get some coefficients
(bin_coef <- plogis(coef(m1)[[1]]))  # prob of bycatch occurring
(bin_CI <- plogis(confint(m1)))
(bin_SD <- mean(c((bin_coef-bin_CI[1]) / 1.96), (bin_CI[2]-bin_coef) / 1.96))
(gamma_coef <- exp(coef(m2)[[1]]))   # given bycatch, the mean birds/m/day
(gamma_CI <- exp(confint(m2)))
(gamma_SD <- mean(c((gamma_coef-gamma_CI[1]) / 1.96), (gamma_CI[2]-gamma_coef) / 1.96))

Est.F.arctica.bycatch.modelapproach <- rep.int(Totalnetdays * sample(rnorm(244, bin_coef, bin_SD)) * sample(rnorm(244, gamma_coef, gamma_SD)),1000)
summary(Est.F.arctica.bycatch.modelapproach)
quantile(Est.F.arctica.bycatch.modelapproach, c(0.025, 0.975))

Est.F.arctica.bycatch.simple <- sum(birddataMFRIall$F.arctica)*(Totalnetdays/sum(birddataMFRIall$effort,na.rm = TRUE))


## Harp seal
birddataMFRIall$C.grylle.bycatch <- birddataMFRIall$C.grylle / birddataMFRIall$effort
birddataMFRIall$C.grylle.non.zero <- ifelse(birddataMFRIall$C.grylle.bycatch > 0, 1, 0)
m1 <- glm(C.grylle.non.zero ~ 1, data=birddataMFRIall, family=binomial(link=logit))
summary(m1)
m2 <- glm(C.grylle.bycatch ~ 1, data=subset(birddataMFRIall, C.grylle.non.zero == 1), family=Gamma(link=log))
summary(m2)

# Get some coefficients
(bin_coef <- plogis(coef(m1)[[1]]))  # prob of bycatch occurring
(bin_CI <- plogis(confint(m1)))
(bin_SD <- mean(c((bin_coef-bin_CI[1]) / 1.96), (bin_CI[2]-bin_coef) / 1.96))
(gamma_coef <- exp(coef(m2)[[1]]))   # given bycatch, the mean birds/m/day
(gamma_CI <- exp(confint(m2)))
(gamma_SD <- mean(c((gamma_coef-gamma_CI[1]) / 1.96), (gamma_CI[2]-gamma_coef) / 1.96))

Est.C.grylle.bycatch.modelapproach <- rep.int(Totalnetdays * sample(rnorm(244, bin_coef, bin_SD)) * sample(rnorm(244, gamma_coef, gamma_SD)),1000)
summary(Est.C.grylle.bycatch.modelapproach)
quantile(Est.C.grylle.bycatch.modelapproach, c(0.025, 0.975))

Est.C.grylle.bycatch.simple <- sum(birddataMFRIall$C.grylle)*(Totalnetdays/sum(birddataMFRIall$effort,na.rm = TRUE))

# Eider 
birddataMFRIall$S.mollisma.bycatch <- birddataMFRIall$S.mollisma / birddataMFRIall$effort
birddataMFRIall$S.mollisma.non.zero <- ifelse(birddataMFRIall$S.mollisma.bycatch > 0, 1, 0)
m1 <- glm(S.mollisma.non.zero ~ 1, data=birddataMFRIall, family=binomial(link=logit))
summary(m1)
m2 <- glm(S.mollisma.bycatch ~ 1, data=subset(birddataMFRIall, S.mollisma.non.zero == 1), family=Gamma(link=log))
summary(m2)

# Get some coefficients
(bin_coef <- plogis(coef(m1)[[1]]))  # prob of bycatch occurring
(bin_CI <- plogis(confint(m1)))
(bin_SD <- mean(c((bin_coef-bin_CI[1]) / 1.96), (bin_CI[2]-bin_coef) / 1.96))
(gamma_coef <- exp(coef(m2)[[1]]))   # given bycatch, the mean birds/m/day
(gamma_CI <- exp(confint(m2)))
(gamma_SD <- mean(c((gamma_coef-gamma_CI[1]) / 1.96), (gamma_CI[2]-gamma_coef) / 1.96))

Est.S.mollisma.bycatch.modelapproach <- rep.int(Totalnetdays * sample(rnorm(244, bin_coef, bin_SD)) * sample(rnorm(244, gamma_coef, gamma_SD)),1000)
summary(Est.S.mollisma.bycatch.modelapproach)
quantile(Est.S.mollisma.bycatch.modelapproach, c(0.025, 0.975))

Est.S.mollisma.bycatch.simple <- sum(birddataMFRIall$S.mollisma)*(Totalnetdays/sum(birddataMFRIall$effort,na.rm = TRUE))

# Cormorants
birddataMFRIall$Phalacro.bycatch <- birddataMFRIall$Phalacro / birddataMFRIall$effort
birddataMFRIall$Phalacro.non.zero <- ifelse(birddataMFRIall$Phalacro.bycatch > 0, 1, 0)
m1 <- glm(Phalacro.non.zero ~ 1, data=birddataMFRIall, family=binomial(link=logit))
summary(m1)
m2 <- glm(Phalacro.bycatch ~ 1, data=subset(birddataMFRIall, Phalacro.non.zero == 1), family=Gamma(link=log))
summary(m2)

# Get some coefficients
(bin_coef <- plogis(coef(m1)[[1]]))  # prob of bycatch occurring
(bin_CI <- plogis(confint(m1)))
(bin_SD <- mean(c((bin_coef-bin_CI[1]) / 1.96), (bin_CI[2]-bin_coef) / 1.96))
(gamma_coef <- exp(coef(m2)[[1]]))   # given bycatch, the mean birds/m/day
(gamma_CI <- exp(confint(m2)))
(gamma_SD <- mean(c((gamma_coef-gamma_CI[1]) / 1.96), (gamma_CI[2]-gamma_coef) / 1.96))

Est.Phalacro.bycatch.modelapproach <- rep.int(Totalnetdays * sample(rnorm(244, bin_coef, bin_SD)) * sample(rnorm(244, gamma_coef, gamma_SD)),1000)
summary(Est.Phalacro.bycatch.modelapproach)
quantile(Est.Phalacro.bycatch.modelapproach, c(0.025, 0.975))

Est.Phalacro.bycatch.simple <- sum(birddataMFRIall$Phalacro)*(Totalnetdays/sum(birddataMFRIall$effort,na.rm = TRUE))

# Long tailed duck
birddataMFRIall$C.hyemalis.bycatch <- birddataMFRIall$`C. hyemalis` / birddataMFRIall$effort
birddataMFRIall$C.hyemalis.non.zero <- ifelse(birddataMFRIall$C.hyemalis.bycatch > 0, 1, 0)
m1 <- glm(C.hyemalis.non.zero ~ 1, data=birddataMFRIall, family=binomial(link=logit))
summary(m1)
m2 <- glm(C.hyemalis.bycatch ~ 1, data=subset(birddataMFRIall, C.hyemalis.non.zero == 1), family=Gamma(link=log))
summary(m2)

# Get some coefficients
(bin_coef <- plogis(coef(m1)[[1]]))  # prob of bycatch occurring
(bin_CI <- plogis(confint(m1)))
(bin_SD <- mean(c((bin_coef-bin_CI[1]) / 1.96), (bin_CI[2]-bin_coef) / 1.96))
(gamma_coef <- exp(coef(m2)[[1]]))   # given bycatch, the mean birds/m/day
(gamma_CI <- exp(confint(m2)))
(gamma_SD <- mean(c((gamma_coef-gamma_CI[1]) / 1.96), (gamma_CI[2]-gamma_coef) / 1.96))

Est.C.hyemalis.bycatch.modelapproach <- rep.int(Totalnetdays * sample(rnorm(244, bin_coef, bin_SD)) * sample(rnorm(244, gamma_coef, gamma_SD)),1000)
summary(Est.C.hyemalis.bycatch.modelapproach)
quantile(Est.C.hyemalis.bycatch.modelapproach, c(0.025, 0.975))

Est.C.hyemalis.bycatch.simple <- sum(birddataMFRIall$`C. hyemalis`)*(Totalnetdays/sum(birddataMFRIall$effort,na.rm = TRUE))

