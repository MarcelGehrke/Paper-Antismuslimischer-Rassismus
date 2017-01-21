###################################################################
######## Duisburger Beiträge zur soziologischen Forschung #########
######## "Antimuslimischer Rassismus ein neues Phänomen?" #########
###################################################################


############
#Vorbeitung#
############

#installieren und laden der benötigten r-packages#
install.packages("foreign",dependencies=T)
install.packages("car",dependencies=T)
install.packages("effects",dependencies=T)
install.packages("lmtest",dependencies=T)
install.packages("sem",dependencies=T)
install.packages("lavaan",dependencies=T)
install.packages("semPlot",dependencies=T)
install.packages("QuantPsyc",dependencies=T)
install.packages("boot",dependencies=T)
install.packages("pastecs",dependencies=T)
install.packages("psych")

library(foreign)
library(car)
library(effects)
library(lmtest)
library(sem)
library(lavaan)
library(semPlot)
library(boot)
library(QuantPsyc)
library(pastecs)
library(psych)

#laden des in SPSS aufbereiteten ALLBUS 2012 Datensatzes
allbus_mod<-read.spss("C:/Users/gehrk/Desktop/Uni/Beiträge_aktuell/ZA4614_v1-1-1.sav", to.data.frame=T,max.value.labels=T)


######################################################################################
###Multiple Regressionen: Rassismus u. antimuslimischer Rassismus (je zwei Modelle)###
######################################################################################

colnames(allbus_mod)

#Regressionen Rassismus#
MRassismus<-lm(Rassismus_Index ~ Autori_Index + Anomie_Index + BefWirtschftlLage + Lebenszufriedenheit + LinksRechts 
               + Ost + Land + Alter + Bildungsjahre + Kontakt_Index
               ,data=allbus_mod)
summary(MRassismus)

#Reressionen Antimuslimischer Rassismus#
MAntiMus<-lm(AntiMus_Index ~ Autori_Index + Anomie_Index + BefWirtschaftlLage + Lebenszufriedenheit + LinksRechts 
             + Ost + Land + Alter + Bildungsjahre + Kontakt_Index
             ,data=allbus_mod)
summary(MAntiMus)

lm.beta(MRassismus)
lm.beta(MAntiMus)

par(mfrow=c(2,2))
plot(MRassismus)
plot(MAntiMus)

###Deskription###
mean(allbus_mod$Lebenszufriedenheit,na.rm=T)
prop.table(table(allbus_mod$LinksRechts))
prop.table(table(allbus_mod$Land))
prop.table(table(allbus_mod$K4))

colnames(allbus_mod)
allbus_beitr<-allbus_mod[c(1:34)]
summary(allbus_beitr)

options(scipen=100)
options(digits=3)
descr<-stat.desc(allbus_beitr) 
write.csv2(descr, file = "descr.csv",row.names=T)

cbind(Freq=table(allbus_beitr$an1),Cumul=cumsum(table(allbus_beitr$an1)),relative=prop.table(table(allbus_beitr$an1)))
cbind(Freq=table(allbus_beitr$an2),Cumul=cumsum(table(allbus_beitr$an2)),relative=prop.table(table(allbus_beitr$an2)))
cbind(Freq=table(allbus_beitr$an3),Cumul=cumsum(table(allbus_beitr$an3)),relative=prop.table(table(allbus_beitr$an3)))
cbind(Freq=table(allbus_beitr$an4),Cumul=cumsum(table(allbus_beitr$an4)),relative=prop.table(table(allbus_beitr$an4)))
cbind(Freq=table(allbus_beitr$Anomie_Index),Cumul=cumsum(table(allbus_beitr$Anomie_Index)),relative=prop.table(table(allbus_beitr$Anomie_Index)))

#Cronbachs Alpha#
allbus_auto<-allbus_mod[c(14:15)]
alpha(allbus_auto)

allbus_ano<-allbus_mod[c(3:6)]
alpha(allbus_ano)

allbus_rass<-allbus_mod[c(9:12)]
alpha(allbus_rass)

allbus_antimus<-allbus_mod[c(9:13)]
alpha(allbus_amuss)

allbus_kontakt<-allbus_mod[c(19:22)]
alpha(allbus_kontakt)


###############################
###Strukturgleichungsmodelle###
###############################

SEMRassismus<-'Rassismus=~ R1 + R2 + R3 + R4
Autori=~ AU1 + AU2
Anom=~ an1 + an2 +an3 + an4
Rassismus ~ Alter + Ost + Autori + Anom + Bildungsjahre + Kontakt_Index
Autori ~ Alter + Bildungsjahre + Kontakt_Index + Anom 
Anom ~ Ost + Bildungsjahre + Alter
Kontakt_Index ~ Ost + Alter + Bildungsjahre
Bildungsjahre ~ Alter'
fitRassismus<-sem(SEMRassismus,data=allbus_mod,mimic="EQS")

SEMAntiMus<-'AntiMus=~ AR1 + AR2 + AR3 + AR4 + AR5
Autori=~ AU1 + AU2
Anom=~ an1 + an2 +an3 + an4
AntiMus ~ Alter + Ost + Autori + Anom + Bildungsjahre + Kontakt_Index
Autori ~ Alter + Bildungsjahre + Kontakt_Index + Anom 
Anom ~ Ost + Bildungsjahre + Alter
Kontakt_Index ~ Ost + Alter + Bildungsjahre
Bildungsjahre ~ Alter'
fitAntiMus<-sem(SEMAntiMus,data=allbus_mod,mimic="EQS")

summary(fitRassismus, standardized=T, fit.measures=T, rsquare=T)
summary(fitAntiMus, standardized=T, fit.measures=T, rsquare=T)

coef(fitRassismus)
coef(fitAntiMus)

fitMeasures(fitRassismus)
fitMeasures(fitAntiMus)

#Graphische Darstellung der SEMs#
par(mfrow=c(1,1))

semPaths(fitRassismus,what="std",whatLabels="std",style="lisrel",layout="spring",
         nCharNodes=20,sizeMan=10,sizeMan2=5,shapeMan="rectangle",
         residScale=5,optimizeLatRes=T,curveAdjacent=T)

semPaths(AntiMus,what="std",whatLabels="std",style="lisrel",layout="spring",
         nCharNodes=20,sizeMan=10,sizeMan2=5,shapeMan="rectangle",
         residScale=5,optimizeLatRes=T,curveAdjacent=T)
