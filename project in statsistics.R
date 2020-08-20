library(dplyr)
library(readr)
library(tidyr)
library(tidyverse)
library(readr)
library(Hmisc)
library("DescTools")
library(ggplot2)
library(car)
library(lattice)
library(outliers)
library(splines)
library(ggplot2)
require(emmeans)
library(emmeans)
require(multcomp)
library(multcomp)
library(haven)
#glwssa
Sys.setlocale("LC_CTYPE","Greek")
##erwtima 1
library(haven)
h1 <- read_sav("H.1.Mat_Tim.sav")
is.data.frame(h1)

str(h1)
class(h1$Material)
material1<-as.numeric(h1$Material)
f1<-factor(material1,labels=c("Low","Medium","High"))
typeof(f1)
attributes(f1)
unclass(f1)
h1$Material<-f1
#stoixeia gia olo to d.f. perigrafika

Desc(h1[,"Time"])
str(h1)
summary(h1)
levels(h1$Material)
class(h1$Material)
g <- function(x)c(N=length(x),MIN=min(x,na.rm=TRUE),MAX=max(x,na.rm=TRUE),
                  MEDIAN=median(x,na.rm=TRUE), MEAN=mean(x,na.rm=TRUE),
                  SD=sd(x,na.rm=TRUE))
by(h1$Time,h1$Material,FUN=g)

#erwtima2
attach(h1)
bx1<-with(h1,Boxplot(h1$Time~h1$Material,notch=F,plot=T,varwidth=TRUE,cex.axis=0.7,show.names=T,outline=T,
             cex.lab=1.5,pch=1,las=1,xaxs="i",xlab="Levels of Material",ylab="Time",col="lightgreen",
             id=list(labels=rownames(h1))))
bx1
#parakatw blepoume tis antistoixes times twn outliers sta group
bx11<-with(h1,boxplot(h1$Time~h1$Material,notch=F,plot=T,varwidth=TRUE,cex.axis=0.7,show.names=T,outline=T,
                cex.lab=1.5,pch=1,las=1,xaxs="i",xlab="Levels of Material",ylab="Time",col="lightgreen",
                id=list(labels=rownames(h1))))
bx11

#erwtima 3
##parakatw kanoume kapoious elegxous gia thn kanoniki 
##tha doume to istogramma gia thn time sunolika prin spasei stis kathgories ths material
with(h1,{hist(h1$Time,breaks="FD",
              freq=FALSE,ylab="Density",ylim=c(0,0.11))
             lines(density(h1$Time),lwd=3,col="red")
              lines(density(h1$Time,adjust=1.2),lwd=1,col="blue")
                  rug(Time)
                  box()})
histogram(~ h1$Time |h1$Material,data=h1,
              type="percent",equal.widths=T,
          col="pink",breaks = "FD",
          main="Histogram of Time for all levels of Material")
#diagramma gia optiko elegxo sthn kanoniki katanomi
qqPlot(h1$Time~h1$Material,data=h1,distribution="norm",
       layout=c(1,3),envelope=F)  

#parathroume oti den yparxei se olous tis stathmes tou paragonta prosarmogi sthn kanoniki katanomh 

dim(h1)
#tha kanoume elegxo Shapiro-Wilk, gt exoume n=45 <50 (alliws an htan n>50 ua pairname kolmogorov (ks.test))
i<-1
shapirodata<-list()
while(i<=3)
{
  k<-i
  x<-h1$Time[unclass(h1$Material)==k]
  shapirodata[[k]]<-shapiro.test(x)
  i<-i+1
}
#ta apotelesmata apo to shapiro einai:
i<-1
while(i<=3)
{
  k<-i
  print(
    levels(h1$Material)[k]);print(shapirodata[[k]])
  i<-i+1
}
#xreiazetai h library(car)
bartlett.test(h1$Time~h1$Material,data=h1)
#exoume omoiogeneia stis diaspores, dioti p-value = 0.1511>0.05, omws den exoume kanonikh katanomh
#ara o pio aksiopistos elegxos einai to Levene 's Test
leveneTest(h1$Time~h1$Material,data=h1,center=mean)
leveneTest(h1$Time~h1$Material,data=h1,center=median)
#ara apo ola ta test blepw oti exw  omoiogeneia stis diaspores

#erwtima5
#tha kanoume elegxo Box-Cox
bc1<-powerTransform(h1$Time~h1$Material,family="bcPower",h1)
summary(bc1)
#ara tha paroume ton metasxhmatismo log(x),giati einai polu konta sto 0
spreadLevelPlot(h1$Time~h1$Material,data=h1)
#To idio prokuptei ki apo to parapanw
h11<-transform(h1,lnTime=log(Time))
View(h11)
library(splines)
detach(h1)

#tha eleksoume an exoume kanonikothta
i<-1
shapirodata1<-list()
while(i<=3)
{
  k<-i
  x<-h11$lnTime[unclass(h11$Material)==k]
  shapirodata1[[k]]<-shapiro.test(x)
  i<-i+1
}
shapirodata1
#ta apotelesmata apo to shapiro einai:
i<-1
while(i<=3)
{
  k<-i
  print(
    levels(h11$Material)[k]);print(shapirodata1[[k]])
  i<-i+1
}
##tha eleksoume ki pali thn omoiogeneia ths diasporas
bartlett.test(h11$lnTime~h11$Material,data=h11)
#exoume omoiogeneia stis diaspores, dioti p-value = 0.4063>0.05
#ara o pio aksiopistos elegxos einai to Levene 's Test
leveneTest(h11$lnTime~h11$Material,data=h11,center=mean)
#ara blepoume oti ki pali exoume omoiogeneia, dioti Pr(>F)=0.8512>0.05
leveneTest(h11$lnTime~h11$Material,data=h11,center=median)
#ara blepoume oti ki pali exoume omoiogeneia, dioti Pr(>F)= 0.8353>0.05
detach
#tha doume ki optika an isxuei h kanonikh katanomi kai h omoiogeneia twn diasporwn
bx2<-with(h11,Boxplot(h11$lnTime~h11$Material,notch=F,plot=T,varwidth=TRUE,cex.axis=0.7,show.names=T,outline=T,
                     cex.lab=1.5,pch=1,las=1,xaxs="i",xlab="Levels of Material",ylab="Time",col="lightgreen",
                     id=list(labels=rownames(h1))))
bx2
##
par(mfrow=c(1,2))
with(h11,{hist(h11$lnTime,breaks="FD",
              freq=FALSE,ylab="Density",ylim=c(0,1),xlim=c(0,5))
  lines(density(h11$lnTime),lwd=3,col="red")
  lines(density(h11$lnTime,adjust=1.2),lwd=1,col="blue")
  rug(lnTime)
  box()})
#diagramma gia optiko elegxo sthn kanoniki katanomi
qqPlot(h11$lnTime~h11$Material,data=h1,distribution="norm",
       layout=c(1,3),envelope=F)  
library(outliers)
#erwtima 6
names(h11)
model1<-aov(h11$lnTime~h11$Material,h11) 
model1
summary.lm(model1)
#blepoume oti Pr(>F)=0.00113<0.05.Synepws h epidrasi toy paragonta material einai statistika shmantiki.

#######erwtima 8
#parakatw tha elenksoyme thn kanonikothta twn upoloipwn tou montelou
shapiro.test(model1$res)
#parathroume oti to  p-value = 0.2941>0.05, ara h mhdeniki upothesi, dhladh h kanonikothta ginetai apodekth
#kanoume kai elegxo gia kolmogorov-Smirnov
ks.test(model1$res,mean(model1$res),sd(model1$res))
mean(model1$res)
#p-value = 0.8696>0.05,ara kai pali exoume kanoniki

##kanoume optiko elegxo gia ta parapanw
par(mfrow=c(2,2))
plot(model1)
par(mfrow=c(1,1))
qqnorm(model1$res)
qqline(model1$res)
qqPlot(model1$res,distribution="norm",
       layout=c(2,4),envelope=FALSE,
       main="?djustment to norm distribution")
#exoume kanoniki, den xreiazetai na ta eksairesoume

##parakatw blepoumme to diagranna timwn kataloipwn gia na doume an fainetai grafika an h mesi timi einai 0 kai uparxei statheri diaspora
plot(model1$res,main="Values of residuals")
abline(lm(model1$residuals~model1$fitted.values),col="red")

##erwtima 7
require(emmeans)
library(emmeans)
attach(h11)
pairw1<-emmeans(model1,pairwise ~ Material)
pairw1
#eidame oti diaferoun oi High kai Low, High-Medium, enw oi (Medium kai Low) den diaferoun
#blepoume ki ta diastimata empistosunhs gia tis ana 2 diafores
TukeyHSD(model1)
#diastiamta empistosinis gia tis meses times
plot(TukeyHSD(model1))
by(h11[,"lnTime"],h11$Material,mean)
#blepoume gia poies stathmes exoume epikalupseis
plot(pairw1)

##########################################################################################
###################   provlima 2                       ####################################
###########################################################################################

#eisagoume to dataset
reg<-read.table("cars1920.txt",header = T)
reg

is.data.frame(reg)  #elegxoume an einai data frame
#perigrafika metra 
str(reg)
summary(reg)#περιγραφικά μετρα για καθε μεταβλητη ξεχωριστα
summary(reg$speed)#περιγραφικά μετρα για το Distance_mm
dim(reg)                    

names(reg) 
apply(reg,MARGIN=2,"class") # h alliws: apply(regression,MARGIN=2,mode)

#############################################################################
######################de to kaname 

# Μετατροπή ποσοτικής μεταβλητής σε παράγοντα (Ordered factor)
reg$speed  # οι τιμές είναι σε άυξουσα διάταξη
fspeed<- as.ordered(reg$speed)
fspeed
View(fspeed)
levels(fspeed) <-seq(1:19)
fspeed
# brhkame poses times epanalambanontai me thn 
length(table(reg$speed))
# Προσθήκη της νέας μεταβλητής (στήλης) στο πλαίδιο δεδομένων
reg1 <- cbind.data.frame(reg, fspeed)
View(reg1)
#######################################################################################
#######################################################################################

# διάγραμμα διασποράς και ευθεία γραμμικής παλινδρόμης
library(ggplot2)
ggplot(data = reg) +
  geom_point(mapping = aes(x =reg$speed, y =reg$dist), shape=21, size = 3) +
  labs(x  = "speed",  y  = "dist")  +
  geom_smooth(mapping = aes(x =reg$speed, y =reg$dist ),method  = "lm",  se = FALSE)+
  labs(title = "Diagramma diasporas", subtitle = NULL)
#blepoume oti nai men h eutheia prosarmozetai alla uparxoun sfalmata logw twn epanalhpsevn pou tha melethsoume parakatw
lm1<- lm(reg$dist ~ reg$speed, reg)
names(lm1)
summary(lm1)
re<-residuals(lm1)

#blepoume oti Multiple R-squared=0.6511, pou deixnei thn poso kala to grammiko montelo prosarmozetai sta dedomena
#fainetai oti o suntelesths a einai statistika shmantikos ki exei ektimwmeni timi 3.9324
#to idio kai h stathera b einai statistika shmantiki kai exei ektimwmeni timi -17.5791
#y=3.9324 x+-17.5791
require(multcomp)
library(multcomp)
#diastima empistosunhs gia tous suntelestes
confint(lm1)
#elegxw thn kanonikothta twn upoloipwn optika

#1. Έλεγχος προσαρμογής των υπολοίπων στην κανονική κατανομή
##to QQplot gia ta upoloipa tou montelou einai:
par(mfrow=c(1,1))
qqnorm(residuals(lm1),ylab="Residuals",main="") #δημιουργει τα σημεια
qqline(residuals(lm1))    #κανει την ευθεια παλινδρομησης
##to istogramma gia ta upoloipa tou montelou einai:
hist(residuals(lm1),xlab="Residuals",main="")
shapiro.test(lm1$res)
#blepoume oti h upothesi ths kanonikothtas twn upoloipwn aporriptetai
library(nortest)
r<-rstandard(lm1)
lillie.test(r)
#ksekiname me metasxhmatismou logarithmou(sel 259 biblio karlis tzoufras), gia na doume an tha ikanopoihthei h kanonikothta twn upoloipwn, alliws proxwrame se metasxhmatismo tupou box kox
reg$logdist<-log(reg$dist)
lm2<- lm(reg$logdist ~ reg$speed, reg)
lm2
#logdist=1.67612+0.12077(speed)=> dist=exp(1.67612)*([exp(0.12077)]^speed)
summary(lm2)
shapiro.test(lm2$res)

#efoson twra isxuei h kanonikothta tha kanoume ki ptiko elegxo gia thn omoskedastikothta twn upoloipwn

plot(rstudent(lm2),fitted(lm2),xlab="studentized residuals",ylab="fitted values")

#Σταθερή διασπορά (ομοιογένεια της διασποράς των υπολοίπων για το μοντελο lm2)
par(mfrow = c(1,3))
plot(x=fitted(lm2),
     y=residuals(lm2),
     xlab="Fitted",
     ylab="Residuals")
abline(h=0)
lm2
plot(lm2, which = 1) 
plot(lm2, which = 3)
par(mfrow = c(1,1))
#blepoume oti den exoume kapoio susthmatiko tropo sumperiforas twn shmeiwn, opote dexomaste thn upothesi ths omodkedastikothtas
##to QQplot gia to grammiko montelo lm2
plot(lm2, which = 2)
#blepoume oti den exoume kapoio susthmatiko tropo sumperiforas twn shmeiwn, opote dexomaste thn upothesi ths omodkedastikothtas



##Αn proxwrousame se metasxhmatismo box kox, tote
# h proteinomeni dunami metasximatismou einai h 1/2, opote tha pairname th riza tou dist
bc2<-powerTransform(reg$dist~reg$speed,family="bcPower",reg)
bc2
summary(bc2)

reg$sqrdist<-sqrt(reg$dist)
lm3<- lm(reg$sqrdist ~ reg$speed, reg)
lm3
#blepoume oti h prosarmogi twn upoloipwn sthn kanoniki ikanopoieitai ki pali opws kai me to logarithmo
shapiro.test(lm3$res)

#####erwtima 4
##tha sunexisoume thn analush me to montelo lm2, opou h metablhth apokrishs einai h logdist
#erwthsh 4 Έλεγχος αυτοσυσχέτισης 1ης τάξης των σφαλμάτων (Durbin-Watson test)
n <- length(residuals(lm2))
plot(tail(residuals(lm2),n-1) ~ head(residuals(lm2),n-1), xlab=
       expression(hat(epsilon)[i]),ylab=expression(hat(epsilon)[i+1]))
abline(h=0,v=0,col=grey(0.75))
#Από το παραπάνω γράφημα έχω μια πρώτη εικόνα ότι τα υπόλοιπα είναι ασυσχέτιστα.

require(lmtest)
dwtest(reg$logdist ~speed, data=reg)
#blepoume oti  p-value = 0.2194>0,05, ara ta upoloipa exoun mhdenikh autosusxetisi prwtis taksis
###erwtima 5
#4 Εύρεση ασυνήθιστων παρατηρήσεων (Mahalanobis)

# Σημεία με απόσταση Mahalanobis > 2p/n πρέπει να ερευνώνται περεταίρω
lm2<- lm(reg$logdist ~ reg$speed, reg)
hatv <- hatvalues(lm2)  # Mahalanobis
sum(hatv) 
mah <- 2*2/50   
plot(hatv,ylab="Leverages",main="Index plot of Leverages")
abline(h = mah)

hatv[hatv > mah]
#Άρα, σύμφωνα με την απόσταση Mahalanobis προέκυψαν οι 1,2,50
#ως αυνήθιστες παρατηρήσεις.

# 5. Cook's distance 
cook <- signif(cooks.distance(lm2))
cook
cook[cook>4/50]
# Με χρήση αυτόματης συνάρτησης από τη διαδικασία της παλινδρόμησης
plot(lm2, which = 4)
#Σύμφωνα με την απόσταση του Cook προέκυψαν ως ασυνήθιστες παρατήρησεις οι 1,3,4.

##added variable plots
avPlots(lm2,ask=FALSE,id.method="identify",main="Added-Variable Plots")

library(car)
influencePlot(lm2,id.method="indetify",main="Influence Plot",sub="Circle Size is proportional to Cook's distance")


# Χρησιμοποιώντας αυτοματη συνάρτηση της R
# που περιέχει και άλλα μέτρα διερεύνησης παράτυπων
# σημείων που επηρεάζουν τους συντελεστές της γραμ.παλινδρ.
im <- influence.measures(lm2)
summary(im)
im$is.inf
which(apply(im$is.inf,1,any))
plot(reg$logdist,reg$speed)

#erwtima 6

reg$speed  # οι τιμές είναι σε άυξουσα διάταξη
fspeed<- as.ordered(reg$speed)
fspeed
View(fspeed)
levels(fspeed) <-seq(1:19)
fspeed
# brhkame poses times epanalambanontai me thn 
tb<-table(reg$speed)
tb1<-as.data.frame(tb)
colnames(tb1)<-c("parathrhseis","suxnothta")
#o pinakas suxnothtwn einai
tb1

###erwtima 7

lm2f <- lm(reg$logdist~ fspeed, reg)
summary(lm2f)#Multiple R-squared:  0.8021#me tiw epanalipseis
anova(lm2, lm2f)
summary(lm2)#Multiple R-squared:  0.6763#xwris tiw epanalipseis
#blepw oti den uparxei sathstika shmantiki diafora twn upoloipwn

# pure error (διασπορά που εξηγείται από τις επαναλήψεις)
#model1
sqrt(9.5621/48)##0.4463299
#model2
sqrt(5.8450/31)##0.4342216


lm22<- lm(reg$logdist~reg$speed+I(reg$speed^2),reg)
summary(lm22)

anova(lm2f, lm22)
#efoson den diaferoun stathstika ta 2 montela,
#epilegoume to pio aplo montelo dhladh to lm2f

lm13<- lm(reg$logdist~reg$speed+I(reg$speed^13),reg)
summary(lm13)
anova(lm2f,lm13)

lmsun<-lm(reg$logdist~reg$speed+I(reg$speed^13)+I(reg$speed^2),reg)
summary(lmsun)
anova(lmsun,lm2f)
require(lmtest)

##erwtima 9
# 1. Έλεγχος προσαρμογής των υπολοίπων στην κανονική κατανομή

par(mfrow=c(1,2))#δημιουργει παραθυρο με 1 γραμμη και 2 στηλες και θα βαλω 2 γρ.παρ.
qqnorm(residuals(lm2f),ylab="Residuals",main="") 
qqline(residuals(lm2f))    
hist(residuals(lm2f),xlab="Residuals",main="")
par(mfrow = c(1,1))
shapiro.test(residuals(lm2f))# p-value = 0.3086>0,05, ara exoume prosarmogi sthn kanonikh katanomh
ks.test(residuals(lm2f),mean(residuals(lm2f)),sd(residuals(lm2f)))#p-value = 1>0,05,opote kai paliprosarmogi sthn kanoniki

#parakatw tha kanoume elegxo gia na doume ean ta upoloipa exoun statheri diaspora
par(mfrow = c(1,3))
plot(x=fitted(lm2f),
     y=residuals(lm2f),
     xlab="Fitted",
     ylab="Residuals")
abline(h=0)
lm2f
plot(lm2f, which = 1) 
plot(lm2f, which = 3)
par(mfrow = c(1,1))
#blepoume oti den exoume kapoio susthmatiko tropo sumperiforas twn shmeiwn, opote dexomaste thn upothesi ths omodkedastikothtas

# 3 Έλεγχος αυτοσυσχέτισης 1ης τάξης των σφαλμάτων (Durbin-Watson test)

n <- length(residuals(lm2f))
plot(tail(residuals(lm2f),n-1) ~ head(residuals(lm2f),n-1),
     xlab=expression(hat(epsilon)[i]),
     ylab=expression(hat(epsilon)[i+1]))
abline(h=0,v=0,col=grey(0.75))
require(lmtest)
dwtest(reg$logdist ~fspeed, data=reg)
#apo to Durbin-Watson test proekupse oti ta upoloipa tou montelou exoun mhdenikh autosusxetisi prwths taksis

##gia to montelo lm2f
summary(lm2f)
#logdist=3.434062+3.119536*fspeed=>dist=exp(3.434062)*exp(3.119536*fspeed)
confint(lm2f)
#ara to de gia th stathera tou montelou lm2f
#                2.5 %      97.5 %
#  (Intercept)  3.2882717  3.57985186
##to de gia thn parametro lm2f tou montelou einai
#(2.4024166  3.83665473)

#######################################################################################
####################################################################################
###################                provlima  3             #########################
##################################################################################
###################################################################################
library(haven)
ColorStudyL <- read_sav("ColorStudyL.sav")
View(ColorStudyL)
is.data.frame(ColorStudyL)
csl<-as.data.frame(ColorStudyL)
csl

 #dhmiourgoume th sthlh COMBINATIONS 
 COMBINATIONS<-matrix(0,0,nrow=240,ncol=1)
 k<-1 
 l<-0
 i<-0
 while (k<241)
 {  i<-i+1
 l<-1
 for (l in 1:15)
 {
   COMBINATIONS[k]<-i
   k<-k+1
 }
 }  
 
 csl<-cbind.data.frame(csl,COMBINATIONS)
 csl
 
csl1<-csl
csl$Material
str(csl$Material)

material1<-as.numeric(csl$Material)
f1<-factor(material1,labels=c("Bruxzir","Prettau","Katana","e*max"))
typeof(f1)
attributes(f1)
unclass(f1)
csl1$Material<-f1
csl1$Material



csl$Solution
str(csl$Solution)

sol1<-as.numeric(csl$Solution)
f2<-factor(sol1,labels=c( "Tea","Coffee","Wine","Aging"))
typeof(f2)
attributes(f2)
unclass(f2)
csl1$Solution<-f2
csl1$Solution

nm<-as.numeric(csl$COMBINATIONS)
nm
f3<-factor(nm,labels = c("Bruxzir Tea",
                         "Bruxzir Coffee",
                         "Bruxzir Wine",
                         "Bruxzir Aging",
                         "Prettau Tea",
                         "Prettau Coffee",
                         "Prettau Wine",
                         "Prettau Aging",
                         "Katana Tea",
                         "Katana Coffee",
                         "Katana Wine",
                         "Katana Aging",
                         "e*max Tea",
                         "e*max Coffee",
                         "e*max Wine",
                         "e*max Aging"))
 attributes(f3)
unclass(f3)
csl1$COMBINATIONS<-f3
View(csl1)
class(csl1$COMBINATIONS)
csl2<-as.data.frame(csl1)
#perigrafika stoixeia gia olo to data frame
summary(csl2)
##tha eksetasoume tis proupotheseis tou montelou gia na efarmosoume thn anova
by(csl2[,"L"],csl2$COMBINATIONS,summary)
names(csl2)
attach(csl2)
##kanoume ta thhkogrammata gia thn parametro tou xrwmatos gia olous tous sunduasmous twn paragontwn
par(mfrow=c(1,1))
with(csl2,Boxplot(L~Material*Solution,
                  notch=F,outline=T,col="lightgreen",
                  show.names=T,pch=1,
                  xlab="COMBINATIONS",ylab="L",
                  main="Boxplot of L for all combinations",
                  xaxs="i",id=list(labels=rownames(csl2))))
#parathroume oti oi diaspores gia ta diafora thhkogrammata einai anises

##parakatw tha kanoume elegxo kanonikothtas gia kathe deigma pou antistoixei stous  diaforous sunduasmous pou exoumeQ
histogram(~L|COMBINATIONS,data=csl2,type="percent",breaks="FD",col="red",main="Histogram for all combinations")
i<-1
par(mfrow=c(2,4))
while(i<=8)
{
  k<-i
  x<-csl2$COMBINATIONS[k]
  qqnorm(csl2$L[unclass(csl2$COMBINATIONS)==k],ylab="combination",main="qqplot")
  qqline(csl2$L[unclass(csl2$COMBINATIONS)==k])
  i<-i+1
}
##parakatw kanoume to qqplot gia touw prwtous 8 sunduasmous twn stathmwn Solution-Material
i<-1
par(mfrow=c(2,4))
while(i<=8)
{
  k<-i
  qqnorm(csl2$L[unclass(csl2$COMBINATIONS)==k],ylab="combination",main="qqplot")
  qqline(csl2$L[unclass(csl2$COMBINATIONS)==k])
  i<-i+1
}
##parakatw kanoume to qqplot gia tous upoloipous 8 sunduasmous twn stathmwn Solution-Material
#to spasame se 2 oxtades gia na fainontai kalutera
par(mfrow=c(2,4))
for(i in 9:16)
{
  k<-i
  qqnorm(csl2$L[unclass(csl2$COMBINATIONS)==k],ylab="combination",main="qqplot")
  qqline(csl2$L[unclass(csl2$COMBINATIONS)==k])
}
###to qqplot gia tous 16 sunduasmous einai:
qqPlot(L~COMBINATIONS,data=csl2,distribution="norm",layout=c(3,6),envelope=F)

#akolouthei elegxos kanonikothtas gia kathe sunduasmo twn stathmwn twn 2 paragontwn me shapiro
i<-1
shapirocsl1<-list()
while(i<=16)
{
  k<-i
 x<-csl2$L[unclass(csl2$COMBINATIONS)==k]
 shapirocsl1[[k]]<-shapiro.test(x)
 i<-i+1
}

i<-1
while(i<=16)
{
  k<-i
  print(
    levels(csl2$COMBINATIONS)[k]);print(shapirocsl1[[k]])
  i<-i+1
}
#blepoume gia olous tous sunduasmous  oti akolouthoun kanoniki giati den aporriptetai h ho me stathmi shmantikothtas 5%
library(car)
bartlett.test(L~COMBINATIONS,data=csl2)
#opote h Ho aporriptetai dioti p-value=9.634e-05<0.05, ara dexomaste oti exoume anomoiogeneia twn diasporwn
leveneTest(L~COMBINATIONS,data=csl2,center=mean)
#ki edw aporriptetai h upothesi ths omoskedastikothta, dioti 0.0005528<0,05
leveneTest(L~COMBINATIONS,data=csl2,center=median)
##ki edw aporriptetai h upothesi ths omoskedastikothta, dioti 0.0005528<0,05
#elegxos gia na mas proteinei kapoio metasximatismo 
spreadLevelPlot(L~COMBINATIONS,data=csl2)
#afou h dynami poy ptoteinetai apo ton parapanw elegxo einai megalyteri
#ths apolyths timhs toy 2, tha paroume kapoio metasxhmatismo apo ton Box-Cox
bc3<-powerTransform(csl2$L~csl2$COMBINATIONS,family="bcPower",csl2)
summary(bc3)

csl2$Ltr=L^(-1)
csl2
#apo to box cok protathhke o metasxhmathsmos me dunami -1
bartlett.test(Ltr~COMBINATIONS,data=csl2)
leveneTest(Ltr~COMBINATIONS,data=csl2,center=mean)
leveneTest(Ltr~COMBINATIONS,data=csl2,center=median)
#ara kai pali aporritetoi h ho
csl2$l1<-L^(-4)
bartlett.test(l1~COMBINATIONS,data=csl2)
leveneTest(l1~COMBINATIONS,data=csl2,center=mean)
leveneTest(l1~COMBINATIONS,data=csl2,center=median)
View(csl2)

bartlett.test(logL~COMBINATIONS,data=csl2)
leveneTest(logL~COMBINATIONS,data=csl2,center=mean)
leveneTest(logL~COMBINATIONS,data=csl2,center=median)
#me kanenan metasxhmatismo den mporesame na diorthosoume thn anomoiogeneia twn metasxhmatismwn
#oute me to spreadlevel plot, alla oute ki me box cox

cochran.test(L~COMBINATIONS,csl2)
i<-1
dixonC1<-list()
while(i<=16)
{
  k<-i
  x<-csl2$L[unclass(csl2$COMBINATIONS)==k]
  dixonC1[[k]]<-dixon.test(x,type=0,opposite=FALSE,two.sided=TRUE)
  i<-i+1
}
dixonC1
i<-1
bgrubb1<-list()
while(i<=16)
{
  k<-i
  x<-csl2$L[unclass(csl2$COMBINATIONS)==k]
  bgrubb1[[k]]<-grubbs.test(x,type=10,opposite=FALSE,two.sided=TRUE)
  i<-i+1
}
bgrubb1
csl22<-csl2[-c(220),]
View(csl22)
table(csl22$COMBINATIONS=="e*max Wine")
i<-1
shapirocsl1<-list()
while(i<=16)
{
  k<-i
  x<-csl22$L[unclass(csl22$COMBINATIONS)==k]
  shapirocsl1[[k]]<-shapiro.test(x)
  i<-i+1
}

i<-1
while(i<=16)
{
  k<-i
  print(
    levels(csl22$COMBINATIONS)[k]);print(shapirocsl1[[k]])
  i<-i+1
}
#blepoume gia olous tous sunduasmous  oti akolouthoun kanoniki giati den aporriptetai h ho me stathmi shmantikothtas 5%
library(car)
bartlett.test(L~COMBINATIONS,data=csl22)
leveneTest(L~COMBINATIONS,data=csl22,center=mean)
leveneTest(L~COMBINATIONS,data=csl22,center=median)
#pali den exoume omoiogeneia parolo pou afairesame to 220 pou emoiaze gia paratupo

csl23<-csl2[-c(222),]
View(csl23)
table(csl23$COMBINATIONS=="e*max Wine")
i<-1
shapirocsl1<-list()
while(i<=16)
{
  k<-i
  x<-csl23$L[unclass(csl23$COMBINATIONS)==k]
  shapirocsl1[[k]]<-shapiro.test(x)
  i<-i+1
}

i<-1
while(i<=16)
{
  k<-i
  print(
    levels(csl23$COMBINATIONS)[k]);print(shapirocsl1[[k]])
  i<-i+1
}
#blepoume gia olous tous sunduasmous  oti akolouthoun kanoniki giati den aporriptetai h ho me stathmi shmantikothtas 5%
library(car)
bartlett.test(L~COMBINATIONS,data=csl23)
leveneTest(L~COMBINATIONS,data=csl23,center=mean)
leveneTest(L~COMBINATIONS,data=csl23,center=median)  
#pali me thn afairesh ths 222 den allakse h anomoiogeneia stiw diaspores
csl24<-csl2[-c(2),]
View(csl24)
table(csl24$COMBINATIONS=="Bruxzir Tea")
i<-1
shapirocsl1<-list()
while(i<=16)
{
  k<-i
  x<-csl24$L[unclass(csl24$COMBINATIONS)==k]
  shapirocsl1[[k]]<-shapiro.test(x)
  i<-i+1
}

i<-1
while(i<=16)
{
  k<-i
  print(
    levels(csl24$COMBINATIONS)[k]);print(shapirocsl1[[k]])
  i<-i+1
}
#blepoume gia olous tous sunduasmous  oti akolouthoun kanoniki giati den aporriptetai h ho me stathmi shmantikothtas 5%
library(car)
bartlett.test(L~COMBINATIONS,data=csl24)
leveneTest(L~COMBINATIONS,data=csl24,center=mean)
leveneTest(L~COMBINATIONS,data=csl24,center=median)
#ara oute apo thn afairesg tou 2 diorthwthke h anomoiogeneia twn diasporwn

csl25<-csl2[-c(11),]
View(csl25)
table(csl25$COMBINATIONS=="Bruxzir Tea")
i<-1
shapirocsl1<-list()
while(i<=16)
{
  k<-i
  x<-csl25$L[unclass(csl25$COMBINATIONS)==k]
  shapirocsl1[[k]]<-shapiro.test(x)
  i<-i+1
}

i<-1
while(i<=16)
{
  k<-i
  print(
    levels(csl25$COMBINATIONS)[k]);print(shapirocsl1[[k]])
  i<-i+1
}
#blepoume gia olous tous sunduasmous  oti akolouthoun kanoniki giati den aporriptetai h ho me stathmi shmantikothtas 5%
library(car)
bartlett.test(L~COMBINATIONS,data=csl25)
leveneTest(L~COMBINATIONS,data=csl25,center=mean)
leveneTest(L~COMBINATIONS,data=csl25,center=median)
#ara oute apo thn afairesg tou 2 diorthwthke h anomoiogeneia twn diasporwn

csl26<-csl2[-c(220,222),]

table(csl24$COMBINATIONS=="e*max Wine")
i<-1
shapirocsl1<-list()
while(i<=16)
{
  k<-i
  x<-csl26$L[unclass(csl26$COMBINATIONS)==k]
  shapirocsl1[[k]]<-shapiro.test(x)
  i<-i+1
}

i<-1
while(i<=16)
{
  k<-i
  print(
    levels(csl26$COMBINATIONS)[k]);print(shapirocsl1[[k]])
  i<-i+1
}
#blepoume gia olous tous sunduasmous  oti akolouthoun kanoniki giati den aporriptetai h ho me stathmi shmantikothtas 5%
library(car)
bartlett.test(L~COMBINATIONS,data=csl26)
leveneTest(L~COMBINATIONS,data=csl26,center=mean)
leveneTest(L~COMBINATIONS,data=csl26,center=median)
#ara oute apo thn afairesg tou 2 diorthwthke h anomoiogeneia twn diasporwn

csl27<-csl2[-c(11,2),]
View(csl27)
table(csl27$COMBINATIONS=="Bruxzir Tea")
i<-1
shapirocsl1<-list()
while(i<=16)
{
  k<-i
  x<-csl27$L[unclass(csl27$COMBINATIONS)==k]
  shapirocsl1[[k]]<-shapiro.test(x)
  i<-i+1
}

i<-1
while(i<=16)
{
  k<-i
  print(
    levels(csl27$COMBINATIONS)[k]);print(shapirocsl1[[k]])
  i<-i+1
}
#blepoume gia olous tous sunduasmous  oti akolouthoun kanoniki giati den aporriptetai h ho me stathmi shmantikothtas 5%
library(car)
bartlett.test(L~COMBINATIONS,data=csl27)
leveneTest(L~COMBINATIONS,data=csl27,center=mean)
leveneTest(L~COMBINATIONS,data=csl27,center=median)

csl28<-csl2[-c(11,2,220,222),]

i<-1
shapirocsl1<-list()
while(i<=16)
{
  k<-i
  x<-csl28$L[unclass(csl28$COMBINATIONS)==k]
  shapirocsl1[[k]]<-shapiro.test(x)
  i<-i+1
}

i<-1
while(i<=16)
{
  k<-i
  print(
    levels(csl28$COMBINATIONS)[k]);print(shapirocsl1[[k]])
  i<-i+1
}
#blepoume gia olous tous sunduasmous  oti akolouthoun kanoniki giati den aporriptetai h ho me stathmi shmantikothtas 5%
library(car)
bartlett.test(L~COMBINATIONS,data=csl28)
leveneTest(L~COMBINATIONS,data=csl28,center=mean)
leveneTest(L~COMBINATIONS,data=csl28,center=median)

#tha kanoume analush diasporas
attach(csl2)
ano1<-aov(L~Material+Solution+Material:Solution,data=csl2)
summary(ano1)
model1<-lm(L~Material+Solution+Material:Solution)
summary(model1)
confint(model1)

#blepoume oti exoume statistika shmantikes kuries epidraseis alla 
#kai statistika shmantiki allilepidrasi, opote to montelo mas de mporei 
#na aplopoihthei. Ara, h sxesh metaksi kathe paragonta kai ths apokrisis L 
#diaferei apo epipedo se epipedo tou allou paragonta, gi ayto kai tha kanoyme 
#ana dyo sygkriseis 
#to R^2 deixnei to pososto ths apokrishs pou ekshgeitai apo to montelo 
anova(ano1)
#blepoume tous ektimhtes twn epidrasewn kathe kathgotrias
model.tables(ano1,type="effects",level=0.95)


# parakatw ginetai elegxos kanonikothtas twn upoloipen tou montelou
#kanoume elegxo Kolmogorov Smirnov, dioti  (N=240>50)
#ara isxuei h upothesi ths kanonikothtas isxuei
ks.test(ano1$res, mean(ano1$res), sdano1$res)
#ara h upothesi ths kanonikothtas isxuei afou p-value = 0.9646>0.05
#parakatw epivevaiwnetai ki to idio sumperasma kai optika
par(mfrow=c(1,1))
qqnorm(ano1$res)
qqline(ano1$res)
#emfanisi kai twn pithanwn paratupwn
qqPlot(ano1$res,  distribution="norm", 
       layout=c(2,4), envelope=FALSE,main="Adjustment to norm distribution")

#kanoume to diagramma twn kataloipwn gia na doume ean fainetai graphika na exoume mesi timh 0 kai statheri diaspora
plot(ano1$fitted.values, ano1$residuals, xlab="fitted values", ylab="residuals")
abline(lm(ano1$residuals ~ ano1$fitted.values), col="red")
#parathroume oti den uparxei kapoios susthmatikos tropos sumperiforas twn upoloipwn ,
#opote exoume statheri diaspora twn upoloipwn
#parakatw vlepoume oti ta sfalmata den parousiazoun autosusxetisi prwtis taksis
durbinWatsonTest(ano1)

#logw allhlepidrasewn, tha kanoume ana 2 sugkriseis
library(emmeans)
intervals(ano1)

#blepoume ta de epidrasewn kathe kathgorias
x<-ano1$effects
model.tables(ano1,type="means")#meses times ana kathgoria
##h allhlepidrash twn 2 paragontwn epivevaiwnetai kai apo ta parakatw 2 diagrammata
par(mfrow=c(1,1))
interaction.plot(Material,Solution,L)
interaction.plot(Solution,Material,L)
#epipleon grafikes epidraseis twn 2 paragontwn
plot.design(L~Material+Solution+Material:Solution)

#efoson exoume allhlepidraseis tha kanoume ana 2 sugkriseis , gia na doume ths epidraseis twn stahmwn kathe kathe paragonta se kathe stahmi tou kathe paragonta
em<-emmeans(ano1,pairwise~Solution+Material+Solution:Material)
em
contrast(em,"pairwise")
#blepoume ta 95% diasthmamta empistosunhs tis meses times olwn twn sunduasmwn twn stathmwn twn 2 paragontwn
###sto parakatw tha dwsoume emfash ston tropo pou epidra to sulution se kathe stathmi tou Material
##koitame tis epikalupseis twn diasthmatwn gia na doume an exoun satistika shmantikh diafora
#px eite exw Wine eite Coffee kai 
plot(em,comparisons=TRUE)

##parakatw vlepoume akomh ena grafhma pou deixnei tis meses times
#kai ta antistoixa diastumata empistosunhs tous tous gia kathe sunduasmo
##twn statmwn tou material kai toy solution, gia na doume pws epidra 
#o kathe sundiasmos tvn matiria kai solution
#stin parametro toy xrwmatos L.
##parathroyme oti kathe staumh toy paragonta solution otan syndiastei me thn staumh katana toyparagonta material exei thn megaluterh epidrash sthn parametro toy xrwmatos(l).
require(effects)
plot(allEffects(ano1))
plot.design()

#parakatw vlepoume to grafhma pou deixnei pws epidra to Solution se kathe stathmi tou Material
ano2<-aov(L~Solution+Material+Solution:Material,data=csl2)
plot(allEffects(ano2))
plot.design()
#px vlepoume oti otan exoume tis stathmes Bruxzir kai Prettau, den uparxei kapoia statistika shmantiki diafora
#sthn parametro tou xrwmatos, opoia stathmi ki an paroume gia to solution,
#dioti ola ta diasthmata empistosunhs gia tis meses times epikaluptontai
##sto parakatw dinetai emfash sto tropo pou epidra h Material stis stathmes tou Solution
em1<-emmeans(ano1,pairwise~Material+Solution+Solution:Material)
plot(em1,comparisons=TRUE)

#ana 2 sugkriseis twn meswn timwn kai me th Methodo Tukey
TukeyHSD(ano1)
str(TukeyHSD(ano1))
par(mfrow=c(1,1))
#parakatw ginontai grafika ola ta diasthmata empistosunhs gia tis diafores twn meswn timwn pou prokuptoun apo tis ana 2 sugkriseis twn sunduasmwn apo tiw stathmes twn
#Material kai Solution
plot(TukeyHSD(ano1))





############################################################################
###############                problhma 4             #####################
########################################################################


# year.n
#  Ηλικία σε χρόνια (1,2,3,4,5,6,7,8) 

# DV
#  ruthmos vuthisis h anupswshs (se xiliosta ana etos) 
# και είναι η εξαρτημένη μεταβλητή 

# AGE
# hlikia ktiriou 

# id
# kwdikos ktiriou

#Ta ktiria einai 40 kai kanoume metriseis ana 8 eti ara exoume 320 metrhseis

library(haven)
library(nlme)
library(lme4)
library(Hmisc)
library(lattice)  
library(grid)
library(RLRsim)
library(car)
library(WWGbook)

def<-read.table("Def1.txt",header = T)
def
attach(def)

age.f <- factor(AGE)
# Το έτος χρησιμοποιείται ως σταθερός παράγοντας (fixed factor) αλλά και ως
# τυχαία συνεχής μεταβλητή 
year.f <- factor(year.n)

# Το συμπληρωμένο πλαίσιο δεδομένων.
def.updated <- data.frame(def, age.f, year.f)
head(def.updated, n = 10)

summary(year.f)#gia kathe xrono blepoume poses metrhseis dv kthriwn eginan
summary(year.f)/320#pososto tou aritmou metrhsewn
summary(age.f)#plhtos kthriiwn poy stin arxi ths erevnas eixan thn antistoixi hlhkia
summary(age.f)/320
table(age.f, year.f)
table(age.f, year.f)/320

#perigrafiki statistiki
g <- function(x)c(N=length(x),MIN=min(x,na.rm=TRUE),MAX=max(x,na.rm=TRUE),
                  MEDIAN=median(x,na.rm=TRUE), MEAN=mean(x,na.rm=TRUE),
                  SD=sd(x,na.rm=TRUE))
sum(is.na(DV))  # blepoume oti den exoume xamenes times
s1 <- summarize(DV,by=llist(year.f),g);
#parakatw dinoume ton pinaka perigrafikhs statistikhs ana etos gia to ruthmo vuthishs
s1 
s2 <- summarize(DV,by=llist(age.f),g);
s2
class(s2)
s1.1 <- within(s1, {
  MEAN <- round(MEAN, 1) 
  SD <- round(SD, 1)})
s1.1#strogulopoish se ena dekadiko psifio
View(s1.1)  

#   h synarthsh pou montelopoiei ta dedomena gia kathe ktirio
def.g1 <- groupedData(DV ~ year.n | id, 
                         outer = ~ age.f, data = def.updated)
def.g1#ta omadopoiei sth katallhlh morfh diaxronikwn dedomenwn
#oi diaxronikes kampules gia kathe kthrio

plot(def.g1, display = "id of building", outer = TRUE, aspect = 2, key = F,
     xlab = "Time (Years)", ylab = "DV", 
     main = "Individual Data by AGE Group") 
#sto parapanw diagramma blepoume to ruthmo vuthishs/anupswsis gia kathe arxiko hlikiako group kata thn 8ateh
##parathroume oti sta perissotera group pou prokuptoun apo thn arxikh hlikia tou kthriou, uparxei diafora sto ruthmo vuthiseis twn kthruwn sthn arxh ths metrhshs, me eksairesh kuriws to hlikiako group 309
# h synarthsh ayth montelopoiei ta dedomena gia kathe omada AGE Group
#vlepoume oti gia kathe kthrio exoume peripou mia fthinousa grammiki sxesh tou ruthmou vuthisis kai tou xronou, gia auto
#tha melethsoume grammika th sxesh me to xrono
def.g2 <- groupedData(DV ~ year.n | age.f, 
                         order.groups = F, data = def.updated)

plot(def.g2, display = "age", aspect = 2, key = F, 
     xlab = "Time (Years)", ylab = "DV", 
     main = "Mean Profiles by AGE Group")
#deixnei th mesh timh tou ruthmou buthisis twn kthriwn me to perasma tou xronou gia kathe hlikiako group


# ginetai proetoimasia gia grammikh palindromhsh ths DV ws synarthsh 
# toU xronou
# omadas AGE ,  
# allhlepidrash xronou kai AGE 
# epipleon symperilambanontai 2 tyxaies epidraseis 
# o tyxaios statheros oros(diafora metaksi twn kthriwn),
#  h tyxaia epidrash toy xronou

# (random coefficient model).

levels(age.f)<-seq(1:22)
year.2 <- year.n - 1
year.2sq <- year.2*year.2
age2 <- AGE
age2[AGE == 22] <- 0
for(i in 1:21)
{
age2[AGE == i] <- i
}
age2.f <- factor(age2)

def.updated <- subset(data.frame(def, age2.f, year.2), !is.na(DV))

# eidiki periptwsh omadopoihmenou plaisiou dedomenwn
def.grouped <- groupedData(DV ~ year.2 | id, data=def.updated, 
                              order.groups = F)

# to plhres montelo

model1.fit <- lme(DV ~ year.2  + age2.f +
                      year.2:age2.f , 
                    random = ~ year.2, method="REML",
                    data = def.grouped)

summary(model1.fit) #  den einai efikto na ektimhthoun oi parametroi toy

# gia to logo ayto diwxnoume apo to montelo to stathero oro pou afora thn tyxaia epidrash
#toy kthriou, "random = ~ year.2 - 1"
model2.fit <- lme(DV ~ year.2 + age2.f +
                      year.2:age2.f , 
                    random = ~ year.2-1, method="REML", 
                    data = def.grouped)

summary(model2.fit)
anova(model2.fit)

#vlepw oti o tuxaios paragontas year einai statistika shmantikos


#xrhsimopoioume th siadikasia ML ektimhshs
#gia na brethoun oi statistika shmantikoi statheroi paragontes

model2.ml.fit <- update(model2.fit, method = "ML")
summary(model2.ml.fit)
anova(model2.ml.fit)

#oloklhrwnoume th diadikasia eureshs monteloy, trexontas to beltisto sto opoio
# sto opoio katalhksame me th methodo "REML"
model3.ml.fit <- update(model2.ml.fit, 
                          fixed = ~ year.2+ age2.f + 
                            year.2:age2.f)
summary(model3.ml.fit)
anova(model3.ml.fit)
# sugkrinoume ta duo telika montela to model3.ml.fit kai  model2.ml.fit
anova(model2.ml.fit, model3.ml.fit)

#eida oti o statheros paragontas age 2 den einai satistika shmantikos,opote tha ton afairesw
model4.ml.fit<-update(model3.ml.fit, 
                   fixed = ~ year.2 +
                     year.2:age2.f)
#sugkrinw ta 2 montela me ton stathero paragonta age.2 kai xwris auton
anova(model4.ml.fit,model3.ml.fit)
#einai isodunama, logw  p-value=0.9958, ara krataw, to pio aplo dhladh xwris to stathero paragonta age.2.f
anova(model4.ml.fit)
#tha kanw to montelo xwris th statheri epidrasi year.2:age2.f 
model5.ml.fit<-update(model4.ml.fit, 
                      fixed = ~ year.2 )
summary(model5.ml.fit)
#sugrinw ta montela model5.ml.fit,model4.ml.fit
anova(model4.ml.fit,model5.ml.fit)
##ara einai isodunama, logw tou p-value= 0.1672>0.05 opote to teliko montelo einai model5.ml.fit
intervals(model5.ml.fit)

getVarCov(model5.ml.fit, individual="1", type="marginal")
model5.ml.fit$coefficients#oi parametroi tou montelou
fixef(model5.ml.fit)#oi statheres eidraseis tou montelou
#ara h ektimush tou ruthmou vuthishs me to perasma tou xronou einai:
fitted.values(model5.ml.fit)

# exw mia ektimhsh gia kathe omada xrhsimopoiwntas tous syntelestes tou montelou, dioti h AGE apodeixthhke mh statistika shmantikh, opote den exw diafora sth vuthish analoga me thn arxikh hlikia
curve( -0.02180253*x+0.09553327, 0, 8, xlab = "Year.n minus 1", 
      ylab = "Marginal Predicted DV", lty = 1, ylim=c(0,0.8), lwd = 2)

# h sunarthsh bohtha sthn anaparastash "augPred" ths perithwrias tis ektimwmenhs
# timi (timi omadas)kai ths atomikis .
plot(augPred(model5.ml.fit, level = 1),
     xlab="AGE minus 1", ylab="Predicted DV",
     key = list(lines = list(lty = c(1,1), col = c(28,6), 
                             lwd = c(1,1)), 
                text = list(c("marginal mean profile",
                              "subject-specific profile")), columns = 2))
#sto parapanw diagramma vlepoume thn prosarmogi tou montelou mas
#sta dedomena ousistika vlepoume to montelo provlepsis 
#ths vuthisis tou kthriou gia thn 8etia

#######diagnwstikoi elegxoi gia to montelo
qqnorm(model5.ml.fit,  abline=c(0,1))
model5.ml.fit$residuals
ks.test(model5.ml.fit$residuals,mean(model5.ml.fit$residuals),sd(model5.ml.fit$residuals))
#apo to opoio prokuptei oti dexomaste thn kanonikothta twn upoloipwn, dioti
#p-value = 0.9434>0.05

###parakatw vlepw oti den parousiazetai kapoiow susthmatikos tropos sumperigoras
##opote den exw problhma me thn omoiogeneia twn upoloipwn
plot(model5.ml.fit, resid(., type="p") ~ fitted(.), 
      aspect=2, abline=0)


####parakatw tha ginoun oi elegxoi twn proupothesewn twn upoloipwn ana omada
# diagramma diasporas upoloipwn
plot(model5.ml.fit, resid(., type="p") ~ fitted(.) | factor(AGE), 
     layout=c(4,6), aspect=2, abline=0)
#eyresh twn ktiriwn poy deixnoun megala upoloipa(se stathmi simantikotitas=0.05)
plot(model5.ml.fit, resid(., type="p") ~ fitted(.) | factor(AGE), 
     id = 0.05, layout=c(4,6), aspect=2, abline=0)

# optikos elegxos prosarmoghs sthn kanoniki katanomi
qqnorm(model5.ml.fit, ~resid(.) | factor(AGE), layout=c(4,6),
       aspect = 2, id = 0.05) 

# upoloipa omadwn
plot(model5.ml.fit, resid(.) ~ year.2, abline=0)
#apo to opoio vlepoume oti den exoume provlhma sthm diaspora, dioti den parousiazetai
#kapoios susthmatikos tropos sumperiforas

#gia thn kanonikothta twn tuxaiwn paragontwn 
# οπτικός έλεγχος για τους τυχαίους 
# παράγοντες
par(mfrow=c(1,1))
qqnorm(ranef(model5.ml.fit)[,1])
qqline(ranef(model5.ml.fit)[,1])
shapiro.test(ranef(model5.ml.fit)[,1]) 
# apo to opoio vlepoume oti oi tuxaioi paragontes akolouthoun kanoniki katanomh
#dioti to p-value = 0.8153>0.05, dhladh dexomaste upothesi ths kanonikothtas


