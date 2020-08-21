#Κάναμε εγκατάσταση το package ISLR
require(ISLR)
Carseats
str(Carseats)
??Carseats
summary(Carseats)
#Ορίζουμε σωστά τις μεταβλητές
# Η μεταβλητή Shelveloc είναι ordinal.
Carseats$ShelveLoc<-factor(Carseats$ShelveLoc, ordered = T, levels=c('Bad','Medium','Good'))
is.ordered(Carseats$ShelveLoc)
summary(Carseats$ShelveLoc)
str(Carseats)
#επισκόπηση με τις σωστά ορισμένες μεταβλητές
summary(Carseats)
# Ανάλυση για τις κατηγορικές μεταβλητές:
# Για το ShelveLoc
attach(Carseats)
###Πίνακας Συχνοτήτων

minShelveLoc01<-min(Sales[Urban=="No"])
maxShelveLoc01<-max(Sales[Urban=="No"])
rangeShelveLoc01<-maxShelveLoc01-minShelveLoc01 
result01<-rbind(mean01,median01,sd01,var01,skew01,rangeShelveLoc01,quantile01)
result01
mean11<-mean(Sales[Urban=="Yes"])
median11<-median(Sales[Urban=="Yes"])freq.Shel<-table(ShelveLoc)
freqShel<- as.data.frame(freq.Shel)
colnames(freqShel) <- c("Quality of Shelving Location","Frequency")
freqShel
relfreq.Shel<-prop.table(table(Carseats$ShelveLoc))
relfreqShel<-as.data.frame(relfreq.Shel)
colnames(relfreqShel)<-c("Quality of Shelving Location","Relative Frequency")
tab<-cbind(freqShel,relfreqShel[,2])
colnames(tab)<-c("Quality of Shelving Location","Frequency","Relative Frequency")
tab
###Barplot
bar.freq.Shel<-barplot(freq.Shel,main="Shelving Location", xlab="Quality of Shelving Location",ylab="Frequency",horiz = F,cex.names = 0.8,col = c('red','orange','green'))
### PieChart
label<-paste(names(freq.Shel),"\n",freq.Shel, sep="")
pie.freq.Shel<-pie(freq.Shel, col=c('red','orange','green'), main="Shelving Location",labels = label)
legend("topright",names(freq.Shel),fill=c('red','orange','green'))
# Για το Urban
###Πίνακας Συχνοτήτων
freq.ur<-table(Carseats$Urban)
frequr<-as.data.frame(freq.ur)
colnames(frequr)<-c("Urban","Frequency")
frequr
relfreq.Ur<-prop.table(table(Carseats$Urban))
relfreqUr<-as.data.frame(relfreq.Ur)
colnames(relfreqUr)<-c("Urban","Relative Frequency")
tab1<-cbind(frequr,relfreqUr[,2])
colnames(tab1)<-c("Urban","Frequency","Relative Frequency")
tab1
### Barplot
bar.freq.ur<-barplot(freq.ur,main="Is the Store Location Urban?",xlab="Answer",ylab = "Frequency", horiz=TRUE,cex.names=0.8,col=c("red","blue"))
###Pie Chart
label1<-paste(names(freq.ur),"\n",freq.ur,sep = "")
pie.freq.ur<-pie(freq.ur,col=c("red","blue"),main="Is The Store Location Urban?", labels=label1)
legend("topright",names(freq.ur),fill=c("red","blue"))               
#Για το US
freq.us<-table(Carseats$US)
frequs<-as.data.frame(freq.us)          
colnames(frequs)<-c("US","Frequency")
levels(frequs$US)<-c("No","Yes")
frequs
relfreq.Us<-prop.table(table(Carseats$US))
relfreqUs<-as.data.frame(relfreq.Us)
colnames(relfreqUs)<-c("US","Relative Frequency")
tab2<-cbind(frequs,relfreqUs[,2])
colnames(tab2)<-c("US","Frequency","Relative Frequency")
tab2
### Barplot
bar.freq.us<-barplot(freq.us,main="Is the store in US?",xlab="Answer",ylab="Frequency",horiz=TRUE,cex.names=0.8,col=c("red","blue"))
### Pie Chart
label2<-paste(names(freq.us),"\n",freq.us,sep="")
pie.freq.us<-pie(freq.us,col=c("red","blue"),main="Is the store in US?",labels=label2)
legend("topright",names(freq.us),fill=c("red","blue"))
#κατασκευή crosstabulation matrix
###Για τις Shelvelock και Urban
table(ShelveLoc,Urban)
###Θα εξετάσουμε εάν οι μεταβλητές Shelvelock και Urban είναι ανεξάρτητες ή όχι
test1<-chisq.test(ShelveLoc,Urban)
###Από το τεστ προκύπτει ότι ισχύει η μηδενική υπόθεση, δηλαδή ότι οι Shelvelock και Urban είναι ανεξάρτητες, διότι το p-value ήταν 0.2544
##installibrary(gginference)
###Γραφικά το αποτέλεσμα του Χ^2 test
ggchisqtest(test1,colaccept="grey",colreject="black")
###το στατιστικό της x^2 για τις shelveloc και urban είναι
stat1<-test1$statistic
###Βλέπουμε ότι το στατιστικό δέν ανήκει στην απορριπτική περιοχή, 'αρα δεχόμαστε την Ηο,δηλαδή shelveloc και Urban ανεξάρτητες
#Για τις Shelvelock και US
table(ShelveLoc,US)
###Θα εξετάσουμε εάν οι μεταβλητές Shelvelock και US είναι ανεξάρτητες ή όχι
test2<-chisq.test(ShelveLoc,US)
###Από το τεστ προκύπτει ότι ισχύει η μηδενική υπόθεση, δηλαδή ότι οι Shelveloc και US είναι ανεξάρτητες, διότι το p-value ήταν 0.2541>0,05
###Γραφικά το αποτέλεσμα του Χ^2 test για την shelveloc και US
ggchisqtest(test2,colaccept="grey",colreject="black")
###το στατιστικό της x^2 για τις shelveloc και US είναι
stat2<-test2$statistic
###Βλέπουμε ότι το στατιστικό δέν ανήκει στην απορριπτική περιοχή, 'αρα δεχόμαστε την Ηο,δηλαδή shelveloc και US ανεξάρτητες
#Για τις Urban and US
table(Urban,US)
###Θα εξετάσουμε εάν οι μεταβλητές Urban και US είναι ανεξάρτητες ή όχι
test3<-chisq.test(Urban,US)
###Από το τεστ προκύπτει ότι ισχύει η μηδενική υπόθεση, δηλαδή ότι οι Urban και US είναι ανεξάρτητες, διότι το p-value ήταν 0.4082>0,05
###Γραφικά το αποτέλεσμα του Χ^2 test για την Urban και US
ggchisqtest(test3,colaccept="grey",colreject="black")
###το στατιστικό της x^2 για τις Urban και US είναι
stat3<-test3$statistic
###Βλέπουμε ότι το στατιστικό δέν ανήκει στην απορριπτική περιοχή, 'αρα δεχόμαστε την Ηο,δηλαδή Urban και US ανεξάρτητες
#Για τις Urban and US
#Υπολογισμός των σημαντικότερων παραμέτρων για κάθε μεταβλητή χωρισμένη σε υποδείγματα ανάλογα με την εκάστοτε κατηγορική
#Για την "Sales" για κάθε δείγμα (Bad shelving Location-Medium shelving Location-Good shelving Location)
library(e1071)
quantile0<-quantile(Sales[ShelveLoc=="Bad"])
quantile0<-as.data.frame(quantile0)
quantile1<-quantile(Sales[ShelveLoc=="Medium"])
quantile1<-as.data.frame(quantile1)
quantile2<-quantile(Sales[ShelveLoc=="Good"])
quantile2<-as.data.frame(quantile2)
mean0<-mean(Sales[ShelveLoc=="Bad"])
median0<-median(Sales[ShelveLoc=="Bad"])
sd0<-sd(Sales[ShelveLoc=="Bad"])
var0<-var(Sales[ShelveLoc=="Bad"])
skew0<-skewness(Sales[ShelveLoc=="Bad"])
kurt0<-kurtosis(Sales[ShelveLoc=="Bad"])
minShelveLoc0<-min(Sales[ShelveLoc=="Bad"])
maxShelveLoc0<-max(Sales[ShelveLoc=="Bad"])
rangeShelveLoc0<-maxShelveLoc0-minShelveLoc0 
result0<-rbind(mean0,median0,sd0,var0,skew0,rangeShelveLoc0,quantile0)
result0
mean1<-mean(Sales[ShelveLoc=="Medium"])
median1<-median(Sales[ShelveLoc=="Medium"])
sd1<-sd(Sales[ShelveLoc=="Medium"])
var1<-var(Sales[ShelveLoc=="Medium"])
skew1<-skewness(Sales[ShelveLoc=="Medium"])
kurt1<-kurtosis(Sales[ShelveLoc=="Medium"])
minShelveLoc1<-min(Sales[ShelveLoc=="Medium"])
maxShelveLoc1<-max(Sales[ShelveLoc=="Medium"])
rangeShelveLoc1<-maxShelveLoc1-minShelveLoc1 
result1<-rbind(mean1,median1,sd1,var1,skew1,rangeShelveLoc1,quantile1)
result1
mean2<-mean(Sales[ShelveLoc=="Good"])
median2<-median(Sales[ShelveLoc=="Good"])
sd2<-sd(Sales[ShelveLoc=="Good"])
var2<-var(Sales[ShelveLoc=="Good"])
skew2<-skewness(Sales[ShelveLoc=="Good"])
kurt2<-kurtosis(Sales[ShelveLoc=="Good"])
minShelveLoc2<-min(Sales[ShelveLoc=="Good"])
maxShelveLoc2<-max(Sales[ShelveLoc=="Good"])
rangeShelveLoc2<-maxShelveLoc2-minShelveLoc2 
quantile2
result2<-rbind(mean2,median2,sd2,var2,skew2,rangeShelveLoc2,quantile2)
result2
resultsshel<-cbind(result0,result1,result2)
resultsshel
colnames(resultsshel)<-c("Bad","Medium","High")
rownames(resultsshel)<-c("Mean","Median","SD","Var","Skewness","Kurtosis","Range","Quantile 0%","Quantile 25%","Quantile 50%","Quantile 100%")
resultsshel
#Για την "Sales" για κάθε δείγμα (Rural Location, Urban Location)
quantile01<-quantile(Sales[Urban=="No"])
quantile01<-as.data.frame(quantile01)
quantile11<-quantile(Sales[Urban=="Yes"])
quantile11<-as.data.frame(quantile11)
mean01<-mean(Sales[Urban=="No"])
median01<-median(Sales[Urban=="No"])
sd01<-sd(Sales[Urban=="No"])
var01<-var(Sales[Urban=="No"])
skew01<-skewness(Sales[Urban=="No"])
kurt01<-kurtosis(Sales[Urban=="No"])
sd11<-sd(Sales[Urban=="Yes"])
var11<-var(Sales[Urban=="Yes"])
skew11<-skewness(Sales[Urban=="Yes"])
kurt11<-kurtosis(Sales[Urban=="Yes"])
minShelveLoc11<-min(Sales[Urban=="Yes"])
maxShelveLoc11<-max(Sales[Urban=="Yes"])
rangeShelveLoc11<-maxShelveLoc11-minShelveLoc11 
result11<-rbind(mean11,median11,sd11,var11,skew11,rangeShelveLoc11,quantile11)
result11
resultsurban<-cbind(result01,result11)
resultsurban
colnames(resultsurban)<-c("Rural","Urban")
rownames(resultsurban)<-c("Mean","Median","SD","Var","Skewness","Kurtosis","Range","Quantile 0%","Quantile 25%","Quantile 50%","Quantile 100%")
resultsurban
#Για την "Sales" για κάθε δείγμα ( Out of the US, In the US)
quantile02<-quantile(Sales[US=="No"])
quantile02<-as.data.frame(quantile02)
quantile12<-quantile(Sales[US=="Yes"])
quantile12<-as.data.frame(quantile12)
mean02<-mean(Sales[US=="No"])
median02<-median(Sales[US=="No"])
sd02<-sd(Sales[US=="No"])
var02<-var(Sales[US=="No"])
skew02<-skewness(Sales[US=="No"])
kurt02<-kurtosis(Sales[US=="No"])
minShelveLoc02<-min(Sales[US=="No"])
maxShelveLoc02<-max(Sales[US=="No"])
rangeShelveLoc02<-maxShelveLoc02-minShelveLoc02 
result02<-rbind(mean02,median02,sd02,var02,skew02,rangeShelveLoc02,quantile02)
result02
mean12<-mean(Sales[US=="Yes"])
median12<-median(Sales[US=="Yes"])
sd12<-sd(Sales[US=="Yes"])
var12<-var(Sales[US=="Yes"])
skew12<-skewness(Sales[US=="Yes"])
kurt12<-kurtosis(Sales[US=="Yes"])
minShelveLoc12<-min(Sales[US=="Yes"])
maxShelveLoc12<-max(Sales[US=="Yes"])
rangeShelveLoc12<-maxShelveLoc12-minShelveLoc12 
result12<-rbind(mean12,median12,sd12,var12,skew12,rangeShelveLoc12,quantile12)
result12
resultsus<-cbind(result02,result12)
resultsus
colnames(resultsus)<-c("Out of the US","In the US")
rownames(resultsus)<-c("Mean","Median","SD","Var","Skewness","Kurtosis","Range","Quantile 0%","Quantile 25%","Quantile 50%","Quantile 100%")
resultsus
#θα εξετάσω τώρα εάν η μέση τιμή τω πωλήσεων εξαρτάται από το αν το κατάστημα είναι στην Αμερική ή όχι
###θα εξετάσω αρχικά τις μέσες τιμές μέσω θηκογραμμάτων
library(graphics)
boxplot(Sales~US,main="Sales by contintent US or another",col=c("darkgreen","orange"),xlab=("Is the shop in US or No"),ylab="Sales (in thousands)",xlab="Is the store in the US or No")
###θα κάνω έλεγχο υποθέσεων για τις μέσες τιμές των δειγμάτων που προκύπτουν για τη sales ανάλογα με το αν ένα κατάστημα είναι στην Αμερική ή όχι
df<-as.data.frame(Sales)
Sales.df<-data.frame(Sales,US)
Sales.df
freq.ur<-table(Sales.df$US)
frequr<-as.data.frame(freq.ur)
frequr
colnames(frequr)<-c("Is the store In US?","Frequency")
rownames(frequr)<-c("1","2")
frequr
###Βλέπω ότι και τα 2 δείγματα έχουν συχνότητα εμφάνισης >30, το ένα έχει 142 και το άλλο 258,άρα από ΚΟΘ και τα 2 δείγματα ακολοθούν κανονική κατανομή,άρα μπορω να εφαρμόσω t-test
##Ho η μέση τιμή των πωλήσεων για την αμερική δεν διαφέρει από αυτήν σε άλλη ήπειρο
t.test(Sales[US=="No"],Sales[US=="Yes"])
##βλέπω οτι p-value<0.05 άρα οι μέσες τιμές διαφέρουν, με τις πωλήσεις στην Αμερική να έχουν μεγαλύτερη μέση τιμή, άρα προτιμότερο είναι ένα κατάστημα στην Αμερική με πιθανότητα 95%
t.test(Sales[US=="No"],Sales[US=="Yes"],conf.level=0.99)
##Βλέπω ότι πάλι απορρίπτεται η Ηο, δηλαδή οι μέσες τιμές διαφέρουν κι για επίπεδο σημαντικότητας 99%
#θα εξετάσω τώρα εάν η μέση τιμή τω πωλήσεων εξαρτάται από το αν το κατάστημα είναι αστικό ή όχι
###θα εξετάσω αρχικά τις μέσες τιμές μέσω θηκογραμμάτων
boxplot(Sales~Urban,main="Sales by urban location or rural",col=c("red","blue"),xlab=("Is the shop Urban?"),ylab="Sales (in thousands)")
###θα κάνω έλεγχο υποθέσεων για τις μέσες τιμές των δειγμάτων που προκύπτουν για τη sales ανάλογα με το αν ένα κατάστημα είναι αστικό ή όχι
df<-as.data.frame(Sales)
Sales.df<-data.frame(Sales,Urban)
Sales.df
freq.ur<-table(Sales.df$Urban)
frequr<-as.data.frame(freq.ur)
frequr
colnames(frequr)<-c("Is the store urban?","Frequency")
rownames(frequr)<-c("1","2")
frequr
###Βλέπω ότι και τα 2 δείγματα έχουν συχνότητα εμφάνισης >30, το ένα 'εχει 118 και το άλλο 282,άρα από ΚΟΘ και τα 2 δείγματα ακολοθούν κανονική κατανομή,άρα μπορω να εφαρμόσω t-test
##Ho η μέση τιμή των πωλήσεων για την αστική περιοχή δεν διαφέρει από την αγροτική
t.test(Sales[Urban=="No"],Sales[Urban=="Yes"])
##βλέπω οτι p-value>0.05 άρα οι μέσες τιμές είναι ίσες, καθώς αποδέχομαι την Ηο λόγω p-value με πιθανότητα 0,95.
##Άρα το αν είναι αστικό το κατάστημα ή όχι δεν επηρεάζει τις πωλήσεις με πιθανότητα 0,95
###Εξετάζω τώρα με πιθατότητα 0,99
t.test(Sales[Urban=="No"],Sales[Urban=="Yes"],conf.level=0.99)
##Βλέπω ότι p-value>0,05, οπότε αποδέχομαι την Ηο, δηλαδή οι μέσες τιμές είναι ίδιες με πιθανότητα 0,99, οπότε το αν είναι αστικό ή όχι ένα κατάστημα δεν επηρεάζει τις πωλήσεις με 0,99 πιθανότητα(0 ανήκει στο δε)
#Τώρα θα εξετάσω εάν το Shelving Location επηρεάζει τις πωλήσεις.Όμως από το Shelving Location προκύπτουν 3 κατηγορίες, άρα θα εφαρμόσω ANOVA μέ ένα παράγοντα
Sales.df<-data.frame(Sales,ShelveLoc)
###Μέσες τιμές και διάμεσοι των επιπέδων
par(mfrow=c(1,2))
plot.design(Sales.df)
plot.design(Sales.df,fun=median)
par(mfrow=c(1,1))
plot.factor(Sales.df)
###θηκόγραμμα gia sales με shelveloc
boxplot(Sales~ShelveLoc,Sales.df,col=rainbow(3),main="Sales by Shelving location")
###Για να κάνω ANOVA πρέπει η Sales να ακολουθεί κανονική κατάνομή
###1.Γραφικά
qqnorm(Sales.df$Sales)
qqline(Sales.df$Sales)
##install dgof,χρειαζεται για το kstest,graphics
require(graphics)
library(dgof)
hist(Sales.df$Sales,freq=FALSE,col="grey")
xbar<-mean(Sales.df$Sales)
s<-sd(Sales.df$Sales)
##βιβλιοθηκη ggplot2
library(ggplot2)
hist(Sales.df$Sales,freq=FALSE,col="grey",prob=T)
xbar<-mean(Sales.df$Sales)
curve(dnorm(Sales.df$Sales,xbar,s),col="darkblue",add=TRUE)
###kolmogorov Smirnov Test
ks.test(Sales.df$Sales,"pnorm")
###Από τα αποτελέσματα του test, βλέπουμε ότι ακολουθεί κανανονική κατανομή
###Για να εφαρμόσω ANOVA, πρέπει να ελέγξω 'εαν οι πληθυσμοί των δειγμάτων που προκύπτουν έχουν ίδια διασπορά
x1<-as.vector(Sales.df$Sales[ShelveLoc=="Bad"])
x1
x2<-as.vector(Sales.df$Sales[ShelveLoc=="Medium"])
x2
x3<-as.data.frame(Sales.df$Sales[ShelveLoc=="Good"])
x3
#σύγκριση διασπορών των bad shelving location sales και των medium shelving location sales
var_ab<-var.test(x1,x2,ratio=1,alternative="two.sided",conf.level=0.95)
var_ab
##Βλπεπω ότι p-value=0,6369>0,05,άρα η Ηο γίναται δεκτή δηλαδή οι διασπορές τωνbad shelving location sales και των medium shelving location sales είναι ίσες
#'Ελεγχος διασπορών medium shelving location και good shelving location
##var_ab<-var.test(x2,x3,ratio=1,alternative="two.sided",conf.level=0.95)
var_ab<-var.test(x2,x3,ratio=1,alternative="two.sided",conf.level=0.95)
var_ab
##Βλπεπω ότι p-value=0,6368>0,05,άρα η Ηο γίναται δεκτή δηλαδή οι διασπορές των good shelving location sales και των medium shelving location sales είναι ίσες
#σύγκριση διασπορών των bad shelving location sales και των good shelving location sales
var_ab<-var.test(x1,x3,ratio=1,alternative="two.sided",conf.level=0.95)
var_ab
##Βλπεπω ότι p-value=0,6368>0,05,άρα η Ηο γίναται δεκτή δηλαδή οι διασπορές των bad shelving location sales και των good shelving location sales είναι ίσες
#Έφόσον ισχύουν οι προυποθέσεις της ANOVA,θα εξετάσω εάν τα 3 δειγματα που προκύπτουν για την Sales μέσω της ShelveLoc, έχουντ ις ίδιες μέσες τιμές
##μοντέλο ανάλυσης διασποράς με ένα παράγοντα
Sales.aov=aov(formula=Sales~ShelveLoc,data=Sales.df)
Sales.aov
###θα υπολογιστεί ο πίνακας ANOVA
summary(Sales.aov)
##Βλπεπω ότι το P-value είναι μικρότερο από 0,05,άρα η Ηο απορρίπτεται, πράγμα που σημαίνει ότι τουλάχιστον ένα είδος τοποθεσίας του προ'ι'οντος στο ράφι επηρεάζει με διαφορετικό τρόπο τις πωλήσεις.
### Γραφικά η απορριπτική περιοχή είναι
##εγκατάσταση gginference
library(gginference)
ggaov(Sales.aov,colaccept ="grey",colreject="black")
##Υπολογισμός F στατιστικού
Fcrit<-qf(p=0.05,df1=2,df2=397,lower.tail=F)
Fcrit
##το στατιστικό ανήκει στην απορριπτική περιοχή, άρα Ηο απορρίπτεται,πράγμα που σημαίνει ότι τουλάχιστον ένα είδος τοποθεσίας του προ'ι'οντος στο ράφι επηρεάζει με διαφορετικό τρόπο τις πωλήσεις.
###('Eνας άλλος τρόπου ελέγχου της Ηο) Σύγκριση του στατιστικου Fcrit που είναι 3.018452 με τη κρίσιμη τιμή που απο τον πίνακα της ανόβας είναι 92.23 
###στιστικο<κρίσιμης τιμής, άρα η Ηο απορρίπτεται.
###Θα δούμε παρακάτω ποιά μέση τιμή διαφέρει από τις άλλες,παρακάτω θα υπολογίσουμε 95% δε για όλα τα ζεύγη διαφορών ανάμεσα στις μέσες τιμές
mca.Sales<-TukeyHSD(Sales.aov,which="ShelveLoc",conf.level=0.95)
mca.Sales
##Όλες οι μέσες τιμές διαφέρουν μεταξύ τους όπως βλέπουμε από το παραπάνω τεστ.
fitted.values(Sales.aov)
par(mfrow=c(3,1))
hist(resid(Sales.aov))
qqnorm(resid(Sales.aov))
qqline(resid(Sales.aov))
plot(fitted(Sales.aov), resid(Sales.aov))
abline(h=0)
###ScatterPlot για τις ποσοτικές μεταβλητές
set<-Carseats[,c("Sales","CompPrice","Income","Advertising","Population","Price","Age","Education")]
plot(set)
nums.cols<-sapply(Carseats,is.numeric)
cor.data<-cor(Carseats[,num.cols])
print(cor.data)
install.packages('caTools')
library(caTools)
### Σπάω το δείγμα μου σε training sample και test sample με αναλογία 80%-20%
set.seed(3)
sample<-sample.split(Carseats$Sales,SplitRatio = 0.8)
train<-subset(Carseats,sample==TRUE)
test<-subset(Carseats,sample==FALSE)
####Θα κάνω Παλινδρόμηση για το Sales(με ενδιaφέρει να προβλέπω την τιμή του)
#παλινδρόμηση:
model<-lm(Sales~.,train)
print(summary(model))
anova(model)
#Στο μοντέλο μου θα προσθέσω όλες τις ποσοτικές μεταβλητές και μετά θα κρίνω τη σημαντικότητά τους.
#model1<-lm(Sales~CompPrice+Income+Advertising+Population+Price+Age+Education,data=train) 
#print(summary(model1)) #μάλλον είναι πολλαπλή παλινδρόμηση
#οι Μεταβλητές με τα 3 αστεράκια είναι αυτές που παίζουν σημαντικό ρόλο στο μοντέλο.
#Γίνεται t-test για να δω αν είναι ή όχι σημαντική η μεταβλητή. 
## Για τα residuals:
##Θέλουμε να είναι κοντά στο μηδέν τα υπόλοιπα και να ακολουθούν κανονική κατανομή
res<-residuals(model)
class(res)
res<-as.data.frame(res)
plot(model)
####Predictions
Sales.Predictions<-predict(model,test)
results<-cbind(Sales.Predictions,test$Sales)
colnames(results)<-c('Predicted','Actual')
results<-as.data.frame(results)
min(results)
results
mse<-mean((results$Actual-results$Predicted)^2)
print('mse')
mse
SSE<-sum((results$Predicted-results$Actual)^2)
SST<-sum((mean(Carseats$Sales)-results$Actual)^2)
R2<-1-SSE/SST
R2
##Αρα έχει περίπου 84% προσαρμογή.
###Περιορισμός της γραμμικής παλινδρόμησης στις πιο χρησιμες μεταβλητές
model1<-lm(Sales~CompPrice+Income+Advertising+Price+ShelveLoc+Age,train)
print(summary(model1))
Sales.Predictions1<-predict(model1,test)
results1<-cbind(Sales.Predictions1,test$Sales)
colnames(results1)<-c('Predicted','Actual')
results1<-as.data.frame(results1)
min(results1)
results1
mse<-mean((results1$Actual-results1$Predicted)^2)
print('mse')
mse
SSE<-sum((results1$Predicted-results1$Actual)^2)
SST<-sum((mean(Carseats$Sales)-results1$Actual)^2)
R2new<-1-SSE/SST
R2new
##R2new=0,8491>0,8451, οπότε έχουμε λίγο καλύτερη προσαρμογή για το test set
###########ΛΟΓΙΣΤΙΚΗ ΠΑΛΙΝΔΡΟΜΗΣΗ################
####Θα κάνουμε πρόβλεψη για την δίτιμη μεταβλητή US.
##Σπάμε το δείγμα σε 80% για training και 20% για test.
set.seed(1)
split1<-sample.split(Carseats$US,SplitRatio = 0.8)
final.train1<-subset(Carseats,split1==TRUE)
final.test1<-subset(Carseats,split1==FALSE)
## Υπολογίζω το αρχικό μοντέλο.
final.log.model<-glm(US~.,family=binomial(link='logit'),data=final.train1)
summary(final.log.model)
anova(final.log.model)
##Φαίνεται ότι οι σημαντικές μετάβλητές για το μοντέλο της λογιστικης για us είναι το advertising και το population
#Prediction
#fitted.prob<-predict(final.log.model,final.test1,type='response')
#fitted.results1<-ifelse(fitted.prob>0.5,1,0)
#misClassError<-mean(fitted.results1!=final.test1$US)
#misClassError
###Accuracy
#print(1-misClassError)
###Confusion Matrix
#table(final.test1$US,fitted.prob>0.5)
#anova(final.log.model)
###Προσπαθούμε να βελτιώσουμε το μοντέλο, χρησιμοποιώντας μόνο τις χρήσιμες μεταβλητές
impr.log.model<-glm(US~Advertising+Population,family=binomial(link='logit'),data=final.train1)
summary(impr.log.model)
###Το AIC λαμβάνει μικρότερη τιμή άρα το μοντέλο μας είναι καλύτερο.
#fitted.prob1<-predict(impr.log.model,final.test1,type='response')
#fitted.results2<-ifelse(fitted.prob1>0.5,2,1)
#misClassError1<-mean(fitted.results2!=final.test1$US)
#misClassError1
###Accuracy
#print(1-misClassError1)
#table(impr.log.model$US,fitted.prob1>0.5)
#fitted.prob1<-predict(impr.log.model1,final.test1,type='response')
#fitted.results2<-ifelse(fitted.prob1>0.5,1,0)
#misClassError1<-mean(fitted.results2!=final.test1$US)
#table(impr.log.model1$us,fitted.prob1>0.5)
###Residuals
residuals(impr.log.model, type="d")
residuals(impr.log.model, type="pear")
plot(residuals(impr.log.model, type="d"), xlab="Index", ylab="Deviance Residuals")
abline(h=0)
plot(residuals(impr.log.model, type="pear"), xlab="Index", ylab="Pearson Residuals")
abline(h=0)
#########ΔΕΝΤΡΑ ΤΑΞΙΝΟΜΗΣΗΣ#########
library(tree)
mean(Sales)
#### H μέση τιμή είναι περίπου 8
High=ifelse(Sales<=8,"No","Yes")
Carseats=data.frame(Carseats,High)
tree.carseats=tree(High~.-Sales,Carseats)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats,pretty=0)
tree.carseats
set.seed(2)
train=sample(1:nrow(Carseats), 200)
Carseats.test=Carseats[-train,]
High.test=High[-train]
tree.carseats=tree(High~.-Sales,Carseats,subset=train)
tree.pred=predict(tree.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(86+57)/200
set.seed(3)
cv.carseats=cv.tree(tree.carseats,FUN=prune.misclass)
names(cv.carseats)
#cv.carseats δείχνει το μέγεθος του δένδρου, κοιτώντας ποίο έχει μικρότερη deviation
cv.carseats
par(mfrow=c(1,2))
plot(cv.carseats$size,cv.carseats$dev,type="b")
plot(cv.carseats$k,cv.carseats$dev,type="b")
prune.carseats=prune.misclass(tree.carseats,best=9)
plot(prune.carseats)
text(prune.carseats,pretty=0)
tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(94+60)/200
prune.carseats=prune.misclass(tree.carseats,best=15)
plot(prune.carseats)
text(prune.carseats,pretty=0)
tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(86+62)/200
####dokimazw
str(Carseats)

High=ifelse(Sales<=8,"No","Yes")
Carseats=data.frame(Carseats,High)
head(Carseats)


tree.carseats=tree(High~.-Sales,Carseats)
summary(tree.carseats)


par(mar=c(3,1,3,1))
plot(tree.carseats)
text(tree.carseats,pretty=0,cex=0.5)
tree.carseats


set.seed(2)
train=sample(1:nrow(Carseats), 200)

Carseats.test=Carseats[-train,]
High.test=High[-train]

tree.carseats=tree(High~.-Sales,Carseats,subset=train)

tree.pred=predict(tree.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(86+57)/200


set.seed(3)
cv.carseats=cv.tree(tree.carseats,FUN=prune.misclass)
names(cv.carseats)
cv.carseats



plot(cv.carseats$size ,cv.carseats$dev ,type="b",
     ylab = "cross-validation error rate", xlab = "size")
plot(cv.carseats$k ,cv.carseats$dev ,type="b",
     ylab = "cost-complexity parameter k", xlab = "size")


prune.carseats=prune.misclass(tree.carseats,best=9)
plot(prune.carseats)
text(prune.carseats,pretty=0,cex=0.85)


tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(94+60)/200

prune.carseats=prune.misclass(tree.carseats,best=15)
plot(prune.carseats)
text(prune.carseats,pretty=0,cex=0.85)
tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(86+62)/200

