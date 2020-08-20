library(survey)
library(haven)
library(labelled)
library(dplyr)
library(plotrix)
library(jtools)


NSDUH_2016 <- read_sav("NSDUH_/NSDUH_2016.SAV")
x<-NSDUH_2016
head(x,n=20)
tail(x,n=20)
dim(x)
#parakatw krataw orismenes mono metablhtes pou tha xrhsimopoihsw
#gia na elattwsw to xrono epeksergasias, kathws exw 56897x2668 metablhtes

KeepVars <-c(  
  "ANALWT_C" , 	# main analytic weight
  "VESTR" , 		# sampling strata
  "VEREP" , 		# primary sampling units
  "CIGTRY" , 		# age when first smoked a cigarette
  "PDEN10" ,		# population density variable
  "HEALTH2",		# self-reported health status
  "CIGEVER",   #ever smoked a cigarette
  "COCAGE",   #   AGE WHEN FIRST USED COCAINE
  "MJEVER",#EVER USED MARIJUANA/HASHISH 
  "COUTYP4", # COUNTY METRO/NONMETRO STATUS 
  "NEWRACE2",#RACE/HISPANICITY RECOD
  "PREG",#RC-PREGNANT FEMALES AGED 12-44 
  "COCEVER", #EVER USED COCAINE   
  "ABODHER", #RC-HEROIN DEPENDENCE OR ABUSE - PAST YEAR
  "IRSEX", # IMPUTATION REVISED GENDER 
  "PNRANYFLAG", # RC-ANY PAIN RELIEVER - EVER USED 
  "PNRANYYR",#ANY PAIN RELIEVER - PAST YEAR USE 
  "IRPNRNMAGE", #  PAIN RELIEVER AGE OF FIRST MISUSE - IMPUTATION REVISED 
  "CIGYR" ,#RC-CIGARETTES - PAST YEAR USE 
  "YMDEYR",        #RC-YOUTH: PAST YEAR MAJOR DEPRESSIVE EPISODE (MDE) pg482
  "CIGFLAG",#RC-CIGARETTES - EVER USED
  "CATAG3" ,      #AGE CATEGORY RECODE (5LEVELS)
  "PNRNMYR" ,#ANY PAIN RELIEVER - PAST YEAR USE #kataxrhsh
  "PSYANYFLAG",        #ANY PSYCHOTHERAPEUTICS - EVER USED PG123
  "PSYANYYR",#  ANY PSYCHOTHERAPEUTICS - PAST YEAR USE 
  "ILLFLAG",  # ILLICIT DRUG - EVER USED 
  "ILLYR",#ANY ILLICIT DRUG - PAST YEAR USE 
  "HLTINMNT"  ,     #  MENTAL OR EMOTIONAL DIFFICULTIES 
  "HLTINDRG"   ,            #DRUG ABUSE
  "HLTINALC",#ALCOHOL ABUSE OR ALCOHOLISM
  "IRMARIT"  ,      # MARITAL STATUS  
  "IREDUHIGHST2" , #EDUCATION 
  "WRKDPSTYR" ,   # PAST 12 MOS, WORKED AT ANY JOB
  "YHLTMDE" , #SAW/TALK TO HEALTH PROF ABOUT MDE IN PST YR 
  "APPDRGMON2"   ,# "RC-APPROACHED BY SOMEONE SELLING DRUGS"
  "DIFOBTCOC", #"COCAINE FAIRLY OR VERY EASY TO OBTAIN "
  "DIFOBTMRJ" ,#   "MARIJUANA FAIRLY OR VERY EASY TO OBTAIN"
  "DIFOBTHER"  ,#"HEROIN FAIRLY OR VERY EASY TO OBTAIN "
  "DIFOBTLSD", #"LSD FAIRLY OR VERY EASY TO OBTAIN "
  "DIFOBTCRK", #FAIRLY OR VERY EASY TO OBTAIN    
  "EDUHIGHCAT" ,#EDUCATION CATEGORIES
  "AMIYR_U", #AMI IND (1/0) BASED ON REVISED PREDICTED SMI PROB/is an indicator for Any Mental Illness (AMI)
  "SMMIYR_U", #SMMI IND (1/0) BASED ON REVISED PREDICTED SMI PROB pg399(severe or moderate mental illness)
  "SMIYR_U" , #Serious Mental Illness (SMI) indicator 
  "YMDEUDPY", #PAST YEAR MDE AND SUB DEP OR ABUSE 
  "MHSUITHK",#SERIOUSLY THOUGHT ABOUT KILLING SELF IN PAST YEAR
  "MHSUITRY" , #ATTEMPTED TO KILL SELF IN PAST YEAR 
  "DSTNRV30", # "HOW OFTEN FELT NERVOUS PAST 30 DAYS "
  "ABUSEALC",#ALCOHOL ABUSE - PAST YEAR
  "ABUSEMRJ",#RC-MARIJUANA ABUSE - PAST YEAR
  "ABUSECOC",#COCAINE ABUSE - PAST YEAR 
  "ABUSEHER",#HEROIN ABUSE - PAST YEAR
  "ABUSEPYHAL",#-HALLUCINOGEN ABUSE - PAST YEAR 
  "ABUSEPYMTH",#METHAMPHETAMINE ABUSE - PAST YEAR
  "ABUSEPYTRQ",#  RC-TRANQUILIZER ABUSE - PAST YEAR
  "ABUSEPYSTM",#RC-STIMULANT ABUSE - PAST YEAR
  "ABUSEPYSED",#SEDATIVE ABUSE - PAST YEA
  "ABUSEPYPSY",# RC-PSYCHOTHERAPEUTIC ABUSE - PAST YEAR
  "ABUSEPYILL",#RC-ILLICIT DRUG ABUSE - PAST YEAR
  "WRKDPSTYR",#PAST 12 MOS, WORKED AT ANY JOB
  "NEWRACE2",# RC-RACE/HISPANICITY RECODE (7 LEVELS) 
  "HLTINNOS",#covered by health insurance
  "IRALCAGE",#ALCOHOL AGE OF FIRST USE
  "MHSUTK_U",# RC-SERIOUSLY THOUGHT ABOUT KILLING SELF IN PAST YEAR    Freq 
  "AMDEY2_U" ,#RC-ADULT PAST YEAR MDE - 'UNKNOWN' IMPUTED AS 'NO'    Freq Pct . = Age "
  "YALTMDE",# RC-YOUTH: SAW/TALK TO ALT SERV PROF ABOUT MDE IN PY
  "ACOUNMDE",#SAW/TALK TO COUNSELOR ABT DEPRESSVE FEELINGS IN PY 
  "YTXMDEYR",#SAW OR TALK TO MD/PROF FOR MDE IN PAST YEAR
  "YRXMDEYR",# USED RX MEDICATION FOR MDE IN PAST YEAR 
  "YMDETXRX" ,# RCVD TRT/CNSLG OR RX MED FOR MDE IN PST YR 
  "YDOCMDE", #SAW/TALK TO GEN PRAC/FAM MD ABOUT MDE IN PY)
  "YPSY1MDE" ,#SAW/TALK TO PSYCHOLOGIST ABT MDE IN PST YR
  "YPSY2MDE",# RC-YOUTH: SAW/TALK TO PSYCHIATRIST ABT MDE IN PST YR
  "YSOCMDE",#: SAW/TALK TO SOCIAL WORKER ABOUT MDE IN PY 
  "YOMHMDE",#SAW/TALK TO OTH MENT HLTH PROF ABOUT MDE-PY
  "YNURSMDE",#SAW/TALK TO NURSE/OCC THERA ABOUT MDE IN PY 
  "YRELMDE",# SAW/TALK TO RELIG ADVISOR ABOUT MDE IN PY 
  "YHBCHMDE", #SAW/TALK TO ANOTHER HEALER ABOUT MDE IN PY
  "IRMJAGE", # MARIJUANA AGE OF FIRST USE - IMPUTATION REVISED   
  "IRMCDCHP" ,  #IMPUTATION REVISED CAIDCHIP
  "IRMEDICR", #MEDICARE - IMPUTATION REVISED
  "IRPRVHLT",#PRIVATE HEALTH INSURANCE - IMPUTATION REVISED
  "IRINSUR4"#RC-OVERALL HEALTH INSURANCE - IMPUTATION REVISED
)
x1<- x[ ,KeepVars]#krataw apo to dataset mono tis metablhtes pou
#tha eksagw sumperasmata

########epanakwdikopoihsh orismenwn metablhtwn
x1$IRALCAGE<-ifelse(x1$IRALCAGE==991,NA,x1$IRALCAGE)#hlikia prwths xrhshs alkool
x1$fulo<-as.factor(x1$IRSEX)#dhmiourgia ths metablhths  fulo ws aplhs kathgorikhs apo thn IRSEX

x1$YMDEYR[x1$YMDEYR==2]<-0#apothhkeuw me 0 osous den ekdhlwsan MDE thn prohgoumenh
#xronia, prokeimenou na exw mia metablhth me 0 kai 1, gia na thn xrhsismopoihsw 
#sthn euresh athroismatos gia na dw posous emfanisan ena xarakthristiko,
#opws edw MDE


x1$drugabuse<-ifelse(x1$HLTINDRG>2,NA,x1$HLTINDRG)
x1$drugabuse[x1$drugabuse==2]<-0
sum(is.na(x1$drugabuse))#para polla NA , kalo einai na mhn thn xrhsimopoihsw
#sthn analush , kathws tha brw paraplanhtika apotelesmata

########deigmatolhptikos sxediasmos gia to x1/antistoixei
#####se polustadiakh strwmatopoihmenh deigmatolhpsia
desg <- svydesign(id = ~VEREP , strata = ~VESTR , weights = ~ANALWT_C , data = x1 , nest = TRUE )
#ANALWT_C --LEVEL SMPLE WGHT ,
#VESTR ANALYSIS STRATUM
#VEREP- ANALYSIS REPLICATE


#parakatw epeksergazomai to uposunolo twn enhlikwn

###Mental illness among Adults
#parakatw blepw gia tis metablhtes twn enhlikwn se ti pososta exw NA

dfadults<-subset(x1,CATAG3>=2)#uposunolo enhlikwn
sapply(dfadults, function(dfadults) sum(is.na(dfadults)))#sunolo NA
#gia kathe metablhth twn enhlikwn sto x1 dataset
proportionNA<-sapply(dfadults, function(dfadults) sum(is.na(dfadults))/length(dfadults))#βρήκα σε τι ποσοστά έχω ΝΑ σε διάφορες μεταβλητές

proportionNA<-as.matrix(proportionNA)

keepnonna<-character()#sto dianusma prosthetw kathe fora metablhth pou exei pososto NA
#katw apo 30%

for(i in 1:dim(proportionNA)[1])
{
  if(proportionNA[i,1]<0.3)
  {
    keepnonna<-append(keepnonna,names(proportionNA[i,1]))
  }
}
#metablhtes tou x1 periorismenou stous enhlikes pou exoun pososto <30%
keepnonna

#Na shmeiwthei oti exw hdh xrhsimopoihsei metablhtes gia tis opoies exei ginei
#imputation apo to institouto ths ereunas, malista uparxoun eidkes enothtes sto codebook
#tou 2016 pou parousiazoun tis imputated metablhtes. Malista, edw prospathhsa na kanw
#imputation sto arxiko dataset me thn bibliothhkh mice alla h R mou ebgaze oti
#den mporouse na to ektelesei giati eixa 56897x2668 metablhtes ki eixe thema me thn mnhmh.
#Etsi perisoristika se metablhtes me sxedon mhdnika pososta NA

#######parakatw psaxnw gia tous ellhnikous metablhtes pou eixan pososto NA 0%
keepnonna0<-character()

for(i in 1:dim(proportionNA)[1])
{
  if(proportionNA[i,1]==0)
  {
    keepnonna0<-append(keepnonna0,names(proportionNA[i,1]))
  }
}
keepnonna0#edw briskontai oi metablhtes tou x1 gia tous enhlikes me mhdeniko
#pososto NA

####arxiko deigmatolhptiko sxedio periorismeno stous enhlikes
desg2=subset(desg,CATAG3>=2)

#sunolo enhlikwn me Any Mental Illness
svytotal(~AMIYR_U,desg2,na.rm=TRUE)
confint(svytotal(~AMIYR_U,desg2,na.rm=TRUE),df = degf( desg2))#de gia to total

#pososto enhlikwn me any mental illness sto sunolo twn enhlikwn
desg3=update(desg2,one=1)#dhmiourgw th sthlh pou exei pantou 1 gia na
#ektimhsw to sunolo twn enhlikwn sto plhthusmo prokeimenou na brw
#to pososto twn enhlikwn me any mental illness sto sunolo twn enhlikwn
svyratio(~AMIYR_U,~one,desg3,na.rm=TRUE)

#sunolo me any mentall ilness stous enhlikous me bash to fulo
svyby(~AMIYR_U,~IRSEX,desg2,svytotal,na.rm=TRUE)
barplot(svyby(~AMIYR_U,~IRSEX,desg2,svytotal,na.rm=TRUE),names.arg=c(" Male","Female"), col=c("blue","pink"),
        main="Total of adults with any mental ilness for males and females",xlab="sex",ylab="Total of adults",beside=TRUE)


#Any Mental Illness stous enhlikous analoga me to hlhkiako group
svyby(~AMIYR_U,~CATAG3,desg2,svytotal,na.rm=TRUE)

#sunolo enhlikwn me severe Meantal Illness
svytotal(~SMIYR_U,desg2,na.rm=TRUE)
confint(svytotal(~SMIYR_U,desg2,na.rm=TRUE),df = degf( desg2))#de gia total

#pososto enhlikwn me severe mental illness sto sunolo twn enhlikwn
desg3=update(desg2,one=1)
svyratio(~SMIYR_U,~one,desg3,na.rm=TRUE)
confint(svyratio(~SMIYR_U,~one,desg3,na.rm=TRUE),design=desg3,df=degf(desg3),level=0.95 )#δε για το ποσοστό


#sunolo enhlikwn me severe mental ilness analoga me to hlikiako group
a<-svyby(~SMIYR_U,~CATAG3,desg2,svytotal,na.rm=TRUE)
tb<-svytable(~SMIYR_U+CATAG3, desg2)#pinakas pou deixnei 
#sto plhthusmo ton sunduasmo emfanishs severe mental illness  me thn hlikia

tb#blepw oti h deuterh grammi tou pinaka tautizetai me thn entolh
#a , opote o pinakas autos einai enas deuteros tropos upologismou
#tou synolou twn enhlikwn me severe mental illness analoga me to hlikiako group

barplot(a,names.arg=c(" 18-25 Years Old","26-34 Years Old","35-49 Years Old","5 - 50 or Older"), col=c(7,2,3,4),
        main="Severe Mental Illness among Adult Age Groups",xlab="Age group",ylab="Total population of Severe MEntall Illness")

#to pososto twn enhlikwn me opoiodhpote mentall ilness pou phran psuxotherapeutika
#farmaka thn prohgoumenh xronia

desgpsy<-subset(desg2,PSYANYYR==1)#deigmatolhptiko sxedio gia tous enhlikes
#pou eixan parei opoiodhpote psuxotherapeutiko farmako thn prohgoumenh xronia

desgpsy1<-update(desgpsy,one=1)#gia na ektimhsw to sunolo autwn pou phran opoiodhpote
#psuxotherapeutiko farmako thn prohgoumenh xronia

svyratio(~AMIYR_U,~one,desgpsy1)# 0.2678045
confint(svyratio(~AMIYR_U,~one,desgpsy1,na.rm=TRUE),design=desgpsy1,df=degf(desgpsy1),level=0.95)#δε για ποσοστό
x1$ILLYR
x1$MHSUTK_U

#pososto twn enhlikwn pou eixan sobares skepseis autoktonias apo to sunolo
#aytwn pou phran paranoma narkwtika ton prohgoumeno xrono
desgill<-subset(desg2,ILLYR==1)#deigmatolhptiko sxedio gia autous pou ekanan paranomh xrhsh narkwtikwn persu kai htan enhlikes
desgill1<-update(desgill,one=1)
svyratio(~MHSUTK_U,~one,desgill1,na.rm=TRUE)# 0.09998954, se=0.004369855
confint(svyratio(~MHSUTK_U,~one,desgill1,na.rm=TRUE),design=desgill1,df=degf(desgill1))#de gia to pososto


#pososto twn  ΝΑ ths MHSUITRY
q<-subset(x1,CATAG3>1)#periosristhka edw giati orizetai mono gia tous enhlikous
sum(is.na(q$MHSUITRY))
length(q$MHSUITRY)

sum(is.na(q$MHSUITRY))/length(q$MHSUITRY)#polu mikro pososto NΑ #pou epixeirhsan na autoktonhsoun to prohgoumeno xrono,ths takshs 0,7%, ara tha th melethsw sto sunolo twn enhlikwn


#sunolo enhlikwn pou prospathsan na autoktonhsoun persu
svytotal(~MHSUITRY,desg2,na.rm=TRUE)
#de gia to sunolo enhlikwn pou prospathsan na autoktonhsoun persu
confint(svytotal(~MHSUITRY,desg2,na.rm=TRUE),df = degf( desg2))
#pososto enhlikwn pou prospathsan na autoktonhsoun apo to sunolo twn enhlikwn
svyratio(~MHSUITRY,~one,desg3,na.rm=TRUE)

#me bash to fulo apopeires autoktonias ton teleutaio xrono
c<-svyby(~MHSUITRY,~IRSEX,desg2,svytotal,na.rm=TRUE)
c
barplot(c,names.arg=c("1-Male","2-Female"), col=c(4,2),
        main="Apopeires autoktonies enhlikwn to 2016",xlab="sex",ylab="sunolo atomwn pou prospathsan na autoktonhsoun")

#tha dw an uparxoun diafors sto sunolo twn prospatheiwn twn autoktoniwn 
#twn enhlikwn analoga me to fulo, kanontas x2 test
svychisq(~MHSUITRY  + IRSEX,desg2,statistic = "Chisq")
#pvalue=0.07004>0.05 dexomai thn Ho, ara den uparxei eksarthsh
#metaksu tou fulou kai twn prospatheiwn autoktonias stous enhlikous

tbl1 <- svytable(~MHSUITRY + IRSEX, desg2)
tbl1#ektimwmenos pinakas sunafeias gia ton plhthusmo


#sunolo enhlikwn pou epixeirhsan na autoktonhsoun analoga me to hlikiako group 
b<-svyby(~MHSUITRY,~CATAG3,desg2,svytotal,na.rm=TRUE)
b
barplot(b,names.arg=c(" 18-25","26-34 ","35-49 ","50+ "), col=c(2,3,4,5),
        main="Apopeires autoktonias enhlikwn 2016",xlab="age groups",ylab="Sunolo atomwn pou prospathsan na autoktonhsoun")
svychisq(~MHSUITRY  + CATAG3,desg2,statistic = "Chisq")
#ara h hlikia ephreazei thn prospatheia autoktonias, p-value < 2.2e-16(aporriptw thn Ho, exw eksarthsh)

#sunolo atomwn pou ekanan xrhsh opoioudhpote psuxotherapeytikou analoga me to an epixeirhsan na autoktonhsoun h oxi
k<-svyby(~PSYANYYR,~MHSUITRY,desg2,svytotal,na.rm=TRUE)
k
tb0<-svytable(~PSYANYYR+MHSUITRY,desg2)
tb0
#diagramma pitas
survey1 <- c(no_suitry=107639355.0, yes_suitry=887815.5)

pct <-c(107639355.0,887815.5)	
lbls <- paste(names(survey1),pct)	
pie(survey1,
    col=c("blue", "pink"),
    labels=lbls ,main="enhlikoi xrhstes psuxotherapeytikwn analoga me thn apopeira autoktonias , 2016")

dfadults$MHSUTK_U

#pososto enhlikwn pou skefthkan sobara na autoktonhsoun apo to sunolo autwn pou parousiasan severe mental illness
desgsmi<-subset(desg2,SMIYR_U==1)#deigmatolhptiko sxedio gia enhlikes me SMI
desgmi1<-update(desgsmi,one=1)
#to pososto einai:
svyratio(~MHSUTK_U,~one,design=desgmi1,na.rm=TRUE)# 0.5586543

#pososto enhlikwn pou prospathsan na autoktonhsoun apo to sunolo autwn pou parousiasan severe mentall illness

svyratio(~MHSUITRY,~one,design=desgmi1,na.rm=TRUE)# 0.06153756

x1$PNRNMYR

#pososto enhlikwn pou ekanan kataxrhsh opoioudhpote pain reliever apo to sunolo autwn pou parousiasan severe mentall illness
svyratio(~PNRNMYR,~one,design=desgmi1,na.rm=TRUE)#0.1591555


###################Yposunolo efhbwn######################


#parakatw meletaw ta pososta se ΝΑ gia diafores metablhtes gia to uposunolo twn efhbwn
#tis meletaw pairnontas to uposunolo, giati uparxoun metablhtes pou exoun pantou NA gia tous enhlikes, px h YMEYR
#opote an tis meletousame sto arxiko dataset tha eixame polu megalo pososto NA ki tha tis eksairousame apo th meleth,
#omws auto tha htan problhma giati gia tous efhbous tha htan mia arketa xrhsimh metablhth

dfadoloscents<-subset(x1,CATAG3==1)#uposunolo efhbwn
##eksetazw pososta NA 

sapply(dfadoloscents, function(dfadoloscents) sum(is.na(dfadoloscents)))
proportionNA1<-sapply(dfadoloscents, function(dfadoloscents) sum(is.na(dfadoloscents))/length(dfadoloscents))#βρήκα σε τι ποσοστά έχω ΝΑ σε διάφορες μεταβλητές
attributes(proportionNA1)
proportionNA1<-as.matrix(proportionNA1)
dim(proportionNA1)[1]

keepnonna1<-character()#apothhkevw tis metablhtes me pososto NA katw apo  30%

for(i in 1:dim(proportionNA1)[1])
{
  if(proportionNA1[i,1]<0.3)
  {
    keepnonna1<-append(keepnonna1,names(proportionNA1[i,1]))
  }
}

#oi metablhtes tou x1 periorismenes stous efhbous pou exoun pososto NA katw apo 30% einai
keepnonna1
sum(is.na(dfadoloscents$YMDEUDPY))/length(dfadoloscents$YMDEUDPY)#mikro pososto ,ths takshs tou 3% gia YMDEUDPY,ara tha thn xrhsimopoihsw gia thn analush

sum(is.na(dfadoloscents$APPDRGMON2))/length(dfadoloscents$APPDRGMON2)#polu mikro pososto ΝΑ gia thn APPDRGMON2,ths takshs 0,03%



keepnonna01<-character()#edw tha apothkeusw tis metablhtes me mhdenika pososta NA gia tous
for(i in 1:dim(proportionNA1)[1])
{
  if(proportionNA1[i,1]==0)
  {
    keepnonna01<-append(keepnonna01,names(proportionNA1[i,1]))
  }
}
keepnonna01


sum(is.na(dfadoloscents$YHLTMDE))/length(dfadoloscents$YHLTMDE)#edw blepw oti exw missing values gia thn YHLTMDE se pososto
#80%, ara h xrhsh ths sthn analush tha odhgouse se paraplanhtika apotelesmata

###########deigmatolhptiko sxedio gia tous efhbous##############
desg3<-subset(desg,CATAG3==1)#CATAG3==1 antistoixei sthn hlikiaki omada 
#twn efhbwn

#sunolo efhbwn pou parousiasan MDE ton prohgoumeno xrono
svytotal(~YMDEYR,desg3,na.rm=TRUE)#3119338
confint(svytotal(~YMDEYR,desg3,na.rm=TRUE),df=degf(desg3))#de

#prosthetw th sthlh one gia na brw pososto efhbwn me MDE pesru
desg4=update(desg3,one=1)
#pososto twn efhbwn me MDE persu 
svyratio(~YMDEYR,~one,desg4,na.rm=TRUE)#0.1289387


#sunolo efhbwn me  MDE analoga me to fulo
t<-svyby(~YMDEYR,~IRSEX,desg3,svytotal,na.rm=TRUE)

#diagramma pitas tou sunolou twn efhbwn me  MDE analoga me to fulo
survey1 <- c(male=t$YMDEYR[1], female=t$YMDEYR[2])
pct <-c(round(t$YMDEYR[1],0),round(t$YMDEYR[2],0))	
lbls <- paste(names(survey1), pct)
pie(survey1, col=c("blue", "red"), labels=lbls,main="Efhboi me MDE analoga me to fulo ,2016")

#x2 test gia na dw an to fulo paizei rolo sto an tha parousiasei  enas efhbos sobaro katathlhptiko epeisodio
svychisq(~YMDEYR+IRSEX ,design=desg3,statistic = "Chisq")
# p-value < 2.2e-16,ara uparxei eksartisi

# sunolo efhbwn me MDE pou eksartatai apo ousies h tis kataxrazetai 

svytotal(~YMDEUDPY ,desg3 , na.rm = TRUE )
confint(svytotal(~YMDEUDPY ,desg3 , na.rm = TRUE ),df=degf(desg3))

svyratio(~YMDEUDPY,~one,desg4,na.rm=TRUE)#pososto efhbwn me MDE ki eksarthmenwn apo ousies
#(i.e.,illicit drug use disorder or alcohol use disorder) 

#pososto twn efhbwn pou parousiasan MDE thn prohgoumenh xronia apo to sunolo twn efhbwn pou ekanan xrhsh illdrug(paranomwn narkwtikwn)
desgill2<-subset(desg3,ILLYR==1)#deigmatolhptiko sxedio gia efhbous pou ekanan xrhsh paranomwn narkwtikwn thn prohgoumenh xronia
desgill21<-update(desgill2,one=1)
svyratio(~ YMDEYR, ~ one, desgill21,na.rm =TRUE)#0.2546689

x1$YMDEYR
#to pososto twn efhbwn pou phran psuxotherapeutika apo to sunolo autwn pou parousiasn MDE
desgmde<-subset(desg3,YMDEYR==1)#deigmatolhptiko sxedio gia tous efhbous parousiasan MDE
desgmde1<-update(desgmde,one=1)
svyratio(~PSYANYYR ,~one ,desgmde1,na.rm =TRUE)# 0.3997205 


#synolo efhbwn pou proseggisthkan apo kapoion gia na tous poulhsei narkwtika
svytotal(~ APPDRGMON2, desg3,na.rm =TRUE)
#to pososto ten efhben stous opoious prosferthke kapoios na tous poulhsei narkwtika
svyratio(~ APPDRGMON2, ~ one, desg4,na.rm =TRUE)
#synolo efhbwn pou proseggisthkan apo kapoion
#gia na tous poulhsei narkwtika #analoga me thn perioxh
svyby(~APPDRGMON2,~COUTYP4,desg3,svytotal,na.rm = TRUE )


#x2 test gia na dw an to eidos ths perioxhs paizei rolo sto an enas  
#tha proseggistei apo kapoion gia na tou poulhsei narkwtika
svychisq(~APPDRGMON2+COUTYP4 ,design=desg3,statistic = "Chisq")
# p-value 5.374e-05 <0.05 ,ara ephreazetai apo to eidos ths perioxhs

#sunolo efhbwn pou pistevoun oti mporoun na exoun eukolh 
#prosbash se diafora eidh narkwtikwn
svytotal(~DIFOBTCOC+DIFOBTMRJ+DIFOBTHER +DIFOBTLSD+DIFOBTCRK,desg3,na.rm=TRUE)

keepnonna0#gia poies metablhtes den exw missing values 
#gia tous efhbous

#sunolo efhbwn me ethismo se diafores ousies
svytotal(~ABUSEALC+ABUSEMRJ+ABUSECOC+ABUSEHER+ABUSEPYHAL+ABUSEPYMTH+ABUSEPYTRQ+ABUSEPYSTM+ABUSEPYSED+ABUSEPYPSY+ABUSEPYILL,
         desg3,na.rm=TRUE)


#Sumperasmata genikou plhthusmou


#ThA ypologisw sto sunolo tou plhthusmou to sunolo twn atomwn pou kanoun kataxrhsh diaforwn ousiwn (pain relievers thn teleutaia xronia)
d<-svytotal(~ABUSEALC+ABUSEMRJ+ABUSECOC+ABUSEHER+ABUSEPYHAL+ABUSEPYMTH+ABUSEPYTRQ+ABUSEPYSTM+ABUSEPYSED+ABUSEPYPSY+ PNRNMYR ,desg,na.rm=TRUE)
d

sum(is.na(x1$NEWRACE2))/length(x1$NEWRACE2)
sum(is.na(x$ILLFLAG))
#sunolo atomwn pou exoun kanei xrhsh paranomwn narkwtikwn 
#analoga me thn ethnikothta tous
s<-svyby(~ILLFLAG,~NEWRACE2,desg,svytotal,na.rm=TRUE)
s
#3o ερώτημα

#the eleksw an h xrhsh narkwtikwn ephreazetai apo to fulo h oxi
svychisq(~ ILLFLAG + IRSEX,desg,statistic = "Chisq")
#ara ephreazetai h xrhsh paranomwn narkwtikwn apo to fulo (p-value < 2.2e-16)
svytable(~ILLFLAG + IRSEX, desg)

#sunolo atomwn pou ekanan xrhsh paranomwn narkwtikwn analoga me to fulo
svyby(~ILLFLAG,~IRSEX,desg,svytotal,na.rm=TRUE)

#tha eleksw an uparxei eksathsh metaksu ths katastashs ugeias
#kai ths xataxrhshs opoioudhpote pain reliever
sum(is.na(x$HEALTH2))/length(x$HEALTH2)#mikro pososto NA
#εκτιμώμενος πίνακας συνάφειας για το πληθυσμό ths katastashw ugeias
#kai ths kataxrshshs opoioudhpote pain reliever
tbl2 <- svytable(~PNRNMYR+HEALTH2, desg)
tbl2
#sunolo atomwn pou kanoun kataxrhsh opoioudhpote pain reliever
#analoga me thn katastash ugeias
svyby(~PNRNMYR,~HEALTH2,svytotal,design=desg,na.rm=TRUE)
#tautizetai me th deuterh grammi tou tbl2/ 2os tropos upologismou
#hdh blepw oti uparxoun diafores sto synolo twn atomwn me kataxrhsh opoioudhpote pain reliever analoga me thn katastash ugeias

svychisq(~ PNRNMYR + HEALTH2,desg,statistic = "Chisq")
#pvalue=1.608e-12 apporiptw thn Ho, ara uparxei eksarthsh
#opote opws htan anamenomeno blepoume oti h kataxrhsh 
#opoioudhpote pain reliever eksartatai apo thn katastash ugeias



##############t test
#tha eleksw an to fulo paizei rolo sthn hlikia prwths xrhshs alkool

sum(is.na(x1$IRALCAGE))#sumfwna me thn kwdikopoihsh tou codebook pg110 
#blepw oti ta NA ths IRALCAGE einai osa o arithmos autwn pou den ekanan pote xrhsh alkool sth zwi tous , ara gia na
#bew th mesh hlikia prwtis xrhshs tha krathsw mono autous pou ekanan xrhsh alkooltoulaxiston mia fora sth zwi tous

#deigmatolhptiko sxedio gia osous ekanan xrhsh 
#alkooltoulaxiston mia fora sth zwi tous
desg5<-subset(desg,is.na(IRALCAGE)==FALSE)

svyttest(IRALCAGE~fulo,design=desg5)
#alkoolnai<-subset(x1,is.na(IRALCAGE)==FALSE)
##desg6<-svydesign(id = ~VEREP , strata = ~VESTR , weights = ~ANALWT_C , data =alkoolnai , nest = TRUE )
#deigmatolhptiko sxedio opws to desg5
#svyttest(IRALCAGE~fulo,design=desg6)#bgazei to idi0

svyby(~IRALCAGE,~fulo,desg5,svymean)#meses hlikies prwths xrhshs alkool analoga me to fulo
svyttest(IRALCAGE~fulo,design=desg5)
#Ara to fulo ephreazei th mesi hlikia prwths xrhshs alkool 
#dioti p-value < 2.2e-16



#########me t test elegxw an to fulo ephreazei th mesh hlikia prwths xrhshs marixouanas

sum(x1$IRMJAGE==991)
#sumfwna me thn kwdikopoihsh tou codebook pg110 ,osoi den ekanan pote xrhsh marixouanas
#ekxwrhthakan ws 991,pou einai sto sunolo 33108, osa kai ta NA ths IRMJAGE
#etsi gia na brw th mesh hlikia tha krathsw mono autous pou xrhsimopoihsan marixouana
#estw ki mia fora sth zwi tous

#deigmatolhptiko sxedio gia autous pou xrhsimopoihsan marixouana
#estw ki mia fora sth zwi tous
desg7<-subset(desg,x1$IRMJAGE!=991)
#mesh hlikia prwths xrhshs marixouanas analoga me to fulo
svyby(~IRMJAGE,~fulo,desg7,svymean)

svyttest(IRMJAGE~fulo,design=desg7)
#ara to fulo ephreazei th mesh hlikia prwths xrhshs marixouanas (p-value = 9.29e-07)


###########Logistikh palindromhsh#############################
#koitaw ta NA gia olo to dataset, gia na dw se poies metablhtes 
#mporw na kanw logistikh paalindromhsh
sapply(x1, function(x1) sum(is.na(x1)))
proportionNA2<-sapply(x1, function(x1) sum(is.na(x1))/length(x1))#to pososta twn NA sto x1
attributes(proportionNA2)
proportionNA2<-as.matrix(proportionNA2)
dim(proportionNA2)[1]

keepnonna2<-character()

for(i in 1:dim(proportionNA2)[1])
{
  if(proportionNA2[i,1]<0.3)
  {
    keepnonna2<-append(keepnonna2,names(proportionNA2[i,1]))
  }
}

#oi metablhtes tou  x1  pou exoun pososto NA katw apo 30% einai
keepnonna2


keepnonna02<-character()
for(i in 1:dim(proportionNA2)[1])
{
  if(proportionNA2[i,1]==0)
  {
    keepnonna02<-append(keepnonna02,names(proportionNA2[i,1]))
  }
}
keepnonna02

x1$HEALTH2
sum(is.na(x1$HEALTH2))/length(x1$HEALTH2)#epeidh thewrw thn katastash ugeias shmantikh alla exei NA,
#ta opoia einai omws liga tha ta afairesw gia na thn xrhsimopoihsw

desgfinal<-update(desg,is.na(x1$HEALTH2)==FALSE)#deigmatolhptiko sxedio gia olo to plhuysmo xwris ta NA ths HEALTH2

#Logistikh palindromhsh gia thn  PNRNMYR


#Fit the unweighted logistic regression model

e<-subset(x1,is.na(x1$HEALTH2)==FALSE)#gia na mhn exw NA gia thn HEALTH2
dim(e)
library(caTools)#bibliothhkh gia na xwrisw to deigma se training set ki test set
set.seed(1)
split1<-sample.split(e$PNRNMYR,SplitRatio=0.8)
train1<-subset(e,split1==TRUE)#training set
test1<-subset(e,split1==FALSE)#test set

glm.out <- glm(PNRNMYR~ NEWRACE2+ CATAG3 + IRSEX + PREG + EDUHIGHCAT + ABUSEALC+ ABUSEMRJ + ABUSECOC + ABUSEHER + ABUSEPYHAL+
                 ABUSEPYMTH+ABUSEPYTRQ+ABUSEPYSTM+ABUSEPYSED+ABUSEPYPSY+ABUSEPYILL+WRKDPSTYR+  
                 HLTINNOS+IRMCDCHP+IRMEDICR+IRPRVHLT+IRINSUR4+HEALTH2 , data=train1,family=binomial(logit))
#problepseis ki aksiologhsh tou montelou
fitted.prob<-predict(glm.out,newdata=test1,type='response')
fitted.prob<-ifelse(fitted.prob>0.5,1,0)
library(ROCR)
library(Metrics)#metrikes gia logistikh
str(test1$PNRNMYR)
str(fitted.prob)
pr<-prediction(as.vector(fitted.prob),as.vector(test1$PNRNMYR))
perf<-performance(pr,measure="tpr",x.measure="fpr")
plot(perf)
auc(test1$PNRNMYR,fitted.prob)#######AUC=0.5310826
#ara sumfwna me AUC pou einai konta sto 0,5 den problepei
#kalutera apo to tuxaio tropo ara to montelo den einai kalo
#gi auto parakatw tha kanw allh logistikh palindromhsh



##################################################################################################
###Logistikh palindromhsh gia thn MHSUTK_U #-SERIOUSLY THOUGHT ABOUT KILLING SELF IN PAST YEAR

#xwris na labw upopsh th stathmish dhladh tha kanw aplh logistikh 
#palindromhsh gia to deigma dfadults ki oxi gia to plhthusmo stoxo  twn enhlikwn ths Amerikhs

#ousiastika parakatw kanw logistikh palindromhsh xwris na labw
#upopsh to deigmatolhptiko sxedio

keepnonna0#blepw poies metablhtes mporw na xrhsimopoihsw pou 
#den exoun NA sto dataset twn enhlikw prokeimenou na mhn exw
#thema με τη λογιστική παλινδρόμηση


library(caTools)#bibliothhkh gia na xwrisw to dataset se train set και test set
set.seed(1)
split1<-sample.split(dfadults$MHSUTK_U,SplitRatio=0.8)
train1<-subset(dfadults,split1==TRUE)#training set
test1<-subset(dfadults,split1==FALSE)#test set

#arxiko montelo
mod2 <- glm(MHSUTK_U~ NEWRACE2+ CATAG3 + IRSEX + PREG + EDUHIGHCAT + ABUSEALC+ ABUSEMRJ + ABUSECOC + ABUSEHER + ABUSEPYHAL+
              ABUSEPYMTH+ABUSEPYTRQ+ABUSEPYSTM+ABUSEPYSED+ABUSEPYPSY+ABUSEPYILL+WRKDPSTYR+  
              HLTINNOS+IRMCDCHP+IRMEDICR+IRPRVHLT+IRINSUR4+AMIYR_U+SMMIYR_U+SMIYR_U+IRMARIT, data=train1,family=binomial(logit))
#problepseis ki aksiologhsh montelou
fitted.prob<-predict(mod2,newdata=test1,type='response')
fitted.prob<-ifelse(fitted.prob>0.5,1,0)
library(ROCR)
library(Metrics)
str(test1$MHSUTK_U)
str(fitted.prob)
pr<-prediction(as.vector(fitted.prob),as.vector(test1$MHSUTK_U))
perf<-performance(pr,measure="tpr",x.measure="fpr")
plot(perf)
auc(test1$MHSUTK_U,fitted.prob)#AUC= 0.7351111 kalh endeiksh gia kalh 
#proasmogh tou montelou ths logistikhs palindromhshs sta dedomena

table(test1$MHSUTK_U)#pragmatika dedomena


#gia thn aksiologhsh ths prosarmoghs tou montelou
library(caret)
library(e1071)
levels(fitted.prob)
confusionMatrix(data=as.factor(fitted.prob),reference=as.factor(test1$MHSUTK_U))#confusion matrix

summary(mod2)

######aplopoihsh montelou
#tha krathsw mono tous stathstika shmantikous orous oi opoioi sumfwna me to p-value einai:
#AMIYR_U, SMMIYR_U,SMIYR_U ,IRMARIT  ,ABUSEPYPSY, CATAG3 ,IRSEX, EDUHIGHCAT 
#kai tha thewrhsw neo montelo pou tha exei mono autous ws regressors

#neo aplopoihmeno montelo
mod21 <- glm(MHSUTK_U~ AMIYR_U+SMMIYR_U+SMIYR_U+IRMARIT+ABUSEPYPSY+CATAG3+IRSEX+EDUHIGHCAT, data=train1,family=binomial(logit))
#Prediction
fitted.prob<-predict(mod21,newdata=test1,type='response')
fitted.prob<-ifelse(fitted.prob>0.5,1,0)
library(ROCR)
library(Metrics)

pr<-prediction(as.vector(fitted.prob),as.vector(test1$MHSUTK_U))
perf<-performance(pr,measure="tpr",x.measure="fpr")
plot(perf)
auc(test1$MHSUTK_U,fitted.prob)#AUC=0.7342559
#AUC:Computes the area under the curve of a given performance measure

#gia thn aksiologhsh ths akribeias tou montelou
library(caret)
library(e1071)
levels(fitted.prob)
confusionMatrix(data=as.factor(fitted.prob),reference=as.factor(test1$MHSUTK_U))
#blepw oti o confusion matrix tou prohgoumenou montelou
#moiazei me auton ara exoume endeikseis gia isodunamia twn 2 montelwn
#thn opoia tha eksetasoume me thn xrhsh ths entolhs anova 

anova(mod2,mod21,test="LRT")

#ara blepw oti Pr(>Chi)=0.08518>0.05, ara ta 2 montela eiani isodunama
#etsi karatw to aplopihsmeno το mod21

summary(mod21)#mh stathstika shmantikos oros to ABUSEPYPSY gia 
#to montelo mod21 dokimazw twra na diwksw kai to ABUSEPYPSY 
#gia na dw ti montelo tha prokupsei

#trito montelo
mod22 <- glm(MHSUTK_U~ AMIYR_U+SMMIYR_U+SMIYR_U+IRMARIT+CATAG3+IRSEX+EDUHIGHCAT, data=train1,family=binomial(logit))
#Prediction ki Ypologismos AUC
fitted.prob<-predict(mod22,newdata=test1,type='response')#problepseis montelou
fitted.prob<-ifelse(fitted.prob>0.5,1,0)#an h pithanothta einai megaluterh tou 0,5 na ekxwrei thn problepsh ws 1 
library(ROCR)
library(Metrics)
str(test1$MHSUTK_U)
str(fitted.prob)
pr<-prediction(as.vector(fitted.prob),as.vector(test1$MHSUTK_U))
perf<-performance(pr,measure="tpr",x.measure="fpr")
plot(perf)
auc(test1$MHSUTK_U,fitted.prob)#AUC=0.7342559

#The AUC measure computes the area under the curve is viewed as
#a measure of a forecast's accuracy. A measure of 1 would indicate 
#a perfect model. A measure of 0.5 would indicate 
#a random forecast.

#gia thn aksiologhsh ths prosarmoghs tou montelou
library(caret)
library(e1071)
levels(fitted.prob)
confusionMatrix(data=as.factor(fitted.prob),reference=as.factor(test1$MHSUTK_U))#confusion matrix

#sugkrish montelwn
anova(mod21,mod22,test="LRT")
#ara ta montela einai isodunama(Pr(>Chi)=0.9131>0,05) krataw to pio aplo to mod22
summary(mod22)#blepw oloi oi oroi tou mod22 einai statistika 
#shmantikoi ara den mporw na aplopoihsw epipleon to montelo, 
#menei loipon na to aksiologhsw, dhladh na upologisw diafores
#metrikes gia th logiatikh palindromhsh

table(test1$MHSUITHK)#impalanced data 6/100 analogia

#exw mh isorrophmena dedomena ara ayto pou me endiaferei idiaitera 
#na ektimhsw einai to recall/sensitivity
#dhladh na brw posous assous apo tous assous tou dataset katafere na brei to montelo
cm<-confusionMatrix(data=as.factor(fitted.prob),reference=as.factor(test1$MHSUTK_U))
cm1<-as.matrix(cm)
cm1
#metrikes gia to teliko montelo
accuracy1<-(cm1[1,1]+cm1[2,2])/(sum(cm1))#impalanced data
accuracy1
precision1=cm1[2,2]/(cm1[2,2]+cm1[1,2])#apo tous assous pou proeblepse to montelo, posoi pragmatika htan assoi
precision1#0.4854167
f=2*(precision1*recall1)/(precision1+recall1)
f#0.548881
f1=2*cm1[2,2]/(2*cm1[2,2]+cm1[1,2]+cm1[2,1])
f1#precision ki recall sunduastika, apotelei ton armoniko tous meso


misclasserror=(cm1[1,2]+cm1[2,1])/(sum(cm1))#FP+FN/(TP+TN+FP+FN)
misclasserror=#pososto lathoos ektimhsewn einai:0.04492669
#kalo einai na mhn dwsoume idiaiterh baruthta sto
#misclasserror giati den exw isorrophmena dedomena

#pososto swstwn problepsewn
1-misclasserror# 0.9550733

sensitivity1=cm1[2,2]/(cm1[2,1]+cm1[2,2])#recall posous apo tous assous tou 
#dataset mporesa ki brhka me to montelo
sensitivity1#0.6314363 , kalh endeiksh efoson den exw isorropia sto dataset
specificity1=cm1[1,1]/(cm1[1,1]+cm1[1,2])
specificity1

#tha upologiskoume thn pithanothta na exoume  MHSUTK_U=1
#(pithanothta kapoios enhlikos na eixe sobares skepseis autoktonias), 
#h pithanothta sumfwna me to montelo ths logistikhs
#prokuptei ws p=e^z/(1+e^z), opou
z= -5.31611+ 5.97653*AMIYR_U+0.97585*SMMIYR_U+2.00914*SMIYR_U +0.06512*IRMARIT +CATAG3*(-0.50659)+(-0.69361 )* IRSEX+(-0.18119)*EDUHIGHCAT
#sunolikh suneisfora metablhtwn pou mporoun na odhghsoun kapoion se sobare
#skepseis autoktonias

#o tupos gia thn ektimvmenh pithanothta ths
#MHSUTK_U thaa einai o ekshs sumfwna me tous suntelestes tou montelou:

#p_MHSUTK_U=1/(1 + exp[–(-5.31611+ 5.97653*AMIYR_U+0.97585*SMMIYR_U+2.00914*SMIYR_U 
#+0.06512*IRMARIT +CATAG3*(-0.50659)+(-0.69361 )* IRSEX+
#(-0.18119)*EDUHIGHCAT)])


#Fit the weighted logistic regression model/
#Lambanontas upopsh th stathmish, dhladh to deigmatolhptiko sxedio

###logistikh palindromhsh ths MHSUTK_U
#(SERIOUSLY THOUGHT ABOUT KILLING SELF IN PAST YEAR )


#deigmatolhptiko sxedio gia tous enhlikous
#h metablhth  MHSUTK_U anaferetai apokleistika stous enhlikous
desgadults<- svydesign(id = ~VEREP , strata = ~VESTR , weights = ~ANALWT_C , data =dfadults , nest = TRUE )
#arxiko montelo me stathiseis, ksekinaw kanontas palindromhsh me 
#tous idious orous pou ekana palindromhsh
#sto montelo xwris tis stathmiseis, dioti sto deigma eixa
#kalh prosarmogh ki prospathw twra na to genikeusw 
#sto plhthusmo pairnontas to antistoixo stathmismeno montelo

#To montelo meta thn stathmish einai:
svyglm.out <- svyglm(MHSUTK_U~NEWRACE2+ CATAG3 + IRSEX + PREG + EDUHIGHCAT + ABUSEALC+ ABUSEMRJ + ABUSECOC + ABUSEHER + ABUSEPYHAL+
                       ABUSEPYMTH+ABUSEPYTRQ+ABUSEPYSTM+ABUSEPYSED+ABUSEPYPSY+ABUSEPYILL+WRKDPSTYR+  
                       HLTINNOS+IRMCDCHP+IRMEDICR+IRPRVHLT+IRINSUR4+AMIYR_U+SMMIYR_U+SMIYR_U+IRMARIT,family=quasibinomial(logit),  data=dfadults, design=desgadults)
summary(svyglm.out)
#krataw statistika shmantikes metablhtes kai thewrw neo aplopihmeno montelo pou 
#tha to sugkrinw me to palio
#IRSEX   CATAG3 EDUHIGHCAT ABUSEPYPSY AMIYR_USMMIYR_USMIYR_U (stathstika shmantikoi oroi)

#aplopoihmeno montelo
svyglm.out1<- svyglm(MHSUTK_U~IRSEX+CATAG3+EDUHIGHCAT+ABUSEPYPSY+AMIYR_U+SMMIYR_U+SMIYR_U ,family=quasibinomial(logit),  data=dfadults, design=desgadults)
svyglm.out1

anova(svyglm.out1,svyglm.out,test="Chisq")#sugkrish montelwn
#isodunama montela(p= 0.36706 >0,05) ,opote pairnw to pio aplo ,to svyglm.out1
summary(svyglm.out1)
#eksetazw ki se auto ti tha prokupsei eksairwntas to statistika ashmanto oro ABUSEPYPSY, an exei diafora

#etsi to trito montelo me stathmiseis tha einai
svyglm.out2<- svyglm(MHSUTK_U~IRSEX+CATAG3+EDUHIGHCAT+AMIYR_U+SMMIYR_U+SMIYR_U ,family=quasibinomial(logit),  data=dfadults, design=desgadults)
summary(svyglm.out2)
anova(svyglm.out1,svyglm.out2)
#dexomai thn Ho(p= 0.42262 >0,05) ki etsi krataw to pio aplo montelo,
#giati exw isodunamia montelwn
svyglm.out2$coefficients
#Teliko montelo einai to svyglm.out2
#afou einai isodunamo me to prohgoumeno kai einai to pio aplo

#opote twra efoson elaba upospsh to deigmatolhptiko sxediasmo, 
#exw montelo pou ektima thn pithanothta enas enhlikos na
#exei sobares skepseis autoktonias se mia xronia ki h 
#ektimwmenh pithanothta tha dinetai apo ton ekshs tupo
#sumfwna me tous suntelestes tou montelou

#p_MHSUTK_U=1/(1 + exp[–(-5.97199 +  6.66776 *AMIYR_U+0.97261 *SMMIYR_U+1.85512*SMIYR_U +
#(-0.40434)*CATAG3+(-0.83479)* IRSEX+(-0.14039)*EDUHIGHCAT)])







