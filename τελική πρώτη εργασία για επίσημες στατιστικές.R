#Προαιρετική Εργασία 1
#Όνομα:Μπαλκούδη Μιχαέλα
#ΑΕΜ:776
########################################################3
#Κάτω από κάθε ερώτηση να τοποθετήσετε το κώδικα-απάντηση της αντίστοιχης ερώτησης
#Μπορείτε για κάθε απάντηση να χρησιμοποιήσετε οποιοδήποτε μοτίβο κώδικα έχετε διδαχθεί
#An den emfanizontai sosta ta ellinika epilegetai apo to menu tools->global options->code->saving->default code encoding->utf-8
#epeita epilegetai apply kleinete to arxeio kai to ksanaanoigete
str(DelayedFlights)
library(dplyr)
library(readr)
library(tidyr)
library(tidyverse)
library(readr)
DelayedFlights <- read_csv("C:/Users/user/Desktop/DelayedFlights.csv")
View(DelayedFlights)
#Ερώτηση 1:να βρείτε (αν υπάρχουν) και να εμφανίσετε το πλήθος των κενών γραμμών σε κάθε στήλη του dataset
a<-c()
for(i in 1:30)
{ 
  is.na(DelayedFlights[,i])
  a[i]<-sum(is.na(DelayedFlights[,i]))
}
#Παρακάτω βλέπουμε το διάνυσμα που στην i-στήλη δείχνει το πλήθος των κενών γραμμών στην i-στη στήλη του dataset.
print(a)
#Ερώτηση 2:να υπολογίσετε και να εμφανίσετε ποια ημέρα σε ποιον μήνα σημειώθηκαν οι περισσότερες καθυστερήσεις πτήσεων
  
DelayedFlights$Month1<-factor(DelayedFlights$Month)
DelayedFlights$DayofMonth1<-factor(DelayedFlights$DayofMonth)

a5<-filter(DelayedFlights,ArrDelay>0)%>%
group_by(DayofMonth1,Month1)%>%
tally%>%arrange(desc(n))
a5


#Άρα, οι περισσότερες καθυστερήσεις πτήσεων συνέβησαν τη δεύτερη ημέρα του Ιανουαρίου.

#Ερώτηση 3: να υπολογίσετε και να εμφανίσετε τον ημερήσιο μέσο όρο καθυστερήσεων για καθέναν από τους θερινούς μήνες του 2008

a6<-filter(DelayedFlights,Month==6,ArrDelay>0)
a7<-group_by(a6,Month1,DayofMonth1)
a8<-summarise(a7,m1=mean(ArrDelay,na.rm=TRUE))
#Στον παρακάτω πίνακα βέπουμε για κάθε μέρα του Ιουνίου τον ημερήσιο μέσο όρο των καθυστερήσεων των πτήσεων.
a8

a9<-filter(DelayedFlights,Month==7,ArrDelay>0)
a10<-group_by(a9,Month1,DayofMonth1)
a11<-summarise(a10,m2=mean(ArrDelay,na.rm=TRUE))
#Στον παρακάτω πίνακα βέπουμε για κάθε μέρα του Ιουλίου τον ημερήσιο μέσο όρο των καθυστερήσεων των πτήσεων.
a11

a12<-filter(DelayedFlights,Month==8,ArrDelay>0)
a13<-group_by(a12,Month1,DayofMonth1)
a14<-summarise(a13,m3=mean(ArrDelay,na.rm=TRUE))
#Στον παρακάτω πίνακα βλέπουμε για κάθε μέρα του Αυγούστου τον ημερήσιο μέσο όρο των καθυστερήσεων των πτήσεων.
a14

#Ερώτηση 4: να υπολογίσετε και να εμφανίσετε το όνομα της αεροπορικής εταιρίας που είχε το μεγαλύτερο πλήθος κωδικών ακύρωσης τύπου Β
str(DelayedFlights$UniqueCarrier)
DelayedFlights$UniqueCarrier1<-factor(DelayedFlights$UniqueCarrier)
length(levels(DelayedFlights$UniqueCarrier1))
a15<-group_by(DelayedFlights,DelayedFlights$UniqueCarrier1)%>%
  filter(CancellationCode=='B')%>%
  tally%>%arrange(desc(n))
a15
#Άρα,η αεροπορική MQ είχε το μεγαλύτερο πλήθος κωδικών ακύρωσης τύπου Β. 


#Ερώτηση 5: να βρείτε τους κωδικούς των πτήσεων με τον μεγαλύτερο αριθμό καθυστερήσεων

DelayedFlights$FlightNum1=factor(DelayedFlights$FlightNum)
length(levels(DelayedFlights$FlightNum1))
DelayedFlights$dif<-DelayedFlights$ArrTime-DelayedFlights$CRSArrTime
a16<-group_by(DelayedFlights,FlightNum1)%>%
  filter(dif>0)%>%
  tally%>%arrange(desc(n))
a16
#'Αρα, ο κωδικός πτήσης 50 έχει το μεγαλυτερο αριθμό καθυστερήσεων  1340.


#Ερώτηση 6: να βρείτε και να υπολογίσετε το όνομα του μεγαλύτερου σε απόσταση προορισμού με τις περισσότερες καθυστερήσεις
DelayedFlights$Dest1<-factor(DelayedFlights$Dest)
#Θα βρω αρχικά τη μεγαλύτερη απόσταση προορισμού
a18<-filter(DelayedFlights,ArrDelay>0)%>%
  arrange(desc(Distance))
select(a18,Distance)
#Θα βρω το πλήθος των καθυστερήσεων των  προορισμών  με τη μεγαλύτερη απόσταση, δηλαδή 4962.
a19<-group_by(a18,Dest1)%>%
  filter(Distance==4962)%>%
  tally%>%arrange(desc(n))
a19
#΄Αρα, ο HNL είναι το όνομα του μεγαλύτερου σε απόσταση προορισμού με τις περισσότερες καθυστερήσεις

#Ερώτηση 7: να βρείτε και να εμφανίσετε τους προορισμούς που είχαν την μεγαλύτερη καθυστέρηση (πτήσεις που εκτελέστηκαν)

#Θα εξετάσω ποιός από τους προορισμούς είχε τη μεγαλύτερη καθυστέρηση, για να το κάνω αυτό
#θα εξετάσω τη μέση καθυστέρηση πτήσεων για κάθε προορισμό κι θα βρώ ποιος θα έχει τη μεγαλύτερη
#μέση καθυστέρηση

a20<-group_by(DelayedFlights,Dest1)%>%
  filter(Cancelled==0,ArrDelay>0)%>%
  summarise(me=mean(ArrDelay),na.rm=TRUE)
a20
arrange(a20,desc(me))
#Άρα, ο προορισμός με τη μεγαλύτερη μέση καθυστέρηση πτήσεων είναι o ΜQT.


#Ερώτηση 8: να βρείτε και να εμφανίσετε το όνομα της αεροπορικής εταιρείας που είχε τις μεγαλύτερες καθυστερήσεις που οφείλονται σε καθυστερημένη άφιξη αεροσκαφών
DelayedFlights$UniqueCarrier1<-factor(DelayedFlights$UniqueCarrier)
#Θα υπολογίσω για την κάθε αεροπορική εταιρεία τη μέση τιμή των καθυστερήσεων που 
# που οφείλονται σε καθυστερημένη άφιξη αεροσκαφών  και θα συγκρίνω τις μέσες τιμές τους
# για να δω ποια είχε τη μεγαλύτερη καθυστέρηση. 
a26<-group_by(DelayedFlights,UniqueCarrier1)

a27<- filter(a26,ArrDelay>0,LateAircraftDelay>0)%>%
  summarise(me1=mean(LateAircraftDelay,na.rm=TRUE))
a271<-arrange(a27,desc(me1))
a271
#Άρα, η εταιρεία YV έχει τη μεγαλύτερη μέση τιμή καθυστερήσεων που οφείλονται σε καθυστερημένη άφιξη αεροσκαφών

class(DelayedFlights$DayofMonth)
DelayedFlights$Month1<-factor(DelayedFlights$Month)
#Ερώτηση 9: να υπολογίσετε πόσες ακυρώσεις πτήσεων τύπου Α σημειώθηκαν την 13η ημέρα κάθε μήνα

DelayedFlights$Month1<-factor(DelayedFlights$Month)
a28<-filter(DelayedFlights,CancellationCode=="A",DayofMonth==13,Cancelled==1)
a29<-group_by(a28,Month1) %>% 
  summarise(n())
a29
#Aρα, ακυρώσεις πτήσεων τύπου Α  την 13η ημέρα του μήνα, έγιναν μόνο κατά τον Νοέμβριο με πλήθος 2
#και το Δεκέμβριο με πλήθος 3.

#Ερώτηση 10: υπολογίσετε και να εμφανίσετε την μέση καθυστέρηση πτήσεων που εκτελέστηκαν από την 10η μέχρι την 23 Απριλίου 2008
a30<-filter(DelayedFlights,DayofMonth>=10,Month==4,Cancelled==0)
a31<-filter(a30,DayofMonth<=23,ArrDelay>0)%>%
  summarise(mean(ArrDelay,na.rm=TRUE))
a31
#Άρα, η μέση καθυστέρηση πτήσεων που εκτελέστηκαν από την 10η μέχρι την 23 Απριλίου 2008 είναι 41.3.

#Ερώτηση 11: να υπολογίσετε και να εμφανίσετε τον μήνα που σημειώθηκε η μεγαλύτερη καθυστέρηση που οφειλόταν σε έλεγχους ασφαλείας κατά τις ώρες 06.00-14.00
#α τρόπος
#Εφόσον πρόκειται για καθυστερήσεις που οφείλονται σε ελέγχους ασφαλείες,
#θα εργαστώ ως προς τους χρόνους αναχωρήσεων DepTime.
a32<-filter(DelayedFlights,CRSDepTime>=0600,DepDelay>0,SecurityDelay>0)%>%
  filter(CRSDepTime<=1400)%>%
  arrange(desc(SecurityDelay))%>%
  select(Month,SecurityDelay,DepDelay)
a32
#Άρα,ο μήνας που σημειώθηκε η μεγαλύτερη καθυστέρηση που οφειλόταν σε ελέγχους ασφαλείας κατά τις ώρες 06.00-14.00 είναι ο Απρίλιος.

#β τρόπος
#υπολογίζω κι δεύτερο τρόπο σε περίπτωση που η καθυστέρηση για λόγους ασφαλείες ισυδυναμεί
#με το ενδεχόμενο η καθυστέρηση αναχώρησης DepDelay να είναι ίση με την SecurityDelay
a34<-filter(DelayedFlights,CRSDepTime>=0600)%>%
  filter(CRSDepTime<=1400,SecurityDelay==DepDelay)%>%
  arrange(desc(SecurityDelay))%>%
  select(Month,SecurityDelay,DepDelay)
a34
#Άρα,ο μήνας που σημειώθηκε η μεγαλύτερη καθυστέρηση που οφειλόταν σε ελέγχους ασφαλείας κατά τις ώρες 06.00-14.00 είναι ο Δεκέμβριος.

#Ερώτηση 12: να υπολογίσετε και να εμφανίσετε ποιος κωδικός πτήσης(αριθμός πτήσης) είχε το πρώτο δεκαήμερο του Νοεμβρίου του 2008 την μεγαλύτερη προ του αναμενόμενου χρόνου άφιξη στον προορισμό της
#α τρόπος: θεωρώ τη διαφορά CRSArrTime-ArrTime κι θέλω να είναι θετική
#για να έχω το χρόνο άφιξης πριν τον αναμενόμενο-προγραμματισμένο χρόνο άφιξης.
a35<-filter(DelayedFlights,Month==11,DayofMonth<=10,Cancelled==0)
a35$diafora1<-a35$CRSArrTime-a35$ArrTime
a35<-filter(a35,diafora1>0)
a36<-arrange(a35,desc(diafora1))
a37<-select(a36,UniqueCarrier,FlightNum,diafora1)
a37
#άρα  οι κωδικοί πτήσεων NW-428,B6-128,CO-209 το πρώτο δεκαήμερο του Νοεμβρίου του 2008 είχαν
#την μεγαλύτερη προ του αναμενόμενου χρόνου άφιξη στον προορισμό τους

# β τρόπος:Υπολογίζω το χρόνο άφιξης πριν τον αναμενόμενο χρόνο άφιξης,
# θεωρώντας μόνο τις αρνητικές τιμές της μεταβλητής ΑrrDelay
a35<-filter(DelayedFlights,Month==11,DayofMonth<=10,Cancelled==0)
a35<-filter(a35,ArrDelay<0)
a36<-arrange(a35,ArrDelay)
a37<-select(a36,UniqueCarrier,FlightNum,ArrDelay)
a37
#'Αρα, ο κωδικός πτήσης(αριθμός πτήσης) που είχε το πρώτο δεκαήμερο του Νοεμβρίου του 2008 την μεγαλύτερη προ του αναμενόμενου χρόνου άφιξη στον προορισμό της ήταν DL-95.

#'Αρα, βλέπω οτι υπάρχουν λάθη στη μεταβλητή ArrDelay σε σχέση με τη πραγματική διαφορά του χρόνου άφιξης κι του
#προγραμματισμένου.

#Ερώτηση 13 : να υπολογίσετε και να εμφανίσετε ποιο αεροδρόμιο (τοποθεσία αναχώρησης) είχε το δεύτερο δεκαήμερο του Αυγούστου 2018 τις περισσότερες πτήσεις με καθυστέρηση(αναχωρίσεων) μεγαλύτερη από μισή ώρα που οφείλονται στους αερομεταφορείς
DelayedFlights$Origin1<-factor(DelayedFlights$Origin)
a38<-filter(DelayedFlights,Month==8,DayofMonth>=11)
a39<-filter(a38,DayofMonth<=20,CarrierDelay>30)
a40<-group_by(a39,Origin1)%>%
  tally%>%arrange(desc(n))
a40
#Άρα η τοποθεσία αναχώρησης ATL είχε το δεύτερο δεκαήμερο του Αυγούστου 2018 τις περισσότερες πτήσεις με καθυστέρηση(αναχωρίσεων) μεγαλύτερη από μισή ώρα που οφείλονται στους αερομεταφορείς.

#Ερώτηση 14: να βρείτε και να εμφανίσετε τις πτήσεις που εκτράπηκαν από την πορεία τους αλλά ολοκληρώθηκαν καθώς και τον συνολικό χρόνο που απαιτήθηκε.

a41<-filter(DelayedFlights,Diverted==1,Cancelled==0)
dim(a41)
#Οπότε βλέπω ότι το πλήθος των πτήσεων που εκτράπηκαν αλλά ολοκληρώθηκαν ήταν 7754
#Η μεταβλητή του χρόνου εκτέλεσης μίας πτήσης δίνεται από την ActualElapsedTime
a41$ActualElapsedTime
length(a41$ActualElapsedTime)
dim(DelayedFlights)
str(a41)
#Παρατηρώ ότι οι τιμές της ActualElapsedTime για τις πτήσεις που εκτράπηκαν αλλά ολοκληρώθηκαν 
#περιέχουν πολλά NA , οπότε παρακάτω θα υπολογίσω το πλήθος τους.
is.na(a41$ActualElapsedTime)
length(is.na(a41$ActualElapsedTime))
#Βλέπω ότι οι ΝΑ τιμές της ActualElapsedTimε για τις πτήσεις που εκτράπηκαν αλλά ολοκληρώθηκαν 
# ήταν 7754, όσο κι το πλήθος αυτών των πτήσεων.
#Άρα, δεν έχω καμία πληροφορία για το συνολικό χρόνο που απαιτήθηκε για μία πτήση που εκτράπηκε αλλα εκτελέστηκε.

#Ερώτηση 15: ποιος μήνας είχε την μεγαλύτερη τυπική απόκλιση σε καθυστερήσεις ("πιο απρόβλεπτος μήνας"). Ως απόκλιση να θεωρηθεί η διαφορά ανάμεσα στον προγραμματισμένο και τον πραγματικό χρόνο εκτέλεσης της πτήσης
d<-c()
DelayedFlights$new<-abs(DelayedFlights$ActualElapsedTime-DelayedFlights$CRSElapsedTime)
for(i in 1:12)
{
  print(i)
  j<-filter(DelayedFlights,DelayedFlights$Month==i)
  d[i]<-sd(j$new,na.rm=TRUE)
  print(d[i])
}
which.max(d)
#'Αρα, ο μήνας με τη μεγαλύτερη τυπική απόκλιση σε καθυστερήσεις ("πιο απρόβλεπτος μήνας") είναι ο Ιούλιος.

#'#Τρόπος υποβολής εργασίας:αποστολή email στο smos@uom.edu.gr με το όνομα του github repository από το github
#όπου έχετε ανεβάσει το παραπάνω αρχείο με συμπληρωμένες τις αντίστοιχες ενότητες των απαντήσεων

#'