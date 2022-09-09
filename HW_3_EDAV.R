library(dplyr)


#####################3.1##################
###a
dirty_data<-read.csv("C:/Users/mixmp/Desktop/διερευνητικη ανάλυση/ΕΡΓΑΣΙΕΣ/homework3_Balkoudi/data.txt",
         header=TRUE, stringsAsFactors = FALSE)
View(dirty_data)
glimpse(dirty_data)
sapply(dirty_data, class)#retrieve the classes of all columns

#to pososto autwn pou den exoun missing values
sum(complete.cases(dirty_data)==TRUE)#the number of complete cases
sum(complete.cases(dirty_data)==TRUE)/nrow(dirty_data)#the percentage of complete cases

#vlepw me optiko tropo missing values kathe grammhs ki sthlhs ola mazi
library(visdat)
vis_miss(dirty_data)

#c
is.special <- function(x){
  if (is.numeric(x)) !is.finite(x) else is.na(x)
}
sapply(dirty_data, is.special)
dirty_data[sapply(dirty_data, is.special)]<-NA

#########################3.2################################3

##a
library(editrules)
#periexei tis sunthkes gia tis metablhtes
E <- editfile("C:/Users/mixmp/Desktop/διερευνητικη ανάλυση/ΕΡΓΑΣΙΕΣ/homework3_Balkoudi/rules.txt")
print(E)
##b
ve <- violatedEdits(E, dirty_data)
summary(ve,E)#vriskei posoi periorismoi den ikanopoiountai

#The constraint 2*Petal.Width<=Petal.Length is violated 3 times
#The constraint  Sepal.Length<=30 is violated 2 times
#The constraint Petal.Length<=Sepal.Length is violated 2 times
#The constraint Petal.Width<=Sepal.Width is violated 2 times
#The constraint Petal.Width<=Sepal.Width is violated 2 times
#The constraint  0<=Sepal.Width is violated 1 time

plot(ve)#grafikh parastash periorismwn pou dem ikanopoiountai
##!pososto twn periorismwn pou den ikanopoiountai
#I observe from the previous plot that there are 90 records with no violations
percentage_of_the_data_has_no_errors<-90/nrow(dirty_data)
percentage_of_the_data_has_no_errors#t percentage of the data has no errors
#d
petal_too_long <- which(ve[,7])#deixnei se poies grammes,parathrhseis den ikanopoieitai o 
#periorismos pou antistoixei ston 7o periorismo(7 grammh tou txt arxeiou me periorismous)

petal_too_long#I observe that on 35 and 43 the constraint Petal.Length<=Sepal.Length is violated
##e
boxplot(dirty_data$Sepal.Length)
boxplot.stats(dirty_data$Sepal.Length)

#stats: a vector of length 5, containing the extreme of the lower whisker, the lower ‘hinge’, the median, the upper ‘hinge’ 
        #and the extreme of the upper whisker.
#n:the number of non-NA observations in the sample
#conf:the lower and upper extremes of the ‘notch’ (if(do.conf)). See the details.
#out:the values of any data points which lie beyond the extremes of the whiskers (if(do.out)).

#!deixnei se poies grammes exw outliers
outliers_of_sepal.length<-boxplot.stats(dirty_data$Sepal.Length)$out# the Sepal.Length outliers
which(dirty_data$Sepal.Length %in% outliers_of_sepal.length )#the rows of the dataset with the Sepal.Length outliers


#sos!!!!!!!!!!!!!!antistoixw ta outliers mias metablhths me NA
#Replacing the the Sepal.Length outliers with NA values on the corresponding rows of the dataset
for (i in which(dirty_data$Sepal.Length %in% outliers_of_sepal.length ))
{
  dirty_data[i,"Sepal.Length"]=NA
}

boxplot(dirty_data$Sepal.Length)
#After the replacement of Sepal.Length outliers with NA, we observe that there are not any Sepal.Length outliers

########################3.3###########################3
#a read the conversion rules.
library(deducorrect)
R <- correctionRules("C:/Users/mixmp/Desktop/διερευνητικη ανάλυση/ΕΡΓΑΣΙΕΣ/homework3_Balkoudi/correct.txt")
print(R)#The constraint rule to avoid non positive values from Petal.Width
filter(dirty_data,Petal.Width<=0)#there were not any non positive Petal.width values on the dirty_data

correct.iris <- correctWithRules(R, dirty_data)#diorthwnw to data set me bash to arxeio twn periorismwn
correctWithRules(R, dirty_data)$corrections#No corrections needed on the variable Petal.Width, because there were not any non positive Petal.width values
correct.iris$corrected
View(correct.iris)
View(correct.iris[["corrected"]])
View(dirty_data)

###c
#!!!!!!!!!!!!!!!!!!!!!!
le <- localizeErrors(E, dirty_data)
summary(le$adapt)#the violations of each variable
summary(violatedEdits(E, dirty_data))

#the corrections of each constraint
#vazw NA stιs metablhtes pou den ikanopoioun tous periorismous
loc <- which(dirty_data$Petal.Length <= 0)
dirty_data[loc, "Petal.Length"] <- NA
loc <- which(dirty_data$Sepal.Length <= 0)
dirty_data[loc, "Sepal.Length"] <- NA
loc <- which(dirty_data$Petal.Width <= 0)
dirty_data[loc, "Petal.Width"] <- NA
loc <- which(dirty_data$Sepal.Width <= 0)
dirty_data[loc, "Sepal.Width"] <- NA
loc <- which(dirty_data$Sepal.Length>30)
dirty_data[loc, "Sepal.Length"] <- NA
loc <- which(dirty_data$Petal.Length >= dirty_data$Sepal.Length)
dirty_data[loc, "Petal.Length"] <- NA
loc <- which(dirty_data$Petal.Width >= dirty_data$Sepal.Width)
dirty_data[loc, "Petal.Width"] <- NA
loc <- which(dirty_data$Petal.Length<2*dirty_data$Petal.Width)
dirty_data[loc, "Petal.Length"] <- NA

#briskw poso suxna den ikanopoiountai oi periorismoi
#gia thn kathe metablhth
le <- localizeErrors(E, dirty_data)
summary(violatedEdits(E, dirty_data))
#I observe that there are not any violations on all constraints, because every valuee that caused violation was replaced with NA



##################################3.4#############################################3
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!111111111
#antikathistw oles tis NA times me thn KNN methodo

###a
library(VIM)
dirty_data1<-kNN(dirty_data)
which(is.na(dirty_data1))#All the NA values have been replaced by the kNN method

#3.4b
library(Hmisc)
# This is a function offered by the article
# x : vector to be imputed
# last : value to use if last value of x is empty
seqImpute <- function(x,last){
  n <- length(x)
  x <- c(x,last)
  i <- is.na(x)
  while(any(i)){
    x[i] <- x[which(i) + 1]
    i <- is.na(x)
  }
  x[1:n]
}
#Ordered Data Frame By Species and Imputation
dirty_data_spec <- dirty_data[order(dirty_data$Species),]#sorting the dataset on Species
View(dirty_data_spec)

#taksinomww dedomena me bash mia metablhth
data[order(data$metablhth),]


dirty_data_spec$Petal.Width <- seqImpute(dirty_data_spec$Petal.Width, median(dirty_data_spec$Petal.Width, na.rm = TRUE))
sum(is.na(dirty_data_spec$Petal.Width))#After using sequential hotdeck imputation to impute Petal.Width, there are not any NA Petal.Width values

df1=data.frame(pre_impute =  dirty_data[order(dirty_data$Species),]$Petal.Width, after_impute= dirty_data_spec$Petal.Width)  
df1#I observe that when we have NA value on Petal.width after imputation the NA value is replaced by the next one value on the dataset
#If we had an NA value on the dataset on the last row ,it would have been replaced by median(dirty_data_spec_SL$Petal.Width)

#3. Ordered Data Frame By Species & Sepal.Length

dirty_data_spec_SL <-arrange(dirty_data, Species,Sepal.Length)#sorting the dataset on Species and Sepal.Length
dirty_data_spec_SL
dirty_data_spec_SL$Petal.Width <- seqImpute(dirty_data_spec_SL$Petal.Width, median(dirty_data_spec_SL$Petal.Width, na.rm = TRUE))

df2=data.frame(pre_impute =  dirty_data[order(dirty_data$Species,dirty_data$Sepal.Length),]$Petal.Width, after_impute= dirty_data_spec_SL$Petal.Width)  
df2#I observe that when we have NA value on Petal.width after imputation the NA value is replaced by the next one value on the dataset. If we had an NA
#value on the dataset on the last row ,it would have been replaced by median(dirty_data_spec_SL$Petal.Width)


#Comparing The imputed vectors
comparison<- data.frame(order_by_species = dirty_data_spec$Petal.Width, order_by_species_and_Sepal.Length = dirty_data_spec_SL$Petal.Width)
head(comparison)
#The ordering made a big difference. But they are just permuted in some different order, to confirm that:

#Set difference
setdiff(comparison$order_by_species, comparison$order_by_species_and_Sepal.Length )#So the difference that we observed before,it was because of the permutation
#in some different order


