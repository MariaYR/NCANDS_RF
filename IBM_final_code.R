
#NCANDS

mydata <-read.delim("Sample2017.tab", header = TRUE, sep = "\t", quote = "")

head(mydata) 
nrow(mydata)
install.packages("plyr") 
library("plyr") 

count(mydata, vars = "housing")
count(mydata, vars = "MalDeath")
count(mydata, vars = "chlvng")
count(mydata, vars = "mal1lev")
count(mydata, vars = "mal2lev ")
count(mydata, vars = "mal3lev")
count(mydata, vars = "StaTerr")

na_count <-sapply(mydata, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count



#count unique children 
library(tidyverse) 
mydata %>% 
  as.tibble() %>% 
  count(ChID)

#seems like the sample is limited to unique children
#count unique reports
mydata %>% 
  as.tibble() %>% 
  count(RptID)

mydata %>% 
  as.tibble() %>%
  group_by(RptID)%>%
  count(ChID)

table(mydata$StaTerr)
#1 XX - missing

#child was prior victim
table(mydata$chprior)
#1    2    9 
#2972 9055  160

#pertinent variables 
subyr #submission year 
StaTerr #state territory 
RptID #report id 
ChID #child id 
RptCnty #report county 
table(mydata$InvDate) #The date when CPS first had face-to-face contact with the alleged victim of child maltreatment

table(mydata$rptsrc) 
#1, 1387 - social service personnel 
#2, 1041 - medical personnel 
#3, 603 - mental health personnel 
#4, 2166 - legal, law enforcement, or criminal justice 
#5, 2208 - education personnel 
#6, 84 - child daycare provider 
#7, 49 - foster care provider
#8, 35 - alleged victim 
#9, 751 - parent 
#10, 834 - other relative
#11, 561 - friends/neghbor
#13, 918 - anonymous reporter 
#88, 962 - other
#99, 640 - unkown or missing 

table(mydata$rptdisp)
#1 2346, substantiated
#2 119, indicated, or reason to suspect
#3 99, alternative response, disposition victim
#4 1919, alternative response - disposition not a victim 
#5 7526, unsubstantiated
#6 8, unsubstantiated due to intentionally false reporting 
#7 146, closed, no finding 
#88 69, other
#99 7, unknown or missing 

table(mydata$RpDispDt)#Report disposition date 

table(mydata$notifs) #mandated or courtesy contacting of other agencies with overlapping or potentially overlapping jurisdiction
#concerning a report of child maltreatment 
#1 5176, none 
#2 3421, police/prosecutor 
#3 59, licensing agency 
#4 12, both #2 and #3
#8 452, other 
#9 735, unkown or missing 

#child ethnicity/hispanic or latino
table(mydata$CEthn)
#1 2475, Yes
#2 8084, No
#3 419,  Unable to determine
#9 1019, unkown or missing 

#Child's Living Arrangement 
table(mydata$chlvng)
#1 1041, Married Parents 
#2 347, Married Parents and Step Parents
#3 660, Unmarried Parents 
#4 446, Parents and Cohabiting Partner
#5 1039, Both Parents, Marital Status 
#6 2452, Single parent, mother only
#7 326, Single parent, father only
#8 746, Single parent, mother and other adult 
#9 159, Single parent, father and other adult 
#10 245, Non-parent relative caregiver (includes relative foster care)
#11 171, Non-relative caregiver (includes non-relative foster care)
#12 47, group home or residential facilty
#88 53, other setting 
#99 2259, uknown

#inadequate housing
table(mydata$fchouse)
#1 280, yes inadequate housing 
#2 4695, no 
#9 5804, unkown or missing 

table(mydata$fcmoney)
#1 723, yes
#2 5215, no 
#9 4677, missing or unknown 

table(mydata$postserv)
#This field indicates whether services began or continued for the child in the report or
#the child's family as a result of the CPS response to reported allegations
#1 3879, yes
#2 5471, no
#9 909, unkown or missing 

table(mydata$cochrep)
#1 400, Yes
#2 4031, No
#9 3173, Unkown or missing 

table(mydata$housing)
#Activities designed to assist individuals or families in locating, obtaining, or 
#retaining suitable housing.
#1 78, Yes
#2 5548, No
#9 2646, Unkown or missing 

table(mydata$RptVictim)
#0 10145, 
#1 2094, child is a victim on this report 
#explanation for above 
#IF any Mal1Lev through Mal4Lev has the value:
#1 = Substantiated, or 2 = Indicated or 
#Reason to Suspect OR If MalDeath = 1 (Child Died) THEN this value is 1 (Child is a Victim on This Report); 
#else it is 0 (Child is NOT a Victim on This Report)

#check number of missing values
na_count

#check factors 
str(mydata, list.len=ncol(mydata))

RptTm #report time 
InvStrTm #Investigation Disposition Date time
#note all time/date variables are in differnt time zones, convert to utc for now 

#create time difference variables
RptDt #report date
#The month, day, and year that the responsible agency was notified of the suspected 
#child maltreatment. If a State combines several allegations into one report, the date 
#of the report is the date of the initial allegation. The determination as to whether 
#additional allegations are considered part of the original report or whether they are 
#considered a new report is left to each State's procedures.

InvDate #investiagtion Date
#The date when CPS first had face-to-face contact with the alleged victim of child 
#maltreatment. If this face-to-face contact is not possible, the date would be when 
#CPS initially contacted any party who could provide information essential to the 
#investigation or assessment

RpDispDt #report disposition date 
#The month, day, and year that a decision was made by CPS or by a court regarding the disposition of a CPS response.
#This is the date that the final report disposition was determined. This date identifies 
#all reports to be included in the current data submission period. If maltreatment dispositions 
#are associated with different disposition dates, the report disposition date is assigned to 
#the most recent of these dates.

#number of days between report date and investigation date 
rptdt_test <- as.Date(mydata$RptDt, format="%Y-%m-%d", tz="UTC")
invdate_test <- as.Date(mydata$InvDate, format="%Y-%m-%d", tz="UTC")
mydata$rpt_to_inv <- difftime(as.POSIXct(invdate_test), as.POSIXct(rptdt_test, tz="UTC"), units="days")
table(mydata$rpt_to_inv)
#convert original to date as well 
mydata$RptDt <- as.Date(mydata$RptDt, format="%Y-%m-%d", tz="UTC")
mydata$InvDate <- as.Date(mydata$InvDate, format="%Y-%m-%d", tz="UTC")

#one variable between investigation date and disposition date 
rptdisdt_test <- as.Date(mydata$RpDispDt, format="%Y-%m-%d", tz="UTC")
mydata$inv_to_dis <- difftime(as.POSIXct(rptdisdt_test), as.POSIXct(invdate_test, tz="UTC"), units="days")
table(mydata$inv_to_dis)
#convert original to date as well 
mydata$RpDispDt <- as.Date(mydata$RpDispDt, format="%Y-%m-%d", tz="UTC")


#number of days between investigation date and service date
ServDate #service date, services that begin as a result of CPS response
#A service date that is more than 90 days past the disposition date of the report is 
#considered invalid for inclusion
srvdt_test <- as.Date(mydata$ServDate, format="%Y-%m-%d", tz="UTC")
mydata$inv_to_srv <- difftime(as.POSIXct(srvdt_test), as.POSIXct(invdate_test, tz="UTC"), units="days")
table(mydata$inv_to_srv)
#convert original to date as well 
mydata$ServDate <- as.Date(mydata$ServDate, format="%Y-%m-%d", tz="UTC")

#number of days between report date and removal date
RmvDate #removal date
#The month, day, and year that the child was removed from the care and supervision of his or her parents 
#or parental substitutes, during or as a result of the CPS response. If a child has been removed 
#more than once, the removal date is the first removal resulting from the CPS response.
#This is the date associated with the removal indicated in the variable Foster Care 
#Services (FosterCr).
rmdt_test <- as.Date(mydata$RmvDate, format="%Y-%m-%d", tz="UTC")
mydata$inv_to_dis <- difftime(as.POSIXct(rmdt_test), as.POSIXct(rptdt_test, tz="UTC"), units="days")
table(mydata$inv_to_dis)
#convert original to date as well 
mydata$RmvDate <- as.Date(mydata$RmvDate, format="%Y-%m-%d", tz="UTC")

#number of days betwen report date and petition date 
PetDate #petition date
#The month, day, and year that the juvenile court petition was filed.
petdt_test <- as.Date(mydata$PetDate, format="%Y-%m-%d", tz="UTC")
mydata$rpt_to_pet <- difftime(as.POSIXct(petdt_test), as.POSIXct(rptdt_test, tz="UTC"), units="days")
table(mydata$rpt_to_pet)
#convert original to date as well 
mydata$PetDate <- as.Date(mydata$PetDate, format="%Y-%m-%d", tz="UTC")

str(mydata, list.len=ncol(mydata))

#drop factors
drop <- c("RptID","ChID", "Per1ID","Per2ID","Per3ID","AFCARSID","FCDchDt","StFCID")
mydata_clean = mydata[,!(names(mydata) %in% drop)]
str(mydata_clean, list.len=ncol(mydata))

drop_idvars <- c("StaTerr", "RptID", "ChID", "RptTm", "InvStrTm", "Per1ID", "Per2ID", "Per3ID","AFCARSID","FCDchDt","StFCID")
mydata_clean = mydata_clean[,!(names(mydata_clean) %in% drop_idvars)]
str(mydata_clean, list.len=ncol(mydata_clean))

#convert all integer to numeric
#index by column 
mydata_clean[,1:2] <- lapply(mydata_clean[,1:2], as.numeric)
#check
str(mydata_clean)

#probably easier to do this with a for loop. *shrug*
mydata_clean[,5:6] <- lapply(mydata_clean[,5:6], as.numeric)
str(mydata_clean)

mydata_clean[,8:51] <- lapply(mydata_clean[,8:51], as.numeric)
str(mydata_clean)

mydata_clean[,53:55] <- lapply(mydata_clean[,53:55], as.numeric)
str(mydata_clean)

mydata_clean[,57] <- lapply(mydata_clean[,57], as.numeric)
str(mydata_clean)

mydata_clean[,59:135] <- lapply(mydata_clean[,59:135], as.numeric)
str(mydata_clean, list.len=ncol(mydata_clean))

#remove columns with too many NAs
na_count

mydata_clean %>% drop_na(per2rel:per2mal1)
#remove other vars that have many NAs and aren't necessary
#general threshold, over 10k NAs
drop.cols <- c('chmal2', 
               'mal2lev',
               'chmal3', 
               'mal4lev',
               'per2rel', 
               'per2prnt', 
               'per2cr', 
               'Per2Age', 
               'per2sex', 
               'P2RacAI', 
               'P2RacAs',   
               'P2RacBl',
               'P2RacNH', 
               'P2RacWh',
               'p2racud',
               'Per2Ethn', 
               'per2mil' ,  
               'per2pior',    
               'per2mal1',    
               'per2mal2',   
               'per2mal3',  
               'per2mal4',
               'per3rel',      
               'per3prnt',     
               'per3cr',       
               'Per3Age',      
               'per3sex',     
               'P3RacAI',      
               'P3RacAs',      
               'P3RacBl',      
               'P3RacNH',      
               'P3RacWh',      
               'p3racud',      
               'Per3Ethn',     
               'per3mil',      
               'per3pior',     
               'per3mal1',    
               'per3mal2',  
               'per3mal3',    
               'per3mal4') 
               
mydata_cleaner <- mydata_clean %>% select(-one_of(drop.cols))
str(mydata_cleaner, list.len=ncol(mydata_cleaner))

na_count2 <-sapply(mydata_cleaner, function(y) sum(length(which(is.na(y)))))
na_count2 <- data.frame(na_count2)
na_count2

#a few left 
drop.cols2 <- c('mal3lev', 
                'chmal4', 
                'RmvDate', 
                'PetDate', 
                'per1rel',        
                'per1prnt',       
                'per1cr',         
                'Per1Age',        
                'per1sex',        
                'P1RacAI',        
                'P1RacAs',        
                'P1RacBl',        
                'P1RacNH',        
                'P1RacWh',        
                'p1racud',        
                'Per1Ethn',       
                'per1mil',        
                'per1pior',       
                'per1mal1',       
                'per1mal2',       
                'per1mal3',       
                'per1mal4')       

mydata_cleaner <- mydata_cleaner %>% select(-one_of(drop.cols2))

na_count3 <-sapply(mydata_cleaner, function(y) sum(length(which(is.na(y)))))
na_count3 <- data.frame(na_count3)
na_count3

str(mydata_cleaner)

#remove date vars
drop.cols3 <- c('RptDt', 'InvDate','RpDispDt', 'ServDate')
mydata_cleaner <- mydata_cleaner %>% select(-one_of(drop.cols3))
str(mydata_cleaner)

#convert 'difftime' vars to numeric 
mydata_cleaner[,70:71] <- lapply(mydata_cleaner[,70:71], as.numeric)
str(mydata_cleaner)

#convert response variable to factor
mydata_cleaner$rptdisp <- as.factor(mydata_cleaner$rptdisp)
str(mydata_cleaner)

table(mydata_cleaner$rptdisp)

#Combine 5 &6 
#merge 88 & 99

mydata_cleaner$rptdisp <- mapvalues(mydata_cleaner$rptdisp, from = c("6"), to = c("5"))
table(mydata_cleaner$rptdisp)

mydata_cleaner$rptdisp <- mapvalues(mydata_cleaner$rptdisp, from = c("99"), to = c("88"))
table(mydata_cleaner$rptdisp)

#Current label definitions
#1 substantiated
#2 indicated or reason to suspect
#3 alternative response disposition-victim
#4 alternative response disposition-not a victim
#5 unsubstantiated or unsubstantiated due to intentionally false reporting
#7 closed-no finding
#88 other or Unknown or Missing

#re-code rptdisp to 3 levels: 
#1 = substantiated,indicated, or alt response (victim disposition) 
#2 = unsubstantiated, unsubstantiated due to intentionally false reporting
#3 = alternative response (not victim)

mydata_cleaner$rptdisp <- mapvalues(mydata_cleaner$rptdisp, from = c("2"), to = c("1"))
table(mydata_cleaner$rptdisp)

mydata_cleaner$rptdisp <- mapvalues(mydata_cleaner$rptdisp, from = c("3"), to = c("1"))
table(mydata_cleaner$rptdisp)

mydata_cleaner$rptdisp <- mapvalues(mydata_cleaner$rptdisp, from = c("4"), to = c("3"))
table(mydata_cleaner$rptdisp)

mydata_cleaner$rptdisp <- mapvalues(mydata_cleaner$rptdisp, from = c("5"), to = c("2"))
table(mydata_cleaner$rptdisp)

#get rid of others or missing (88) and closed no finding (7)
list_of_values <- c("1", "2", "3")
mydata_cleaner <- filter(mydata_cleaner, rptdisp %in% list_of_values)
table(mydata_cleaner$rptdisp)

#replace unknowns with na 
install.packages("naniar")
library(naniar)

#unkowns coded as 9
mydata_cleaner <- replace_with_na_at(data = mydata_cleaner,
                   .vars = c('chsex', 'ChAge', 'ChRacAI', 'ChRacAs', 
                             'ChRacBl', 'ChRacNH', 'ChRacWh', 'ChRacUD', 'CEthn',
                             'chmil', 'chprior', 'chmal1', 'MalDeath', 'cdalc', 'cddrug', 
                             'cdrtrd', 'cdemotnl', 'cdvisual', 'cdlearn', 'cdphys', 'cdbehav', 
                             'cdmedicl', 'fcalc', 'fcdrug', 'fcrtrd', 'fcemotnl', 'fcvisual', 'fclearn', 
                             'fcphys', 'fcmedicl', 'fcviol', 'fchouse', 'fcmoney', 'fcpublic', 'postserv', 
                             'famsup', 'fampres', 'fostercr', 'juvpet', 'cochrep', 'adopt', 'casemang', 
                             'counsel', 'daycare', 'educatn', 'employ', 'famplan', 'health', 'homebase',
                             'housing', 'transliv', 'inforef', 'legal', 'menthlth', 'pregpar', 'respite', 
                             'ssdisabl', 'ssdelinq', 'subabuse', 'transprt', 'othersv'),
                   condition = ~.x == 9)

#replace unable to determine for race vars (coded as 3) to na as well 
mydata_cleaner <- replace_with_na_at(data = mydata_cleaner,
                                     .vars = c('ChRacAI', 'ChRacAs', 'ChRacBl', 'ChRacNH',
                                                'ChRacWh','CEthn', 'ChRacUD'),
                                               condition = ~.x == 3)
table(mydata_cleaner$ChRacAI)

#replace unkowns coded as 99
mydata_cleaner <-replace_with_na_at(data = mydata_cleaner,
                   .vars = c('rptsrc', 'notifs','chlvng', 'mal1lev'),
                   condition = ~.x == 99)

na_count2 <-sapply(mydata_cleaner, function(y) sum(length(which(is.na(y)))))
na_count2 <- data.frame(na_count2)
na_count2

#remove subyr and RptCnty
drops <- c("subyr","RptCnty", "RptVictim")
mydata_cleaner <- mydata_cleaner %>% select(-one_of(drops))
names(mydata_cleaner)

drops2 <- c("RptVictim")
mydata_cleaner <- mydata_cleaner %>% select(-one_of(drops2))
names(mydata_cleaner)

drops3 <- c("mal1lev")
mydata_cleaner <- mydata_cleaner %>% select(-one_of(drops3))
names(mydata_cleaner)

#drop rptvictim and mal1lev - these occur as a result of the disposition
#this drop accuracy dramatically
#drops2 <- c("RptVictim","mal1lev")
#mydata_cleaner <- mydata_cleaner %>% select(-one_of(drops2))

#recode NOs (2) to 0 

mydata_cleaner <- mydata_cleaner %>% 
  mutate_at(c('ChRacAI', 'ChRacAs', 
              'ChRacBl', 'ChRacNH', 'ChRacWh', 'ChRacUD', 'CEthn',
              'chmil', 'chprior', 'chmal1', 'MalDeath', 'cdalc', 'cddrug', 
              'cdrtrd', 'cdemotnl', 'cdvisual', 'cdlearn', 'cdphys', 'cdbehav', 
              'cdmedicl', 'fcalc', 'fcdrug', 'fcrtrd', 'fcemotnl', 'fcvisual', 'fclearn', 
              'fcphys', 'fcmedicl', 'fcviol', 'fchouse', 'fcmoney', 'fcpublic', 'postserv', 
              'famsup', 'fampres', 'fostercr', 'juvpet', 'cochrep', 'adopt', 'casemang', 
              'counsel', 'daycare', 'educatn', 'employ', 'famplan', 'health', 'homebase',
              'housing', 'transliv', 'inforef', 'legal', 'menthlth', 'pregpar', 'respite', 
              'ssdisabl', 'ssdelinq', 'subabuse', 'transprt', 'othersv'), 
            funs(recode(., `2`=0)))
#check
table(mydata_cleaner$ChRacUD)

#relevel rptdisp so that unsubstantiated is 1st
mydata_cleaner$rptdisp <- relevel(mydata_cleaner$rptdisp, "2")
table(mydata_cleaner$rptdisp)

#drop empty classes
mydata_cleaner$rptdisp = droplevels(mydata_cleaner$rptdisp)
table(mydata_cleaner$rptdisp)

#random forest on sample, proof of concept  
install.packages("randomForest")
library(randomForest)
#summary(mydata_cleaner)
#str(mydata_cleaner)

# Split into Train and Validation sets
# Training Set : Validation Set = 70 : 30 (random)
set.seed(1234)

mydata_cleaner <- na.roughfix(mydata_cleaner)
train <- sample(nrow(mydata_cleaner), 0.7*nrow(mydata_cleaner), replace = FALSE)
TrainSet <- mydata_cleaner[train,]
ValidSet <- mydata_cleaner[-train,]
summary(TrainSet)
summary(ValidSet)

#run random forest on all variables to predict report disposition (rptdisp)
#Ntree: Number of trees to grow. This should not be set to too small a number, 
#to ensure that every input row gets predicted at least a few times.

#Mtry: Number of variables randomly sampled as candidates at each split. 
#Note that the default values are different for classification (sqrt(p) 
#where p is number of variables in x) and regression (p/3) 

# Create a Random Forest model with default parameters
#options(stringsAsFactors = FALSE)
#model1 <- randomForest(rptdisp ~ ., data = TrainSet, importance = TRUE)
#model1 

#fine tuning parameters = increase mtry (number of variables tried at each split)from 8 to 12
model2 <- randomForest(rptdisp ~ ., data = TrainSet, ntree = 500, mtry = 12 , importance = TRUE)
model2 #error rate 24.22%

#increase mtry to 14
model3 <- randomForest(rptdisp ~ ., data = TrainSet, ntree = 750, mtry = 14, importance = TRUE)
model3 #error rate 24.4%

#decrease mtry to 13 
model4 <- randomForest(rptdisp ~ ., data = TrainSet, ntree = 750, mtry = 13, importance = TRUE)
model4 #error rate 24.34%

#Call:
#  randomForest(formula = rptdisp ~ ., data = TrainSet, ntree = 750,      mtry = 13, importance = TRUE) 
#Type of random forest: classification
#Number of trees: 750
#No. of variables tried at each split: 13

#OOB estimate of  error rate: 24.34%
#Confusion matrix:
#  2   1   3 class.error
#2 4835 249 159  0.07781804
#1 1039 736  45  0.59560440
#3  486  69 793  0.41172107

# Predicting on training set
predTrain <- predict(model2, TrainSet, type = "class")
# Checking classification accuracy
table(predTrain, TrainSet$rptdisp)

#predTrain    2    1    3
#         2 5177  482  270
#         1    4 1316    4
#         3   62   22 1074

# Predicting on Validation set
predValid <- predict(model2, ValidSet, type = "class")
# Checking classification accuracy
mean(predValid == ValidSet$rptdisp) 
#accuracy = 75.1%

table(predValid,ValidSet$rptdisp)
#   predValid    2    1    3
#             2 2098  465  214
#             1  114  267   13
#             3   79   12  344

#outcome of interst is 2. 
#2 = unsubstantiated, unsubstantiated due to intentionally false reporting

#check important variables
# To check important variables
importance(model2)   
#how much removing each variable reduces the accuracy of the model
#so the higher the number the more important the variable

varImpPlot(model2, main = "Variable Importance: Final Model") 

# identify the right mtry for model
a=c()
i=5
for (i in 3:20) {
  model5 <- randomForest(rptdisp ~ ., data = TrainSet, ntree = 750, mtry = i, importance = TRUE)
  predValid <- predict(model5, ValidSet, type = "class")
  a[i-2] = mean(predValid == ValidSet$rptdisp)
}

a

plot(3:20,a, main = "Model Accuracy for mtry = 3:20")
#mtry best at about 12, model 2

######
# prevention services vars
#'famsup', 'fampres', 'postserv', 'casemang', counsel, daycare, educatn, employ, famplan
#health, 'housing', 'transliv', homebase, inforef, 'legal', 'menthlth', 'pregpar', 'respite', 
#'ssdisabl', 'ssdelinq', 'subabuse', 'transprt', 'othersv'
#######

#random forest exaplainer
#install.packages("randomForestExplainer")
library(randomForestExplainer)
#https://cran.rstudio.com/web/packages/randomForestExplainer/vignettes/randomForestExplainer.html

#best model - model 2
model2

#try one more model with 650 trees 

#distributiuon of minimal depth 
library(forcats)
min_depth_frame <- min_depth_distribution(model2)
#Minimal depth for a variable in a tree equals to the depth of the node 
#which splits on that variable and is the closest to the root of the tree. 
#If it is low than a lot of observations are divided into groups on the 
#basis of this variable
save(min_depth_frame, file = "min_depth_frame.rda")
load("min_depth_frame.rda")
head(min_depth_frame, n = 10)

# plot_min_depth_distribution(forest) 
plot_min_depth_distribution(min_depth_frame)
# y -axis = top ten variables 
#The x-axis ranges from zero trees to the maximum number of trees in which 
#any variable was used for splitting (B^) which is in this case equal to 500 and
#is reached by all variables plotted.

#The default option, "top_trees", penalizes missing values and this 
#penalization makes the interpretation of the values less obvious – 
#to address that we can calculate the mean minimal depth only using 
#non-missing observations

plot_min_depth_distribution(min_depth_frame, 
                            mean_sample = "relevant_trees",
                            min_no_of_trees = 12,
                            main = "Distribution of Minimal Depth & Mean: Top 10 Variables")
#relevant_trees ignores missing values, min_no_of_trees sets threshold for 
#number of trees that varibale must be included in for plotting 

plot_min_depth_distribution(min_depth_frame, 
                            mean_sample = "relevant_trees",
                            k = 15,
                            min_no_of_trees = 20,
                            main = "Distribution of Minimal Depth & Mean: Top 15 Variables")
#same variables as above, used as final plot 


#variable importance measures
importance_frame <- measure_importance(model2)
save(importance_frame, file = "importance_frame.rda")
load("importance_frame.rda")
importance_frame
#68 rows

plot_multi_way_importance(importance_frame, 
                          size_measure = "no_of_nodes",
                          min_no_of_trees = 20)
#default x-measure = times_a_root
#default y_measure = mean_min_depth
# negative relation between times_a_root and mean_min_depth

plot_multi_way_importance(importance_frame, 
                          size_measure = "p_value",
                          min_no_of_trees = 20)

plot_multi_way_importance(importance_frame, 
                         x_measure = "accuracy_decrease", 
                         y_measure = "gini_decrease", 
                         size_measure = "p_value") 
                         #min_no_of_trees = 20,
                         #no_of_labels = 5)
#plot importance of pairs
plot_importance_ggpairs(importance_frame)

#replot using these three measures: 
#no_nodes & accuracy_decrease
#times_a_root & no_nodes

#final importance plots
plot_multi_way_importance(importance_frame, 
                          x_measure = "accuracy_decrease", 
                          y_measure = "gini_decrease", 
                          size_measure = "no_of_nodes") 

plot_multi_way_importance(importance_frame, 
                          x_measure = "accuracy_decrease", 
                          y_measure = "gini_decrease", 
                          size_measure = "times_a_root") 

#To extract the names of 5 most important variables according to both 
#the mean minimal depth and number of trees in which a variable appeared, 
#pass importance_frame to the function important_variables 

# (vars <- important_variables(forest, k = 5, measures = c("mean_min_depth", "no_of_trees"))) # gives the same result as below but takes longer
(vars <- important_variables(importance_frame, k = 5, measures = c("mean_min_depth", "no_of_trees")))
#[1] "fostercr"   "rptsrc"     "inv_to_srv" "famsup"     "fcdrug"    

#obtain a data frame containing information on mean conditional 
#minimal depth of variables with respect to each element of vars
interactions_frame <- min_depth_interactions(model2, vars)
save(interactions_frame, file = "interactions_frame.rda")
load("interactions_frame.rda")
head(interactions_frame[order(interactions_frame$occurrences, decreasing = TRUE), ])

# plot_min_depth_interactions(model2) # calculates the interactions_frame for default settings so may give different results than the function below depending on our settings and takes more time
plot_min_depth_interactions(interactions_frame)
#interactions are ordered by decreasing number of occurrences
#Using the default mean_sample = "top_trees" penalizes interactions 
#that occur less frequently than the most frequent one.

#plot highest interactions
plot_predict_interaction(model2, mydata_cleaner, "fostercr", "ChRacBl")
plot_predict_interaction(model2, mydata_cleaner, "fostercr", "CEthn")
plot_predict_interaction(model2, mydata_cleaner, "fostercr", "chprior")
plot_predict_interaction(model2, mydata_cleaner, "fostercr", "ChRacWh")

plot_predict_interaction(model2, mydata_cleaner, "rpt_to_inv", "ChRacBl")
plot_predict_interaction(model2, mydata_cleaner, "inv_to_srv", "ChRacBl")

table(mydata_cleaner$fostercr)

#interactions of interest
plot_predict_interaction(model2, mydata_cleaner, "rptdisp", "ChRacBl")

#random forest explainer full 
explain_forest(model2, interactions = TRUE, data = mydata_cleaner)
