rm(list = ls())
library(boot)
library(bootstrap)
############
#Reading CSV and storing them in the dataframes
############

MCR<-read.csv('E:/ds-sem1/545/multipleChoiceResponses.csv')

forex<-read.csv('E:/ds-sem1/545/conversionRates.csv',stringsAsFactors = FALSE)

schema<-read.csv('E:/ds-sem1/545/schema.csv')

MCR$CompensationAmount<- as.numeric(as.character(gsub(',','',MCR$CompensationAmount)))

##Currency Conversion
library(dplyr)
MCR<-left_join(MCR, forex, by = c("CompensationCurrency"="originCountry"))
MCR$X<-NULL
MCR$CompensationAmount <- MCR$CompensationAmount * MCR$exchangeRate
subCol<-subset(schema, (Asked == "All" | Column=='CompensationAmount'))
s<-subCol[- grep("FreeForm", subCol$Column),]
sn<-s[- grep("Freeform", s$Column),]
s11<-sn[sn$Column!="EmploymentStatus" & sn$Column!="ParentsEducation"& sn$Column!="Tenure" &sn$Column!="LearningPlatformUsefulnessTradeBook" & sn$Column!="LearningPlatformUsefulnessPodcasts" & sn$Column!="LearningPlatformUsefulnessSO", ]
subset1<-MCR[,as.character(s11$Column)]
MCRX<-subset(subset1, Country=="United States")
MCRX$Country <-NULL
cleanTrainSet<-na.omit(MCRX)
dim(cleanTrainSet)
colSums(is.na(cleanTrainSet))

####
#Integer variables only
MCR1<-cbind(MCR[,grep("LearningCategory", names(MCR))],MCR$GenderSelect,MCR$Country,MCR$Age, MCR$EmploymentStatus, MCR$CompensationAmount, MCR$LearningDataScience, MCR$StudentStatus, MCR$CareerSwitcher)
colnames(MCR1)[7] <- "GenderSelect"
colnames(MCR1)[8] <- "Country"
colnames(MCR1)[9] <- "Age"
colnames(MCR1)[10] <- "EmploymentStatus"
colnames(MCR1)[11] <- "CompensationAmount"
colnames(MCR1)[12] <- "LearningDataScience"
colnames(MCR1)[13] <- "StudentStatus"
colnames(MCR1)[14] <- "CareerSwitcher"
names(MCR1)
MCR2<-subset(MCR1, Country=="United States")

MCRT<-MCR2[,-c(7,8,10,12,13,14)]
cleanT<-na.omit(MCRT)

##################
#Selecting columns for test data
##################

subCol1<-subset(schema, (Asked == "All"))
s2<-subCol1[- grep("FreeForm", subCol1$Column),]
s3<-s2[- grep("Freeform", s2$Column),]
#s3<-s5[- grep("LearningCategory", s5$Column),]
s4<-s3[s3$Column!="EmploymentStatus" & s3$Column!="ParentsEducation" & s3$Column!="Tenure" &s3$Column!="LearningPlatformUsefulnessTradeBook" & s3$Column!="LearningPlatformUsefulnessPodcasts" & s3$Column!="LearningPlatformUsefulnessSO", ]

###############
#Select testdata 1
###############

#unique(MCR1$EmploymentStatus)
students<-subset(MCR, (EmploymentStatus == "Not employed, and not looking for work" | EmploymentStatus == "I prefer not to say")& LearningDataScience =="Yes, I'm focused on learning mostly data science skills" & StudentStatus=="Yes")
subset2<-students[,as.character(s4$Column)]

#student<-subset(MCR1, (EmploymentStatus == "Not employed, and not looking for work" | EmploymentStatus == "I prefer not to say")& LearningDataScience =="Yes, I'm focused on learning mostly data science skills" & StudentStatus=="Yes")

####
students<-subset(subset2, Country=="United States")
#student1<-student1[,-c(7,8,10,11,12,13,14)]

students[is.na(students)] <- sample(1:100, size=sum(is.na(students)), replace=T)
students$Country <-NULL
cleanTestSet1<-na.omit(students)
dim(cleanTestSet1)

##################
train<- cleanT
test<- student

common<-intersect(train$Country,test$Country)
t2<-test[0,]
for (j in 1:length(common)){
  t2<-rbind(t2,test[test$Country==common[j],])
}


test<-t2
dim(test)

####################
###############
#Select testdata 2
###############

unique(MCR$EmploymentStatus)
NonStudents<-subset(MCR, (EmploymentStatus == "Not employed, and not looking for work" | EmploymentStatus == "I prefer not to say")& LearningDataScience =="Yes, I'm focused on learning mostly data science skills" & StudentStatus=="No")
subset3<-NonStudents[,as.character(s4$Column)]
Nonstudents<-subset(subset3, Country=="United States")
Nonstudents[is.na(Nonstudents)] <- sample(1:100, size=sum(is.na(Nonstudents)), replace=T)
Nonstudents$Country <-NULL
cleanTestSet2<-na.omit(Nonstudents)
dim(cleanTestSet2)

###############
#Select testdata 3
###############

unique(MCR$EmploymentStatus)
Unemp<-subset(MCR, (EmploymentStatus == "Not employed, but looking for work"))
subset4<-Unemp[,as.character(s4$Column)]
Unemp<-subset(subset4, Country=="United States")
Unemp$Country <-NULL
Unemp[is.na(Unemp)] <- sample(1:100, size=sum(is.na(Unemp)), replace=T)

#subset4[is.na(subset3)] <- 0
cleanTestSet3<-na.omit(Unemp)
dim(cleanTestSet3)

###############
#Select testdata 4
###############

unique(MCR$EmploymentStatus)
unique(MCR$CareerSwitcher)
switchers<-subset(MCR, (CareerSwitcher == "Yes"))
subset5<-switchers[,as.character(s4$Column)]
#subset4<-Unemp[,as.character(s4$Column)]
Unemp<-subset(subset5, Country=="United States")
MCR$CompensationAmount <- MCR$CompensationAmount * MCR$exchangeRate
subCol<-subset(schema, (Asked == "All" | Column=='CompensationAmount'))
s<-subCol[- grep("FreeForm", subCol$Column),]
sn<-s[- grep("Freeform", s$Column),]
s11<-sn[sn$Column!="EmploymentStatus" & sn$Column!="ParentsEducation"& sn$Column!="Tenure" &sn$Column!="LearningPlatformUsefulnessTradeBook" & sn$Column!="LearningPlatformUsefulnessPodcasts" & sn$Column!="LearningPlatformUsefulnessSO", ]
subset1<-MCR[,as.character(s11$Column)]
MCRX<-subset(subset1, Country=="United States")
MCRX$Country <-NULL
cleanTrainSet<-na.omit(MCRX)
dim(cleanTrainSet)
colSums(is.na(cleanTrainSet))

####
#Integer variables only
MCR1<-cbind(MCR[,grep("LearningCategory", names(MCR))],MCR$GenderSelect,MCR$Country,MCR$Age, MCR$EmploymentStatus, MCR$CompensationAmount, MCR$LearningDataScience, MCR$StudentStatus, MCR$CareerSwitcher)
colnames(MCR1)[7] <- "GenderSelect"
colnames(MCR1)[8] <- "Country"
colnames(MCR1)[9] <- "Age"
colnames(MCR1)[10] <- "EmploymentStatus"
colnames(MCR1)[11] <- "CompensationAmount"
colnames(MCR1)[12] <- "LearningDataScience"
colnames(MCR1)[13] <- "StudentStatus"
colnames(MCR1)[14] <- "CareerSwitcher"
names(MCR1)
MCR2<-subset(MCR1, Country=="United States")

MCRT<-MCR2[,-c(7,8,10,12,13,14)]
cleanT<-na.omit(MCRT)

##################
#Selecting columns for test data
##################

subCol1<-subset(schema, (Asked == "All"))
s2<-subCol1[- grep("FreeForm", subCol1$Column),]
s3<-s2[- grep("Freeform", s2$Column),]
#s3<-s5[- grep("LearningCategory", s5$Column),]
s4<-s3[s3$Column!="EmploymentStatus" & s3$Column!="ParentsEducation" & s3$Column!="Tenure" &s3$Column!="LearningPlatformUsefulnessTradeBook" & s3$Column!="LearningPlatformUsefulnessPodcasts" & s3$Column!="LearningPlatformUsefulnessSO", ]

###############
#Select testdata 1
###############

#unique(MCR1$EmploymentStatus)
students<-subset(MCR, (EmploymentStatus == "Not employed, and not looking for work" | EmploymentStatus == "I prefer not to say")& LearningDataScience =="Yes, I'm focused on learning mostly data science skills" & StudentStatus=="Yes")
subset2<-students[,as.character(s4$Column)]

#student<-subset(MCR1, (EmploymentStatus == "Not employed, and not looking for work" | EmploymentStatus == "I prefer not to say")& LearningDataScience =="Yes, I'm focused on learning mostly data science skills" & StudentStatus=="Yes")

####
students<-subset(subset2, Country=="United States")
#student1<-student1[,-c(7,8,10,11,12,13,14)]

students[is.na(students)] <- sample(1:100, size=sum(is.na(students)), replace=T)
students$Country <-NULL
cleanTestSet1<-na.omit(students)
dim(cleanTestSet1)

##################
train<- cleanT
test<- student

common<-intersect(train$Country,test$Country)
t2<-test[0,]
for (j in 1:length(common)){
  t2<-rbind(t2,test[test$Country==common[j],])
}


test<-t2
dim(test)

####################
###############
#Select testdata 2
###############

unique(MCR$EmploymentStatus)
NonStudents<-subset(MCR, (EmploymentStatus == "Not employed, and not looking for work" | EmploymentStatus == "I prefer not to say")& LearningDataScience =="Yes, I'm focused on learning mostly data science skills" & StudentStatus=="No")
subset3<-NonStudents[,as.character(s4$Column)]
Nonstudents<-subset(subset3, Country=="United States")
Nonstudents[is.na(Nonstudents)] <- sample(1:100, size=sum(is.na(Nonstudents)), replace=T)
Nonstudents$Country <-NULL
cleanTestSet2<-na.omit(Nonstudents)
dim(cleanTestSet2)

###############
#Select testdata 3
###############

unique(MCR$EmploymentStatus)
Unemp<-subset(MCR, (EmploymentStatus == "Not employed, but looking for work"))
subset4<-Unemp[,as.character(s4$Column)]
Unemp<-subset(subset4, Country=="United States")
Unemp$Country <-NULL
Unemp[is.na(Unemp)] <- sample(1:100, size=sum(is.na(Unemp)), replace=T)

#subset4[is.na(subset3)] <- 0
cleanTestSet3<-na.omit(Unemp)
dim(cleanTestSet3)

###############
#Select testdata 4
###############

unique(MCR$EmploymentStatus)
unique(MCR$CareerSwitcher)
switchers<-subset(MCR, (CareerSwitcher == "Yes"))
subset5<-switchers[,as.character(s4$Column)]
#subset4<-Unemp[,as.character(s4$Column)]
Unemp<-subset(subset5, Country=="United States")
MCR$CompensationAmount <- MCR$CompensationAmount * MCR$exchangeRate
subCol<-subset(schema, (Asked == "All" | Column=='CompensationAmount'))
s<-subCol[- grep("FreeForm", subCol$Column),]
sn<-s[- grep("Freeform", s$Column),]
s11<-sn[sn$Column!="EmploymentStatus" & sn$Column!="ParentsEducation"& sn$Column!="Tenure" &sn$Column!="LearningPlatformUsefulnessTradeBook" & sn$Column!="LearningPlatformUsefulnessPodcasts" & sn$Column!="LearningPlatformUsefulnessSO", ]
subset1<-MCR[,as.character(s11$Column)]
MCRX<-subset(subset1, Country=="United States")
MCRX$Country <-NULL
cleanTrainSet<-na.omit(MCRX)
dim(cleanTrainSet)
colSums(is.na(cleanTrainSet))
cleanTrainSet1<-na.omit(subset1)
####
#Integer variables only
MCR1<-cbind(MCR[,grep("LearningCategory", names(MCR))],MCR$GenderSelect,MCR$Country,MCR$Age, MCR$EmploymentStatus, MCR$CompensationAmount, MCR$LearningDataScience, MCR$StudentStatus, MCR$CareerSwitcher)
colnames(MCR1)[7] <- "GenderSelect"
colnames(MCR1)[8] <- "Country"
colnames(MCR1)[9] <- "Age"
colnames(MCR1)[10] <- "EmploymentStatus"
colnames(MCR1)[11] <- "CompensationAmount"
colnames(MCR1)[12] <- "LearningDataScience"
colnames(MCR1)[13] <- "StudentStatus"
colnames(MCR1)[14] <- "CareerSwitcher"
names(MCR1)
MCR2<-subset(MCR1, Country=="United States")

MCRT<-MCR2[,-c(7,8,10,12,13,14)]
cleanT<-na.omit(MCRT)

##################
#Selecting columns for test data
##################

subCol1<-subset(schema, (Asked == "All"))
s2<-subCol1[- grep("FreeForm", subCol1$Column),]
s3<-s2[- grep("Freeform", s2$Column),]
#s3<-s5[- grep("LearningCategory", s5$Column),]
s4<-s3[s3$Column!="EmploymentStatus" & s3$Column!="ParentsEducation" & s3$Column!="Tenure" &s3$Column!="LearningPlatformUsefulnessTradeBook" & s3$Column!="LearningPlatformUsefulnessPodcasts" & s3$Column!="LearningPlatformUsefulnessSO", ]

###############
#Select testdata 1
###############

#unique(MCR1$EmploymentStatus)
students<-subset(MCR, (EmploymentStatus == "Not employed, and not looking for work" | EmploymentStatus == "I prefer not to say")& LearningDataScience =="Yes, I'm focused on learning mostly data science skills" & StudentStatus=="Yes")
subset2<-students[,as.character(s4$Column)]

#student<-subset(MCR1, (EmploymentStatus == "Not employed, and not looking for work" | EmploymentStatus == "I prefer not to say")& LearningDataScience =="Yes, I'm focused on learning mostly data science skills" & StudentStatus=="Yes")

####
students<-subset(subset2, Country=="United States")
#student1<-student1[,-c(7,8,10,11,12,13,14)]

students[is.na(students)] <- sample(1:100, size=sum(is.na(students)), replace=T)
students$Country <-NULL
cleanTestSet1<-na.omit(students)
dim(cleanTestSet1)
clt1<-na.omit(subset2)

##################
train<- cleanT
test<- student

common<-intersect(train$Country,test$Country)
t2<-test[0,]
for (j in 1:length(common)){
  t2<-rbind(t2,test[test$Country==common[j],])
}


test<-t2
dim(test)

####################
###############
#Select testdata 2
###############

unique(MCR$EmploymentStatus)
NonStudents<-subset(MCR, (EmploymentStatus == "Not employed, and not looking for work" | EmploymentStatus == "I prefer not to say")& LearningDataScience =="Yes, I'm focused on learning mostly data science skills" & StudentStatus=="No")
subset3<-NonStudents[,as.character(s4$Column)]
Nonstudents<-subset(subset3, Country=="United States")
Nonstudents[is.na(Nonstudents)] <- sample(1:100, size=sum(is.na(Nonstudents)), replace=T)
Nonstudents$Country <-NULL
cleanTestSet2<-na.omit(Nonstudents)
dim(cleanTestSet2)
clt2<-na.omit(subset3)

###############
#Select testdata 3
###############

unique(MCR$EmploymentStatus)
Unemp<-subset(MCR, (EmploymentStatus == "Not employed, but looking for work"))
subset4<-Unemp[,as.character(s4$Column)]
Unemp<-subset(subset4, Country=="United States")
Unemp$Country <-NULL
Unemp[is.na(Unemp)] <- sample(1:100, size=sum(is.na(Unemp)), replace=T)

#subset4[is.na(subset3)] <- 0
cleanTestSet3<-na.omit(Unemp)
dim(cleanTestSet3)
clt3<-na.omit(subset4)
###############
#Select testdata 4
###############

unique(MCR$EmploymentStatus)
unique(MCR$CareerSwitcher)
switchers<-subset(MCR, (CareerSwitcher == "Yes"))
subset5<-switchers[,as.character(s4$Column)]
#subset4<-Unemp[,as.character(s4$Column)]
Swit<-subset(subset5, Country=="United States")
Swit$Country <-NULL

#subset4[is.na(subset3)] <- 0
cleanTestSet4<-na.omit(Swit)
dim(cleanTestSet4)

clt4<-na.omit(subset5)

##################
#best Subset selection
##################
library(leaps)
bestss <- regsubsets(CompensationAmount~., data = cleanTrainSet, nvmax = 40, method = "Exhaustive")
bss.sum<-summary(bestss)

cp1<-which(bss.sum$cp == min(bss.sum$cp))
pred.full <- predict.regsubsets(bestss, cleanTestSet1, id=cp1)
bic1<-which(bss.sum$bic == min(bss.sum$bic))
pred.full <- predict.regsubsets(bestss, cleanTestSet1, id=bic1)
x11()
plot(bss.sum$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
x11()
plot(bss.sum$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")


##################
#Linear regression
##################
lm.fit<-glm(CompensationAmount~.,data=cleanT)

NonStudentSalary <- predict.lm(lm.fit, student1)
studentSalary <- predict.lm(lm.fit, cleanTestSet2)
UnemploySalary <- predict.lm(lm.fit, cleanTestSet3)
CareerSwitcherSalary <- predict.lm(lm.fit, cleanTestSet4)
CompensationAmount<-abs(NonStudentSalary)
student1<-cbind(student1, CompensationAmount)
error1<-cv.glm(student1, lm.fit)
error1$delta[1]


###############
#Ridge
###############
library(glmnet)
trainmat<-as.matrix(cleanT)
testmat1<-as.matrix(student1)
ridgefit <- cv.glmnet(trainmat, cleanT[,9], alpha=0)
names(ridgefit)
bestlam1 <- ridgefit$lambda.min
ridgepred <- predict(ridgefit, s = bestlam1, newx = testmat1, type = "response")
ins.yhatr<-ridgepred

###############
#Lasso
###############
library(glmnet)
Lassofit <- cv.glmnet(trainmat, cleanT[,8], alpha=1)
names(Lassofit)
bestlam <- Lassofit$lambda.min
Lassopred <- predict(Lassofit, s = bestlam, newx = testmat1, type = "response")
sal<-Lassopred
i <- which(Lassofit$lambda == Lassofit$lambda.min)
LassoTestError <- Lassofit$cvm[i]
LassoTestError

####################
#Pcr
##################
library(pls)
pcr.fit = pcr(Apps ~ . , data = train1, scale = TRUE, validation = "CV")
cv.pcr <- crossval(pcr.fit, segments = 10)


#########################################
######CLASSIFICATION
#########################################
unique(schema$Asked)



subCol3<-subset(schema, (Asked == "Learners" | Asked == "All"))



subCol4<-subset(schema, (Asked == "worker1" | Asked == "All"))

subCol5<-subset(schema, (Asked == "worker1" | Asked == "All"))

subCol6<-subset(schema, (Asked == "OnlineLearners" | Asked == "All"))

subCol7<-subset(schema, (Asked == "Non-worker" | Asked == "All"))

subCol8<-subset(schema, (Asked == "CodingWorker-NC" | Asked == "All"))
