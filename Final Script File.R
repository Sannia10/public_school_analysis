#DATA CLEANING AND PREPARATION
#merge all files
setwd("C:/Users/Sannia/Desktop/Business Analytics/Project BA")
#install.packages("dplyr")
library(dplyr)
#install.packages("ggplot2")
library(ggplot2)
ba <- read.csv("ba data.csv", na.strings = c("", " ", "NA", "N/A"))
View(ba)

#making new variables
ba$TotalChildScore<- ba$child_math_scale2+ba$child_english_scale2+ba$child_urdu_scale2
ba$TotalTeacherScore<- ba$teacher_english_scale2+ba$teacher_math_scale2+ba$teacher_urdu_scale2
ba$TeacherAverageScore<- (ba$teacher_english_scale2+ba$teacher_math_scale2+ba$teacher_urdu_scale2)/3

#Cleaning ParentData
ParentData <- select(ba, childcode,ch2_s1q10, ch2_s1q11, ch2_s1q8, ch2_s1q9)
ParentData <- rename(ParentData,"FatherEducation" = "ch2_s1q11"  )
ParentData <- rename(ParentData,"MotherEducation" = "ch2_s1q10"  )
ParentData <- rename(ParentData,"MotherLivesinHouse" = "ch2_s1q8"  )
ParentData <- rename(ParentData,"FatherLivesinHouse" = "ch2_s1q9"  )
table(is.na(ParentData))

#Cleaning TeacherData
TeacherData<- select(ba, childcode, teacher_gender,teacher_age, teacher_years_in_teaching, teacher_years_in_school,teacher_qualification, teacher_training, teacher_salary, contract_type,bonus, marr, pvt_tution, teacher_english_scale2, teacher_math_scale2, teacher_urdu_scale2, TotalTeacherScore, TeacherAverageScore,  local)
TeacherData<- rename(TeacherData, "Teacher_Gender"= "teacher_gender", "Teacher_Age"="teacher_age", "Experience_in_Teaching"="teacher_years_in_teaching", "Experience_in_School"="teacher_years_in_school", "Teacher_Education"="teacher_qualification", "Teacher_Training"="teacher_training", "Monthly_Salary"="teacher_salary", "Contract_Type"="contract_type", "Teacher_Bonus"="bonus","Teacher_Marital_Status"="marr", "Teacher_Pvt_Tution"="pvt_tution", "Teacher_English_Score"= "teacher_english_scale2","Teacher_Math_Score"="teacher_math_scale2","Teacher_Urdu_Score"="teacher_urdu_scale2", "Local_Teacher"="local")
table(is.na(TeacherData))
colSums(is.na(TeacherData))

summary(TeacherData)
str(TeacherData)

#Cleaning ChildrenData
ChildData <- select(ba, childcode, child_female, child_english_scale2, child_math_scale2, child_urdu_scale2,TotalChildScore, avgscore) 
ChildData <- rename(ChildData, "Child_Gender" = "child_female")
ChildData <- rename(ChildData, "EnglishScore" = "child_english_scale2")
ChildData <- rename(ChildData, "UrduScore" = "child_urdu_scale2")
ChildData <- rename(ChildData, "MathScore" = "child_math_scale2")
ChildData <- rename(ChildData, "ChildAverageScore" = "avgscore")
table(is.na(ChildData))
#ChildData <- na.omit(ChildData)
View(ChildData)

#Cleaning DistrictData
DistrictData <- select (ba, childcode, tq2_s0q2, tq2_s6q7a_type, child_english_scale2, child_math_scale2, child_urdu_scale2)
DistrictData <- rename(DistrictData, "District" = "tq2_s0q2")
DistrictData <- rename(DistrictData, "TypeofSchool" = "tq2_s6q7a_type")

View(DistrictData)
#DistrictData <- na.omit(DistrictData)
table(is.na(DistrictData))


#final data
Data<- data.frame(DistrictData,ChildData, ParentData, TeacherData)
table(is.na(Data))
colSums(is.na(Data))
View(Data)

#removing repeated columns
Data$childcode.1<- NULL
Data$childcode.2<- NULL
Data$childcode.3<- NULL
Data$child_english_scale2<- NULL
Data$child_math_scale2<- NULL
Data$child_urdu_scale2<- NULL

#recoding
Data$Child_Gender<- recode(Data$Child_Gender, `1`="Female", `0`="Male")
Data$Local_Teacher<- recode(Data$Local_Teacher, `1`="Local", `0`="Non-Local")
Data$Teacher_Bonus<- recode(Data$Teacher_Bonus, `1`="Bonus", `0`="No Bonus")
Data$Teacher_Pvt_Tution<- recode(Data$Teacher_Pvt_Tution, `1`="Yes", `0`="No")
Data$Teacher_Marital_Status<- recode(Data$Teacher_Marital_Status, `1`="Married", `0`="Single")
head(Data)

table(is.na(Data))
colSums(is.na(Data))
FinalData<- na.omit(Data)
colSums(is.na(FinalData))
View(FinalData)

#csv file
#write.csv(FinalData, file = "BAProject.csv")


data<- read.csv("BAProject.csv")
#install.packages("Rtsne")
library(Rtsne)
library(cluster)
library(ggplot2)
library(dplyr)

data[["ChildAverageScore"]] <- ordered(cut(data$ChildAverageScore, c(0, 350, 650, 1000),  labels = c("Low", "Average", "High")))
data[["TeacherAverageScore"]]<- ordered(cut(data$TeacherAverageScore, c(0, 350, 650, 1000),  labels = c("Low", "Average", "High")))                             
View(data)

#FIRST ROUND OF CLUSTER ANALYSIS
#subsets on basis of scores
data<- subset(data, data$TypeofSchool=="Private School in sample mauza" | data$TypeofSchool== "Private School outside mauza"| data$TypeofSchool=="Government School outside mauza"| data$TypeofSchool=="Government School in sample mauza")
HighScore<- subset(data, data$ChildAverageScore== "High")
LowScore<- subset(data, data$ChildAverageScore== "Low")
AverageScore<- subset(data, data$ChildAverageScore== "Average")

#cluster based on high scores
gower_dist <- daisy(HighScore[, c(-1,-2)], metric = "gower")
gower_mat <- as.matrix(gower_dist)

#checking similar medoids
HighScore[
  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

#checking dissimilar medoids
HighScore[
  which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

#silhoutte method for clusters
sil_width <- c(NA)

#loop for cluster number 
for(i in 2:10){
  
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  sil_width[i] <- pam_fit$silinfo$avg.width
}

#plotting
plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")


lines(1:10, sil_width)

#metoids of high score
pam_fit <- pam(gower_dist, diss = TRUE, k = 2)
HighScore[pam_fit$medoids, ]

#summay results of highscore


#visualization for high score clUsters
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
         name = HighScore$childcode)

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))

#cluster based on lowscore
gower_dist1 <- daisy(LowScore[, c(-1,-2)], metric = "gower")
gower_mat1 <- as.matrix(gower_dist1)
LowScore[
  which(gower_mat1 == min(gower_mat1[gower_mat1 != min(gower_mat1)]),
        arr.ind = TRUE)[1, ], ]

LowScore[
  which(gower_mat1 == max(gower_mat1[gower_mat1 != max(gower_mat1)]),
        arr.ind = TRUE)[1, ], ]

#silhoutte method for clusters
sil_width1 <- c(NA)

#loop for cluster number 
for(i in 2:10){
  
  pam_fit1 <- pam(gower_dist,
                  diss = TRUE,
                  k = i)
  sil_width1[i] <- pam_fit1$silinfo$avg.width
}

#plotting
plot(1:10, sil_width1,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")


lines(1:10, sil_width1)

#metoids of low score
pam_fit1 <- pam(gower_dist1, diss = TRUE, k =2 )
LowScore[pam_fit1$medoids, ]

#visualization for low score clUsters
tsne_obj1 <- Rtsne(gower_dist1, is_distance = TRUE)

tsne_data1 <- tsne_obj1$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster1 = factor(pam_fit1$clustering),
         name = LowScore$childcode)

ggplot(aes(x = X, y = Y), data = tsne_data1) +
  geom_point(aes(color = cluster1))



#SECOND ROUND OF CLUSTER ANALYSIS
#subsets based on private vs public school
PrivateSchool<- subset(data, data$TypeofSchool== "Private School in sample mauza" | data$TypeofSchool== "Private School outside mauza")
PublicSchool<- subset(data, data$TypeofSchool== "Government School outside mauza"| data$TypeofSchool=="Government School in sample mauza")

#private school
gower_dist3 <- daisy(PrivateSchool[, c(-1,-2)], metric = "gower")
gower_mat3 <- as.matrix(gower_dist3)
PrivateSchool[
  which(gower_mat3 == min(gower_mat3[gower_mat3 != min(gower_mat3)]),
        arr.ind = TRUE)[1, ], ]

PrivateSchool[
  which(gower_mat3 == max(gower_mat3[gower_mat3 != max(gower_mat3)]),
        arr.ind = TRUE)[1, ], ]


#metoids
pam_fit3 <- pam(gower_dist3, diss = TRUE, k = 3)
PrivateSchool[pam_fit3$medoids, ]


#visualization
tsne_obj3 <- Rtsne(gower_dist3, is_distance = TRUE)

tsne_data3 <- tsne_obj3$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster3 = factor(pam_fit3$clustering),
         name = PrivateSchool$childcode)

ggplot(aes(x = X, y = Y), data = tsne_data3) +
  geom_point(aes(color = cluster3))


pam_results3 <- PrivateSchool %>%
  dplyr::select(-childcode, -X) %>%
  mutate(cluster3 = pam_fit3$clustering) %>%
  group_by(cluster3) %>%
  do(the_summary = summary(.))

pam_results3$the_summary




#public school
gower_dist4 <- daisy(PublicSchool[, c(-1,-2)], metric = "gower")
gower_mat4 <- as.matrix(gower_dist4)
PublicSchool[
  which(gower_mat4 == min(gower_mat4[gower_mat4 != min(gower_mat4)]),
        arr.ind = TRUE)[1, ], ]

PublicSchool[
  which(gower_mat4 == max(gower_mat4[gower_mat4 != max(gower_mat4)]),
        arr.ind = TRUE)[1, ], ]

#silhoutte method for clusters
sil_width4 <- c(NA)

#loop for cluster number 
for(i in 2:10){
  
  pam_fit4 <- pam(gower_dist4,
                  diss = TRUE,
                  k = i)
  sil_width4[i] <- pam_fit4$silinfo$avg.width
}

#plotting
plot(1:10, sil_width4,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")


lines(1:10, sil_width4)

#metoids
pam_fit4 <- pam(gower_dist4, diss = TRUE, k =2 )
PublicSchool[pam_fit4$medoids, ]




#visualization
tsne_obj4 <- Rtsne(gower_dist4, is_distance = TRUE)

tsne_data4 <- tsne_obj4$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster4 = factor(pam_fit4$clustering),
         name = PublicSchool$childcode)

ggplot(aes(x = X, y = Y), data = tsne_data4) +
  geom_point(aes(color = cluster4))



#summary results for public school clusters
pam_results4 <- PublicSchool %>%
  dplyr::select(-childcode) %>%
  mutate(cluster4 = pam_fit4$clustering) %>%
  group_by(cluster4) %>%
  do(the_summary = summary(.))

pam_results4$the_summary
pam_results4$the_summary

#Association Rules

#Import the data file again because the clustering technique led to changes in the data set that were not required for the Association Rules technique
library(arules)
library(arulesViz)
data <- read.csv("BAProject.csv")

#Convert the numeric variables into factors
data[["TotalChildScore"]] <- ordered(cut(data$TotalChildScore, c(0, 1000, 2000, 3000)), labels = c("Low", "Average", "High"))

data[["EnglishScore"]] <- ordered(cut(data$EnglishScore, c(0, 350, 650, 1000)), labels = c("Low", "Average", "High"))
data[["MathScore"]] <- ordered(cut(data$MathScore, c(0, 350, 650, 1000)), labels = c("Low", "Average", "High"))
data[["UrduScore"]] <- ordered(cut(data$UrduScore, c(0, 350, 650, 1000)), labels = c("Low", "Average", "High"))


data[["Teacher_Age"]] <- ordered(cut(data$Teacher_Age, c(15, 30, 45, 60)), labels = c("Young", "Middle-aged", "Old"))
data[["Monthly_Salary"]] <- ordered(cut(data$Monthly_Salary, c(0, 7000, 14000, 21000)), labels = c("Low", "Average", "High"))

data[["TotalTeacherScore"]] <- ordered(cut(data$TotalTeacherScore, c(0, 1000, 2000, 3000)), labels = c("Low", "Average", "High"))
data[["Teacher_English_Score"]] <- ordered(cut(data$Teacher_English_Score, c(0, 350, 800, 1000)), labels = c("Low", "Average", "High"))
data[["Teacher_Math_Score"]] <- ordered(cut(data$Teacher_Math_Score, c(0, 350, 800, 1000)), labels = c("Low", "Average", "High"))
data[["Teacher_Urdu_Score"]] <- ordered(cut(data$Teacher_Urdu_Score, c(400, 600, 800, 1000)), labels = c("Low", "Average", "High"))

#Remove individual score columns that are not needed
data$EnglishScore <- NULL
data$MathScore <- NULL
data$UrduScore <- NULL
data$ChildAverageScore <- NULL
data$Teacher_English_Score <- NULL
data$Teacher_Math_Score <- NULL
data$Teacher_Urdu_Score <- NULL
data$TeacherAverageScore <- NULL

#Remove unwanted columns
data$X <- NULL
data$X.1 <- NULL
data$childcode <- NULL

library("arules")
str(data)

Final <- as(data, "transactions")

Grules <- apriori(Final, parameter = list(support = 0.01, conf = 0.7, maxlen=5))
Grules
inspect(Grules[1:20])
rulesScoreHigh <- subset(Grules, subset = rhs%in% "TotalChildScore=High")
rulesScoreHigh
inspect(sort(rulesScoreHigh, by = "lift")[1:10])

plot(rulesScoreHigh, measure = c("support", "confidence"), shading="lift")

plot(rulesScoreHigh, method = "grouped")


rulesScoreHigh1 <- sample(rulesScoreHigh, 10)
plot(rulesScoreHigh1, method="graph")

oneRule <- sample(rulesScoreHigh, 1)
plot(oneRule, method="doubledecker", data = Final)



#Regression
rdata <- read.csv("BAProject.csv")
rdata$MotherEducation <- NULL
rdata$MotherLivesinHouse <- NULL

#linear regression 
reg <- lm(ChildAverageScore~District+TypeofSchool+Child_Gender+FatherEducation+FatherLivesinHouse+Teacher_Gender+Teacher_Age
          +Experience_in_Teaching+Experience_in_School+Teacher_Education+Teacher_Training+Monthly_Salary+Contract_Type+Teacher_Bonus+Teacher_Marital_Status+
            Teacher_Pvt_Tution+TeacherAverageScore+Local_Teacher,rdata)
summary(reg)
#vars we have explain 81.03% of average grades 

#logarithmic regression
summary(rdata$ChildAverageScore)
rdata$ChildAverageScore2 <- ordered(cut(rdata$ChildAverageScore, c(0, 650, 1000)), labels = c(0, 1)) #where 0 represents low and 1 represents high

#start with a training and test set
prop.table(table(rdata$ChildAverageScore2))

#70:30 - works
.7*4213
set.seed(20)
train <- rdata[sample(4213,2949),]
set.seed(20)
test <- rdata[-sample(4213,2949),]
prop.table(table(train$ChildAverageScore2))

#all variables on average grades
reg_all <- glm(ChildAverageScore2~District+TypeofSchool+Child_Gender+FatherEducation+FatherLivesinHouse+Teacher_Gender+Teacher_Age
               +Experience_in_Teaching+Experience_in_School+Teacher_Education+Teacher_Training+Monthly_Salary+Contract_Type+Teacher_Bonus+Teacher_Marital_Status+
                 Teacher_Pvt_Tution+TeacherAverageScore+Local_Teacher+Teacher_Gender*Child_Gender,train, family = "binomial")
summary(reg_all)
pred_prob<-predict(reg_all,test) 
pred_prob
summary(pred_prob)
predicted_outcome<-rep("0",1264) 
predicted_outcome[pred_prob>0.5]<-"1"
table(predicted_outcome)
table(predicted_outcome,test$ChildAverageScore2)
mean(predicted_outcome==test$ChildAverageScore2)


#for parent v. teacher impact seperately 
reg_p <- glm(ChildAverageScore2~FatherEducation+FatherLivesinHouse,train, family="binomial")
reg_t <- glm(ChildAverageScore2~Teacher_Gender+Teacher_Age +Experience_in_Teaching+Experience_in_School+Teacher_Education+Teacher_Training+Monthly_Salary+Contract_Type
             +Teacher_Bonus+Teacher_Marital_Status+Teacher_Pvt_Tution+TeacherAverageScore+Local_Teacher,train, family = "binomial")
reg_c <- glm(ChildAverageScore2~District+TypeofSchool+Child_Gender,train, family = "binomial")
summary(reg_p)
summary(reg_t)
summary(reg_c)

pred_prob2<-predict(reg_p,test) 
pred_prob2
summary(pred_prob2)
predicted_outcome2<-rep("0",1264) 
predicted_outcome2[pred_prob2>0.5]<-"1"
table(predicted_outcome2)
table(predicted_outcome2,test$ChildAverageScore2)
mean(predicted_outcome2==test$ChildAverageScore2)

pred_prob3<-predict(reg_t,test) 
pred_prob3
summary(pred_prob3)
predicted_outcome3<-rep("0",1264) 
predicted_outcome3[pred_prob3>0.5]<-"1"
table(predicted_outcome3)
table(predicted_outcome3,test$ChildAverageScore2)
mean(predicted_outcome3==test$ChildAverageScore2)




#Plots
library (ggplot2)
View(data)
data$Percentages <- (data$EnglishScore+data$MathScore+data$UrduScore)/3000*100
data$Performance <- ifelse(data$Percentages >= 65, "Good Performer", "Bad Performer")
ggplot(data, aes(District, Percentages))+geom_boxplot(fill="light blue",color="red")+theme_bw()+labs(title = "Students Peformance across Districts", subtitle = "Percentages vs. Districts")
ggplot(data, aes(Child_Gender, Performance))+geom_jitter(color = "red", fill = "blue")+theme_bw() +labs(x="Gender",title = "Students Gender", subtitle = "Performance vs. Gender")
ggplot(data, aes(X, Percentages))+geom_hex()+theme_bw()+facet_wrap(~TypeofSchool)+labs(x="Students",title = "Performances in Different Types of School", subtitle = "Percentages vs. Students") 
ggplot(data, aes(FatherEducation, MotherEducation, color = Performance))+theme_bw()+geom_point(position="jitter")+ labs(x="Mother's Education", y="Father's Education", title = "Parents' Education", subtitle = "Parents Education vs. Students Performance")
ggplot(data, aes(MotherLivesinHouse, FatherLivesinHouse, color = Performance)) + geom_point(position = "jitter") + theme_bw() + labs(x = "Father Lives in House?", y= "Mother in Lives in House?", title = "Parents Residential Status")
ggplot(data, aes(ChildAverageScore, TeacherAverageScore)) + geom_smooth(model = lm, fill = "Green") + theme_bw() + facet_wrap(~Teacher_Gender)+ labs(x = "Student's Avg. Score", y="Teacher's Average Score", title = "Teacher's Scores effect on Student's Score",subtitle = "Teacher's Marks vs. Student's Marks")
ggplot(data, aes(Experience_in_School,Experience_in_Teaching, color=Performance))+geom_jitter()+theme_bw()+labs(x="Experience in current School", y= "Experience in Teaching", title = "Teacher's Experience & Student's Performance")
ggplot(data, aes(Teacher_Bonus,Contract_Type)) + geom_tile(aes(fill = Performance), color = "black")+theme_bw()+labs(x="Teacher Bonus", y="Contract Type", title = "Teachers Contract and Bonuses")
ggplot(data, aes(Teacher_Education, Teacher_Training, color = Performance))+geom_jitter()+theme_bw()+labs(x="Teachers Education", y="Teachers Training", title = "Teachers Education and Training")
ggplot(data, aes(Monthly_Salary, Percentages)) + geom_point(color = "red",fill="blue",alpha = 0.5)+theme_bw() + facet_wrap(~Local_Teacher) + labs(x="Monthly Salary", title = "Teachers Salary and Locality")


#TREE

library(ISLR)
library(tree)
library(readr)
library(dplyr)
library(e1071)
library(caret)

#Decision Tree
library(tree)
Teacher_d <- select(data, District, TypeofSchool, Child_Gender, Teacher_Gender, Teacher_Age, Experience_in_Teaching, Experience_in_School, Teacher_Education, Teacher_Training, Monthly_Salary, Contract_Type)
High <- ifelse(data$Percentages >= 75, "Good Performer", "Bad Performer")
High
length(High)
Teacher_d <- data.frame(Teacher_d, High)
set.seed(123)
train <- sample(1:nrow(Teacher_d), nrow(Teacher_d)/1.25) 
test <- -train
training_data <- Teacher_d[train,]
testing_data <- Teacher_d[test,]
testing_High <- High[test] 

tree_model <- tree(High~.,training_data) 
summary(tree_model)
plot(tree_model, type = "uniform")
text(tree_model, pretty = 0)
cv_tree <- cv.tree(tree_model, FUN = prune.misclass)
names(cv_tree)
plot(cv_tree$size, cv_tree$dev, type = 'b')
prune_model <- prune.misclass(tree_model, best = 4)
plot(prune_model)
text(prune_model, pretty = 0)
tree_pred <- predict(prune_model, testing_data, type = 'class')
mean(tree_pred == testing_High)
confusionMatrix(testing_data$High, tree_pred)
install.packages("e1071")

