
#Importing libraries we need

library(rpart)
library(caret)
library(dplyr)
library(ggplot2)

#Importing Data_sets

Train_data=read.csv("D:/CORONA/Dataset/train.csv")
Test_data=read.csv("D:/CORONA/Dataset/test.csv")
head(Train_data)
nrow(Train_data)
head(Test_data)
nrow(Test_data)

#Visualisation

ggplot(Train_data,aes(x=Country.Region,y=ConfirmedCases))+geom_point()
ggplot(Train_data,aes(x=Lat,y=ConfirmedCases))+geom_point()
ggplot(Train_data,aes(x=Long,y=ConfirmedCases))+geom_point()

plot(density(Train_data$Lat))
plot(density(Train_data$Long))

#WE DON'T HAVE UNIQUE ID's FOR TRAIN AND TEST DATA
#THAT'S WHY WE CANNOT COMBINE DATA.

#Transforming COnfirmed Cases in training data.

Train_data$log_ConfirmedCases = log(Train_data$ConfirmedCases + 1)

#We need to change the numeric date variable to date format variable

Train_data$Date=as.Date(Train_data$Date)
summary(Train_data$Date)

Test_data$Date=as.Date(Test_data$Date)
summary(Test_data$Date)

#Instead of using date variable we will create a Number of days variable which is difference between ConfirmedCase
#date and Since the outbreak started

Train_data$No_Days=as.integer(Train_data$Date-min(Train_data$Date))
summary(Train_data$No_Days)
Test_data$No_Days=as.integer(Test_data$Date-min(Train_data$Date))
summary(Test_data$No_Days)

#Modelling and traning

# using 5-fold cross-validation.

NO_folds = trainControl(method = "cv", number = 5) #Number of folds
param_grid = expand.grid(.cp = seq(0, 0.01, 0.001)) # Check values of cp b/w 0 and 0.01

grid_search = train(
  log_ConfirmedCases ~ Country.Region + Lat + Long + Province.State + No_Days,  
  data = Train_data, 
  method = "rpart", # CART algo
  trControl = NO_folds, 
  tuneGrid = param_grid
)

print(grid_search)

#Best Complexity parameter we got is 0.000

tree <- rpart(
  log_ConfirmedCases ~ Country.Region + Lat + Long + Province.State + No_Days,
  data = Train_data, 
  cp = 0
)

#Accuracy check

Train_data$Predict= exp(predict(tree, newdata = Train_data)) - 1

#Need to change negative values to compute RMSLE

for(i in 1:length(Train_data$Predict))
{
  if(Train_data$Predict[i] < 0 )
  {
    Train_data$Predict[i] = 0
  }
}

#Root mean squared logrithmic error

RMSLE = sqrt(mean((log(Train_data$Predict + 1) - log(Train_data$ConfirmedCases + 1))^2))
RMSLE

#Prediction 

Test_Predict= exp(predict(tree, newdata = Test_data)) - 1

#Submission

d = data.frame(Id = Test_data$Id, ConfirmedCases = Test_Predict)
write.csv(d,file = "submission12.csv",row.names = F)
