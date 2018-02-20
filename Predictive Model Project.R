MPG = read.csv("MPG Information.csv") 
Tech = read.csv("Technical Spec Information.csv")
YO = read.csv("Year and Origin Information.csv")


str(MPG) 
str(Tech)
str(YO)


X = merge(MPG, Tech, by="VIN")
Combined_Data = merge(X, YO, by = "VIN")
Reg_Data = Combined_Data[,-c(1:2)]  #Data set that we are using


for (i in 1:nrow(Reg_Data))
 {
  Reg_Data$new_displacement[i] = 1/(Reg_Data$displacement[i])
}

Reg_Data = Reg_Data[,-3]

for (i in 1:nrow(Reg_Data))
{
  Reg_Data$horsepower[i] = log10(Reg_Data$horsepower[i])
}
#horsepower is not a good indicaton transformations are done to it

for (i in 1:nrow(Reg_Data))
{
  Reg_Data$acceleration[i] = sqrt(Reg_Data$acceleration[i])
}

for (i in 1:nrow(Reg_Data))
{
  Reg_Data$weight[i] = (Reg_Data$weight[i])^2
}

#for (i in 1:nrow(Reg_Data))
#{
#  Reg_Data$model.year[i] = (Reg_Data$model.year[i])
#}


#acceleration is not a good indicator so we need different transformations

Reg_Data = Reg_Data[,-2] #remove cylinder because it is a bad predictor 
Reg_Data = Reg_Data[,-6]

#explanatory_reg = lm(mpg~.,Reg_Data)
#summary(explanatory_reg)

plot(Reg_Data)

set.seed(12345)

row = nrow(Reg_Data) #takes the number of rows from the data

trainindex = sample(row, 280, replace=FALSE) #sets training index
Training = Reg_Data[trainindex,] #Training Data
validation = Reg_Data[-trainindex,] #Validation Data

library('forecast') #We use this package for the 'accuracy' function

#Regular Regression Model
reg_all = lm(mpg~.,Training)
summary(reg_all)
PredBase<-predict(reg_all,validation)
accuracy(PredBase,validation$mpg)

reg_null = lm(mpg~1,Training) #base intercept model
summary(reg_null)

#Forward Regression Model
reg_forward = step(reg_null, scope=list(upper=reg_all), direction='forward')
summary(reg_forward)
PredForward<-predict(reg_forward,validation)
accuracy(PredForward,validation$mpg) #accuracy measures of PredForward over validation dataset


#Backward Regression Model
reg_backward = step(reg_all, direction='backward')
summary(reg_backward)
PredBackward<-predict(reg_backward,validation)
accuracy(PredBackward,validation$mpg) #accuracy measures of PredBackward over validation dataset


#Reg Both
reg_both<-step(reg_all, direction='both')
summary(reg_both)
PredBoth<-predict(reg_both,validation)
accuracy(PredBoth,validation$mpg) #accuracy measures of PredBoth over validation dataset

#exhaustive method

#library('leaps')
#exhaustive = regsubsets(mpg~., data=Training, nbest=1, nvmax=10, method="exhaustive")
#exhaustive_summary= summary(exhaustive)
#exhaustive_summary$which
#exhaustive_summary$adjr2
#exhaustive_summary$bic

#best_adj_rsq = which.max(exhaustive_summary$adjr2)
#best_bic = which.min(exhaustive_summary$bic)

#coef(exhaustive, best_adj_rsq)
