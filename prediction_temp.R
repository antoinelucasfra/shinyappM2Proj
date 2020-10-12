# script for prediction attempt at thesuicide rate per country in the next year? few years???
#according our data

# recup function

get.error <- function(class,pred){
  cont.tab <- table(class,pred)
  print(cont.tab)
  return((cont.tab[2,1]+cont.tab[1,2])/(sum(cont.tab)))
}

get.sensitivity <- function(class,pred){
  cont.tab <- table(class,pred)
  return((cont.tab[2,2])/(sum(cont.tab[2,])))
}

get.specificity <- function(class,pred){
  cont.tab <- table(class,pred)
  return((cont.tab[1,1])/(sum(cont.tab[1,])))
}

#selection of the data useful for prediction and splitting of the dataset 
# -> classical procedure of CV and prediction -> logistical regression
str(suicide)

data_predict <- suicide[,c("suicides.100k.pop","year","population","suicides_no","sex","age","generation","country")]

dim(data_predict)
head(data_predict)
summary(data_predict)

#idée -> prédire pour l'année 2017 le nombre de suicide dans le monde pour chaque pays 
# ensuite on décompose aussi pour chaque variable age,sexe,generation ?

#splitting the data into two datasets
n <- nrow(data_predict)
n.train <- n/20
n.test <- n-n.train
ind.train <- sample(1:nrow(data_predict),n.train)
data_train <- data_predict[ind.train,]
data_test <- data_predict[-ind.train,]

dim(data_train)
dim(data_test)

mod.logistic <- lm(suicides.100k.pop~year+population+sex+age,data=data_train)

pred.logistic.test <- predict(mod.logistic,newdata=data_test,type="response")



# 
# get.error(data_test$suicides.100k.pop,pred.logistic.test)
# get.specificity(data.test$default,pred.logistic.test)
# get.sensitivity(data.test$default,pred.logistic.test)
# 
# pred.logistic.grid <- predict(mod.logistic,newdata=data.grid,type="response") > 0.5
# 
# mod <- lm(suicides.100k.pop~year+population+sex+age,data=data_predict)



