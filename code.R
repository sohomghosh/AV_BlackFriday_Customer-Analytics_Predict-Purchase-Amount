'''
Removing user id & product id for the time being
Gender, City Category, Maritial Status to be converted to 1-Hot encoded form
Age (in bins) to be mapped to intergers starting from 1,2...
Occupation? Need to figure out something or keep as it is
Stay_In_Current_City_Years: 4+ change to 4 and lets try (capping)
Other fileds lets keep as it as.

Product_Category_1 > Product_Category_2 > Product_Category_3 always.
t-test, ANOVA for categorial independent variables like product category we can find out their impact on the dependent variable
'''

train_data<-read.csv("C:\\Users\\SatyakiBh\\Desktop\\BlackFriday\\BlackFriday\\train.csv")
test_data<-read.csv("C:\\Users\\SatyakiBh\\Desktop\\BlackFriday\\BlackFriday\\test-comb.csv")

one_hot_gender<-with(train_data,data.frame(model.matrix(~Gender-1,train_data),User_ID,Product_ID))
table(train_data$Age)
age_recoded <- as.numeric(train_data$Age)
#0-17  18-25  26-35  36-45  46-50  51-55    55+
table(train_data$Occupation) #Let's keep occupation as it is

table(train_data$City_Category)#Let's convert it into one-hot encoded form
one_hot_city<-with(train_data,data.frame(model.matrix(~City_Category-1,train_data),User_ID,Product_ID))

Stay_In_Current_City_Years_capped<-ifelse(train_data$Stay_In_Current_City_Years=="4+",4,train_data$Stay_In_Current_City_Years)
train_data$Stay_In_Current_City_Years
typeof(Stay_In_Current_City_Years_capped)

one_hot_Marital_Status<-with(train_data,data.frame(model.matrix(~Marital_Status-1,train_data),User_ID,Product_ID))
table(train_data$Product_ID)

new_train_data<-cbind(one_hot_gender,age_recoded,train_data$Occupation,one_hot_city,Stay_In_Current_City_Years_capped,one_hot_Marital_Status,train_data$Product_Category_1,train_data$Product_Category_2,train_data$Product_Category_3,train_data$Purchase)
ready_train_data<-new_train_data[,-c(10,11,14,15)]



library(mice)
#Lets treat the missing values using mice

#t-test, ANOVA for categorial independent variables like product category we can find out their impact on the dependent variable

#Modelling with h2o as done by winning solution on this modifies dataset