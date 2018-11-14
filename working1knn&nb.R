LP<-read.csv("training_data_cleaned.csv")
#backing up the dataset
LP_backup<-LP
LP$Credit_History<-as.factor(LP$Credit_History)
LP$TotalIncome<-LP$ApplicantIncome+LP$CoapplicantIncome
LP<-LP[,c(-6,-7)]
LP$Urban<-0
LP$Semiurban<-0
LP$Rural<-0
for (i in 1:nrow(LP)){
  if(LP$Property_Area[i]=="Urban"){
    LP$Urban[i]<-1
  }
  if(LP$Property_Area[i]=="Semiurban"){
    LP$Semiurban[i]<-1
  }
  if(LP$Property_Area[i]=="Rural"){
    LP$Rural[i]<-1
  }
}
View(LP)
LP<-LP[,-9]
LP$Gender<-ifelse(LP$Gender=="Male",1,2)
LP$Married<-ifelse(LP$Married=="Yes",1,0)
LP$Education<-ifelse(LP$Education=="Graduate",1,2)
LP$Self_Employed<-ifelse(LP$Self_Employed=="Yes",1,0)
#partitioning into 60% training set & 40% validation set
set.seed(1)
train.index<-sample(c(1:dim(LP)[1]),0.6*dim(LP)[1])
train.df<-LP[train.index,]
valid.df<-LP[-train.index,]
install.packages("e1071")
library(e1071)
View(LP)
LP.nb<-naiveBayes(Loan_Status~.,data=train.df)
LP.nb
pred.prob <- predict(LP.nb, newdata = valid.df, type = "raw")
pred.class <- predict(LP.nb, newdata = valid.df)
pred.class
pred.prob
df <- data.frame(actual = valid.df$Loan_Status, predicted = pred.class, pred.prob)
View(df)
install.packages("Matrix")
install.packages("SparseM")
install.packages("caret")
library(Matrix)
library(caret)
library(SparseM)
pred.train <- predict(LP.nb, newdata = train.df)
confusionMatrix(pred.class, train.df$Loan_Status)
pred.valid <- predict(LP.nb, newdata = valid.df)
confusionMatrix(pred.valid, valid.df$Loan_Status)

# train.index2<-sample(c(1:dim(LP_new)[1]),0.6*dim(LP_new)[1])
# train.df2<-LP_new[train.index2,]
# valid.df2<-LP_new[-train.index2,]
# LPnew.nb<-naiveBayes(Loan_Status~.,data=train.df2)
# test.data2<-data.frame(Gender="Male",Married="No",Dependents=1,Education="Graduate",Self_Employed="Yes",ApplicantIncome=3,CoapplicantIncome=4,LoanAmount=6,Property_Area="Urban",Credit_History="1")
# prednew<-predict(LPnew.nb,test.data2)
# prednew
# str(test_data)
# test_data$Loan_Amount_Term<-factor(test_data$Loan_Amount_Term)
# test_data$Credit_History<-factor(test_data$Credit_History)
# test_data$BinnedAppincome<-as.factor(cut(test_data$ApplicantIncome,10))
# test_data$BinnedcoAppincome<-as.factor(cut(test_data$CoapplicantIncome,10))
# test_data$BinnedLoanAmount<-as.factor(cut(test_data$LoanAmount,10))
# test_data<-test_data[,-c(6,7,8)]
# prednewtst<-predict(LPnew.nb,!is.na(test_data))
# prednewtst
library("class", lib.loc="~/R/win-library/3.4")
install.packages("car")
library(car)

defaultclass<-train.df$Loan_Status
defaultvalid<-valid.df$Loan_Status
knn1<-knn(train=train.df[,-9],test=valid.df[,-9],cl=defaultclass,k=1)
confusionMatrix(knn1, valid.df$Loan_Status)
table(knn1,valid.df$Loan_Status)
knn3<-knn(train=train.df[,-9],test=valid.df[,-9],cl=defaultclass,k=3)
confusionMatrix(knn3, valid.df$Loan_Status)
table(knn3,valid.df$Loan_Status)
knn5<-knn(train=train.df[,-9],test=valid.df[,-9],cl=defaultclass,k=5)
confusionMatrix(knn5, valid.df$Loan_Status)
table(knn5,valid.df$Loan_Status)
knn7<-knn(train=train.df[,-9],test=valid.df[,-9],cl=defaultclass,k=7)
confusionMatrix(knn7, valid.df$Loan_Status)
table(knn7,valid.df$Loan_Status)
knn9<-knn(train=train.df[,-9],test=valid.df[,-9],cl=defaultclass,k=9)
confusionMatrix(knn9, valid.df$Loan_Status)
table(knn9,valid.df$Loan_Status)
knn11<-knn(train=train.df[,-9],test=valid.df[,-9],cl=defaultclass,k=11)
confusionMatrix(knn11, valid.df$Loan_Status)
table(knn11,valid.df$Loan_Status)
knn13<-knn(train=train.df[,-9],test=valid.df[,-9],cl=defaultclass,k=13)
confusionMatrix(knn13, valid.df$Loan_Status)
table(knn13,valid.df$Loan_Status)
# CT1<-rpart(Loan_Status,data=train.df,method="class",control = rpart.control(maxdepth = 3))
# install.packages("rpart")
# library(rpart)
# CT1<-rpart(Loan_Status,data=train.df,method="class",control = rpart.control(maxdepth = 3))
# CT1<-rpart(Loan_Status~.,data=train.df,method="class",control = rpart.control(maxdepth = 3))
CT1<-rpart(Loan_Status~.,data=train.df,method="class")
