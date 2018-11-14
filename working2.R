install.packages(c("dplyr","knitr"))
install.packages(c("kableExtra","ggplot2","ggpubr","formattable","gtools","scales","corrplot","caret","pROC","ROCR","tidyr","parallel","parallelMap","DT","DMwR","mlr"))
install.packages(c("caretEnsemble"))
library(dplyr)
library(knitr)
library(kableExtra)
library(ggplot2)
library(ggpubr)
library(formattable)
library(gtools)
library(scales)
library(corrplot)
library(caret)
library(pROC)
library(ROCR)
library(tidyr)
library(parallel)
library(parallelMap)
library(mlr)
library(caretEnsemble)
library(DT)
library(DMwR)

Loan_Prediction<-read.csv("training_data_cleaned.csv")
View(Loan_Prediction)
Loan_Prediction$Credit_History<-factor(Loan_Prediction$Credit_History, levels = c(0,1), labels = c("Unmet", "Met"))
# Loan_Prediction$TotalIncome<-Loan_Prediction$ApplicantIncome+Loan_Prediction$CoapplicantIncome
# Loan_Prediction<-Loan_Prediction[,c(-6,-7)]
# View(Loan_Prediction)
# Loan_Prediction$Loan_Status<-factor(Loan_Prediction$Loan_Status,levels=c(1,2),labels=c("Rejected","Approved"))






countsgender <- table(Loan_Prediction$Loan_Status,Loan_Prediction$Gender)
genderbarplot<-barplot(countsgender,
        main="Grouped Bar Plot for Gender",
        xlab="Gender", ylab="Frequency",
        col=c("red", "green"),
        legend=rownames(countsgender))
countsmarried <- table(Loan_Prediction$Loan_Status,Loan_Prediction$Married)
marriedbarplot<-barplot(countsgender,
            main="Grouped Bar Plot for Married",
            xlab="Married", ylab="Frequency",
            col=c("red", "green"),
            legend=rownames(countsmarried))
countsself <- table(Loan_Prediction$Loan_Status,Loan_Prediction$Self_Employed)
selfbarplot<-barplot(countsself,
                        main="Grouped Bar Plot for Self Employed",
                        xlab="Self_Employed", ylab="Frequency",
                        col=c("red", "green"),
                        legend=rownames(countsself))
# text(x = xx, y = counts, label = counts, pos = 3, cex = 0.8, col = "blue")
a<-plot(x=Loan_Prediction$TotalIncome,y=Loan_Prediction$LoanAmount,col=Loan_Prediction$Loan_Status,legend=Loan_Prediction$Loan_Status)
abline(lm(Loan_Prediction$LoanAmount~Loan_Prediction$ApplicantIncome),col="blue")
Loan_Prediction$Urban<-0
Loan_Prediction$Semiurban<-0
Loan_Prediction$Rural<-0
for (i in 1:nrow(Loan_Prediction)){
  if(Loan_Prediction$Property_Area[i]=="Urban"){
    Loan_Prediction$Urban[i]<-1
  }
  if(Loan_Prediction$Property_Area[i]=="Semiurban"){
    Loan_Prediction$Semiurban[i]<-1
  }
  if(Loan_Prediction$Property_Area[i]=="Rural"){
    Loan_Prediction$Rural[i]<-1
  }
}
View(Loan_Prediction)
Loan_Prediction<-Loan_Prediction[,-11]
for (i in 1:nrow(Loan_Prediction)){
  # if(Loan_Prediction$Dependents[i]=="0"){
  #   Loan_Prediction$Dependents[i]<-0
  # }
  # if(Loan_Prediction$Dependents[i]=="1"){
  #   Loan_Prediction$Dependents[i]<-1
  # }
  # if(Loan_Prediction$Dependents[i]=="2"){
  #   Loan_Prediction$Dependents[i]<-2
  # }
  if(Loan_Prediction$Dependents[i]=="3+"){
    Loan_Prediction$Dependents[i]<-3
  }
}