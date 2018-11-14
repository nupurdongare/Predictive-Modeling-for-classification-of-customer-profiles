# install.packages(c("dplyr","knitr"))
# install.packages(c("ggplot2","ggpubr","formattable","gtools","scales","corrplot","caret","pROC","ROCR","tidyr","parallel","parallelMap","DT","DMwR","mlr"))
# install.packages(c("caretEnsemble"))
# library(dplyr)
# library(knitr)
# library(ggplot2)
# library(ggpubr)
# library(formattable)
# library(gtools)
# library(scales)
# library(corrplot)
# library(caret)
# library(pROC)
# library(ROCR)
# library(tidyr)
# library(parallel)
# library(parallelMap)
# library(mlr)
# library(caretEnsemble)
# library(DT)
# library(DMwR)
# library(RColorBrewer)
setwd("C:/Users/nupur/OneDrive/Documents/Data Mining/use case study")
Loan_Prediction<-read.csv("training_data_cleaned.csv")
View(Loan_Prediction)
Loan_Prediction$Credit_History<-factor(Loan_Prediction$Credit_History, levels = c(0,1))
Loan_Prediction$Loan_Status = as.numeric(Loan_Prediction$Loan_Status)-1
Loan_Prediction$Loan_Status<-factor(Loan_Prediction$Loan_Status, levels = c(0,1), labels = c("Rejected", "Accepted"))
Loan_Prediction$Dependents<-factor(Loan_Prediction$Dependents, levels = c(0,1,2,3))
#Barplot for count of Loan_Status
freqLS<-table(Loan_Prediction$Loan_Status)
countls<-barplot(freqLS,main="Count of Loan_Status",
        xlab="Loan_Status", ylab="Frequency",
        col=c("red", "green"))
text(x = countls, y = freqLS, label = freqLS, pos = 3,offset=-3, cex = 1.6, col = "black")
#barplot for count of LS vs gender
freqgen<-table(Loan_Prediction$Loan_Status,Loan_Prediction$Gender)
countgen<-barplot(freqgen,main="Gender",
                 xlab="Gender", ylab="Frequency",
                 col=c("red", "green"),legend = row.names(freqgen))

#barplot for count of LS vs married
freqmar<-table(Loan_Prediction$Loan_Status,Loan_Prediction$Married)
countmar<-barplot(freqmar,main="Married",
                  xlab="Married", ylab="Frequency",
                  col=c("red", "green"),legend = row.names(freqmar))

#barplot for count of LS vs Dependents
freqdep<-table(Loan_Prediction$Loan_Status,Loan_Prediction$Dependents)
countdep<-barplot(freqdep,main="Dependents",
                  xlab="Dependents", ylab="Frequency",
                  col=c("red", "green"),legend = row.names(freqdep))

#barplot for count of LS vs Education
freqedu<-table(Loan_Prediction$Loan_Status,Loan_Prediction$Education)
countedu<-barplot(freqedu,main="Education",
                  xlab="Education", ylab="Frequency",
                  col=c("red", "green"),legend = row.names(freqedu))

#barplot for count of LS vs Self_Employed
freqse<-table(Loan_Prediction$Loan_Status,Loan_Prediction$Self_Employed)
countse<-barplot(freqse,main="Self_Employed",
                  xlab="Self_Employed", ylab="Frequency",
                  col=c("red", "green"),legend = row.names(freqse))

#barplot for count of LS vs Credit_History
freqch<-table(Loan_Prediction$Loan_Status,Loan_Prediction$Credit_History)
countch<-barplot(freqch,main="Credit_History",
                  xlab="Credit_History", ylab="Frequency",
                  col=c("red", "green"),legend = row.names(freqch))

#barplot for count of LS vs Property_Area
freqpa<-table(Loan_Prediction$Loan_Status,Loan_Prediction$Property_Area)
countpa<-barplot(freqpa,main="Property_Area",
                  xlab="Property_Area", ylab="Frequency",
                  col=c("red", "green"),legend = row.names(freqpa))



#

Factor_summary.DF = data.frame(Variable = character(), Test_statistic = numeric(), p_value = numeric(), cramers_v = numeric(), Dependency = character())

factor_cols = (Loan_Prediction[, sapply(Loan_Prediction, is.factor)] %>% names())[-12]


for(i in factor_cols){
  temp.DF = Loan_Prediction[,c(i,"Loan_Status")]%>% na.omit()
  test_value = chisq.test(temp.DF[,1], temp.DF[,2], correct = F)


  k = temp.DF[,1] %>% unique() %>% length()
  r = temp.DF[,2] %>% unique %>% length()



  cbind("Variable" = i, "Test_statistic" = test_value$statistic %>% round(4), p_value = test_value$p.value %>% round(4),
        Dependency = ifelse(test_value$p.value >= 0.05, "Independent", "Dependent")) %>% as.data.frame() -> result.DF

  Factor_summary.DF = rbind(Factor_summary.DF, result.DF)
}

rownames(Factor_summary.DF) <- NULL

Factor_summary.DF %>% mutate(Dependency = ifelse(Dependency == 'Dependent',
                                                 color_tile("white", "orange")(Dependency),
                                                 cell_spec(
                                                   Dependency, "html", color = "white", bold = T,
                                                   background = spec_color(3, end = 0.9, option = "A", direction = -1))
))
Factor_summary.DF
# 
ggplot(Loan_Prediction,aes(x=Loan_Status, y=ApplicantIncome)) + geom_boxplot(aes(fill = Loan_Status)) + stat_summary(fun.y = mean, geom="point", size=2) + xlab('Loan_Status') + ylab('ApplicantIncome') +  ggtitle('ApplicantIncome vs. Loan_Status')
ggplot(Loan_Prediction,aes(x=Loan_Status, y=CoapplicantIncome)) + geom_boxplot(aes(fill = Loan_Status)) + stat_summary(fun.y = mean, geom="point", size=2) + xlab('Loan_Status') + ylab('CoapplicantIncome') +  ggtitle('CoapplicantIncome vs. Loan_Status')
opar <- par(no.readonly=TRUE)
par(fig=c(0, 0.8, 0, 0.8))
plot(Loan_Prediction$ApplicantIncome, Loan_Prediction$CoapplicantIncome,
     xlab="ApplicantIncome",
     ylab="CoapplicantIncome")

par(fig=c(0, 0.8, 0.55, 1), new=TRUE)
boxplot(Loan_Prediction$ApplicantIncome, horizontal=TRUE, axes=FALSE)
par(fig=c(0.65, 1, 0, 0.8), new=TRUE)
boxplot(Loan_Prediction$CoapplicantIncome, axes=FALSE)
par(opar)
d<-density(Loan_Prediction$ApplicantIncome)
plot(d)
d<-density(Loan_Prediction$CoapplicantIncome)
d<-density(Loan_Prediction$ApplicantIncome)
d2<-density(Loan_Prediction$CoapplicantIncome)
plot(d2)
# h<-hist(Loan_Prediction$ApplicantIncome)
# h<-hist(log(Loan_Prediction$ApplicantIncome))
# h2<-hist(Loan_Prediction$CoapplicantIncome)
# h2<-hist(log(Loan_Prediction$CoapplicantIncome))
qplot(ApplicantIncome,data=Loan_Prediction,geom="density",adjust=0.7,size=I(0.7),fill=Loan_Status,alpha=I(0.8))
qplot(CoapplicantIncome,data=Loan_Prediction,geom="density",adjust=0.7,size=I(0.7),fill=Loan_Status,alpha=I(0.8))
ggplot(Loan_Prediction,aes(x=Loan_Status, y=ApplicantIncome)) + geom_boxplot(aes(fill = Loan_Status)) + stat_summary(fun.y = mean, geom="point", size=2) + xlab('Loan_Status') + ylab('ApplicantIncome') +  ggtitle('ApplicantIncome vs. Loan_Status')
ggplot(Loan_Prediction,aes(x=Loan_Status, y=CoApplicantIncome)) + geom_boxplot(aes(fill = Loan_Status)) + stat_summary(fun.y = mean, geom="point", size=2) + xlab('Loan_Status') + ylab('CoapplicantIncome') +  ggtitle('CoapplicantIncome vs. Loan_Status')
ggplot(Loan_Prediction,aes(x=Loan_Status, y=LoanAmount)) + geom_boxplot(aes(fill = Loan_Status)) + stat_summary(fun.y = mean, geom="point", size=2) + xlab('Loan_Status') + ylab('LoanAmount') +  ggtitle('LoanAmount vs. Loan_Status')
freqlma<-table(Loan_Prediction$Loan_Status,Loan_Prediction$Loan_Amount_Term)
countpa<-barplot(freqlma,main="Loan_Amount_Term",
                 xlab="Loan_Amount_Term", ylab="Frequency",
                 col=c("red", "green"),legend = row.names(freqlma))
#Feature Engineering
Loan_Prediction$TotalIncome<-Loan_Prediction$ApplicantIncome+Loan_Prediction$CoapplicantIncome
Loan_Prediction<-Loan_Prediction[,c(-6,-7)]
ggplot(Loan_Prediction,aes(x=Loan_Status, y=TotalIncome)) + geom_boxplot(aes(fill = Loan_Status)) + stat_summary(fun.y = mean, geom="point", size=2) + xlab('Loan_Status') + ylab('TotalIncome') +  ggtitle('TotalIncome vs. Loan_Status')
Loan_Prediction$InstallmentperMonth<-((Loan_Prediction$LoanAmount*1000)/Loan_Prediction$Loan_Amount_Term)
Loan_Prediction<-Loan_Prediction[,c(-6,-7)]
ggplot(Loan_Prediction,aes(x=Loan_Status, y=InstallmentperMonth)) + geom_boxplot(aes(fill = Loan_Status)) + stat_summary(fun.y = mean, geom="point", size=2) + xlab('Loan_Status') + ylab('InstallmentperMonth') +  ggtitle('InstallmentperMonth vs. Loan_Status')
plot(Loan_Prediction$InstallmentperMonth, Loan_Prediction$TotalIncome,
           xlab="InstallmentperMonth",
           ylab="TotalIncome",col=Loan_Prediction$Loan_Status,legend=row.names(Loan_Prediction$Loan_Status))
#creating dummy variables
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

Loan_Prediction<-Loan_Prediction[,-7]
Loan_Prediction$Urban<-factor(Loan_Prediction$Urban,levels = c(0,1))
Loan_Prediction$Semiurban<-factor(Loan_Prediction$Semiurban,levels = c(0,1))
Loan_Prediction$Rural<-factor(Loan_Prediction$Rural,levels = c(0,1))

write.csv(Loan_Prediction,"Loan_Prediction_basic.csv")  ##Dataset for CART
##Below is the dataset for knn & NB
Loan_Prediction<-Loan_Prediction
Loan_Prediction$Gender<-ifelse(Loan_Prediction$Gender=="Male",1,0)
Loan_Prediction$Married<-ifelse(Loan_Prediction$Married=="Yes",1,0)
Loan_Prediction$Education<-ifelse(Loan_Prediction$Education=="Graduate",1,0)
Loan_Prediction$Self_Employed<-ifelse(Loan_Prediction$Self_Employed=="Yes",1,0)
Loan_Prediction$Credit_History<-as.numeric(Loan_Prediction$Credit_History)-1
Loan_Prediction$Dependents<-as.numeric(Loan_Prediction$Dependents)-1
View(Loan_Prediction)
#Normalizing data
scaled_LP<-scale(Loan_Prediction[,-9])
#partitioning into train & validation data
set.seed(1)
train.index<-sample(c(1:dim(Loan_Prediction)[1]),0.5*dim(Loan_Prediction)[1])
train.df<-Loan_Prediction[train.index,]
rem.df<-Loan_Prediction[-train.index,]
valid.index<-sample(c(1:dim(rem.df)[1]),0.3*dim(Loan_Prediction)[1])
valid.df<-rem.df[valid.index,]
test.df<-rem.df[-valid.index,]
#naiveBayes
install.packages("e1071")
library(e1071)
Loan_Prediction.nb<-naiveBayes(Loan_Status~.,data=train.df)
Loan_Prediction.nb
pred.prob.valid <- predict(Loan_Prediction.nb, newdata = valid.df, type = "raw")
pred.class.valid <- predict(Loan_Prediction.nb, newdata = valid.df,type = "class")
pred.prob.valid
pred.class.valid
df <- data.frame(actual = valid.df$Loan_Status, predicted = pred.class.valid, pred.prob.valid)
View(df)
table(pred.class.valid, valid.df$Loan_Status)
# pred.test <- predict(Loan_Prediction.nb, newdata = test.df) 
# table(pred.test, test.df$Loan_Status,positive = "Accepted")
#knn
install.packages("class")
library(class)
Actualtrainclass<-train.df$Loan_Status
Actualvalidclass<-valid.df$Loan_Status
knn1<-knn(train=train.df[,-7],test=valid.df[,-7],cl=Actualtrainclass,k=1)
# confusionMatrix(knn1, valid.df$Loan_Status)
table(knn1,valid.df$Loan_Status)
knn3<-knn(train=train.df[,-7],test=valid.df[,-7],cl=Actualtrainclass,k=3)
# confusionMatrix(knn3, valid.df$Loan_Status)
table(knn3,valid.df$Loan_Status)
knn5<-knn(train=train.df[,-7],test=valid.df[,-7],cl=Actualtrainclass,k=5)
# confusionMatrix(knn5, valid.df$Loan_Status)
table(knn5,valid.df$Loan_Status)
knn7<-knn(train=train.df[,-7],test=valid.df[,-7],cl=Actualtrainclass,k=7)
# confusionMatrix(knn7, valid.df$Loan_Status)
table(knn7,valid.df$Loan_Status)
knn9<-knn(train=train.df[,-7],test=valid.df[,-7],cl=Actualtrainclass,k=9)
# confusionMatrix(knn9, valid.df$Loan_Status)
table(knn9,valid.df$Loan_Status)
knn11<-knn(train=train.df[,-7],test=valid.df[,-7],cl=Actualtrainclass,k=11)
# confusionMatrix(knn11, valid.df$Loan_Status)
table(knn11,valid.df$Loan_Status)
knn13<-knn(train=train.df[,-7],test=valid.df[,-7],cl=Actualtrainclass,k=13)
# confusionMatrix(knn13, valid.df$Loan_Status)
table(knn13,valid.df$Loan_Status)



