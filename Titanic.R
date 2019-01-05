train<-read.csv("train.csv",header = TRUE,sep = ",", stringsAsFactors = FALSE);
test<-read.csv("test.csv",header = TRUE,sep = ",", stringsAsFactors = FALSE);


install.packages("naniar")
library(naniar)
library(dplyr)
library(ggplot2)

install.packages("rbokeh")
library(rbokeh)


head(train)

#missing data


##old package
library(VIM);
aggr(train, col = c("gray", "red"), numbers = T, sortVars = T)



apply(train[,1:12],MARGIN=2,FUN = any_na)

are_na(train[,c("Age")])

which(are_na(train[,c("Age")]));


n_miss(train);
n_miss(train$Age);

pct_miss(train$Age);


miss_var_summary(train);
miss_case_summary(train);
miss_var_table(train);
miss_case_table(train);


vis_miss(train)
gg_miss_var(train,facet=Pclass)
gg_miss_var(train,facet=Sex)
gg_miss_var(train,facet=SibSp)
gg_miss_var(train,facet=Parch)
gg_miss_var(train,facet=Cabin)
gg_miss_var(train,facet=Embarked)

gg_miss_case(train)

gg_miss_upset(test)

gg_miss_fct(x=train,fct=Pclass)




miss_scan_count(data = train, search = list("N/A"))
miss_scan_count(data = train, search = list("missing"))
miss_scan_count(data = train, search = list(""))

unique(substr(train$Cabin,1,1))


train_cabin<-train%>%filter(substr(Cabin,1,1) %in% c("C","E","G","D","A","B","F","T"))
train_cabin_not<-train%>%filter(!substr(Cabin,1,1) %in% c("C","E","G","D","A","B","F","T"))
train_cabin_not$Cabin<-NA
train_2<-rbind(train_cabin,train_cabin_not)


dim(train_cabin)
dim(train_cabin_not)



miss_var_summary(train_2);

vis_miss(train_2)
gg_miss_var(train_2,facet=Pclass)
gg_miss_var(train_2,facet=Sex)
gg_miss_var(train_2,facet=SibSp)
gg_miss_var(train_2,facet=Parch)
gg_miss_var(train_2,facet=Cabin)
gg_miss_var(train_2,facet=Embarked)
gg_miss_var(train_2,facet=Survived)

gg_miss_case(train_2)

gg_miss_upset(train_2)

gg_miss_fct(x=train_2,fct=Pclass)




train_2 %>% arrange(Fare)





train_2 %>% arrange(PassengerId) %>% vis_miss()
train_2 %>% arrange(Pclass) %>% vis_miss()
train_2 %>% arrange(Sex) %>% vis_miss()
test %>% arrange(Name) %>% vis_miss()
test %>% arrange(SibSp) %>% vis_miss()
test %>% arrange(Parch) %>% vis_miss()
test %>% arrange(Ticket) %>% vis_miss()
train_2 %>% arrange(Fare) %>% vis_miss()
test %>% arrange(Cabin) %>% vis_miss()
test %>% arrange(Embarked) %>% vis_miss()



train_2 %>%
  bind_shadow() %>%
  group_by(Age_NA) %>% 
  summarise(sibSp_mean = mean(SibSp), 
            SibSp_sd = sd(SibSp)) 




train_2 %>%
  bind_shadow() %>%
  group_by(Cabin_NA) %>% 
  summarise(Fare_mean = mean(Fare,na.rm = TRUE), 
            Fare_sd = sd(Fare,na.rm = TRUE)) 




bind_shadow(train_2) %>%
  ggplot(aes(x =Fare , 
             color = Cabin_NA)) + 
  geom_density()



bind_shadow(train_2) %>%
  ggplot(aes(x =Fare , 
             color = Age_NA)) + 
  geom_density()





train_2 %>%
  bind_shadow() %>%
  ggplot(aes(x = Cabin_NA,
             y = Fare)) + 
  geom_boxplot()




train_2 %>%
  bind_shadow() %>%
  ggplot(aes(x = Age_NA,
             y = Fare)) + 
  geom_boxplot()


ggplot(train_2,
       aes(x = Fare ,
           y = Pclass)) + 
  geom_miss_point()



#### imputation ####

str_ex


train_2$Name_2<-ifelse(grepl("Mrs.",train_2$Name)==TRUE,"Mrs.",ifelse(grepl("Mr",train_2$Name)==TRUE,"Mr.",
                                                                  ifelse(grepl("Miss",train_2$Name)==TRUE,"Miss.",
                                                                         ifelse(grepl("Master",train_2$Name)==TRUE,"Master."
                                                                                ,ifelse(grepl("Dr",train_2$Name)==TRUE,"Dr.","others")))))    



data_mean_age<-train_2%>%group_by(Name_2,Pclass,Sex)%>%summarize(mean_age=mean(Age,na.rm = TRUE),count=n())

data_mean_age_M<-data_mean_age%>%filter(Sex=="male")
data_mean_age_F<-data_mean_age%>%filter(Sex=="female")


figure() %>% 
  ly_points(data=data_mean_age_M,x=Pclass,y=mean_age, color=Name_2)
# %>% ly_lines(data=data_mean_age_M,x=Pclass,y=mean_age, color=Name_2)


figure() %>% 
  ly_points(data=data_mean_age_F,x=Pclass,y=mean_age, color=Name_2) 
# %>% ly_lines(data=data_mean_age_F,x=Pclass,y=mean_age, color=Name_2)

train_2_NA<-train_2%>%filter(is.na(train_2$Age)==TRUE)
train_2_not_NA<-train_2%>%filter(is.na(train_2$Age)==FALSE)

train_2_not_NA[,c("Pclass","Sex","Embarked","Name_2")]<-lapply(train_2_not_NA[,c("Pclass","Sex","Embarked","Name_2")],as.factor);
train_2_NA[,c("Pclass","Sex","Embarked","Name_2")]<-lapply(train_2_NA[,c("Pclass","Sex","Embarked","Name_2")],as.factor);



library(rpart)

dt<-rpart(Age~Pclass+Sex+SibSp+Parch+Embarked+Name_2+Fare,cp=0.1^20, data=train_2_not_NA)
printcp(dt)
plotcp(dt)
dt_prune<-prune(dt, cp=dt$cptable[which.min(dt$cptable[,"xerror"]),"CP"])
plot(dt_prune,margin=0.1)
text(dt_prune,use.n=T)

dt_prune$variable.importance;
barplot(dt_prune$variable.importance);


install.packages("rattle")
library(rattle)

fancyRpartPlot(dt_prune, cex = 1)


train_2_NA<-train_2_NA %>%
  bind_shadow() 

train_2_not_NA<-train_2_not_NA %>%
  bind_shadow() 



train_2_NA$Age<-predict(dt_prune,train_2_NA,type="vector")
train_2_NA$Age<-round(train_2_NA$Age,1)

train_3<-rbind(train_2_NA,train_2_not_NA);

train_3$Cabin<-ifelse(is.na(train_3$Cabin)==TRUE,"no information",train_3$Cabin)
train_3$Cabin<-as.factor(train_3$Cabin)




###mosaic plot

mosaicplot(~Survived+Pclass,data=train_3,color=TRUE,cex=1.2,
           main="Survival by Pclass",xlab="Survival",ylab="Pclass"); 

mosaicplot(~Survived+Sex,data=train_3,color=TRUE,cex=1.2,
           main="Survival by Sex",xlab="Survival",ylab="Sex"); 

mosaicplot(~Survived+SibSp,data=train_3,color=TRUE,cex=1.2,
           main="Survival by SibSp",xlab="Survival",ylab="SibSp");

mosaicplot(~Survived+Parch,data=train_3,color=TRUE,cex=1.2,
           main="Survival by Parch",xlab="Survival",ylab="Parch");

mosaicplot(~Survived+Embarked,data=train_3,color=TRUE,cex=1.2,
           main="Survival by Embarked",xlab="Survival",ylab="Embarked");

mosaicplot(~Survived+Name_2,data=train_3,color=TRUE,cex=1.2,
           main="Survival by Name_2",xlab="Survival",ylab="Name_2");


###cabin feature engineering

train_3$Cabin_2<-as.factor(ifelse(train_3$Cabin=="no information","no information",substr(train_3$Cabin,1,1)));

mosaicplot(~Survived+Cabin_2,data=train_3,color=TRUE,cex=1.2,
           main="Survival by Cabin_2",xlab="Survival",ylab="Cabin_2");


library(stringr)
train_3$cabin_number<-ifelse(train_3$Cabin=="no information",0,str_count(train_3$Cabin," ")+1);


###family feature engineering

train_3$family<-train_3$SibSp+train_3$Parch
train_3$family_binomial<-as.factor(ifelse(train_3$family>0,1,0))
train_3$SibSp_binomial<-as.factor(ifelse(train_3$SibSp>0,1,0))
train_3$Parch_binomial<-as.factor(ifelse(train_3$Parch>0,1,0))


mosaicplot(~Survived+family, data=train_3,color=TRUE,cex=1.2,
           main="Survival by family",xlab="Survival",ylab="family");

mosaicplot(~Survived+family_binomial, data=train_3,color=TRUE,cex=1.2,
           main="Survival by family_binomial",xlab="Survival",ylab="family_binomial");



###correlation

data_cor_matrix<-cor(train_3[,c("Survived","Age","SibSp","Parch","Fare","cabin_number")]);                                                 
round(data_cor_matrix,2);
library(corrplot)
corrplot(data_cor_matrix,order="hclust",title="Correlation",addrect=3,method="number"); #visualization


###random forest

library(randomForest)
rf<-randomForest(as.factor(Survived)~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+Name_2+Cabin_2+cabin_number+family+family_binomial+SibSp_binomial+Parch_binomial,
                 data=train_3,ntree=100,mtry=4,importance=T,na.action=na.omit)


pred_tr_rf <- predict(rf, newdata=train_3)
t_tr_rf <- table(pred_tr_rf, train_3$Survived)
t_tr_rf

acc_tr_rf <- sum(diag(t_tr_rf))/sum(t_tr_rf)
acc_tr_rf 


#####test dataset 




