#update.packages("ReadStat");
#install.packages("dplyr")
#install.packages("readxl")
#install.packages("haven")
#install.packages("ggplot2")

#install.packages("pdftools")
#install.packages("tidyverse")
#install.packages("ggthemes")
#install.packages("ggrepel")
#install.packages("tm")
#install.packages("stringi")
#install.packages("PerformanceAnalytics")


pckg<-c("dplyr","readxl","haven","ggplot2","pdftools", "tidyverse", "ggplot2", "ggthemes", "ggrepel", "tm", "grid","PerformanceAnalytics")
sapply(pckg,library,character.only=TRUE)


library(dplyr); #edit
library(readxl); #excel load
library(doBy);
library(fmsb); # radar chart
library(ggplot2); #visualization
library(corrplot);# correlation
library(VIM); #missing data detection
library(DMwR); #outlier detection
library(corrplot); #correlation plot
library(PerformanceAnalytics); #correlation chart
library(rpart); #decision tree
library(rattle); #decision tree fancy tree
library(rpart.plot); #decision tree fancy tree
library(RColorBrewer); #decision tree fancy tree
library(ipred); # bagging
library(randomForest); # random forest
library(adabag); # adaptive boosting
library(lme4); #dummy function
library(caret); #standard
library(class); #KNN 
library(VennDiagram); #VennDiagram
library(neuralnet); #ann
library(e1071); #SVM
library(ROCR); #ROC












memory.limit();
memory.limit(100000000000);

kc_house<-read.csv("kc_house_data.csv", na.strings = "NA",stringsAsFactors = FALSE)

#test/training split

## 75% of the sample size
smp_size <- floor(0.75 * nrow(kc_house))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(kc_house)), size = smp_size)

kc_house <- kc_house[train_ind, ]
test <- kc_house[-train_ind, ]

#variable correlation

colnames(kc_house)


chart.Correlation(kc_house[,c("price","bedrooms","bathrooms","sqft_living","sqft_lot",
                              "floors", "view","condition","grade","sqft_above",
                              "sqft_basement","yr_built","yr_renovated","lat","long","sqft_living15","sqft_lot15" )], pch = 19, method = "pearson")








#price category split
kc_house$price_rank<-rank(-kc_house$price,na.last="keep")


kc_house<-kc_house%>%arrange(desc(kc_house$price))

ggplot(data=kc_house,aes(x=price_rank,y=lat))+geom_jitter()
ggplot(data=kc_house,aes(x=lat,y=price_rank))+geom_jitter()
ggplot(data=kc_house,aes(x=long,y=price_rank))+geom_jitter()
summary(kc_house$price)



quantile(kc_house$price)[1]
quantile(kc_house$price)[2]
quantile(kc_house$price)[3]
quantile(kc_house$price)[4]
quantile(kc_house$price)[5]


kc_house[which(kc_house$price>=quantile(kc_house$price)[4]),c("price_q")]<-c("1")
kc_house[which(kc_house$price<quantile(kc_house$price)[4] & kc_house$price>=quantile(kc_house$price)[3]),c("price_q")]<-c("2")
kc_house[which(kc_house$price<quantile(kc_house$price)[3] & kc_house$price>=quantile(kc_house$price)[2]),c("price_q")]<-c("3")
kc_house[which(kc_house$price<quantile(kc_house$price)[2]),c("price_q")]<-c("4")


ggplot(data=kc_house,mapping=aes(x=long,y=lat,color=price_q))+geom_point()
ggplot(data=kc_house,mapping=aes(x=long,y=lat))+geom_point()+stat_density2d()

ggplot(data=kc_house[which(kc_house$price_q=="1"),],mapping=aes(x=long,y=lat))+geom_point()+stat_density2d()
ggplot(data=kc_house[which(kc_house$price_q=="2"),],mapping=aes(x=long,y=lat))+geom_point()+stat_density2d()
ggplot(data=kc_house[which(kc_house$price_q=="3"),],mapping=aes(x=long,y=lat))+geom_point()+stat_density2d()
ggplot(data=kc_house[which(kc_house$price_q=="4"),],mapping=aes(x=long,y=lat))+geom_point()+stat_density2d()






summary(kc_house$lat)
summary(kc_house$long)

kc_house$lat_point<-NA
kc_house$lat_point<-ifelse(kc_house$lat<47.2,1,
                           ifelse(kc_house$lat<47.3,2,
                                  ifelse(kc_house$lat<47.4,3,
                                         ifelse(kc_house$lat<47.5,4,
                                                ifelse(kc_house$lat<47.6,5,
                                                       ifelse(kc_house$lat<47.7,7,
                                                              ifelse(kc_house$lat<47.8,8,0)))))))


kc_house$long_point<-ifelse(kc_house$long<(-122.5),1,
                            ifelse(kc_house$long<(-122.25),2,
                                   ifelse(kc_house$long<(-122.00),3,
                                          ifelse(kc_house$long<(-121.75),4,
                                                 ifelse(kc_house$long<(-121.50),5,
                                                        ifelse(kc_house$long<(-121.25),7,
                                                               ifelse(kc_house$long<(-121.00),8,0)))))))


kc_house$location<-paste(kc_house$lat_point,kc_house$long_point,sep="")

ggplot(data=kc_house,aes(x=as.factor(location),y=price))+geom_boxplot()

ggplot(data=kc_house,aes(x=as.factor(price_q),y=location))+geom_jitter()

df_2<-kc_house%>%group_by(location)%>%summarize(price=mean(price))
df_3<-kc_house%>%group_by(location)%>%summarize(price=median(price))
ggplot(data=df_2,aes(x=as.factor(location),y=price))+geom_col()
ggplot(data=df_3,aes(x=as.factor(location),y=price))+geom_col()


####DIST


#install.packages("Imap")
library("Imap")

kc_house$dist<-NA
for (i in 1:nrow(kc_house)) {
  
  kc_house[i,c("dist")] <- gdist(lon.1 = kc_house$long[i], 
                                 lat.1 = kc_house$lat[i], 
                                 lon.2 = kc_house$long[1], 
                                 lat.2 = kc_house$lat[1], 
                                 units="km")
}

ggplot(data=kc_house,aes(x=dist,y=price))+geom_smooth()
ggplot(data=kc_house,aes(x=dist,y=price))+geom_jitter()


colnames(kc_house)

chart.Correlation(kc_house[,c("price","bedrooms","bathrooms","sqft_living","sqft_lot",
                              "floors", "view","condition","grade","sqft_above",
                              "sqft_basement","yr_built","yr_renovated","lat","long","sqft_living15","sqft_lot15" )], pch = 19, method = "pearson")




#normalization

for (i in c(3:8,10:14,20:21)) {

kc_house[,i]<-scale(kc_house[,i],center=T,scale=T)

}


kc_house$price<-log(kc_house$price)

chart.Correlation(kc_house[,c("dist","price")], col=kc_house$location, pch = 19, method = "pearson")
chart.Correlation(kc_house[,c("lat","price")], col=kc_house$location, pch = 19, method = "pearson")




sapply()

kc_house$price_log<-log(kc_house$price)
kc_house$price_normal<-normalize(kc_house$price)
kc_house$price_log_normal<-normalize((kc_house$price_log))

kc_house$sqft_living_log<-log(kc_house$sqft_living)
kc_house$sqft_living_normal<-normalize(kc_house$sqft_living)
kc_house$sqft_living_log_normal<-normalize(kc_house$sqft_living_log)






ggplot(data=kc_house, mapping=aes(price))+geom_density()
ggplot(data=kc_house, mapping=aes(price_normal))+geom_density()
ggplot(data=kc_house, mapping=aes(price_log))+geom_density()
ggplot(data=kc_house, mapping=aes(price_log_normal))+geom_density()

ggplot(data=kc_house, mapping=aes(sqft_living))+geom_density()
ggplot(data=kc_house, mapping=aes(sqft_living_normal))+geom_density()
ggplot(data=kc_house, mapping=aes(sqft_living_log))+geom_density()
ggplot(data=kc_house, mapping=aes(sqft_living_log_normal))+geom_density()


summary(kc_house$price_normal)
summary(kc_house$price_log)





unique(kc_house$location)
unique(kc_house$location)

loc_group<-c("loc_group_1","loc_group_2","loc_group_3","loc_group_4","loc_group_5")
loc_group_1<-c("11","21","31","41","51","61","71")
loc_group_2<-c("12","22","32","42","52","62","72")
loc_group_3<-c("13","23","33","43","53","63","73")
loc_group_4<-c("14","24","34","44","54","64","74")
loc_group_5<-c("15","25","35","45","55","65","75")



for (i in (1:2)) {
  
  kc_house_loc<-kc_house[substr(kc_house$location,2,2)==i,]
  
  m<-lm(data=kc_house_loc, price~log(dist+1))
  m
  r<-as.character(round(summary(m)$r.squared*100,0))
  r_square<-paste(r,"%",sep="")
  
  g <- ggplot(data=kc_house_loc, aes(x = dist, y = price))
  g <- g + geom_point(aes(color = as.factor(price_q)), shape = 22, size = 3, stroke = 2)
  g <- g + geom_smooth(method = "lm", aes(fill = r_square), formula = y~ log(x+1), color = "red", se = FALSE)
  g
  
  assign(paste("g",i, sep = "_"), g)
  
}

g_1
g_2


#data_group




# EDA 

#grade
ggplot(data=kc_house)+
  geom_bar(mapping=aes(x=as.factor(grade)))


#price (response variable)

ggplot(data=kc_house)+
  geom_histogram(mapping=aes(x=price),binwidth = 200000)

#many histograms with geom_freqpoly
#Assumption : the distribution will move to the right, if the labeling has been proved as being effective.

ggplot(data=kc_house,mapping=aes(x=price, color=as.factor(condition)))+
  geom_freqpoly(bindwidth=200000)

ggplot(data=kc_house[which(kc_house$grade>6),],mapping=aes(x=price, color=as.factor(grade)))+
  xlim(0,4000000)+
  geom_freqpoly(bindwidth=200000)

#smoothing line is affected by outlier around 30 in bedrooms variable
ggplot(data=kc_house,mapping=aes(x=bedrooms, y=price))+
  geom_jitter()+
  geom_smooth()

ggplot(data=kc_house,mapping=aes(x=bedrooms, y=price))+
  xlim(0,10)+
  geom_jitter()+
  geom_smooth()

#factor로 바꿔서 boxplot을 적용
#bedroom variable
kc_house$bedrooms_fac<-as.factor(kc_house$bedrooms)
ggplot(data=kc_house[which(kc_house$bedrooms<12),])+
  geom_boxplot(mapping=aes(x=bedrooms_fac, y=price))+
  geom_smooth(mapping=aes(x=bedrooms, y=price))

#outlier들의 특성이 있을까?

#bathroom variable
kc_house$bathrooms_fac<-as.factor(kc_house$bathrooms)
ggplot(data=kc_house)+
  geom_boxplot(mapping=aes(x=bathrooms_fac, y=price))

#sqrt_living variable
ggplot(data=kc_house,mapping=aes(x=sqft_living, y=price))+
  geom_point()+
  geom_smooth()

ggplot(data=kc_house,mapping=aes(x=sqft_living, y=price))+
  geom_jitter(mapping=aes(col=bedrooms_fac))+
  geom_smooth()

ggplot(data=kc_house,mapping=aes(x=bedrooms_fac, y=sqft_living))+
  geom_jitter()+
  geom_smooth()


M<-cor(kc_house[,c("bedrooms","bathrooms","sqft_living")])
head(round(M,2))

library(corrplot)
corrplot(M, method="number")

kc_house$room<-kc_house$bathrooms+kc_house$bedrooms


M_2<-cor(kc_house[,c("room","sqft_living")])
head(round(M_2,2))
corrplot(M_2, method="number")

M_3<-cor(kc_house[,c("room","price")])
head(round(M_3,2))
corrplot(M_3, method="number")

M_4<-cor(kc_house[,c("sqft_living","price")])
head(round(M_4,2))
corrplot(M_4, method="number")



#sqrt_lot variable

ggplot(data=kc_house,mapping=aes(x=sqft_lot, y=price))+
  geom_point()+
  geom_smooth()

summary(kc_house$sqft_lot)

ggplot(data=kc_house[which(kc_house$sqft_lot<10688),],mapping=aes(x=sqft_lot, y=price))+
  geom_point()+
  geom_smooth()


kc_house$sqft_lot_bi<-NA
kc_house$sqft_lot_bi<-ifelse(kc_house$sqft_lot<10688,"0","1")


p<-ggplot(data=kc_house,aes(x=as.factor(sqft_lot_bi)))+ 
  geom_bar(aes(y = (..count..)) , fill = c('red','green'))
p<-p+xlab("Lot: 0 - No , 1 - Yes") + ylab("Count")
p<-p+labs(title="No lot Versus with lot ")
p<-p+geom_text(aes(y = (..count..),label =   ifelse((..count..)==0,"",
                                                    scales::percent((..count..)/sum(..count..)))), stat="count",colour="black") 
p


ggplot(data=kc_house,mapping=aes(x=sqft_lot_bi, y=price))+
  geom_boxplot()







# lot의 크기는 평균적인 집값을 예측하는 데는 도움이 되지 않음. 다만 outlier들의 분포가 4분위의 경우 더 커


#sqrt_base variable

ggplot(data=kc_house,aes(x=sqft_basement, y=price))+
  geom_jitter()

summary(kc_house$sqft_basement)

#mass zero

kc_house$sqft_basement_log<-ifelse(kc_house$sqft_basement==0,log(1),log(kc_house$sqft_basement))

ggplot(data=kc_house,aes(x=sqft_basement_log, y=price))+
  geom_jitter()

kc_house$sqft_base_bi<-NA
kc_house$sqft_base_bi<-ifelse(kc_house$sqft_basement==0,"0","1")

ggplot(data=kc_house,aes(x=sqft_base_bi, y=price))+
  geom_boxplot()



#floors

ggplot(data=kc_house,aes(x=as.factor(floors), y=price))+
  geom_boxplot()


#water front
#water front의 유무는 가격정보에 영향을 미침     
ggplot(data=kc_house,aes(x=as.factor(waterfront), y=price))+
  geom_boxplot()

#view
#view의 상승에 따라 가격정보에 영향을 미침     
ggplot(data=kc_house,aes(x=view, y=price))+
  geom_point()

ggplot(data=kc_house,aes(x=as.factor(view), y=price))+
  geom_boxplot()

#condition
#If the condition score is three, the outliers spread most widely
ggplot(data=kc_house,aes(x=as.factor(condition), y=price))+
  geom_boxplot()

#grade
ggplot(data=kc_house,aes(x=as.factor(grade), y=price))+
  geom_boxplot()


#condition and grade
ggplot(data=kc_house,aes(x=as.factor(condition), y=as.factor(grade)))+
  geom_boxplot()

#condition and grade are inverse relations?
M_5<-cor(kc_house[,c("condition","grade")])
head(round(M_5,2))
corrplot(M_5, method="number")


#sqft_above
#sqft_above and price show the linear relsationship.
#If we divide this line by grade, grade under 5 does not show any difference in their prices

ggplot(data=kc_house,mapping=aes(x=sqft_above, y=price))+
  geom_jitter(aes(col=as.factor(grade)))+
  geom_smooth()

ggplot(data=kc_house[which(kc_house$grade<6),],mapping=aes(x=sqft_above, y=price))+
  geom_jitter(aes(col=as.factor(grade)))+
  geom_smooth()

ggplot(data=kc_house[which(kc_house$grade>=6),],mapping=aes(x=sqft_above, y=price))+
  geom_jitter(aes(col=as.factor(grade)))+
  geom_smooth()

#yr_built

kc_house$yr_built_elapsed<-2015-kc_house$yr_built
kc_house$yr_renovated_bi<-ifelse(kc_house$yr_renovated==0,"0","1")
kc_house$yr_renovated_elapsed<-ifelse(kc_house$yr_renovated==0,0,2015-kc_house$yr_renovated)


ggplot(data=kc_house,mapping=aes(x=yr_built_elapsed, y=price))+
  geom_jitter()+
  geom_smooth()

M_6<-cor(kc_house[,c("yr_built_elapsed","price")])
head(round(M_6,2))
corrplot(M_6, method="number")



ggplot(data=kc_house,aes(x=as.factor(yr_renovated_bi), y=price))+
  geom_boxplot()



ggplot(data=kc_house[which(kc_house$yr_renovated_bi==1),],mapping=aes(x=yr_renovated_elapsed, y=price))+
  geom_jitter()+
  geom_smooth()


M_7<-cor(kc_house[,c("yr_renovated_elapsed","price")])
head(round(M_7,2))
corrplot(M_7, method="number")



#new index
#bathroom per bedrooms

kc_house$bath_per_bed<-ifelse(kc_house$bedrooms==0,0,kc_house$bathrooms/kc_house$bedrooms)
summary(kc_house$bath_per_bed)
ggplot(data=kc_house,aes(x=bath_per_bed, y=price))+geom_jitter()


#이 사유로 이진변수로 변환

ggplot(data=kc_house,aes(x=sqft_living,y=price,col=sqft_base_bi))+geom_jitter()
ggplot(data=kc_house,aes(x=sqft_living,y=price,col=sqft_lot_bi))+geom_boxplot()

ggplot(data=kc_house,aes(x=sqft_living,y=price,col=sqft_lot_bi))+geom_jitter()
ggplot(data=kc_house,aes(x=sqft_living,y=price,col=sqft_lot_bi_2))+geom_jitter()
ggplot(data=kc_house,aes(x=sqft_living,y=price,col=sqft_lot_bi_2))+geom_boxplot()





#####







ggplot(data=kc_house[which(kc_house$zipcode == 98006),],mapping=aes(x=long,y=lat,shape=as.factor(zipcode)))+geom_point()


label<-as.data.frame(kc_house$zipcode);
df<-as.data.frame(cbind(x=kc_house$long,y=kc_house$lat,label));



df<-kc_house[which(kc_house$price_q=="1"),c("long","lat","zipcode")]
colnames(df)<-c("x","y","label")
p<-ggplot(df,aes_all(c("x","y","label")));
p+geom_text();


df$x<-as.character(df$x);
df$y<-as.character(df$y);
df$x<-round(as.numeric(df$x),0);
df$y<-round(as.numeric(df$y),0);

p<-ggplot(df,aes_all(c("x","y","label")));
p+geom_text();



ggplot(data=kc_house,mapping=aes(x=long,y=lat,color=as.factor(grade)))+geom_point()
ggplot(data=kc_house[which(kc_house$grade=="6"),],mapping=aes(x=long,y=lat,color=as.factor(grade)))+geom_point()
ggplot(data=kc_house[which(kc_house$grade=="7"),],mapping=aes(x=long,y=lat,color=as.factor(grade)))+geom_point()
ggplot(data=kc_house[which(kc_house$grade=="8"),],mapping=aes(x=long,y=lat,color=as.factor(grade)))+geom_point()
ggplot(data=kc_house[which(kc_house$grade=="9"),],mapping=aes(x=long,y=lat,color=as.factor(grade)))+geom_point()
ggplot(data=kc_house[which(kc_house$grade=="10"),],mapping=aes(x=long,y=lat,color=as.factor(grade)))+geom_point()
ggplot(data=kc_house[which(kc_house$grade=="11"),],mapping=aes(x=long,y=lat,color=as.factor(grade)))+geom_point()
ggplot(data=kc_house[which(kc_house$grade=="12"),],mapping=aes(x=long,y=lat,color=as.factor(grade)))+geom_point()
ggplot(data=kc_house[which(kc_house$grade=="13"),],mapping=aes(x=long,y=lat,color=as.factor(grade)))+geom_point()
