



pckg<-c("dplyr","readxl","haven","ggplot2","pdftools", "tidyverse",
        "ggplot2", "ggthemes", "ggrepel", "tm", "grid","PerformanceAnalytics",
        "e1071","doBy","fmsb","corrplot","VIM","DMwR","rpart",
        "rattle","rpart.plot","RColorBrewer","ipred",
        "randomForest","adabag","lme4","caret","class","ROCR","Imap","caret","randomForest","naniar","gridExtra","GGally","leaflet")
#sapply(pckg,install.packages,character.only=TRUE) #library loading
sapply(pckg, function(x) require(x,quietly=TRUE,character.only=TRUE,warn.conflicts = FALSE )) #library loading



memory.limit();
memory.limit(100000000000);

kc_house<-read.csv("kc_house_data.csv", na.strings = "NA",stringsAsFactors = FALSE) #data loading


#missing data

apply(kc_house,MARGIN=2,FUN=any_na)
kc_house[!complete.cases(kc_house),]

#outlier 

for (i in (3:ncol(kc_house))) {
  
  lower<-kc_house[which(kc_house[,i]<quantile(kc_house[,i],c(0.01))),]
  assign(paste(colnames(kc_house)[i],"_out_low",sep=""),lower)
  
  up<-kc_house[which(kc_house[,i]>quantile(kc_house[,i],c(0.99))),]
  assign(paste(colnames(kc_house)[i],"_out_up",sep=""),up)
}



#test/training split

smp_size <- floor(0.75 * nrow(kc_house)) # 75% of the sample size

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(kc_house)), size = smp_size)

kc_house<- kc_house[train_ind, ]
kc_house_test <- kc_house[-train_ind, ]


##overal EDA



date_var<-c("date","yr_built","yr_renovated")
fac_var<-c("waterfront","zipcode","grade","condition")
num_var<-c("price","bedrooms","bathrooms","sqft_living","sqft_lot","floors","view","sqft_above","sqft_basement","lat","long","sqft_living15","sqft_lot15")


unique_zipcode<-unique(kc_house$zipcode)
unique_waterfront<-unique(kc_house$waterfront)
unique_grade<-unique(kc_house$grade)
unique_condition<-unique(kc_house$condition)


kc_house$zipcode<-factor(kc_house$zipcode,levels=unique_zipcode)
kc_house$waterfront<-factor(kc_house$waterfront,levels=unique_waterfront)
kc_house$grade<-factor(kc_house$grade,levels=unique_grade)
kc_house$condition<-factor(kc_house$condition,levels=unique_condition)

str(kc_house)

install.packages("parsedate")
library(parsedate)

kc_house$date<-substr(kc_house$date,1,8)
kc_house$date_YMD<-as.Date(kc_house$date,format="%Y%m%d")

#numerical value
chart.Correlation(kc_house[,num_var], pch = 19, method = "pearson")


distribution <- as.data.frame(t(sapply(kc_house[,num_var], quantile)))
distribution$Mean <- sapply(kc_house[,num_var], mean)
distribution$SD <- sapply(kc_house[,num_var], sd)
distribution$skenewss <- sapply(kc_house[,num_var], skewness)
distribution$kurtosis <- sapply(kc_house[,num_var], kurtosis)
distribution<-round(distribution, 2)






###feature engineering

##date, year built, renovated year (date)

# the age of houses
kc_house$age_sold<-as.numeric(substr(kc_house$date,1,4))-kc_house$yr_built
kc_house$age_sold_group<-ifelse(kc_house$age_sold<10,"0~9",
                                ifelse(kc_house$age_sold<20,"10~19",
                                       ifelse(kc_house$age_sold<30,"20~29",
                                              ifelse(kc_house$age_sold<40,"30~39",
                                                     ifelse(kc_house$age_sold<50,"40~49",
                                                            ifelse(kc_house$age_sold<60,"50~59",
                                                                   ifelse(kc_house$age_sold<70,"60~69",
                                                                          ifelse(kc_house$age_sold<80,"70~79",
                                                                                 ifelse(kc_house$age_sold<90,"80~89",
                                                                                        ifelse(kc_house$age_sold<100,"90~99","100+"))))))))))
                                                                                               
  

#binary of renovation
kc_house$yr_renovated_bi<-ifelse(kc_house$yr_renovated==0,"0","1") 
kc_house$yr_renovated_bi<-factor(kc_house$yr_renovated_bi,levels=c("0","1")) #factor

# the year
kc_house$age_sold_reno<-ifelse(kc_house$yr_renovated==0,kc_house$age_sold,as.numeric(substr(kc_house$date,1,4))-kc_house$yr_renovated)
kc_house$yr_built_reno<-ifelse(kc_house$yr_renovated==0,kc_house$yr_built,kc_house$yr_renovated)


v1 <- ggplot(kc_house, aes(yr_built)) +
  geom_bar(fill = "dodgerblue3") +
  ggtitle("the number of houses built by year") +
  theme_classic() +
  theme(text = element_text(face ="bold"),
        plot.title = element_text(hjust = 0.5))


v2 <- ggplot(kc_house, aes(yr_built, price)) +
  geom_smooth(se = TRUE, colour = "dodgerblue3") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
  ggtitle("the price trend of houses built by year") +
  theme_classic() +
  theme(text = element_text(face = "bold"),
        plot.title=element_text(hjust=0.5))

v3 <- ggplot(kc_house, aes(yr_built_reno, price)) +
  geom_smooth(se = TRUE, colour = "dodgerblue3") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
  ggtitle("the price trend of houses built by adjusted year") +
  theme_classic() +
  theme(text = element_text(face = "bold"),
        plot.title=element_text(hjust=0.5))


v4<-ggplot(kc_house)+
  geom_smooth(aes(yr_built_reno, price),se=FALSE, colour = "firebrick2")+
  geom_smooth(aes(yr_built, price),se=FALSE, colour="dodgerblue3")+
  xlab("(built/adjusted built) year ")+
  ggtitle("comparison of price by adjusted year") +
  theme_classic()+
  theme(text = element_text(face = "bold"),
      plot.title=element_text(hjust=0.5))

price_yr<- grid.arrange(v1,v2,v3)
price_yr

v4

 reno_1 <- ggplot(kc_house%>%filter(yr_renovated_bi=="1"), aes(yr_renovated)) +
   geom_bar(fill = "dodgerblue3") +
   theme_classic() 

 reno_1
 
 
 reno_1_1 <- ggplot(kc_house%>%filter(yr_renovated_bi=="1"), aes(yr_built)) +
   geom_bar(fill = "dodgerblue3") +
   theme_classic() 

 reno_1_1
 

 reno_2 <- ggplot(kc_house%>%filter(yr_renovated_bi=="1"), aes(yr_renovated, price)) +
   geom_smooth(se = TRUE, colour = "dodgerblue3") +
   scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
   scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
   theme_classic() 

 reno_2
 
 reno_2_2 <- ggplot(kc_house%>%filter(yr_renovated_bi=="1"), aes(yr_built, price)) +
   geom_smooth(se = TRUE, colour = "dodgerblue3") +
   scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
   scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
   theme_classic()

 reno_2_2
 
  
 price_reno_yr<- grid.arrange(reno_1, reno_1_1, reno_2,reno_2_2)
 price_reno_yr
 

 
reno3 <- ggplot(kc_house,aes(age_sold_group,price,color=yr_renovated_bi))+
   geom_boxplot()+
   ggtitle("renovation effect in the same age band") +
   theme_classic() +
   theme(text = element_text(face ="bold"),
         plot.title = element_text(hjust = 0.5))

reno3



## price 


price_1 <- ggplot(price_out_up, aes(price)) +
  geom_histogram(fill = "dodgerblue3",binwidth = 100000) +
  ggtitle("upper price outlier") +
  theme_classic() +
  theme(text = element_text(face ="bold"),
        plot.title = element_text(hjust = 0.5))

price_2  <- ggplot(price_out_low, aes(price)) +
    geom_histogram(fill = "dodgerblue3",binwidth = 5000) +
    ggtitle("lower price outlier") +
    theme_classic() +
    theme(text = element_text(face ="bold"),
          plot.title = element_text(hjust = 0.5))

 price_outlier<- grid.arrange(price_1,price_2)
 price_outlier



##price category split by quantile

kc_house$price_rank<-rank(-kc_house$price,na.last="keep")
kc_house<-kc_house%>%arrange(desc(kc_house$price)) #price rank

summary(kc_house$price)

quantile(kc_house$price)[1] #qunatile
quantile(kc_house$price)[2]
quantile(kc_house$price)[3]
quantile(kc_house$price)[4]
quantile(kc_house$price)[5]


kc_house[which(kc_house$price>=quantile(kc_house$price)[4]),c("price_q")]<-c("1") #price qunatile 
kc_house[which(kc_house$price<quantile(kc_house$price)[4] & kc_house$price>=quantile(kc_house$price)[3]),c("price_q")]<-c("2")
kc_house[which(kc_house$price<quantile(kc_house$price)[3] & kc_house$price>=quantile(kc_house$price)[2]),c("price_q")]<-c("3")
kc_house[which(kc_house$price<quantile(kc_house$price)[2]),c("price_q")]<-c("4")


summary(kc_house$lat)
summary(kc_house$long)

#location segmentation

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


kc_house_q1<-kc_house[which(kc_house$price_q=="1"),]
kc_house_q2<-kc_house[which(kc_house$price_q=="2"),]
kc_house_q3<-kc_house[which(kc_house$price_q=="3"),]
kc_house_q4<-kc_house[which(kc_house$price_q=="4"),]


html_1<-leaflet() %>% 
  setView(kc_house[1,c("long")],kc_house[1,c("lat")],zoom=10) %>%
  addTiles() %>%
  addMarkers(kc_house_q1[,c("long")],kc_house_q1[,c("lat")],clusterOptions = TRUE)

html_1

html_2<-leaflet() %>% 
  setView(kc_house[1,c("long")],kc_house[1,c("lat")],zoom=10) %>%
  addTiles() %>%
  addMarkers(kc_house_q2[,c("long")],kc_house_q2[,c("lat")],clusterOptions = TRUE)

html_2

html_3<-leaflet() %>% 
  setView(kc_house[1,c("long")],kc_house[1,c("lat")],zoom=10) %>%
  addTiles() %>%
  addMarkers(kc_house_q3[,c("long")],kc_house_q3[,c("lat")],clusterOptions = TRUE)

html_3

html_4<-leaflet() %>% 
  setView(kc_house[1,c("long")],kc_house[1,c("lat")],zoom=10) %>%
  addTiles() %>%
  addMarkers(kc_house_q4[,c("long")],kc_house_q4[,c("lat")],clusterOptions = TRUE)

html_4





##Distance calculation from the highest point

kc_house$dist<-NA
for (i in 1:nrow(kc_house)) {
  
  kc_house[i,c("dist")] <- gdist(lon.1 = kc_house$long[i], 
                                 lat.1 = kc_house$lat[i], 
                                 lon.2 = kc_house$long[1], 
                                 lat.2 = kc_house$lat[1], 
                                 units="km")
}


v5<-ggplot(kc_house,aes(dist,price))+
  xlab("distance (km)")+
  ylab("price (dollar)")+
  geom_smooth(se=TRUE, colour="dodgerblue3")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
  ggtitle("the price by dist from the highest point") +
  theme_classic()+
  theme(text=element_text(face="bold"),
        plot.title=element_text(hjust=0.5))

v5




#lot category split
kc_house$lot_rank<-rank(-kc_house$sqft_lot,na.last="keep")
kc_house<-kc_house%>%arrange(desc(kc_house$sqft_lot)) #lot rank

quantile(kc_house$sqft_lot)[2]
quantile(kc_house$sqft_lot)[3]
quantile(kc_house$sqft_lot)[4]
quantile(kc_house$sqft_lot)[5]

qunatile_lot<-quantile(kc_house$sqft_lot, probs = seq(0, 1, 0.05), na.rm = FALSE, names = TRUE, type = 7)
qunatile_lot[16]

kc_house[which(kc_house$sqft_lot>=qunatile_lot[20]),c("sqft_lot_q")]<-c("1") #95%
kc_house[which(kc_house$sqft_lot<qunatile_lot[20] & kc_house$sqft_lot>=qunatile_lot[19]),c("sqft_lot_q")]<-c("2") #90%
kc_house[which(kc_house$sqft_lot<qunatile_lot[19] & kc_house$sqft_lot>=qunatile_lot[18]),c("sqft_lot_q")]<-c("3") #85%
kc_house[which(kc_house$sqft_lot<qunatile_lot[18] & kc_house$sqft_lot>=qunatile_lot[17]),c("sqft_lot_q")]<-c("4") #80%
kc_house[which(kc_house$sqft_lot<qunatile_lot[17] & kc_house$sqft_lot>=qunatile_lot[16]),c("sqft_lot_q")]<-c("5") #75%
kc_house[which(kc_house$sqft_lot<qunatile_lot[16] & kc_house$sqft_lot>=qunatile_lot[11]),c("sqft_lot_q")]<-c("6") #50~75%
kc_house[which(kc_house$sqft_lot<qunatile_lot[11] & kc_house$sqft_lot>=qunatile_lot[6]),c("sqft_lot_q")]<-c("7")  #25~50%
kc_house[which(kc_house$sqft_lot<qunatile_lot[6]),c("sqft_lot_q")]<-c("8") #0~50%


summary(kc_house[which(kc_house$sqft_lot_q=="1"),][7])


ggplot(kc_house[which(kc_house$sqft_lot_q=="1"),],aes(sqft_lot))+
  geom_histogram(binwidth=5000)+
  theme_classic()
  

ggplot(kc_house[which(kc_house$sqft_lot_q=="6"),],aes(sqft_lot))+
  geom_histogram(binwidth=50)+
  theme_classic()

ggplot(kc_house[which(kc_house$sqft_lot_q=="7"),],aes(sqft_lot))+
  geom_histogram(binwidth=50)+
  theme_classic()

ggplot(kc_house[which(kc_house$sqft_lot_q=="8"),],aes(sqft_lot))+
  geom_histogram(binwidth=50)+
  theme_classic()

unique_loc<-unique(kc_house$location)

for (i in (1:27)) {

  a<-ggplot(kc_house%>%filter(location==unique_loc[i]), aes(sqft_lot, price, col=as.factor(price_q)))+
  geom_jitter()+
  xlab(paste("lot of",unique_loc[i],sep=""))+  
  ylab("price of houses")+
  theme_classic()

  
  assign(paste("a",i,sep=""),a)
}

grid.arrange(a1,a2,a3)
grid.arrange(a4,a5,a6)
grid.arrange(a7,a8,a9)
grid.arrange(a10,a11,a12)
grid.arrange(a13,a14,a15)
grid.arrange(a16,a17,a18)
grid.arrange(a19,a20,a21)
grid.arrange(a19,a20,a21)
grid.arrange(a22,a23,a24)
grid.arrange(a25,a26,a27)

                       
lot_price
  
  
  
price_yr<- grid.arrange(v1,v2,v3)





#price_per_space 

kc_house$price_per_lot<-kc_house$price/kc_house$sqft_lot
kc_house$price_per_living<-kc_house$price/kc_house$sqft_living
kc_house$price_per_all<-kc_house$price/(kc_house$sqft_lot+kc_house$sqft_living)
summary(kc_house$price_per_lot)

#base_binary_variable

kc_house$sqft_base_bi<-NA
kc_house$sqft_base_bi<-ifelse(kc_house$sqft_basement==0,"0","1")
summary(kc_house$sqft_basement_log)












#rooms

kc_house$bath_per_bed<-ifelse(kc_house$bedrooms==0,0,kc_house$bathrooms/kc_house$bedrooms)
kc_house$rooms<-kc_house$bathrooms+kc_house$bedrooms
kc_house$living_per_lot<-kc_house$sqft_living/kc_house$sqft_lot


#date

kc_house$trade_date<-substr(kc_house$date,1,6)
ggplot(data=kc_house,aes(x=trade_date, y=price))+geom_boxplot() #bathroom per bedrooms

# supply and demand

kc_house%>%group_by(trade_date)%>%summarize(count=n())



#summary for mean and median price per lot

summary_1<-kc_house%>%group_by(location,grade)%>%summarize(avg_lot=mean(sqft_lot),avg_price=mean(price))
summary_1$ration<-summary_1$avg_price/summary_1$avg_lot

summary_2<-kc_house%>%group_by(location,grade)%>%summarize(m_lot=median(sqft_lot),m_price=median(price))
summary_2$ration_2<-summary_2$m_price/summary_2$m_lot


kc_house$f_key<-paste(kc_house$grade,kc_house$location,sep="")
summary_1$f_key<-paste(summary_1$grade,summary_1$location,sep="")

kc_house<-left_join(kc_house,summary_1,by="f_key")
kc_house<-kc_house[,-c(42,43)]

summary_2$f_key<-paste(summary_2$grade,summary_2$location,sep="")

kc_house<-left_join(kc_house,summary_2,by="f_key")
kc_house<-kc_house[,-c(45,46)]




ggplot(data=summary_2%>%filter(grade==12),aes(x=location, y=ration_2))+geom_bar(stat="identity") 
ggplot(data=summary_1,aes(x=location, y=ration))+geom_bar(stat="identity") 
ggplot(data=kc_house,aes(x=location, y=sqft_lot))+geom_boxplot() 




nearZeroVar(kc_house,saveMetrics=TRUE)
findCorrelation(cor(DATA_cor))

install.packages("FSelector")
library(FSelector)

colnames(kc_house)
kc_house<-kc_house[,-c(14,16,35,36)]

install.packages("randomForest")
library(randomForest)


skewness_col<-c("price","bedrooms","bathrooms","sqft_living","view","sqft_above","sqft_basement",
                "sqft_living15","sqft_lot15","price_per_lot","price_per_living","price_per_all",
                "yr_renovated_elapsed","living_per_lot")

col_number(skewness_col)

kc_house$price<-exp(kc_house$price)
kc_house$sqft_living<-log(kc_house$sqft_living)
kc_house$sqft_living15<-log(kc_house$sqft_living15)







colnames(kc_house)
kc_house[,c(3,6,7,8,9,10,11,16,17,18,25,39)]<-scale(kc_house[,c(3,6,7,8,9,10,11,16,17,18,25,39)],center=TRUE,scale=TRUE)



kc_house$location.x<-as.factor(kc_house$location.x)











install.packages("kernlab")
library(kernlab)
m<-ksvm(price~.,data=kc_house)

m







model<-randomForest(price~sqft_living+sqft_lot+floors+waterfront+view+grade.x+lat+sqft_living15+location.x+long+avg_price+dist,data=kc_house,importance=TRUE)


tuneRF(kc_house[,c("sqft_living","sqft_lot","floors","waterfront",
                   "view","grade.x","lat","sqft_living15","location.x","long","avg_price","dist")],kc_house$price,mtryStart=8)


grid<-expand.grid(ntree=c(300,400,500,600,700),mtry=c(4))
grid


randomForest(price~sqft_living+sqft_lot+floors+waterfront+view+grade.x+lat+sqft_living15+location.x+long+avg_price+dist,
             data=kc_house,importance=TRUE,ntree=300,mtry=4)
  
 

randomForest(price~sqft_living+sqft_lot+floors+waterfront+view+grade.x+lat+sqft_living15+location.x+long+avg_price+dist,
             data=kc_house,importance=TRUE,ntree=400,mtry=4)

randomForest(price~sqft_living+sqft_lot+floors+waterfront+view+grade.x+lat+sqft_living15+location.x+long+avg_price+dist,
             data=kc_house,importance=TRUE,ntree=500,mtry=4)

randomForest(price~sqft_living+sqft_lot+floors+waterfront+view+grade.x+lat+sqft_living15+location.x+long+avg_price+dist+yr_built,
             data=kc_house,importance=TRUE,ntree=600,mtry=4)

randomForest(price~sqft_living+sqft_lot+floors+waterfront+view+grade.x+lat+sqft_living15+location.x+long+avg_price+dist,
             data=kc_house,importance=TRUE,ntree=800,mtry=4)

 
full_m<-lm(price~sqft_living+sqft_lot+floors+waterfront+view+grade.x+lat+sqft_living15+location.x+long+avg_price+dist,
           data=kc_house)

m<-lm(price~sqft_living+sqft_lot+floors+waterfront+view+grade.x+lat+sqft_living15+location.x+long+avg_price+dist,data=kc_house)
m
vif(m)

null_m<-lm(price~1.,data=kc_house)

forw_m<-step(null_m,direction="both",trace=1,scope=list(lower=null_m,upper=full_m))

forw_m_2<-step(full_m,direction="backward",trace=1,scope=list(lower=null_m,upper=full_m))


summary(forw_m_2)

install.packages("VIF")
library(VIF)
vif(forw_m)


xyplot(kc_house$price~predict(forw_m))
xyplot(resid(forw_m)~predict(forw_m))

kc_house<-read.csv("kc_house_data.csv", na.strings = "NA",stringsAsFactors = FALSE) 
library(randomForest)

randomForest(price~.,data=kc_house[,c(3:21)],importance=TRUE)



importance(model)

varImpPlot(model)

#skewness of all variables
# log transformation

skewness(kc_house[,3], na.rm = FALSE, type = 3) # skewness is 4.28

skewness_col<-c("price","bedrooms","bathrooms","sqft_living","view","sqft_above","sqft_basement",
                "sqft_living15","sqft_lot15","price_per_lot","price_per_living","price_per_all",
                "yr_renovated_elapsed","living_per_lot")
colnames(kc_house)

for (i in c(3:6,13,20,21,30,31,32,35,40)){
  
  kc_house[,i]<-log(kc_house[,i]+1) }

#normalization

for (i in c(3:8,10:14,20:21,27,30,31,32,35,37,38,39,40)) {
  
  kc_house[,i]<-scale(kc_house[,i],center=T,scale=T)
  
}






chart.Correlation(kc_house[,c("dist","price")], col=kc_house$location, pch = 19, method = "pearson")
chart.Correlation(kc_house[,c("lat","price")], col=kc_house$location, pch = 19, method = "pearson")








#variable correlation

colnames(kc_house)

num_col<-c("price","bedrooms","bathrooms","sqft_living","sqft_lot",
           "floors", "view","condition","grade.x","sqft_above",
           "sqft_basement","yr_built","yr_renovated","lat","long","sqft_living15","sqft_lot15","avg_lot","avg_price","ration",
           "m_lot","m_price","ration_2")


DATA_cor<-kc_house[,num_col]; #corr test between x variable and y variable(price)      
data_cor_matrix<-cor(DATA_cor);                                                 
corr<-round(data_cor_matrix,2);


library(randomForest)

rf<-randomForest(price~sqft_living+sqft_lot+waterfront+view+condition+grade+
                   lat+long+sqft_living15+sqft_lot15+location+dist+price_per_lot+
                   price_per_living+price_per_all+bath_per_bed+living_per_lot,data=kc_house,ntree=100,mtry=3,importance=T,na.action=na.omit)






#visualization



#ggplotly(p)
t











contVars <- c("price", "sqft_living", "sqft_lot", "sqft_above", "sqft_basement", "sqft_living15", "sqft_lot15", "yr_built_elapsed")
df3 <- kc_house[,contVars]
df3 <- as.data.frame(melt(df3))

u <- ggplot(df3, aes(value)) +
  geom_density(aes(fill = variable)) +
  facet_wrap(~variable, scales = "free") +
  labs(x = "", y = "", fill = "") +
  theme_minimal() +
  scale_fill_tableau() +
  theme(text = element_text(face = "bold"),
        legend.position = "right",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5))


df2_norm <- kc_house[,contVars]
df2_norm <- as.data.frame(apply(df2_norm, 2,function(x)((x - min(x))/(max(x)-min(x)))))
df2_norm <- as.data.frame(melt(df2_norm))

p <- ggplot(df2_norm, aes(variable, value)) +
  geom_boxplot(aes(fill = variable)) +
  coord_flip() +                                
  scale_fill_tableau() +
  labs(x = "", y = "") +
  theme_minimal() +
  theme(text = element_text(face = "bold"),
        legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank())   


install.packages("GGally")
library("GGally")






ggpairs(kc_house[,contVars]) +
  theme_minimal() +
  theme(text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 90),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())



df1 <- kc_house[,fac_var]
df1 <- sapply(df1, as.factor)
df1 <- as.data.frame(melt(df1))
df1$value <- factor(df1$value, levels=sort(as.numeric(levels(df1$value))), ordered=TRUE)


t <- ggplot(df1, aes(value)) +
  geom_bar(aes(fill = Var2)) + 
  #scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  #scale_x_discrete(expand = c(0,0)) +
  facet_wrap(~Var2, scales = "free", nrow = 3) +
  scale_fill_tableau() +
  ggtitle("Count of each Discrete Variable") +
  labs(fill = "", x = "", y = "") +
  theme_minimal() +
  theme(text = element_text(face = "bold"),
        legend.position = "right",
        axis.text.x = element_text(angle = 0),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5)) 

t

