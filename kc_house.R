

pckg<-c("dplyr","readxl","haven","ggplot2","pdftools", "tidyverse",
        "ggplot2", "ggthemes", "ggrepel", "tm", "grid","PerformanceAnalytics",
        "e1071","doBy","fmsb","corrplot","VIM","DMwR","rpart",
        "rattle","rpart.plot","RColorBrewer","ipred",
        "randomForest","adabag","lme4","caret","class","VennDiagram","ROCR","Imap")
sapply(pckg,library,character.only=TRUE) #library loading


memory.limit();
memory.limit(100000000000);



kc_house<-read.csv("kc_house_data.csv", na.strings = "NA",stringsAsFactors = FALSE) #data loading

#test/training split

smp_size <- floor(0.75 * nrow(kc_house)) # 75% of the sample size

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(kc_house)), size = smp_size)

kc_house <- kc_house[train_ind, ]
test <- kc_house[-train_ind, ]




###feature engineering

##rice category split by quantile
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

##ocation segmentation

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



##Distance calculation from the highest point



kc_house$dist<-NA
for (i in 1:nrow(kc_house)) {
    
    kc_house[i,c("dist")] <- gdist(lon.1 = kc_house$long[i], 
                                   lat.1 = kc_house$lat[i], 
                                   lon.2 = kc_house$long[1], 
                                   lat.2 = kc_house$lat[1], 
                                   units="km")
}




#lot category split
kc_house$lot_rank<-rank(-kc_house$sqft_lot,na.last="keep")
kc_house<-kc_house%>%arrange(desc(kc_house$sqft_lot)) #price rank

quantile(kc_house$sqft_lot)[2]
quantile(kc_house$sqft_lot)[3]
quantile(kc_house$sqft_lot)[4]
quantile(kc_house$sqft_lot)[5]

qunatile_lot<-quantile(kc_house$sqft_lot, probs = seq(0, 1, 0.05), na.rm = FALSE, names = TRUE, type = 7)
qunatile_lot[6]

kc_house[which(kc_house$sqft_lot>=qunatile_lot[20]),c("sqft_lot_q")]<-c("1") #sqft_lot qunatile 
kc_house[which(kc_house$sqft_lot<qunatile_lot[20] & kc_house$sqft_lot>=qunatile_lot[19]),c("sqft_lot_q")]<-c("2")
kc_house[which(kc_house$sqft_lot<qunatile_lot[19] & kc_house$sqft_lot>=qunatile_lot[18]),c("sqft_lot_q")]<-c("3")
kc_house[which(kc_house$sqft_lot<qunatile_lot[18] & kc_house$sqft_lot>=qunatile_lot[17]),c("sqft_lot_q")]<-c("4")
kc_house[which(kc_house$sqft_lot<qunatile_lot[17] & kc_house$sqft_lot>=qunatile_lot[16]),c("sqft_lot_q")]<-c("5")
kc_house[which(kc_house$sqft_lot<qunatile_lot[16] & kc_house$sqft_lot>=qunatile_lot[11]),c("sqft_lot_q")]<-c("6")
kc_house[which(kc_house$sqft_lot<qunatile_lot[11] & kc_house$sqft_lot>=qunatile_lot[6]),c("sqft_lot_q")]<-c("7")
kc_house[which(kc_house$sqft_lot<qunatile_lot[6]),c("sqft_lot_q")]<-c("8")

#price_per_space 

kc_house$price_per_lot<-kc_house$price/kc_house$sqft_lot
kc_house$price_per_living<-kc_house$price/kc_house$sqft_living
kc_house$price_per_all<-kc_house$price/(kc_house$sqft_lot+kc_house$sqft_living)
summary(kc_house$price_per_lot)

#base_binary_variable

kc_house$sqft_base_bi<-NA
kc_house$sqft_base_bi<-ifelse(kc_house$sqft_basement==0,"0","1")
kc_house$sqft_basement_log<-ifelse(kc_house$sqft_basement==0,0,log(kc_house$sqft_basement))
summary(kc_house$sqft_basement_log)

#yr_built

kc_house$yr_built_elapsed<-as.numeric(substr(kc_house$date,1,4))-kc_house$yr_built
kc_house$yr_renovated_bi<-ifelse(kc_house$yr_renovated==0,"0","1")
kc_house$yr_renovated_elapsed<-ifelse(kc_house$yr_renovated==0,0,as.numeric(substr(kc_house$date,1,4))-kc_house$yr_renovated)

#rooms

kc_house$bath_per_bed<-ifelse(kc_house$bedrooms==0,0,kc_house$bathrooms/kc_house$bedrooms)
kc_house$rooms<-kc_house$bathrooms+kc_house$bedrooms
kc_house$living_per_lot<-kc_house$sqft_living/kc_house$sqft_lot


#date

kc_house$trade_date<-substr(kc_house$date,1,6)
ggplot(data=kc_house,aes(x=trade_date, y=price))+geom_boxplot() #bathroom per bedrooms



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
           "floors", "view","condition","grade","sqft_above",
           "sqft_basement","yr_built","yr_renovated","lat","long","sqft_living15","sqft_lot15" )


DATA_cor<-kc_house[,num_col]; #corr test between x variable and y variable(price)      
data_cor_matrix<-cor(DATA_cor);                                                 
corr<-round(data_cor_matrix,2);


library(randomForest)

rf<-randomForest(price~sqft_living+sqft_lot+waterfront+view+condition+grade+
                     lat+long+sqft_living15+sqft_lot15+location+dist+price_per_lot+
                     price_per_living+price_per_all+bath_per_bed+living_per_lot,data=kc_house,ntree=100,mtry=3,importance=T,na.action=na.omit)


