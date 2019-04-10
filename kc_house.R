

pckg<-c("dplyr","readxl","haven","ggplot2","pdftools", "tidyverse",
        "ggplot2", "ggthemes", "ggrepel", "tm", "grid","PerformanceAnalytics",
        "e1071","doBy","fmsb","corrplot","VIM","DMwR","rpart",
        "rattle","rpart.plot","RColorBrewer","ipred",
        "randomForest","adabag","lme4","caret","class","VennDiagram","ROCR")
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

#variable correlation

colnames(kc_house)

num_col<-c("price","bedrooms","bathrooms","sqft_living","sqft_lot",
           "floors", "view","condition","grade","sqft_above",
           "sqft_basement","yr_built","yr_renovated","lat","long","sqft_living15","sqft_lot15" )


DATA_cor<-kc_house[,num_col]; #corr test between x variable and y variable(price)      
data_cor_matrix<-cor(DATA_cor);                                                 
corr<-round(data_cor_matrix,2);   

###feature engineering


#price category split
kc_house$price_rank<-rank(-kc_house$price,na.last="keep")
kc_house<-kc_house%>%arrange(desc(kc_house$price)) #price rank

ggplot(data=kc_house,aes(x=price_rank,y=lat))+geom_jitter()
ggplot(data=kc_house,aes(x=lat,y=price_rank))+geom_jitter()
ggplot(data=kc_house,aes(x=long,y=price_rank))+geom_jitter()
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


ggplot(data=kc_house,mapping=aes(x=long,y=lat,color=price_q))+geom_point()
ggplot(data=kc_house,mapping=aes(x=long,y=lat))+geom_point()+stat_density2d()

ggplot(data=kc_house[which(kc_house$price_q=="1"),],mapping=aes(x=long,y=lat))+geom_point()+stat_density2d()
ggplot(data=kc_house[which(kc_house$price_q=="2"),],mapping=aes(x=long,y=lat))+geom_point()+stat_density2d()
ggplot(data=kc_house[which(kc_house$price_q=="3"),],mapping=aes(x=long,y=lat))+geom_point()+stat_density2d()
ggplot(data=kc_house[which(kc_house$price_q=="4"),],mapping=aes(x=long,y=lat))+geom_point()+stat_density2d()




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

ggplot(data=kc_house,aes(x=as.factor(location),y=price))+geom_boxplot()

ggplot(data=kc_house,aes(x=as.factor(price_q),y=location))+geom_jitter()

df_2<-kc_house%>%group_by(location)%>%summarize(price=mean(price))
df_3<-kc_house%>%group_by(location)%>%summarize(price=median(price))
ggplot(data=df_2,aes(x=as.factor(location),y=price))+geom_col()
ggplot(data=df_3,aes(x=as.factor(location),y=price))+geom_col()


##Distance calculation from the highest point


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


#correlation new

colnames(kc_house)

num_col_2<-c("price","bedrooms","bathrooms","sqft_living","sqft_lot",
           "floors", "view","condition","grade","sqft_above",
           "sqft_basement","yr_built","yr_renovated","lat","long","sqft_living15","sqft_lot15",
           "lat_point","long_point","dist")


DATA_cor<-kc_house[,num_col_2]; #corr test between x variable and y variable(price)      
data_cor_matrix<-cor(DATA_cor);                                                 
corr_2<-round(data_cor_matrix,2);   








#grade - symmetirc bell. 

ggplot(data=kc_house)+
    geom_bar(mapping=aes(x=as.factor(grade)))

#price (response variable) - skewed right and variable transformation needed using log

skew_over_zero<-c("price","bedrooms","sqft_living","sqft_lot","view",
                  "sqft_above","sqft_basement","sqft_living15","sqft_lot15") #log transformation variable

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


#bedroom variable as boxplot
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


#sqrt_lot variable



ggplot(data=kc_house)+
    geom_histogram(mapping=aes(x=sqft_lot),binwidth = 50000)


summary(kc_house$sqft_lot)

lot_outlier<-kc_house[which(kc_house$sqft_lot>50000),]

ggplot(data=kc_house,mapping=aes(x=sqft_lot, y=price))+
    geom_point()+
    geom_smooth()

ggplot(data=kc_house[which(kc_house$sqft_lot<summary(kc_house$sqft_lot)[5]),],mapping=aes(x=sqft_lot, y=price))+
    geom_point()+
    geom_smooth()


#lot category split
kc_house$lot_rank<-rank(-kc_house$sqft_lot,na.last="keep")
kc_house<-kc_house%>%arrange(desc(kc_house$sqft_lot)) #price rank

ggplot(data=kc_house,aes(x=lot_rank,y=lat))+geom_jitter()
ggplot(data=kc_house,aes(x=lat,y=lot_rank))+geom_jitter()
ggplot(data=kc_house,aes(x=long,y=lot_rank))+geom_jitter()




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




ggplot(data=kc_house,mapping=aes(x=long,y=lat,color=sqft_lot_q))+geom_point()
ggplot(data=kc_house,mapping=aes(x=long,y=lat))+geom_point()+stat_density2d()

ggplot(data=kc_house[which(kc_house$sqft_lot_q=="1"),],mapping=aes(x=long,y=lat,col=price_q))+geom_point()+stat_density2d()
ggplot(data=kc_house[which(kc_house$sqft_lot_q=="2"),],mapping=aes(x=long,y=lat,col=price_q))+geom_point()+stat_density2d()
ggplot(data=kc_house[which(kc_house$sqft_lot_q=="3"),],mapping=aes(x=long,y=lat,col=price_q))+geom_point()+stat_density2d()
ggplot(data=kc_house[which(kc_house$sqft_lot_q=="4"),],mapping=aes(x=long,y=lat,col=price_q))+geom_point()+stat_density2d()
ggplot(data=kc_house[which(kc_house$sqft_lot_q=="5"),],mapping=aes(x=long,y=lat,col=price_q))+geom_point()+stat_density2d()
ggplot(data=kc_house[which(kc_house$sqft_lot_q=="6"),],mapping=aes(x=long,y=lat,col=price_q))+geom_point()+stat_density2d()
ggplot(data=kc_house[which(kc_house$sqft_lot_q=="7"),],mapping=aes(x=long,y=lat,col=price_q))+geom_point()+stat_density2d()
ggplot(data=kc_house[which(kc_house$sqft_lot_q=="8"),],mapping=aes(x=long,y=lat,col=price_q))+geom_point()+stat_density2d()


kc_house$price_per_lot<-kc_house$price/kc_house$sqft_lot
kc_house$price_per_living<-kc_house$price/kc_house$sqft_living
kc_house$price_per_all<-kc_house$price/(kc_house$sqft_lot+kc_house$sqft_living)


ggplot(data=kc_house,mapping=aes(x=location, y=price_per_lot))+
    geom_boxplot()

ggplot(data=kc_house,mapping=aes(x=location, y=price_per_living))+
    geom_boxplot()

ggplot(data=kc_house,mapping=aes(x=location, y=price_per_all))+
    geom_boxplot()



ggplot(data=kc_house)+
    geom_histogram(mapping=aes(x=price_per_lot))

ggplot(data=kc_house)+
    geom_histogram(mapping=aes(x=price_per_living))

ggplot(data=kc_house)+
    geom_histogram(mapping=aes(x=price_per_all))


summary(kc_house$price_per_lot)


#correlation new

colnames(kc_house)

num_col_3<-c("price","bedrooms","bathrooms","sqft_living","sqft_lot",
             "floors", "view","condition","grade","sqft_above",
             "sqft_basement","yr_built","yr_renovated","lat","long","sqft_living15","sqft_lot15",
             "lat_point","long_point","dist","price_per_lot","price_per_living","price_per_all")


DATA_cor<-kc_house[,num_col_3]; #corr test between x variable and y variable(price)      
data_cor_matrix<-cor(DATA_cor);                                                 
corr_3<-round(data_cor_matrix,2);   









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







#categorical variable

#sqrt_base variable

ggplot(data=kc_house,aes(x=sqft_basement, y=price))+
    geom_jitter()

summary(kc_house$sqft_basement) #mass zero #median value is zero

kc_house$sqft_base_bi<-NA
kc_house$sqft_base_bi<-ifelse(kc_house$sqft_basement==0,"0","1")
ggplot(data=kc_house,aes(x=sqft_base_bi, y=price))+
    geom_boxplot() #binary variable (0 : no basement, 1: basement )


kc_house$sqft_basement_log<-ifelse(kc_house$sqft_basement==0,0,log(kc_house$sqft_basement))
summary(kc_house$sqft_basement_log)
ggplot(data=kc_house,aes(x=sqft_basement_log, y=price))+
    geom_jitter() #sqft_basement_log




ggplot(data=kc_house,aes(x=as.factor(floors), y=price))+
    geom_boxplot() #floors is meaningful. 


ggplot(data=kc_house,aes(x=as.factor(waterfront), y=price))+
    geom_boxplot() #water front is meaningful

 
ggplot(data=kc_house,aes(x=as.factor(view), y=price))+
    geom_boxplot() #view is meaningful


ggplot(data=kc_house,aes(x=as.factor(condition), y=price))+
    geom_boxplot() #condition is meaningful


ggplot(data=kc_house,aes(x=as.factor(grade), y=price))+
    geom_boxplot() #grade is super meaningful


#sqft_above

ggplot(data=kc_house,mapping=aes(x=sqft_above, y=price))+
    geom_jitter(aes(col=as.factor(grade)))+
    geom_smooth()

ggplot(data=kc_house[which(kc_house$grade<6),],mapping=aes(x=sqft_above, y=price))+
    geom_jitter(aes(col=as.factor(grade)))+
    geom_smooth() #If we divide this line by grade, grade under 5 does not show any difference in their prices

ggplot(data=kc_house[which(kc_house$grade<6),],mapping=aes(x=sqft_living, y=price))+
    geom_jitter(aes(col=as.factor(grade)))+
    geom_smooth() #If we divide this line by grade, grade under 5 does not show any difference in their prices

ggplot(data=kc_house[which(kc_house$grade>=6),],mapping=aes(x=sqft_above, y=price))+
    geom_jitter(aes(col=as.factor(grade)))+
    geom_smooth() #sqft_above and price show the linear relsationship.

ggplot(data=kc_house[which(kc_house$grade>=6),],mapping=aes(x=sqft_living, y=price))+
    geom_jitter(aes(col=as.factor(grade)))+
    geom_smooth() #sqft_above and price show the linear relsationship.

#yr_built

kc_house$yr_built_elapsed<-as.numeric(substr(kc_house$date,1,4))-kc_house$yr_built
kc_house$yr_renovated_bi<-ifelse(kc_house$yr_renovated==0,"0","1")
kc_house$yr_renovated_elapsed<-ifelse(kc_house$yr_renovated==0,0,as.numeric(substr(kc_house$date,1,4))-kc_house$yr_renovated)


ggplot(data=kc_house,mapping=aes(x=yr_built_elapsed, y=price))+
    geom_jitter()+
    geom_smooth()

ggplot(data=kc_house,mapping=aes(x=yr_renovated_elapsed, y=price))+
    geom_jitter()+
    geom_smooth()

ggplot(data=kc_house,aes(x=as.factor(yr_renovated_bi), y=price))+
    geom_boxplot() #renovation shows higher price a little.

ggplot(data=kc_house,aes(x=as.factor(yr_renovated_bi), y=condition))+
    geom_boxplot() #renovation shows lower condition (why?)



kc_house$bath_per_bed<-ifelse(kc_house$bedrooms==0,0,kc_house$bathrooms/kc_house$bedrooms)
summary(kc_house$bath_per_bed)
ggplot(data=kc_house,aes(x=bath_per_bed, y=price))+geom_jitter() #bathroom per bedrooms

kc_house$rooms<-kc_house$bathrooms+kc_house$bedrooms
kc_house$living_per_lot<-kc_house$sqft_living/kc_house$sqft_lot

test<-kc_house%>%filter(living_per_lot>1)

ggplot(data=kc_house,aes(x=rooms, y=price))+geom_jitter() #bathroom per bedrooms
ggplot(data=kc_house,aes(x=living_per_lot, y=price))+geom_jitter() #bathroom per bedrooms



#correlation new

colnames(kc_house)

num_col_3<-c("price","bedrooms","bathrooms","sqft_living","sqft_lot",
             "floors", "view","condition","grade","sqft_above",
             "sqft_basement","yr_built","yr_renovated","lat","long","sqft_living15","sqft_lot15",
             "lat_point","long_point","dist","price_per_lot","price_per_living","price_per_all",
             "sqft_basement_log", "yr_built_elapsed",
             "yr_renovated_elapsed","bath_per_bed","rooms","living_per_lot" )


DATA_cor<-kc_house[,num_col_3]; #corr test between x variable and y variable(price)      
data_cor_matrix<-cor(DATA_cor);                                                 
corr_3<-round(data_cor_matrix,2);  



#이 사유로 이진변수로 변환

ggplot(data=kc_house,aes(x=sqft_living,y=price,col=sqft_base_bi))+geom_jitter()

#zipcode

ggplot(data=kc_house[which(kc_house$zipcode == 98006),],mapping=aes(x=long,y=lat,shape=as.factor(zipcode)))+geom_point()


label<-as.data.frame(kc_house$zipcode);
df<-as.data.frame(cbind(x=kc_house$long,y=kc_house$lat,label));



df<-kc_house[which(kc_house$price_q=="4"),c("long","lat","zipcode")]
colnames(df)<-c("x","y","label")
p<-ggplot(df,aes_all(c("x","y","label")));
p+geom_text();


df$x<-as.character(df$x);
df$y<-as.character(df$y);
df$x<-round(as.numeric(df$x),0);
df$y<-round(as.numeric(df$y),0);

p<-ggplot(df,aes_all(c("x","y","label")));
p+geom_text();



#grade and map
ggplot(data=kc_house,mapping=aes(x=long,y=lat,color=as.factor(grade)))+geom_point()
ggplot(data=kc_house[which(kc_house$grade=="6"),],mapping=aes(x=long,y=lat,color=as.factor(grade)))+geom_point()
ggplot(data=kc_house[which(kc_house$grade=="7"),],mapping=aes(x=long,y=lat,color=as.factor(grade)))+geom_point()
ggplot(data=kc_house[which(kc_house$grade=="8"),],mapping=aes(x=long,y=lat,color=as.factor(grade)))+geom_point()
ggplot(data=kc_house[which(kc_house$grade=="9"),],mapping=aes(x=long,y=lat,color=as.factor(grade)))+geom_point()
ggplot(data=kc_house[which(kc_house$grade=="10"),],mapping=aes(x=long,y=lat,color=as.factor(grade)))+geom_point()
ggplot(data=kc_house[which(kc_house$grade=="11"),],mapping=aes(x=long,y=lat,color=as.factor(grade)))+geom_point()
ggplot(data=kc_house[which(kc_house$grade=="12"),],mapping=aes(x=long,y=lat,color=as.factor(grade)))+geom_point()
ggplot(data=kc_house[which(kc_house$grade=="13"),],mapping=aes(x=long,y=lat,color=as.factor(grade)))+geom_point()


# dist and price_q by location


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










#normalization

#skewness of all variables


skewness<-skewness(kc_house[,num_col_3], na.rm = FALSE, type = 3) # skewness is 4.28
skewness_col<-c("price","bedrooms","bathrooms","sqft_living","view","sqft_above","sqft_basement",
                "yr_renovated","sqft_living15","sqft_lot15","price_per_lot","price_per_living","price_per_all",
                "yr_renovated_elapsed","living_per_lot")

for (i in c(3:6,10,13,14,16,20,21,32,33,34,39,42)){

kc_house[,i]<-log(kc_house[,i]) }


colnames(kc_house)

num_col_3<-c("price","bedrooms","bathrooms","sqft_living","sqft_lot",
             "floors", "view","condition","grade","sqft_above",
             "sqft_basement","yr_built","yr_renovated","lat","long","sqft_living15","sqft_lot15",
             "lat_point","long_point","dist","price_per_lot","price_per_living","price_per_all",
             "sqft_basement_log", "yr_built_elapsed",
             "yr_renovated_elapsed","bath_per_bed","rooms","living_per_lot" )


for (i in c(3:8,10:14,20:21)) {
    
    kc_house[,i]<-scale(kc_house[,i],center=T,scale=T)
    
}


log_group<-

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








