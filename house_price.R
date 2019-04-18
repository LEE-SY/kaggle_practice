

pckg<-c("dplyr","readxl","haven","ggplot2","pdftools", "tidyverse",
        "ggplot2", "ggthemes", "ggrepel", "tm", "grid","PerformanceAnalytics",
        "e1071","doBy","fmsb","corrplot","VIM","DMwR","rpart",
        "rattle","rpart.plot","RColorBrewer","ipred",
        "randomForest","adabag","lme4","caret","class","ROCR","Imap",
        "caret","randomForest","naniar","gridExtra","GGally",
        "leaflet","parsedate","kernlab","moonBook","VIF")
#sapply(pckg,install.packages,character.only=TRUE) #library loading




sapply(pckg, function(x) require(x,quietly=TRUE,character.only=TRUE,warn.conflicts = FALSE )) #library loading

memory.limit();
memory.limit(100000000000);

kc_house_base<-read.csv("kc_house_data.csv", na.strings = "NA",stringsAsFactors = FALSE) #data loading
kc_house<-read.csv("kc_house_data.csv", na.strings = "NA",stringsAsFactors = FALSE) #data loading


#missing data

apply(kc_house,MARGIN=2,FUN=any_na)
kc_house[!complete.cases(kc_house),]

#outlier 

for (i in (3:ncol(kc_house))) {
    
    lower<-kc_house[which(kc_house[,i]<quantile(kc_house[,i],c(0.02))),]
    assign(paste(colnames(kc_house)[i],"_out_low",sep=""),lower)
    
    up<-kc_house[which(kc_house[,i]>quantile(kc_house[,i],c(0.99))),]
    assign(paste(colnames(kc_house)[i],"_out_up",sep=""),up)
}



#test/training split

smp_size <- floor(0.75 * nrow(kc_house)) # 75% of the sample size

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(kc_house)), size = smp_size)

kc_house_base<- kc_house_base[train_ind, ]
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



kc_house$date<-substr(kc_house$date,1,8)
kc_house$date_YMD<-as.Date(kc_house$date,format="%Y%m%d")

#numerical value
chart.Correlation(kc_house[,num_var], pch = 19, method = "pearson")

#distribution
distribution <- as.data.frame(t(sapply(kc_house[,num_var], quantile)))
distribution$Mean <- sapply(kc_house[,num_var], mean)
distribution$SD <- sapply(kc_house[,num_var], sd)
distribution$skenewss <- sapply(kc_house[,num_var], skewness)
distribution$kurtosis <- sapply(kc_house[,num_var], kurtosis)
distribution<-round(distribution, 2)






###feature engineering

##date, year built, renovated year (date)



#date

kc_house$trade_date<-substr(kc_house$date,1,6)
ggplot(data=kc_house,aes(x=trade_date, y=price))+geom_boxplot() 

# supply and demand

kc_house%>%group_by(trade_date)%>%summarize(count=n())




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

#location visualization 1

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


#location visualization 2




loc_1<-ggplot(data=kc_house,mapping=aes(x=long,y=lat,color=price_q))+
    geom_point()+
    ggtitle("price qunatile by location")+
    theme_classic()+
    theme(text = element_text(face ="bold"),
          plot.title = element_text(hjust = 0.5))


loc_1

loc_2<-ggplot(data=kc_house,mapping=aes(x=long,y=lat))+
    geom_point()+
    stat_density2d()+
    ggtitle("price qunatile by location with gradient")+
    theme_classic()+
    theme(text = element_text(face ="bold"),
          plot.title = element_text(hjust = 0.5))

loc_2

loc_q1<-ggplot(data=kc_house[which(kc_house$price_q=="1"),],mapping=aes(x=long,y=lat))+
    geom_point()+
    stat_density2d()+
    ggtitle("price high 1 quantile by location with gradient")+
    theme_classic()+
    theme(text = element_text(face ="bold"),
          plot.title = element_text(hjust = 0.5))

loc_q2<-ggplot(data=kc_house[which(kc_house$price_q=="2"),],mapping=aes(x=long,y=lat))+
    geom_point()+
    stat_density2d()+
    ggtitle("price high 2 quantile by location with gradient")+
    theme_classic()+
    theme(text = element_text(face ="bold"),
          plot.title = element_text(hjust = 0.5))

loc_q3<-ggplot(data=kc_house[which(kc_house$price_q=="3"),],mapping=aes(x=long,y=lat))+
    geom_point()+
    stat_density2d()+
    ggtitle("price high 3 quantile by location with gradient")+
    theme_classic()+
    theme(text = element_text(face ="bold"),
          plot.title = element_text(hjust = 0.5))

loc_q4<-ggplot(data=kc_house[which(kc_house$price_q=="4"),],mapping=aes(x=long,y=lat))+
    geom_point()+
    stat_density2d()+
    ggtitle("price high 4 quantile by location with gradient")+
    theme_classic()+
    theme(text = element_text(face ="bold"),
          plot.title = element_text(hjust = 0.5))


loc_total<- grid.arrange(loc_1,loc_2,loc_q1,loc_q2,loc_q3,loc_q4)
loc_total



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
qunatile_lot

kc_house[which(kc_house$sqft_lot>=qunatile_lot[20]),c("sqft_lot_q")]<-c("1") #95%
kc_house[which(kc_house$sqft_lot<qunatile_lot[20] & kc_house$sqft_lot>=qunatile_lot[19]),c("sqft_lot_q")]<-c("2") #90%
kc_house[which(kc_house$sqft_lot<qunatile_lot[19] & kc_house$sqft_lot>=qunatile_lot[18]),c("sqft_lot_q")]<-c("3") #85%
kc_house[which(kc_house$sqft_lot<qunatile_lot[18] & kc_house$sqft_lot>=qunatile_lot[17]),c("sqft_lot_q")]<-c("4") #80%
kc_house[which(kc_house$sqft_lot<qunatile_lot[17] & kc_house$sqft_lot>=qunatile_lot[16]),c("sqft_lot_q")]<-c("5") #75%
kc_house[which(kc_house$sqft_lot<qunatile_lot[16] & kc_house$sqft_lot>=qunatile_lot[11]),c("sqft_lot_q")]<-c("6") #50~75%
kc_house[which(kc_house$sqft_lot<qunatile_lot[11] & kc_house$sqft_lot>=qunatile_lot[6]),c("sqft_lot_q")]<-c("7")  #25~50%
kc_house[which(kc_house$sqft_lot<qunatile_lot[6]),c("sqft_lot_q")]<-c("8") #0~50%


summary(kc_house[which(kc_house$sqft_lot_q=="1"),][7])



lot_1<-ggplot(kc_house,aes(sqft_lot))+
    geom_histogram(binwidth=1000)+
    ggtitle("lot distribution by 95%")+
    xlim(c(0,qunatile_lot[20]))+
    theme_classic()+
    theme(text=element_text(face="bold"),
          plot.title=element_text(hjust=0.5))

lot_2<-ggplot(kc_house,aes(sqft_lot))+
    geom_histogram(binwidth=10000)+
    ggtitle("lot distribution over 95%")+
    xlim(c(qunatile_lot[20],qunatile_lot[21]))+
    theme_classic()+
    theme(text=element_text(face="bold"),
          plot.title=element_text(hjust=0.5))


lot_total<- grid.arrange(lot_1,lot_2)


#district 

district<-as.matrix(kc_house%>%group_by(location)%>%summarize(count=n())%>%arrange(-count)%>%filter(count>500)%>%select(location))



district_total<-ggplot(kc_house, aes(sqft_lot, price, col=as.factor(price_q)))+
    geom_jitter()+
    xlab("lot of total")+  
    ylab("price of houses")+
    theme_classic()+
    geom_smooth(se=TRUE)

district_total

for (i in (1:11)) {
    
    a<-ggplot(kc_house%>%filter(location==district[i]), aes(sqft_lot, price, col=as.factor(price_q)))+
        geom_jitter()+
        xlab(paste("lot of district ",district[i],sep=""))+  
        ylab("price of houses")+
        theme_classic()+
        geom_smooth(se=TRUE)
    
    
    
    assign(paste("district_",i,sep=""),a)
}

grid.arrange(district_total,district_1,district_2)
grid.arrange(district_3,district_4,district_5)
grid.arrange(district_6,district_7,district_8)
grid.arrange(district_9,district_10,district_11)




#price_per_space (new feature engineering)

summary(ifelse(kc_house$sqft_living==kc_house$sqft_above+kc_house$sqft_basement,1,0))

kc_house$price_per_lot<-kc_house$price/kc_house$sqft_lot
kc_house$price_per_living<-kc_house$price/kc_house$sqft_living
kc_house$price_per_all<-kc_house$price/(kc_house$sqft_lot+kc_house$sqft_living)
summary(kc_house$price_per_lot)

#base_binary_variable

kc_house$sqft_base_bi<-NA
kc_house$sqft_base_bi<-ifelse(kc_house$sqft_basement==0,"0","1")



base_1<-ggplot(kc_house, aes(age_sold_group, price)) +
    geom_boxplot(aes(fill = sqft_base_bi)) +
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

base_2<-ggplot(kc_house, aes(price_q, price)) +
    geom_boxplot(aes(fill = sqft_base_bi)) +
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

base_1
base_2



#rooms

kc_house$bath_per_bed<-ifelse(kc_house$bedrooms==0,0,kc_house$bathrooms/kc_house$bedrooms)
kc_house$rooms<-kc_house$bathrooms+kc_house$bedrooms
kc_house$living_per_lot<-kc_house$sqft_living/kc_house$sqft_lot






#summary for mean and median price per lot

summary_1<-kc_house%>%group_by(location,grade)%>%summarize(avg_living=mean(sqft_living),avg_price=mean(price))
summary_1$ration<-summary_1$avg_price/summary_1$avg_living

summary_2<-kc_house%>%group_by(location,grade)%>%summarize(m_living=median(sqft_living),m_price=median(price))
summary_2$ration_2<-summary_2$m_price/summary_2$m_living


kc_house$f_key<-paste(kc_house$grade,kc_house$location,sep="")
summary_1$f_key<-paste(summary_1$grade,summary_1$location,sep="")

kc_house<-left_join(kc_house,summary_1[,c(3:6)],by="f_key")

summary_2$f_key<-paste(summary_2$grade,summary_2$location,sep="")

kc_house<-left_join(kc_house,summary_2[,c(3:6)],by="f_key")

kc_house$bench_p<-kc_house$avg_price*kc_house$sqft_living



average_price<-ggplot(data=summary_1,aes(x=location, y=ration))+
    geom_bar(stat="identity")+
    ggtitle("average price per lot")+
    theme_classic()+
    theme(text=element_text(face="bold"),
          plot.title=element_text(hjust=0.5))

median_price<-ggplot(data=summary_2,aes(x=location, y=ration_2))+
    geom_bar(stat="identity")+
    ggtitle("median price per lot")+
    theme_classic()+
    theme(text=element_text(face="bold"),
          plot.title=element_text(hjust=0.5))


price_per_lot<- grid.arrange(average_price,median_price)



#zipcode

str(kc_house)
kc_house$zip_1<-substr(kc_house$zipcode,1,3)
kc_house$zip_2<-substr(kc_house$zipcode,4,6)

zip_1<-ggplot(data=kc_house,mapping=aes(x=long,y=lat,color=zip_1))+
    geom_point()+
    ggtitle("house location by zipcode")+
    theme_classic()+
    theme(text = element_text(face ="bold"),
          plot.title = element_text(hjust = 0.5))


zip_1



zip_2<-ggplot(data=kc_house%>%filter(zip_2=="06"),mapping=aes(x=long,y=lat,color=price_q))+
    geom_point()+
    ggtitle("house location by zipcode 06")+
    theme_classic()+
    theme(text = element_text(face ="bold"),
          plot.title = element_text(hjust = 0.5))


zip_2



zip_3<-ggplot(data=kc_house%>%filter(zip_2=="99"),mapping=aes(x=long,y=lat,color=price_q))+
    geom_point()+
    ggtitle("house location by zipcode 99")+
    theme_classic()+
    theme(text = element_text(face ="bold"),
          plot.title = element_text(hjust = 0.5))


zip_3


zip_code<- grid.arrange(zip_1,zip_2,zip_3)
zip_code





# zerovariacne check

nearZeroVar(kc_house,saveMetrics=TRUE)


colnames(kc_house)

num_var_2<-c("price","bedrooms","bathrooms","sqft_living",
             "sqft_lot","floors","view","sqft_above","sqft_basement",
             "lat","long","sqft_living15","sqft_lot15","age_sold",
             "age_sold_reno","dist","price_per_lot","price_per_living", 
             "price_per_all","bath_per_bed","rooms","living_per_lot",
             "avg_living","avg_price","ration","m_living","m_price",
             "ration_2","bench_p")



distribution_2 <- as.data.frame(t(sapply(kc_house[,num_var_2], quantile)))
distribution_2$Mean <- sapply(kc_house[,num_var_2], mean)
distribution_2$SD <- sapply(kc_house[,num_var_2], sd)
distribution_2$skenewss <- sapply(kc_house[,num_var_2], skewness)
distribution_2$kurtosis <- sapply(kc_house[,num_var_2], kurtosis)
distribution_2<-round(distribution_2, 2)


distribution_2[which(distribution_2$skenewss>0),]

write.csv(kc_house,"kc_house.csv",row.names = FALSE)
write.csv(kc_house_base,"kc_house.csv",row.names = FALSE)


# new correlation

colnames(kc_house)

num_var

chart.Correlation(kc_house[,num_var], pch = 19, method = "pearson")



