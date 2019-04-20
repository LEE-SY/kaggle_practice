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

#kc_house_base<-read.csv("kc_house_data.csv", na.strings = "NA",stringsAsFactors = FALSE) #data loading
#kc_house<-read.csv("kc_house_data.csv", na.strings = "NA",stringsAsFactors = FALSE) #data loading


kc_house<-read.csv("test.csv", na.strings = "NA",stringsAsFactors = FALSE) #data loading


#missing data

apply(kc_house,MARGIN=2,FUN=any_na)
kc_house[!complete.cases(kc_house),]



##overal EDA



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





kc_house$trade_date<-substr(kc_house$date,1,6)





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



#district 





#price_per_space (new feature engineering)

summary(ifelse(kc_house$sqft_living==kc_house$sqft_above+kc_house$sqft_basement,1,0))
kc_house$price_per_lot<-kc_house$price/kc_house$sqft_lot
kc_house$price_per_living<-kc_house$price/kc_house$sqft_living
kc_house$price_per_all<-kc_house$price/(kc_house$sqft_lot+kc_house$sqft_living)
summary(kc_house$price_per_lot)

#base_binary_variable

kc_house$sqft_base_bi<-NA
kc_house$sqft_base_bi<-ifelse(kc_house$sqft_basement==0,"0","1")




#rooms

kc_house$bath_per_bed<-ifelse(kc_house$bedrooms==0,0,kc_house$bathrooms/kc_house$bedrooms)
kc_house$rooms<-kc_house$bathrooms+kc_house$bedrooms
kc_house$living_per_lot<-kc_house$sqft_living/kc_house$sqft_lot






#summary for mean and median price per lot



kc_house$f_key<-paste(kc_house$grade,kc_house$location,sep="")

kc_house<-left_join(kc_house,summary_1[,c(3:6)],by="f_key")

summary_2$f_key<-paste(summary_2$grade,summary_2$location,sep="")

kc_house<-left_join(kc_house,summary_2[,c(3:6)],by="f_key")

kc_house$bench_p<-kc_house$avg_price*kc_house$sqft_living






#zipcode

str(kc_house)
kc_house$zip_1<-substr(kc_house$zipcode,1,3)
kc_house$zip_2<-substr(kc_house$zipcode,4,6)





#log transformation highskewness

kc_house$price_log<-log(kc_house$price)
kc_house$sqft_lot_log<-log(kc_house$sqft_lot)
kc_house$sqft_lot15_log<-log(kc_house$sqft_lot15)
kc_house$price_per_lot_log<-log(kc_house$price_per_lot)
kc_house$living_per_lot_log<-log(kc_house$living_per_lot)
kc_house$avg_price_log<-log(kc_house$avg_price)
kc_house$m_price_log<-log(kc_house$m_price)
kc_house$bench_p_log<-log(kc_house$bench_p)


#normalization

kc_house[,num_var_3_log]<-scale(kc_house[,num_var_3_log],center=T,scale=T)




str(kc_house)

x<-num_var_3_log[3:37]
y<-num_var_3_log[1:2]



colnames(kc_house)

#for (i in seq_along(x)) {
#  plot<-ggplot(data=kc_house,aes_string(x = x[i],y= y[2])) + 
#    geom_jitter(alpha = .5,fill = "dodgerblue")+
#    geom_smooth(aes_string(x = x[i],y= y[2]),method=lm, se=FALSE, colour="dodgerblue3")+
#    stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE) +

#    assign(paste("plot_",i,sep=""),plot)

#}

model_names<-c("bedrooms","bathrooms","sqft_living","sqft_lot_log","floors",
               "waterfront","view","condition","grade","sqft_above","sqft_basement",
               "lat","long","sqft_living15","sqft_lot15_log","age_sold","yr_renovated_bi",
               "age_sold_reno","yr_built_reno","location","dist","price_per_lot_log","price_per_living",
               "price_per_all","sqft_base_bi","bath_per_bed","rooms","living_per_lot",
               "avg_living","avg_price_log","ration","m_living","m_price_log",
               "ration_2","bench_p","zip_1","zip_2","price_log","bench_p_log")


model_names_2<-c("bedrooms","bathrooms","sqft_living","sqft_lot_log","floors",
                 "waterfront","view","condition","grade",
                 "lat","sqft_living15","sqft_lot15_log","age_sold","yr_renovated_bi",
                 "age_sold_reno","yr_built_reno","location","dist","price_per_lot_log","price_per_living",
                 "price_per_all","sqft_base_bi","bath_per_bed","living_per_lot",
                 "avg_living","avg_price_log","ration","m_living","m_price_log",
                 "ration_2","bench_p","zip_1","price_log","bench_p_log")




kc_house_test<-kc_house[,model_names_2]

m<-lm(price_log~.,data=kc_house_test)
summary(m)

library(car)
vif(m)

model_names<-colnames(kc_house)[3:61]

kc_house_test<-kc_house[,model_names]



vif<-vif(m)
vif[which(vif[,3]>10),]


model_names_3<-c("bedrooms","bathrooms","sqft_living","floors",
                 "waterfront","view","condition","grade",
                 "lat","sqft_living15","sqft_lot15_log","age_sold","yr_renovated_bi",
                 "location","dist","price_per_living",
                 "price_per_all","sqft_base_bi","bath_per_bed","living_per_lot",
                 "avg_price_log","m_price_log",
                 "zip_1","price_log","bench_p_log")




kc_house_test<-kc_house[,model_names_3]

m<-lm(price_log~.,data=kc_house_test)
summary(m)
vif<-vif(m)
vif[which(vif[,3]>10),]




full_m<-lm(price_log~., data=kc_house_test)

null_m<-lm(price_log~1.,data=kc_house_test)

forw_m<-step(null_m,direction="both",trace=1,scope=list(lower=null_m,upper=full_m))
summary(forw_m)



forw_m_2<-step(full_m,direction="backward",trace=1,scope=list(lower=null_m,upper=full_m))
summary(forw_m_2)



forw_m_3<-step(full_m,direction="forward",trace=1,scope=list(lower=null_m,upper=full_m))
summary(forw_m_3)


install.packages("glmnet")
library(glmnet)

cvfit<-cv.glmnet(x=data.matrix(kc_house_test[,c(1:23,25)]),y=kc_house_test$price_log,alpha=0)
plot(cvfit,xvar="lambda",label=TRUE)


opt_lambda <- cvfit$lambda.min
opt_lambda


fit <- cvfit$glmnet.fit



r2 <- fit$glmnet.fit$dev.ratio[which(fitnet$glmnet.fit$lambda == fitnet$lambda.1se)] 

y_predicted <- predict(fit, s = opt_lambda, newx = data.matrix(kc_house_test[,c(1:23,25)]))

# Sum of Squares Total and Error
sst <- sum((kc_house_test$price_log - mean(kc_house_test$price_log))^2)
sse <- sum((y_predicted - kc_house_test$price_log)^2)

# R squared
rsq <- 1 - sse / sst
rsq
#> [1] 0.9318896


plot(cvif,xvar="lambda",label=TRUE)
