#log transformation highskewness

kc_house$price_log<-log(kc_house$price)
kc_house$sqft_lot_log<-log(kc_house$sqft_lot)
kc_house$sqft_lot15_log<-log(kc_house$sqft_lot15)
kc_house$price_per_lot_log<-log(kc_house$price_per_lot)
kc_house$living_per_lot_log<-log(kc_house$living_per_lot)
kc_house$avg_price_log<-log(kc_house$avg_price)
kc_house$m_price_log<-log(kc_house$m_price)
kc_house$bench_p_log<-log(kc_house$bench_p)


skew<-c("price_log","sqft_lot_log","sqft_lot15_log","price_per_lot_log",
        "living_per_lot_log","avg_price_log","m_price_log","bench_p_log")

colnames(kc_house)

num_var_3_log<-c("price","price_log","bedrooms","bathrooms","sqft_living",
             "sqft_lot","sqft_lot_log","floors","view","sqft_above","sqft_basement",
             "lat","long","sqft_living15","sqft_lot15","sqft_lot15_log","age_sold",
             "age_sold_reno","dist","price_per_lot","price_per_lot_log","price_per_living", 
             "price_per_all","bath_per_bed","rooms","living_per_lot","living_per_lot_log",
             "avg_living","avg_price","avg_price_log","ration","m_living","m_price","m_price_log",
             "ration_2","bench_p","bench_p_log")

#normalization

kc_house[,num_var_3_log]<-scale(kc_house[,num_var_3_log],center=T,scale=T)




distribution_3 <- as.data.frame(t(sapply(kc_house[,num_var_3_log], quantile)))
distribution_3$Mean <- sapply(kc_house[,num_var_3_log], mean)
distribution_3$SD <- sapply(kc_house[,num_var_3_log], sd)
distribution_3$skenewss <- sapply(kc_house[,num_var_3_log], skewness)
distribution_3$kurtosis <- sapply(kc_house[,num_var_3_log], kurtosis)
distribution_3<-round(distribution_3, 2)


fac_var
fac_var_2<-c("waterfront","condition" ,"grade","location","zip_1","zip_2","yr_renovated_bi","sqft_base_bi")


str(kc_house)

x<-num_var_3_log[3:37]
y<-num_var_3_log[1:2]

for (i in seq_along(nm)) {
    plot<-ggplot(data=kc_house,aes_string(x = x[i],y= y[2])) + 
    geom_jitter(alpha = .5,fill = "dodgerblue")+
    geom_smooth(aes_string(x = x[i],y= y[2]),method=lm, se=FALSE, colour="dodgerblue3")
    assign(paste("plot_",i,sep=""),plot)
    
}


plot_34


#bedrooms

  
    ggplot(kc_house)+
    geom_jitter(aes(bedrooms, price_log),se=FALSE, colour = "firebrick2")+
    geom_smooth(aes(bedrooms, price_log),method=lm, se=FALSE, colour="dodgerblue3")+
    theme_classic()+
    theme(text = element_text(face = "bold"),
          plot.title=element_text(hjust=0.5))


model<-lm(price_log~log(bench_p),data=kc_house)
sq_1<-summary(model)$adj.r.squared

#bathrooms

ggplot(kc_house)+
    geom_jitter(aes(bedrooms, price_log),se=FALSE, colour = "firebrick2")+
    geom_smooth(aes(bedrooms, price_log),method=lm, se=FALSE, colour="dodgerblue3")+
    theme_classic()+
    theme(text = element_text(face = "bold"),
          plot.title=element_text(hjust=0.5))

model<-lm(price_log~bathrooms,data=kc_house)
sq_2<-summary(model_bath)$adj.r.squared


#sqft_living

ggplot(kc_house)+
    geom_jitter(aes(sqft_living, price_log),se=FALSE, colour = "firebrick2")+
    geom_smooth(aes(sqft_living, price_log),method=lm, se=FALSE, colour="dodgerblue3")+
    theme_classic()+
    theme(text = element_text(face = "bold"),
          plot.title=element_text(hjust=0.5))

sq_3<-summary(lm(price_log~sqft_living,data=kc_house))$adj.r.squared

#sqft_lot

ggplot(kc_house)+
    geom_jitter(aes(sqft_lot, price_log),se=FALSE, colour = "firebrick2")+
    geom_smooth(aes(sqft_lot, price_log),method=lm, se=FALSE, colour="dodgerblue3")+
    theme_classic()+
    theme(text = element_text(face = "bold"),
          plot.title=element_text(hjust=0.5))

sq_4<-summary(lm(price_log~sqft_lot,data=kc_house))$adj.r.squared

#sqft_lot_log

ggplot(kc_house)+
    geom_jitter(aes(sqft_lot_log, price_log),se=FALSE, colour = "firebrick2")+
    geom_smooth(aes(sqft_lot_log, price_log),method=lm, se=FALSE, colour="dodgerblue3")+
    theme_classic()+
    theme(text = element_text(face = "bold"),
          plot.title=element_text(hjust=0.5))

sq_5<-summary(lm(price_log~sqft_lot_log,data=kc_house))$adj.r.squared

#floors minus 값왜? 

ggplot(kc_house)+
    geom_jitter(aes(floors, price_log),se=FALSE, colour = "firebrick2")+
    geom_smooth(aes(floors, price_log),method=lm, se=FALSE, colour="dodgerblue3")+
    theme_classic()+
    theme(text = element_text(face = "bold"),
          plot.title=element_text(hjust=0.5))

sq_6<-summary(lm(price_log~floors,data=kc_house))$adj.r.squared

#view 

ggplot(kc_house)+
    geom_jitter(aes(view,price),se=FALSE, colour = "firebrick2")+
    geom_smooth(aes(view,price),method=lm, se=FALSE, colour="dodgerblue3")+
    theme_classic()+
    theme(text = element_text(face = "bold"),
          plot.title=element_text(hjust=0.5))

sq_7<-summary(lm(price_log~view,data=kc_house))$adj.r.squared




num_var_3_log







#randomforest
#base_model

str(kc_house)

colnames(kc_house_base)

kc_house_base$waterfront<-as.factor(kc_house_base$waterfront)
kc_house_base$condtion<-as.factor(kc_house_base$condition)
kc_house_base$grade<-as.factor(kc_house_base$grade)
kc_house_base$zipcode<-as.factor(kc_house_base$zipcode)


base_model<-randomForest(price~bedrooms+bathrooms+sqft_living+sqft_lot+
                             floors+waterfront+view+condition+grade+
                             sqft_above+sqft_basement+yr_built+
                             lat+long+sqft_living15+sqft_lot15, 
                         data=kc_house_base, importance=TRUE)

base_model
importance(base_model)
varImpPlot(base_model)

#train_model

colnames(kc_house)
str(kc_house)
kc_house$lcoation<-as.factor(kc_house$location)
kc_house$sqft_base_bi <-as.factor(kc_house$sqft_base_bi )

model<-randomForest(price~sqft_living+sqft_lot+floors+waterfront+
                        view+grade+lat+long+sqft_living15+location+
                        dist+sqft_base_bi+bath_per_bed+living_per_lot+
                        avg_living+avg_price+ration+m_living+m_price+ration_2,
                    data=kc_house,importance=TRUE)


importance(model)
varImpPlot(model)

model_2<-randomForest(price~sqft_living+sqft_lot+floors+waterfront+
                          view+grade+lat+long+sqft_living15+location+
                          dist+sqft_base_bi+bath_per_bed+living_per_lot+
                          avg_living+avg_price+ration+m_living+m_price+ration_2+
                          age_sold+age_sold_reno+yr_renovated_bi+bench_p,
                      data=kc_house,importance=TRUE)

model_2
importance(model_2)
varImpPlot(model_2)


tuneRF(kc_house[,c("sqft_living","sqft_lot","floors","waterfront",
                   "view","grade","lat","long","sqft_living15","location",
                   "dist","sqft_base_bi","bath_per_bed","living_per_lot",
                   "avg_living","avg_price","ration","m_living","m_price","ration_2",
                   "age_sold","age_sold_reno","yr_renovated_bi")],kc_house$price,mtryStart=2)






model_2<-randomForest(price~sqft_living+sqft_lot+floors+waterfront+
                          view+grade+lat+long+sqft_living15+location+
                          dist+sqft_base_bi+bath_per_bed+living_per_lot+
                          avg_living+avg_price+ration+m_living+m_price+ration_2+
                          age_sold+age_sold_reno+yr_renovated_bi,
                      data=kc_house,ntree=600,mtry=16,importance=TRUE)


#preprocessing for other models


kc_house<-
    
    
    
    
    
    
    
    
    full_m<-lm(log(price)~log(sqft_living)+log(sqft_lot)+floors+waterfront+view+grade+lat+log(sqft_living15)+location+long+log(avg_price)+dist+bench_p, data=kc_house)

null_m<-lm(price~1.,data=kc_house)

forw_m<-step(null_m,direction="both",trace=1,scope=list(lower=null_m,upper=full_m))

forw_m_2<-step(full_m,direction="backward",trace=1,scope=list(lower=null_m,upper=full_m))


summary(forw_m_2)

install.packages("VIF")
library(VIF)
vif(forw_m)


xyplot(kc_house$price~predict(forw_m))
xyplot(resid(forw_m)~predict(forw_m))



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