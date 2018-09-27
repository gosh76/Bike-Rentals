# reading data

df = read.csv('F:/EdwisorProject1/day.csv')

str(df)

# 'data.frame':	731 obs. of  16 variables:
#   $ instant   : int  1 2 3 4 5 6 7 8 9 10 ...
# $ dteday    : Factor w/ 731 levels "2011-01-01","2011-01-02",..: 1 2 3 4 5 6 7 8 9 10 ...
# $ season    : int  1 1 1 1 1 1 1 1 1 1 ...
# $ yr        : int  0 0 0 0 0 0 0 0 0 0 ...
# $ mnth      : int  1 1 1 1 1 1 1 1 1 1 ...
# $ holiday   : int  0 0 0 0 0 0 0 0 0 0 ...
# $ weekday   : int  6 0 1 2 3 4 5 6 0 1 ...
# $ workingday: int  0 0 1 1 1 1 1 0 0 1 ...
# $ weathersit: int  2 2 1 1 1 1 2 2 1 1 ...
# $ temp      : num  0.344 0.363 0.196 0.2 0.227 ...
# $ atemp     : num  0.364 0.354 0.189 0.212 0.229 ...
# $ hum       : num  0.806 0.696 0.437 0.59 0.437 ...
# $ windspeed : num  0.16 0.249 0.248 0.16 0.187 ...
# $ casual    : int  331 131 120 108 82 88 148 68 54 41 ...
# $ registered: int  654 670 1229 1454 1518 1518 1362 891 768 1280 ...
# $ cnt       : int  985 801 1349 1562 1600 1606 1510 959 822 1321 ...


# converting variable dteday from factor to date

df$dteday = as.Date(df$dteday,format="%Y-%m-%d")

str(df)

# 'data.frame':	731 obs. of  16 variables:
#   $ instant   : int  1 2 3 4 5 6 7 8 9 10 ...
# $ dteday    : Date, format: "2011-01-01" "2011-01-02" "2011-01-03" ...
# $ season    : int  1 1 1 1 1 1 1 1 1 1 ...
# $ yr        : int  0 0 0 0 0 0 0 0 0 0 ...
# $ mnth      : int  1 1 1 1 1 1 1 1 1 1 ...
# $ holiday   : int  0 0 0 0 0 0 0 0 0 0 ...
# $ weekday   : int  6 0 1 2 3 4 5 6 0 1 ...
# $ workingday: int  0 0 1 1 1 1 1 0 0 1 ...
# $ weathersit: int  2 2 1 1 1 1 2 2 1 1 ...
# $ temp      : num  0.344 0.363 0.196 0.2 0.227 ...
# $ atemp     : num  0.364 0.354 0.189 0.212 0.229 ...
# $ hum       : num  0.806 0.696 0.437 0.59 0.437 ...
# $ windspeed : num  0.16 0.249 0.248 0.16 0.187 ...
# $ casual    : int  331 131 120 108 82 88 148 68 54 41 ...
# $ registered: int  654 670 1229 1454 1518 1518 1362 891 768 1280 ...
# $ cnt       : int  985 801 1349 1562 1600 1606 1510 959 822 1321 ...


# extracting day from dates

df$day = format(df$dteday,"%d")

df$day

#checking if there is any relationship between day and cnt

plot(df$day,df$cnt)

#there is no relationship between day & cnt

#Dropping variables - instant,dteday(since mnth & yr are present),day,casual,registered(since cnt will be used as dependent variable)

df[,c('instant','dteday','day','casual','registered')] = list(NULL)

str(df)

# 'data.frame':	731 obs. of  12 variables:
#   $ season    : int  1 1 1 1 1 1 1 1 1 1 ...
# $ yr        : int  0 0 0 0 0 0 0 0 0 0 ...
# $ mnth      : int  1 1 1 1 1 1 1 1 1 1 ...
# $ holiday   : int  0 0 0 0 0 0 0 0 0 0 ...
# $ weekday   : int  6 0 1 2 3 4 5 6 0 1 ...
# $ workingday: int  0 0 1 1 1 1 1 0 0 1 ...
# $ weathersit: int  2 2 1 1 1 1 2 2 1 1 ...
# $ temp      : num  0.344 0.363 0.196 0.2 0.227 ...
# $ atemp     : num  0.364 0.354 0.189 0.212 0.229 ...
# $ hum       : num  0.806 0.696 0.437 0.59 0.437 ...
# $ windspeed : num  0.16 0.249 0.248 0.16 0.187 ...
# $ cnt       : int  985 801 1349 1562 1600 1606 1510 959 822 1321 ...

head(df)

# season yr mnth holiday weekday workingday weathersit     temp    atemp      hum windspeed  cnt
# 1      1  0    1       0       6          0          2 0.344167 0.363625 0.805833 0.1604460  985
# 2      1  0    1       0       0          0          2 0.363478 0.353739 0.696087 0.2485390  801
# 3      1  0    1       0       1          1          1 0.196364 0.189405 0.437273 0.2483090 1349
# 4      1  0    1       0       2          1          1 0.200000 0.212122 0.590435 0.1602960 1562
# 5      1  0    1       0       3          1          1 0.226957 0.229270 0.436957 0.1869000 1600
# 6      1  0    1       0       4          1          1 0.204348 0.233209 0.518261 0.0895652 1606


# This is a regression problem since dependent variable cnt is continuous.

# Data Distribution Visualizations

#Variables - season,yr,mnth,holiday,weekday,workingday,weathersit - are categorical so no distribution visualization

library(ggplot2)

# Visualizing variable temp distribution

ggplot(df,aes_string(x=df$temp))+geom_histogram()+xlab('temp')+ylab('freq')

# Visualizing variable atemp distribution

ggplot(df,aes_string(x=df$atemp))+geom_histogram()+xlab('atemp')+ylab('freq')

# Visualizing variable hum distribution

ggplot(df,aes_string(x=df$hum))+geom_histogram()+xlab('hum')+ylab('freq')

# Visualizing variable windspeed distribution

ggplot(df,aes_string(x=df$windspeed))+geom_histogram()+xlab('windspeed')+ylab('freq')

# Visualizing variable cnt distribution

ggplot(df,aes_string(x=df$cnt))+geom_histogram()+xlab('cnt')+ylab('freq')

# Variables hum & cnt are close to normal. All other continuous variables are skewed.

# Outliers Analysis

# Boxplots for temp,atemp,hum & windspeed

boxplot(df[,c('temp','atemp','hum','windspeed')])

# hum & windspeed have outliers

# Boxplot for cnt

boxplot(df[,'cnt'],xlab='cnt')

# cnt has no outliers

# Outliers will be replaced with NAs

for (i in c('hum','windspeed')){
  val = df[,i][df[,i] %in% boxplot.stats(df[,i])$out]
  df[,i][df[,i] %in% val] = NA
} 

# checking for missing values

as.data.frame(colSums(is.na(df)))

# season                      0
# yr                          0
# mnth                        0
# holiday                     0
# weekday                     0
# workingday                  0
# weathersit                  0
# temp                        0
# atemp                       0
# hum                         2
# windspeed                  13
# cnt                         0

# Missing values analysis

# We will use mean substitution,median substitution and KNN imputation for missing values and determine which method is the best method for missing values replacement.

# Taking first row of df and value of hum as our test value.We will put this value of 0.8058 to NA and use the 3 methods to check which method gives the replacement closest to the original value.

# Checking mean substitution

# making a copy of df for mean substitution

df1 = df

df1[1,'hum'] = NA

head(df1)

# season yr mnth holiday weekday workingday weathersit     temp    atemp      hum windspeed  cnt
# 1      1  0    1       0       6          0          2 0.344167 0.363625       NA 0.1604460  985
# 2      1  0    1       0       0          0          2 0.363478 0.353739 0.696087 0.2485390  801
# 3      1  0    1       0       1          1          1 0.196364 0.189405 0.437273 0.2483090 1349
# 4      1  0    1       0       2          1          1 0.200000 0.212122 0.590435 0.1602960 1562
# 5      1  0    1       0       3          1          1 0.226957 0.229270 0.436957 0.1869000 1600
# 6      1  0    1       0       4          1          1 0.204348 0.233209 0.518261 0.0895652 1606

df1$hum[is.na(df1$hum)] = mean(df1$hum,na.rm = T)

df1$hum[1]# 0.6291

#So,mean substitution has given a value of 0.6291 and original value was 0.8058

# Checking median substitution

df2 = df

df2[1,'hum'] = NA

df2$hum[is.na(df2$hum)] = median(df2$hum,na.rm=T)

df2$hum[1]#0.6270

#So,median substitution has given a value of 0.6270 and original value was 0.8058

# Checking KNN imputation

library(DMwR)

df3 = df

df3$hum[1] = NA

df3 = knnImputation(df3,k=3)

df3$hum[1]#0.7459

# KNN imputation has given the value of 0.7459 which is closest to 0.8058.So,KNN imputation will be used to replace missing values.

# Using KNN imputation for replacing missing values in original dataframe df

df = knnImputation(df,k=3)

#Checking missing values after imputation

as.data.frame(colSums(is.na(df)))

# season                      0
# yr                          0
# mnth                        0
# holiday                     0
# weekday                     0
# workingday                  0
# weathersit                  0
# temp                        0
# atemp                       0
# hum                         0
# windspeed                   0
# cnt                         0

# Correlation Analysis

# Converting season,yr,mnth,holiday,weekday,workingday,weathersit to factor

catcols = c('season','yr','mnth','holiday','weekday','workingday','weathersit')

for(i in catcols){
  df[,i] = as.factor(df[,i])
}

str(df)

# 'data.frame':	731 obs. of  12 variables:
#   $ season    : Factor w/ 4 levels "1","2","3","4": 1 1 1 1 1 1 1 1 1 1 ...
# $ yr        : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
# $ mnth      : Factor w/ 12 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ holiday   : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
# $ weekday   : Factor w/ 7 levels "0","1","2","3",..: 7 1 2 3 4 5 6 7 1 2 ...
# $ workingday: Factor w/ 2 levels "0","1": 1 1 2 2 2 2 2 1 1 2 ...
# $ weathersit: Factor w/ 3 levels "1","2","3": 2 2 1 1 1 1 2 2 1 1 ...
# $ temp      : num  0.344 0.363 0.196 0.2 0.227 ...
# $ atemp     : num  0.364 0.354 0.189 0.212 0.229 ...
# $ hum       : num  0.806 0.696 0.437 0.59 0.437 ...
# $ windspeed : num  0.16 0.249 0.248 0.16 0.187 ...
# $ cnt       : int  985 801 1349 1562 1600 1606 1510 959 822 1321 ...

# Chi-square test for correlation between factors

pval = c()

#Calculating & storing p-values in vector pval from chisquare test

for(i in catcols){
  for(j in catcols){
    chi2 = chisq.test(df[,i],df[,j])
    pval = c(pval,chi2$p.value)
  }
}

length(pval)#49

#converting pval to matrix m1

m1 = matrix(pval,ncol=7)
m1

#Converting m1 to dataframe chi_df

chi_df = data.frame(m1)

#Setting row names to catcols

row.names(chi_df) = catcols

#Setting column names to catcols

colnames(chi_df) = catcols

chi_df

#              season            yr       mnth       holiday       weekday    workingday    weathersit
# season     0.0000000  9.999288e-01 0.00000000  6.831687e-01  1.000000e+00  8.865568e-01  2.117930e-02
# yr         0.9999288 4.011854e-160 1.00000000  1.000000e+00  9.999996e-01  1.000000e+00  1.273794e-01
# mnth       0.0000000  1.000000e+00 0.00000000  5.593083e-01  1.000000e+00  9.933495e-01  1.463711e-02
# holiday    0.6831687  1.000000e+00 0.55930831 2.706945e-153  8.567055e-11  4.033371e-11  6.008572e-01
# weekday    1.0000000  9.999996e-01 1.00000000  8.567055e-11  0.000000e+00 6.775031e-136  2.784593e-01
# workingday 0.8865568  1.000000e+00 0.99334952  4.033371e-11 6.775031e-136 5.484935e-160  2.537640e-01
# weathersit 0.0211793  1.273794e-01 0.01463711  6.008572e-01  2.784593e-01  2.537640e-01 2.484533e-315

#workingday vs weekday and holiday vs weekday have p-value < 0.01 so we can drop workingday and holiday.

#Dropping workingday & holiday from df

df[,c('holiday','workingday')] = list(NULL)

head(df)

# season yr mnth weekday weathersit     temp    atemp      hum windspeed  cnt
# 1      1  0    1       6          2 0.344167 0.363625 0.805833 0.1604460  985
# 2      1  0    1       0          2 0.363478 0.353739 0.696087 0.2485390  801
# 3      1  0    1       1          1 0.196364 0.189405 0.437273 0.2483090 1349
# 4      1  0    1       2          1 0.200000 0.212122 0.590435 0.1602960 1562
# 5      1  0    1       3          1 0.226957 0.229270 0.436957 0.1869000 1600
# 6      1  0    1       4          1 0.204348 0.233209 0.518261 0.0895652 1606

#Correlation between continuous independent variables

cor(df[,6:9])

#                 temp      atemp        hum  windspeed
# temp       1.0000000  0.9917016  0.1228026 -0.1442859
# atemp      0.9917016  1.0000000  0.1364223 -0.1700440
# hum        0.1228026  0.1364223  1.0000000 -0.2025662
# windspeed -0.1442859 -0.1700440 -0.2025662  1.0000000

#temp and atemp have correlation coeff. of 0.99 so we will drop atemp

df[,'atemp'] = NULL

head(df)

# season yr mnth weekday weathersit     temp      hum windspeed  cnt
# 1      1  0    1       6          2 0.344167 0.805833 0.1604460  985
# 2      1  0    1       0          2 0.363478 0.696087 0.2485390  801
# 3      1  0    1       1          1 0.196364 0.437273 0.2483090 1349
# 4      1  0    1       2          1 0.200000 0.590435 0.1602960 1562
# 5      1  0    1       3          1 0.226957 0.436957 0.1869000 1600
# 6      1  0    1       4          1 0.204348 0.518261 0.0895652 1606

# Bikes Count Vs. Season

colList = scales::hue_pal()(4)

ggplot(df, aes_string(x=df$season,y=df$cnt,fill=df$season))+
geom_boxplot()+
xlab('Season')+ylab('cnt')+
ggtitle("Bikes Count By Season")+
scale_fill_manual('Seasons',labels=c('spring', 'summer', 'fall', 'winter'),values = colList)

#Most bike rentals occur in Fall followed by Summer.

# Bikes Count Vs. Weekday

col1 = scales::hue_pal()(7)

ggplot(df, aes_string(x=df$weekday,y=df$cnt,fill=df$weekday))+
  geom_boxplot()+
  xlab('weekday')+ylab('cnt')+
  ggtitle("Bikes Count By weekdays")+
  scale_fill_manual('Weekdays',labels=c('sun','mon','tue','wed','thu','fri','sat'),values = col1)

# Bikes Count Vs. Weather

ggplot(df, aes_string(x=df$weathersit,y=df$cnt,fill=df$weathersit))+
  geom_boxplot()+
  xlab('Weather')+ylab('cnt')+
  ggtitle("Bikes Count By Weather")+
  scale_fill_manual('Weather',labels=c('clear', 'cloudy', 'thunderstorm', 'rain'),values = colList)

#Most bikes are rented during clear weather

# Bike Counts Vs. temperature

ggplot(df, aes_string(x=df$temp,y=df$cnt))+
geom_point()+
xlab('temp')+ylab('cnt')+
ggtitle("Bikes Count Vs temperature")+geom_smooth()

# As temp increases,bikes count increases.

# Bike Counts Vs. humidity

ggplot(df, aes_string(x=df$hum,y=df$cnt))+
  geom_point()+
  xlab('hum')+ylab('cnt')+
  ggtitle("Bikes Count Vs humidity")+geom_smooth()

# As humidity increases,bike count decreases.

# Bike Counts Vs. Windspeed

ggplot(df, aes_string(x=df$windspeed,y=df$cnt))+
  geom_point()+
  xlab('windspeed')+ylab('cnt')+
  ggtitle("Bikes Count Vs Windspeed")+geom_smooth()

#As windspeed increases,bike count decreases.

# MODELLING

# Splitting df in train & test sets in the ratio of 80:20

require(caTools)

set.seed(101)

sample = sample.split(df$cnt, SplitRatio = .80)

train = subset(df, sample == TRUE)

test  = subset(df, sample == FALSE)

dim(train) #584   9

dim(test) #147   9

# LINEAR REGRESSION

model_lm = lm(cnt~.,train)

summary(model_lm)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3992.2  -352.7    63.0   462.5  2207.6 
# 
# Coefficients:
#               Estimate Std. Error  t value   Pr(>|t|)    
# (Intercept)    1356.56     274.87   4.935   1.06e-06 ***
#   season2       958.08     209.12   4.581   5.70e-06 ***
#   season3       753.00     246.48   3.055   0.002358 ** 
#   season4      1719.00     202.45   8.491   < 2e-16 ***
#   yr1          2051.86      64.40  31.859   < 2e-16 ***
#   mnth2         240.34     160.00   1.502   0.133642    
# mnth3           586.55     192.15   3.053   0.002377 ** 
#   mnth4         473.05     278.03   1.701   0.089421 .  
# mnth5           775.60     298.92   2.595   0.009718 ** 
#   mnth6         730.50     314.11   2.326   0.020396 *  
#   mnth7         296.89     349.51   0.849   0.395998    
# mnth8           757.21     336.07   2.253   0.024640 *  
#   mnth9        1235.58     296.59   4.166   3.59e-05 ***
#   mnth10        511.47     264.25   1.936   0.053429 .  
# mnth11         -103.16     255.08  -0.404   0.686062    
# mnth12          -79.52     199.53  -0.399   0.690394    
# weekday1         94.49     117.96   0.801   0.423447    
# weekday2        270.91     117.81   2.300   0.021840 *  
#   weekday3      293.50     120.15   2.443   0.014880 *  
#   weekday4      388.10     117.24   3.310   0.000992 ***
#   weekday5      440.77     121.24   3.636   0.000303 ***
#   weekday6      393.51     116.60   3.375   0.000790 ***
#   weathersit2  -561.56      87.10  -6.448   2.47e-10 ***
#   weathersit3 -1889.02     235.89  -8.008   6.83e-15 ***
#   temp         4196.15     452.74   9.268   < 2e-16 ***
#   hum         -1361.91     342.93  -3.971   8.08e-05 ***
#   windspeed   -2211.15     483.82  -4.570   6.01e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 756.7 on 557 degrees of freedom
# Multiple R-squared:  0.854,	Adjusted R-squared:  0.8472 
# F-statistic: 125.3 on 26 and 557 DF,  p-value: < 2.2e-16



# R-squared is 0.854 which means that about 85% of variance in dependent variable can be explained by variance in independent variables.
# temp has coeff. of 4196.15 which means that temp is having the most(positive) effect on cnt.
# windspeed has coeff. of -2211.15 which means that windspeed is having the second most effect(negative) on cnt.

#Predicting for test set

pred_lm = predict(model_lm,test[,1:8])

pred_lm[1:10]# pred_lm has some negative values.

#Putting negative values to 0.

pred_lm = ifelse(pred_lm < 0,0,pred_lm)

pred_lm[1:10]

#rounding off

pred_lm = ceiling(pred_lm)

# Calculating Mean Absolute Percent Error(MAPE)

library(Metrics)

mape(test[,9],pred_lm) #  1.335392

# Calculating Mean Absolute Error(MAE)

mae(test[,9],pred_lm) # 602.3673

# DECISION TREE

library(rpart)

model_DT = rpart(cnt~.,train,method='anova')

pred_DT = predict(model_DT,test[,1:8])

pred_DT[1:20]

pred_DT = ceiling(pred_DT)

sum(pred_DT<0)# 0 - no negatives

mape(test[,9],pred_DT)#  1.965942

mae(test[,9],pred_DT)# 678.0204

plot(model_DT)

text(model_DT)

# RANDOM FOREST

library(randomForest)

model_RF = randomForest(cnt~.,train,importance=T,ntree=500)

pred_RF = predict(model_RF,test[,1:8])

pred_RF = ceiling(pred_RF)

sum(pred_RF<0) # 0

mape(test[,9],pred_RF)#  1.401571

mae(test[,9],pred_RF)# 554.6327

# K NEAREST NEIGHBOR

#creating a copy of df for knn regression

df_KNN = df

head(df_KNN)

#Converting factors to numeric for knn regression

catcols_final = c('season','yr','mnth','weekday','weathersit')

for(i in catcols_final){
  df_KNN[,i]=as.numeric(df_KNN[,i])
}

str(df_KNN)

# 'data.frame':	731 obs. of  9 variables:
#   $ season    : num  1 1 1 1 1 1 1 1 1 1 ...
# $ yr        : num  1 1 1 1 1 1 1 1 1 1 ...
# $ mnth      : num  1 1 1 1 1 1 1 1 1 1 ...
# $ weekday   : num  7 1 2 3 4 5 6 7 1 2 ...
# $ weathersit: num  2 2 1 1 1 1 2 2 1 1 ...
# $ temp      : num  0.344 0.363 0.196 0.2 0.227 ...
# $ hum       : num  0.806 0.696 0.437 0.59 0.437 ...
# $ windspeed : num  0.16 0.249 0.248 0.16 0.187 ...
# $ cnt       : int  985 801 1349 1562 1600 1606 1510 959 822 1321 ...

# Splitting df_KNN into train & test sets

set.seed(101)

sample = sample.split(df_KNN$cnt, SplitRatio = .80)

train_KNN = subset(df_KNN, sample == TRUE)

test_KNN  = subset(df_KNN, sample == FALSE)

#Applying knn

model_KNN = FNN::knn.reg(train = train_KNN[,1:8], test = test_KNN[,1:8], y = train_KNN[,9], k = 9)

model_KNN$pred[1:10]

#rounding off to nearest integer

pred_KNN = ceiling(model_KNN$pred)

pred_KNN[1:10]

mape(test_KNN[,9],pred_KNN)#  1.712762

mae(test_KNN[,9],pred_KNN)# 738.449

#Gradient Boosted Trees

library(gbm)

model_GBT = gbm(formula=cnt ~ season+yr+mnth+weekday+weathersit+temp+hum+windspeed,data=train,n.trees = 50000,interaction.depth = 4)

pred_GBT = predict(model_GBT,test[,1:8],n.trees = 50000)

pred_GBT[1:10]

pred_GBT = ceiling(pred_GBT)

mape(test[,9],pred_GBT)#  0.9985976

mae(test[,9],pred_GBT)# 506.4898

summary(model_GBT)

#                  var   rel.inf
# temp             temp 35.419354
# yr                 yr 25.271824
# mnth             mnth 11.605649
# hum               hum  8.668744
# season         season  6.797198
# windspeed   windspeed  4.612378
# weekday       weekday  4.406589
# weathersit weathersit  3.218264


# Gradient Boosted Trees have the lowest MAPE and lowest MAE so GBT is the best algorithm for this problem.

# writing test actual count and predicted count in a dataframe

out = data.frame(test[,9])

colnames(out) = 'Actual'

out$Predicted = pred_GBT

head(out)

#     Actual Predicted
# 1   1263      1065
# 2   1421      1660
# 3   1000      1401
# 4    981      1125
# 5   1985      1324
# 6    506       325

write.csv(out,'F:/EdwisorProject1/outputR.csv',row.names = F)
