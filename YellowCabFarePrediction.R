rm(list = ls())

setwd("C:\\Users\\gtiwa\\OneDrive\\Documents\\R")

## Loading necessary libraries

x = c("ggplot2", "corrgram", "VIM", "usdm", "caret", "randomForest", "e1071",
      "DataCombine", "doSNOW", "inTrees", "rpart.plot", "rpart",'MASS','xgboost','stats',"Metrics","caTools","prob","sampling",
      "data.table","dplyr","ggplot2","plotly","psych",'ISLR','glmnet','car',"rlang","emmeans","ggpubr","multcomp")


lapply(x, require, character.only = TRUE)


## Visualising the dataset
tripdata <- read.csv("2021_Yellow_Taxi_Trip_Data__Jan-Jul_.csv", header = T, na.strings = c(" ", "", "NA"))

head(tripdata,5)

summary(tripdata)

### Dataset Cleaning

is.null(tripdata)

set.seed(123)


df <- tripdata

#############Data Analysis and data cleaning                  #######################
# Changing the data types of variables
df$fare_amount = as.numeric(as.character(df$fare_amount))
df$passenger_count=round(df$passenger_count)

### Removing values which are not within desired range(outlier) depending upon basic understanding of dataset.

# 1.Fare amount has a negative value, which doesn't make sense. A price amount cannot be -ve and also cannot be 0. So we will remove these fields.

dflessfare <- df[which(df$fare_amount < 1 ),]

cat("Number of tuples with fare less than $1",nrow(dflessfare))


df2 <- transform(df, group=cut(fare_amount, 
                                      breaks=c(-600,-400,-200,0,200,400,600,800,1000,1200,1400),labels=c("(-600,-400]","(-400,-200]","(-200,0]", "(200,400]", "(400,600]", "(400,600]","(600,800]","(800,1000]","(1000,1200]","(1200,1400]")))
res <- do.call(data.frame,aggregate(fare_amount~group, df2, 
                                    FUN=function(x) c(Count=length(x))))
res

df = df[-which(df$fare_amount < 1 ),]

#2.Passenger_count variable
for (i in seq(4,11,by=1)){
  print(paste('passenger_count above ' ,i,nrow(df[which(df$passenger_count > i ),])))
}
# so 20 observations of passenger_count is consistenly above from 6,7,8,9,10 passenger_counts, let's check them.
df_passenger_count_6 <- df[which(df$passenger_count > 6 ),]

cat("Tuples having passengers more than 6",nrow(df_passenger_count_6))

# Also we need to see if there are any passenger_count==0

cat("passenger count less than 1",nrow(df[which(df$passenger_count <1 ),]))
# We will remove these 58 observations and 20 observation which are above 6 value because a cab cannot hold these number of passengers.
df = df[-which(df$passenger_count < 1 ),]
df = df[-which(df$passenger_count > 6),]


#########################################################################################################################################################################


#############                        Missing Value Analysis                  #############
missing_val = data.frame(apply(df,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
names(missing_val)[1] =  "CountofMissingvalues"
missing_val = missing_val[order(-missing_val$CountofMissingvalues),]
row.names(missing_val) = NULL
missing_val = missing_val[,c(2,1)]
missing_val
df <- na.omit(df)
#####################Outlier Analysis
# Boxplot for fare_amount


pl1 = ggplot(df,aes(x = factor(passenger_count),y = fare_amount))
pl1 + geom_boxplot(outlier.colour="red", fill = c("orange","red","green","yellow","blue","pink") ,outlier.shape=18,outlier.size=1, notch=FALSE)+ylim(0,100) + theme_classic()



# Replace all outliers with NA and impute
vals = df[,"fare_amount"] %in% boxplot.stats(df[,"fare_amount"])$out
df[which(vals),"fare_amount"] = NA

#lets check the NA's
cat("the observations with NA values",sum(is.na(df$fare_amount)))

# lets check the missing values

str(df)

tripdata=df

####################### Simple Random Sampling ##############################

## Simple random sampling for tripdata



names(tripdata)
nrow(tripdata)

tripdata <- na.omit(tripdata)

#table(tripdata$trip_distance)

m1 <- mean(tripdata$passenger_count, na.rm= TRUE)
m2 <- mean(tripdata$trip_distance)
m3 <- mean(tripdata$fare_amount, na.rm= TRUE)
m4 <- mean(tripdata$total_amount)
sd1 <- sd(tripdata$passenger_count)
sd2 <- sd(tripdata$trip_distance)
sd3 <- sd(tripdata$fare_amount)
sd4 <- sd(tripdata$total_amount)

c1 <- paste("Mean=",m1,"Sd=",sd1)
c2 <- paste("Mean=",m2,"Sd=",sd2)
c3 <- paste("Mean=",m3,"Sd=",sd4)
c4 <- paste("Mean=",m4,"Sd=",sd4)



s1 <- plot_ly(tripdata, x= ~tripdata$passenger_count, type = 'histogram',histnorm = 'probability')%>%
  layout(xaxis = list(title = 'Distribution of Passenger Count',range=list(0,25)), yaxis = list(title = 'Frequency',range=list(0,0.8)))
s2 <- plot_ly(tripdata, x= ~tripdata$trip_distance, type = 'histogram',histnorm = 'probability')%>%
  layout(xaxis = list(title = 'Distribution of Trip Distance',range=list(0,25)), yaxis = list(title = 'Frequency',range=list(0,0.05)))
s3 <- plot_ly(tripdata, x= ~tripdata$fare_amount, type = 'histogram',histnorm = 'probability')%>%
  layout(xaxis = list(title = 'Distribution of Fare Amount',range=list(0,25)), yaxis = list(title = 'Frequency',range=list(0,0.05)))
s4 <- plot_ly(tripdata, x= ~tripdata$total_amount, type = 'histogram',histnorm = 'probability')%>%
  layout(xaxis = list(title = 'Distribution of Total Amount',range=list(0,25)), yaxis = list(title = 'Frequency',range=list(0,0.05)))

set.seed(123)

s <- srswr(500,nrow(tripdata))

rows <- (1:nrow(tripdata))[s!=0]
rows <- rep(rows, s[s!=0])

sample.1 <- tripdata[rows,]

#table(sample.1$trip_distance)

m5 <- mean(sample.1$passenger_count)
m6 <- mean(sample.1$trip_distance)
m7 <- mean(sample.1$fare_amount, na.rm= TRUE)
m8 <- mean(sample.1$total_amount)
sd5 <- sd(sample.1$passenger_count)
sd6 <- sd(sample.1$trip_distance)
sd7 <- sd(sample.1$fare_amount,na.rm = TRUE)
sd8 <- sd(sample.1$total_amount)


c5 <- paste("Mean=",m5,"Sd=",sd5)
c6 <- paste("Mean=",m6,"Sd=",sd6)
c7 <- paste("Mean=",m7,"Sd=",sd7)
c8 <- paste("Mean=",m8,"Sd=",sd8)


s5 <- plot_ly(tripdata, x= ~sample.1$passenger_count, type = 'histogram',histnorm = 'probability')%>%
  layout(xaxis = list(title = 'Distribution of sample Passenger Count',range=list(0,25)), yaxis = list(title = 'Frequency',range=list(0,0.8)))
s6 <- plot_ly(tripdata, x= ~sample.1$trip_distance, type = 'histogram',histnorm = 'probability')%>%
  layout(xaxis = list(title = 'Distribution of sample Trip Distance',range=list(0,25)), yaxis = list(title = 'Frequency',range=list(0,0.2)))
s7 <- plot_ly(tripdata, x= ~sample.1$fare_amount, type = 'histogram',histnorm = 'probability')%>%
  layout(xaxis = list(title = 'Distribution of sample Fare Amount',range=list(0,25)), yaxis = list(title = 'Frequency',range=list(0,0.15)))
s8 <- plot_ly(tripdata, x= ~sample.1$total_amount, type = 'histogram',histnorm = 'probability')%>%
  layout(xaxis = list(title = 'Distribution of sample Total Amount',range=list(0,25)), yaxis = list(title = 'Frequency',range=list(0,0.2)))

s <- subplot(s1, s5, s2, s6,s3, s7, s4, s8, nrows = 4, shareX = TRUE)%>%layout(title = 'Simple random Sampling')
annotations = list( 
  list(x= 0.2, y = 1,text = paste("Population",c1),xref = "paper", yref = "paper",  xanchor = "center",  yanchor = "bottom", showarrow = FALSE ),list(x= 0.8, y = 1,text = paste("Sample",c5),xref = "paper", yref = "paper",  xanchor = "center",  yanchor = "bottom", showarrow = FALSE ),list(x= 0.2, y = 0.70,text = paste("Population",c2),xref = "paper", yref = "paper",  xanchor = "center",  yanchor = "bottom", showarrow = FALSE ),list(x= 0.8, y = 0.70,text = paste("Sample",c6),xref = "paper", yref = "paper",  xanchor = "center",  yanchor = "bottom", showarrow = FALSE ),list(x= 0.2, y = 0.45,text = paste("Population",c3) ,xref = "paper", yref = "paper",  xanchor = "center",  yanchor = "bottom", showarrow = FALSE ),list(x= 0.8, y = 0.45,text = paste("Sample",c7),xref = "paper", yref = "paper",  xanchor = "center",  yanchor = "bottom", showarrow = FALSE ),list(x= 0.2, y = 0.20,text = paste("Population",c4) ,xref = "paper", yref = "paper",  xanchor = "center",  yanchor = "bottom", showarrow = FALSE ),list(x= 0.8, y = 0.20,text = paste("Sample",c8),xref = "paper", yref = "paper",  xanchor = "center",  yanchor = "bottom", showarrow = FALSE )
)
s <- s%>%layout(annotations = annotations) 
s

## Dataset Visulaisation



plot_ly(sample.1,y=~fare_amount,x=~passenger_count,color=~passenger_count,type="box")

plot_ly(sample.1,y=~total_amount,x=~passenger_count,color=~passenger_count,type="box")

plot_ly(sample.1,y=~tip_amount,x=~passenger_count,color=~passenger_count,type="box")

df_passenger <- aggregate(sample.1$total_amount, by=list(sample.1$passenger_count), FUN=length)

df_passenger

plot_ly(x = df_passenger$Group.1,
        y = df_passenger$x,
        color = df_passenger$Group.1,
        colors = "Dark2",
        type = "bar") %>%layout(xaxis = list(dtick = 1))%>% layout(title = "Number of rides having n passengers",
                                                                  xaxis = list(title = "No. of Passengers"),yaxis = list(title = "No. of rides"))

### Adding a class based on Total Amount

dfclass <- sample.1

dfclass$occupancy <- cut(dfclass$passenger_count, c(0,1,10))
levels(dfclass$occupancy) <- c("single","multiple")
head(dfclass,5)

dfclass$earnings <- cut(dfclass$total_amount, c(0,13,20,40))
levels(dfclass$earnings) <- c("low","average","high")
head(dfclass,5)

### Whether tip_amount vary significantly across group

dfclass$earnings <- factor(dfclass$earnings)

dfclass$occupancy = factor(dfclass$occupancy)

m <- aov(dfclass$tip_amount~dfclass$earnings, data=dfclass)

# pass the anova model object to the summary function.
summary(m)

aggregate(dfclass$tip_amount, by=list(dfclass$earnings), summary) # calculate summary stats of data subsets

aggregate(dfclass$tip_amount, by=list(dfclass$earnings), sd)

# Create a Boxplot 
boxplot(dfclass$tip_amount~dfclass$earnings, data=dfclass, main="Tip_amount by earnings", xlab="Earnings", ylab="tip_amount")

# Perform one-way ANOVA and if necessary, calculate the associated pairwise comparisons
m<- aov(dfclass$tip_amount~dfclass$earnings, data=dfclass)
summary(m)

# pairwise t-test 
pairwise.t.test(dfclass$tip_amount, dfclass$earnings, p.adj="none")

# pairwise t-test with bonferroni adjustment 
# bonferroni adjustment
pairwise.t.test(dfclass$tip_amount, dfclass$earnings, p.adj="bonferroni")

# Tukeys Test of Honest Significance Test
TukeyHSD(m)

meangroup = mean(dfclass$tip_amount)


tcritical <- qt(0.05, 497, lower.tail = FALSE)


### Ancova test weith adjustment for trip_distance

a1 <- aov(lm(dfclass$tip_amount~dfclass$earnings + dfclass$fare_amount))

a1

summary(a1)

Anova(lm(dfclass$tip_amount~dfclass$earnings + dfclass$fare_amount), type = 3)

my.model<-lm(dfclass$tip_amount~dfclass$earnings + dfclass$fare_amount,  data = dfclass)

# Set some enviroment variables 
emm_options(contrasts=c("contr.treatment", "contr.poly"))


emmeans(my.model, specs = "earnings")


# We can have also pairwise comparision of groups after adjusting for age. 

# no p-value adjustment
emmeans(my.model, specs = "earnings" , contr = "pairwise",  adjust="none")

# p-value adjustment: tukey method
emmeans(my.model, specs = "earnings" , contr = "pairwise",  adjust="tukey")

# p-value adjustment: bonferroni method for 6 tests
emmeans(my.model, specs = "earnings" , contr = "pairwise",  adjust="bonferroni")

## Two way Anova test

ggboxplot(dfclass, x = "earnings", y = "fare_amount", color = "occupancy",
          palette = c("#00AFBB", "#E7B800"))

ggline(dfclass, x = "earnings", y = "fare_amount", color = "occupancy",
       add = c("mean_se", "dotplot"),
       palette = c("#00AFBB", "#E7B800"))

interaction.plot(x.factor = dfclass$earnings, trace.factor = dfclass$occupancy, 
                 response = dfclass$fare_amount, fun = mean, 
                 type = "b", legend = TRUE, 
                 xlab = "occupancy", ylab="fare_amount",
                 pch=c(1,19), col = c("#00AFBB", "#E7B800","red"))

res.aov2 <- aov(fare_amount ~ earnings + occupancy, data = dfclass)
summary(res.aov2)

res.aov3 <- aov(fare_amount ~ earnings * occupancy, data = dfclass)
summary(res.aov3)

result <- dfclass %>% group_by(fare_amount, earnings, occupancy) %>%summarise(count = n(),mean = mean(fare_amount, na.rm = TRUE),sd = sd(fare_amount, na.rm = TRUE))
head(result,5)

TukeyHSD(res.aov3, which = "earnings")


summary(glht(res.aov2, linfct = mcp(earnings = "Tukey")))

pairwise.t.test(dfclass$fare_amount, dfclass$earnings,p.adjust.method = "BH")

# 1. Homogeneity of variances
plot(res.aov3, 1)
leveneTest(fare_amount ~ occupancy*earnings, data = dfclass)

# 2. Normality
plot(res.aov3, 2)

# Extract the residuals
aov_residuals <- residuals(object = res.aov3)
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )

my_anova <- aov(fare_amount ~ earnings * occupancy, data = dfclass)
Anova(my_anova, type = "III")

################                             Feature selection                 ###################

myModel <- lm(fare_amount~passenger_count+trip_distance+extra+mta_tax+tip_amount+tolls_amount+improvement_surcharge+congestion_surcharge,data=sample.1)

newModel1 = step(myModel, direction="backward")
newModel2 = step(lm(fare_amount~1,data = sample.1),direction= "forward", scope = (~passenger_count+trip_distance+extra+mta_tax+tip_amount+tolls_amount+improvement_surcharge+congestion_surcharge))


myvars <- c('fare_amount','trip_distance','extra','mta_tax','tip_amount','tolls_amount','improvement_surcharge','congestion_surcharge')
df <- sample.1[myvars]

cor(df)


#create pairs plot
pairs.panels(df)

myModel1 <- lm(fare_amount~passenger_count+trip_distance+extra+tip_amount+tolls_amount+improvement_surcharge+congestion_surcharge+trip_distance + trip_distance*tip_amount+trip_distance*tolls_amount,data=sample.1)
summary(myModel)


newModel1 = step(myModel1, direction="backward")
newModel2 = step(lm(fare_amount~1,data = sample.1),direction= "forward", scope = (~passenger_count+trip_distance+extra+tip_amount+tolls_amount+improvement_surcharge+congestion_surcharge+trip_distance + trip_distance*tip_amount+trip_distance*tolls_amount))




### Feature Regularization#########

x = model.matrix(df$fare_amount ~., data=df)


y <- df$fare_amount

fit.lasso = glmnet(x,y)
plot(fit.lasso, xvar="lambda", label=TRUE)
cv.lasso = cv.glmnet(x, y) # glmnet built in cross validation. Default is 10 fold.
plot(cv.lasso)
coef(cv.lasso)

myvariables <- c('fare_amount','trip_distance','mta_tax','tip_amount','improvement_surcharge','congestion_surcharge')
dflasso <- df[myvariables]

best_model <- lm(fare_amount~.,df)
summary(best_model)

base_model <- lm(fare_amount~.,dflasso)
summary(base_model)

fit.ridge = glmnet(x, y, alpha=0)
#Note: alpha=0 is  Ridge regression
#      alpha=1 is Lasso - Default value
#      alpha <1 is Elstic net
plot(fit.ridge, xvar="lambda", label=TRUE)
cv.ridge = cv.glmnet(x, y, alpha=0) # cv.glmnet is the built in cross validation function - default CV K=10.
plot(cv.ridge)
coef(cv.ridge)

## Global F-test for comparison between the base model and the best model


lm2 <- lm(fare_amount ~., data = df)

summary(lm2)

fcritical <- qf(0.05,df1 = 2, df2 = 492, lower.tail = FALSE)
fcritical


resss <- sum((df$fare_amount - fitted(lm2))^2)
resss

regss <- sum((fitted(lm2) - mean(df$fare_amount))^2)
regss

df_1<- 2
df_2 <- 492
fstats <- (regss /df_1)/(resss/df_2)
fstats


#ANOVA for categorical variables with target numeric variable

#aov_results = aov(fare_amount ~ passenger_count * pickup_hour * pickup_weekday,data = train)
aov_results = aov(fare_amount ~ .,data = df)

summary(aov_results)

lm1 <- lm(fare_amount~., data = df)
aov(lm1)

summary(aov(lm1))

res <- resid(lm1)

plot(fitted(lm1), resid(lm1))

#add a horizontal line at 0 

abline(0,0)

par(mfrow=c(2,2))
plot(lm1)
par(mfrow=c(1,1))

###################Model Selection################
#Error metric used to select model is RMSE

#############            Linear regression               #################

ctrl <- trainControl(method = "cv", number = 10)

model <- train(fare_amount ~.,data=df, method = "lm", trControl = ctrl)

print(model)

model$finalModel

min(model$resample$RMSE)

model$resample

leastrmselin <- min(model$resample$RMSE)
leastrmselin

fig1 <- plot_ly(model$resample, x = ~model$resample$Resample, y = ~model$resample$RMSE, type = 'scatter', mode = 'lines')

fig1


#######Logistic Regression########
library(caret)
dflogistic <- df


dflogistic$earnings <- cut(dflogistic$fare_amount, c(0,10,26))
levels(dflogistic$earnings) <- c("low","high")

train_control <- trainControl(method = "cv", number = 10)

# train the model on training set
model1 <- train(earnings ~ trip_distance + extra +  tip_amount + congestion_surcharge, data = dflogistic, trControl = train_control, method = "glm", family="binomial")

# print cv scores
print(model1)

model1$results
model1$resample

avgacclog <- mean(model1$resample$Accuracy)
avgacclog

fig2 <- plot_ly(model1$resample, x = ~model1$resample$Resample, y = ~model1$resample$Accuracy, type = 'scatter', mode = 'lines')

fig2


### Polynomial regression###

library(boot)

set.seed(1)
deltas <- rep(NA, 5)
for (i in 1:5) {
  fit <- glm(fare_amount ~ poly(trip_distance, i)+poly(extra,i)+ poly(tip_amount,i) , data = dflogistic)
  deltas[i] <- cv.glm(dflogistic, fit, K=10)$delta[1]
}
plot(1:5, deltas, xlab = "Degree", ylab = " MSE", type = "l")
d.min <- which.min(deltas)
points(which.min(deltas), deltas[which.min(deltas)], col = "red", cex = 2, pch = 20)

deltas

leastRMSEpr <- (min(deltas))^0.5
leastRMSEpr
#############                             Decision Tree            #####################

ctrl <- trainControl(method = "cv", number = 10)

model2 <- train(fare_amount ~.,data=df, method = "rpart", trControl = ctrl)

print(model2)

model2$finalModel

model2$resample

min(model2$resample$RMSE)

model2$resample

leastrmsedt <- min(model2$resample$RMSE)
leastrmsedt

fig3 <- plot_ly(model2$resample, x = ~model2$resample$Resample, y = ~model2$resample$RMSE, type = 'scatter', mode = 'lines')

fig3

#############                             Random forest            #####################

library(randomForest)
library(caret)
library(e1071)

trControl <- trainControl(method = "cv",number = 10,search = "grid")

rf_default <- train(fare_amount~.,data = df,method = "rf",metric = "RMSE",trControl = trControl)

print(rf_default)

rf_default$finalModel

rf_default$resample

min(rf_default$resample$RMSE)

rf_default$resample

leastrmserf <- min(rf_default$resample$RMSE)
leastrmserf

fig4 <- plot_ly(rf_default$resample, x = ~rf_default$resample$Resample, y = ~rf_default$resample$RMSE, type = 'scatter', mode = 'lines')

fig4



############          Improving Accuracy by using Ensemble technique ---- XGBOOST             ###########################


x <- data.matrix(select(df, -c('fare_amount')))
y <- data.matrix(df[,c('fare_amount')])

xgb_df = xgb.DMatrix(data = x, label = y)



cv <- xgb.cv(data = xgb_df, nrounds = 10, nthread = 2, nfold = 10, metrics = list("rmse"),max_depth = 10, eta = 1, verbose = 0)

print(cv)
print(cv, verbose=TRUE)

cv$evaluation_log

leastrmsexg <- min(cv$evaluation_log$train_rmse_mean)
leastrmsexg

fig5 <- plot_ly(cv$evaluation_log, x = ~cv$evaluation_log$iter, y = cv$evaluation_log$test_rmse_mean, type = 'scatter', mode = 'lines')

fig5

xrmse<- c("Linear Regression","Polynomial Regression","Decision Tress","Random Forest","XgBoost")
yrmse <- c(leastrmselin,leastRMSEpr,leastrmsedt,leastrmserf,leastrmsexg)
dfrmse <- data.frame(xrmse,yrmse)
dfrmse

fig6 <- plot_ly(dfrmse, x = ~dfrmse$xrmse, y = dfrmse$yrmse, type = 'scatter', mode = 'lines')

fig6

