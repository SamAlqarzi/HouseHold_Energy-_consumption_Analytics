##############################################################################################
# Time Series Analysis on Household Energy Consumption
# A School Project for Advanced Data Analytics Class
# goal is to test for power usage relative to time
# data-set is recorded in minutes of power consumption, target variable is 'Global_active_power
# data-set source: 
     #https://archive.ics.uci.edu/ml/datasets/individual+household+electric+power+consumption
# devoleped by: Alqarzi, Sam; Gifford, Portia; McDonalds, Ross; Murphy, MacKenzie; Pulitzer,Jake
#############################################################################################

# install libraries if not installed #
library(ggplot2)
library(vioplot)
library(readxl)
library(tidyverse)
require(data.table)
library(forecast)
library(tseries)
library(dplyr)
library(tidyr) 
library(corrplot)
library(doBy)
library(tiger)
library(lubridate)
library(nycflights13)
library(xts)
library(stringr)
library(chron)
library(rpart)
library(psych)
library(tree)
library(caTools)
library(corrplot)
library(knitr)
library(Matrix)
library(xgboost)
library(caret)
library(DiagrammeR)

######################################
# Stage1: Data Loading and Preparation 
######################################

# set the session directory
setwd("~/Downloads/AdvcdDataAnalyt/Project")
# reading data 
HEC <- read.csv('household_power_consumption.csv')

# data summaries 
View(HEC)
str(HEC)
summary(HEC)

# change other cols to their correct data types, use 'suppressWarnings()' to avoid NAs warnings 
HEC$Global_active_power = suppressWarnings(as.numeric( HEC$Global_active_power))
HEC$Global_reactive_power = suppressWarnings(as.numeric(HEC$Global_reactive_power))
HEC$Voltage  = suppressWarnings(as.numeric(HEC$Voltage ))
HEC$Global_intensity = suppressWarnings( as.numeric(HEC$Global_intensity))
HEC$Sub_metering_1  = suppressWarnings(as.numeric(HEC$Sub_metering_1 ))
HEC$Sub_metering_2  = suppressWarnings(as.numeric(HEC$Sub_metering_2 ))
HEC$Sub_metering_3  = suppressWarnings(as.numeric(HEC$Sub_metering_3 ))

# checking for missing values
sum(is.na(HEC))

# all missing values in numeric vars, fill them with column mean
HEC[sapply(HEC, is.numeric)] <- lapply(HEC[sapply(HEC, is.numeric)], 
                                       function(x) ifelse(is.na(x),
                                                          mean(x, na.rm = TRUE), x))
# check for duplicates 
sum(duplicated(HEC))

# remove first column 'X' which is an index column given by R, dont need it 
HEC <- HEC[-c(1)]

# paste date and time vars to get full timestamp column
HEC <- HEC %>% unite('Datetime', Date:Time, na.rm = TRUE) 

# change the type of the new column 
HEC$Datetime <- as.POSIXct(HEC$Datetime,format="%d/%m/%Y_%H:%M:%S")

# extract date and time components for descriptive analysis and visualization
HEC$hour <- strftime(HEC$Datetime, "%H")
HEC$day_of_month <- format(HEC$Datetime, "%d")
HEC$day_of_week <- weekdays(as.Date(HEC$Datetime))
HEC$week <- strftime(HEC$Datetime, "%V")
HEC$month <- strftime(HEC$Datetime, "%m")
HEC$quarter <- quarters(HEC$Datetime)
HEC$year <- format(HEC$Datetime, "%Y")

# table data records by year and remove year/s with incomplete cycle in the data
table(HEC$year) # 2006 and 2010 removed
HEC <- filter(HEC, year != "2006" & year != "2010")


####################################
#Stage 2: Descriptive Analysis
####################################

# test for correlation between numerical values
HEC_num <- select_if(HEC, is.numeric)
correlations <- cor(HEC_num)
corrplot(correlations, method = 'number')

# statistics summary for the numerical columns in the dataset
summary(HEC_num)


# the data is in minutes, so will calculate the averages and totals of 'global_active_power' by other date and time components
# Average and Standard Deviation 'global_active_power' by year
HEC_num_cols <- aggregate(Global_active_power ~ year, HEC, function(x) c(mean = mean(x), sd = sd(x)))
# total 'global_active_power' by year 
aggregate(Global_active_power ~ year, HEC, FUN =  sum)

# Average and Standard Deviation 'global_active_power' by quarter
aggregate(Global_active_power ~ quarter, HEC, function(x) c(mean = mean(x), sd = sd(x)))
# total 'global_active_power' by quarter
aggregate(Global_active_power ~ quarter, HEC, FUN =  sum)

# Average and Standard Deviation 'global_active_power' by month
aggregate(Global_active_power ~ month, HEC, function(x) c(mean = mean(x), sd = sd(x)))
# total 'global_active_power' by quarter
aggregate(Global_active_power ~ month, HEC, FUN =  sum)

# Average and Standard Deviation 'global_active_power' by day of the week 
aggregate(Global_active_power ~ day_of_week, HEC, function(x) c(mean = mean(x), sd = sd(x)))
# total 'global_active_power' by day of the week
aggregate(Global_active_power ~ day_of_week, HEC, FUN =  sum)

#power usage tends to be high on weekend days, will create binary column  to use in modeling
HEC$iswkend <- if_else(HEC$day_of_week == 'Saturday'| HEC$day_of_week == 'Sunday',1, 0)

# Average and Standard Deviation 'global_active_power' by quarter
aggregate(Global_active_power ~ day_of_month, HEC, function(x) c(mean = mean(x), sd = sd(x)))
# total 'global_active_power' by quarter
aggregate(Global_active_power ~ day_of_month, HEC, FUN =  sum)

# Average and Standard Deviation 'global_active_power' by quarter
aggregate(Global_active_power ~ hour, HEC, function(x) c(mean = mean(x), sd = sd(x)))
# total 'global_active_power' by quarter
aggregate(Global_active_power ~ hour, HEC, FUN =  sum)

# descriptive analysis for other numerical variables (e.g. global intensity and the sub-meterings):

summaryBy(Global_intensity + Sub_metering_1+Sub_metering_2+Sub_metering_3 ~ year + quarter, data = HEC,
          FUN = function(x) { c(mean = mean(x), sd = sd(x)) } )
#total by year-quarter
summaryBy(Global_intensity + Sub_metering_1+Sub_metering_2+Sub_metering_3 ~ year + quarter, data = HEC,
          FUN = sum )

# Average and sd by month 
summaryBy(Global_intensity + Sub_metering_1+Sub_metering_2+Sub_metering_3 ~ month , data = HEC,
          FUN = function(x) { c(mean = mean(x), sd = sd(x)) } )
#total by by month
summaryBy(Global_intensity + Sub_metering_1+Sub_metering_2+Sub_metering_3 ~ month, data = HEC,
          FUN = sum )

# Average and sd by day 
summaryBy(Global_intensity + Sub_metering_1+Sub_metering_2+Sub_metering_3 ~ day_of_month , data = HEC,
          FUN = function(x) { c(mean = mean(x), sd = sd(x)) } )
#total by by day
summaryBy(Global_intensity + Sub_metering_1+Sub_metering_2+Sub_metering_3 ~ day_of_month, data = HEC,
          FUN = sum )

# Average and sd by hour
summaryBy(Global_intensity + Sub_metering_1+Sub_metering_2+Sub_metering_3 ~ hour , data = HEC,
          FUN = function(x) { c(mean = mean(x), sd = sd(x)) } )
#total by by hour
summaryBy(Global_intensity + Sub_metering_1+Sub_metering_2+Sub_metering_3 ~ hour, data = HEC,
          FUN = sum )

#########################
# Stage3: Visualizations 
#########################

# plot active energy consumption by 
vis1 <- aggregate(Global_active_power ~ month + year, HEC, mean) # subset the data
vis1$month <- as.integer(vis1$month) # change to int to pass continuous values into the x scale
plt1 <- ggplot(vis1, aes(x= month, y=Global_active_power, color=factor(year))) +
  geom_point(size = 2)+
  geom_line(size = 1.5)+
  ggtitle('Average Power consumed by month over three years period') +
  scale_x_continuous('Month', labels = as.character(vis1$month), breaks = vis1$month)+
  ylab('Power consumed (kilowatt)')+
  scale_color_manual(values = c('blue3', 'brown4', 'darkseagreen4','yellow4'))+
  labs(colour = 'Year')
plt1


# average daily power consumption for four years by quarter
vis2 <- HEC[,c('Datetime' ,'quarter','Global_active_power')]
vis2$Datetime <- as.Date(vis$Datetime, format = '%Y-%m-%d %H:%M:%S')
vis2 <- aggregate(Global_active_power ~ Datetime + quarter, vis2, mean)

plt2 <- ggplot(vis2, aes(x= Datetime, y=Global_active_power, color= quarter)) +
  geom_point(size = 1)+
  ggtitle(' Daily Average Power consumed by season over four years period') +
  xlab('Daily Record') +
  ylab('power_consumed (kilowatt)')
plt2


# average daily consumtion for each submetering 
vis3 <-  HEC[,c('Datetime','Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3')]
vis3$Datetime <- as.Date(vis3$Datetime, format = '%Y-%m-%d %H:%M:%S')
vis3 <- aggregate(. ~ Datetime,vis3, mean)
plt3 <- ggplot(vis3, aes(x = Datetime)) +
  geom_point(aes( y=Sub_metering_1, colour = 'Sub_metering1'))+
  geom_point(aes( y=Sub_metering_2, colour = 'Sub_metering2'))+
  geom_point(aes( y=Sub_metering_3, colour = 'Sub_metering3'))+
  ggtitle('Average daily power consumed across all three sub_meterings') +
  scale_colour_manual("", 
                      breaks = c("Sub_metering1", "Sub_metering2","Sub_metering3"),
                      values = c("chocolate3", "royalblue4", "maroon4")) +
  xlab('') +
  ylab('power_consumed (watt-hours)')
plt3

# Visualizations.R

# taking the taking the hourly average
hous_eng <- subset(HEC, select = -c(Datetime, day_of_week,week))
hous_eng <- aggregate(. ~ year + quarter + month+ day_of_month + hour, hous_eng, mean)


with(hous_eng, plot(Global_active_power, Global_intensity, 
                    main="Global_intensity by Global_active_power", 
                    col='red', pch=16, cex=.25))



par(mfrow=c(2,2))
# plot global intensity by global active power
with(hous_eng, plot(Global_active_power, Global_intensity,
                    main="Global_intensity by global active power", 
                    col='orange', pch=16))
# plot global intensity by sm1
with(hous_eng, plot(Sub_metering_1, Global_intensity,
                    main="Global_intensity by sub metering 1", 
                    col='red', pch=16) + abline(lm(Global_intensity ~ Sub_metering_1, data=hous_eng)))
# global intensity by sm2
with(hous_eng, plot(Sub_metering_2, Global_intensity,
                    main="Global_intensity by sub metering 2", 
                    col='lightblue1', pch=16) + abline(lm(Global_intensity ~ Sub_metering_2, data=hous_eng)))
# global intensity by sm3
with(hous_eng, plot(Sub_metering_3, Global_intensity,
                    main="Global_intensity by sub metering 3", 
                    col='green1', pch=16) + abline(lm(Global_intensity ~ Sub_metering_3, data=hous_eng)))

plot(factor(hous_eng$month), hous_eng$Global_intensity, col='lightblue', xlab="month (1= Jan, 12= Dec)", ylab="global intensity")


########################
# Stage4: Modeling
########################
  ###############################
  # - Modeling Data Preparation # 
  ###############################

# subset the data, remove datetime components except year, month, day, hour, need it to group by
HEC_day <- subset(HEC, select = -c(Datetime, quarter,  week, day_of_week, hour, iswkend))

# taking the daily average
HEC_day <- aggregate(. ~ year + month+ day_of_month, HEC_day, sum )

# save the new data in a csv file for easy access
write.csv(HEC_day,'HEC_day.csv')

# read the data after set up working directory
HEC_days <- read.csv('HEC_day.csv')

# summary of the data types, and change types accordingly 
str(HEC_day)

# combine date parts to extract the weekend days 
HEC_days$Date <- as.Date(with(HEC_days, paste(year, month, day_of_month,sep="-")), "%Y-%m-%d")

# extracting date components from the new data
HEC_days$day_of_week <- weekdays(as.Date(HEC_days$Date))
HEC_days$week <- strftime(HEC_days$Date, "%V")
HEC_days$quarter <- quarters(HEC_days$Date)

# labeling week and weekend days
HEC_days$iswkend <- if_else(HEC_days$day_of_week == 'Saturday'| HEC_days$day_of_week == 'Sunday',1, 0)

#creating a holiday columns referencing French national holidays, since the data is compiled in France
holidays <- as.Date(c("2007-01-01","2007-04-04","2007-04-05", "2007-05-01", "2007-05-08", 
                      "2007-05-13", "2007-05-24", "2007-07-14", "2007-08-15", "2007-11-01",
                      "2007-11-11", "2007-12-25",
                      #2008
                      "2008-01-01","2008-04-04","2008-04-05", "2008-05-01", "2008-05-08", 
                      "2008-05-13", "2008-05-24", "2008-07-14", "2008-08-15", "2008-11-01",
                      "2008-11-11", "2008-12-25",
                      #2009
                      "2009-01-01","2009-04-04","2009-04-05", "2009-05-01", "2009-05-08", 
                      "2009-05-13", "2009-05-24", "2009-07-14", "2009-08-15", "2009-11-01",
                      "2009-11-11", "2009-12-25"), format = "%Y-%m-%d")

# labeling holidays in the data set
HEC_days$is_holiday <- c()              
for (i in 1:nrow(HEC_days)) {
  if (HEC_days$Date[i]%in% holidays){
    HEC_days$is_holiday[i] <- 1
  }
  else{
    HEC_days$is_holiday[i] <- 0
  }
}

# sort data set by date 
HEC_days <- HEC_days %>% arrange(as.Date(HEC_days$Date, format= "%Y-%m-%d"))

# arraging columns in the dataset
names(HEC_days)
cols <- c("Date", "year","quarter","month","week","day_of_month","day_of_week","iswkend",
          "is_holiday","Voltage","Global_intensity","Global_reactive_power",
          "Sub_metering_1","Sub_metering_2","Sub_metering_3","Global_active_power")

HEC_days <- HEC_days[,c(cols)]

# save this data as csv for modeling easy access
write.csv(HEC_days,'HEC_days.csv')

#############
# - Models # 
############


################################
# * Model 1: linear Regression # 
################################

lm_fitAll <- lm(HEC_days$Global_active_power ~ HEC_days$year + HEC_days$quarter + HEC_days$month + HEC_days$week + HEC_days$day_of_month + HEC_days$day_of_week + HEC_days$iswkend + HEC_days$is_holiday)
summary(lm_fitAll)

lm_fit1 <- lm(HEC_days$Global_active_power ~ HEC_days$year + HEC_days$quarter + HEC_days$month + HEC_days$iswkend + HEC_days$day_of_week + HEC_days$is_holiday)
summary(lm_fit1)

lm_fitWeek <- lm(HEC_days$Global_active_power ~ HEC_days$week)
summary(lm_fitWeek)

###############################
# * Model 2: Tree Models # 
###############################
# read data ---
path = "HEC_days.csv"
DF_days = read.csv(path, na.strings=c('NA',''), stringsAsFactors = TRUE)
df = DF_days
head(df)


#######################
### REGRESSION TREE ###
#######################

train = subset(df, year != 2009)
test = subset(df, year == 2009)

# Fit tree to training set ---
tree.hous = tree(Global_active_power ~ year + as.factor(quarter) + as.factor(month) +
                   day_of_month + as.factor(iswkend) + as.factor(is_holiday) + as.factor(day_of_week), data=train)
#plots tree to describe data
summary(tree.hous)
par(cex=0.65)
plot(tree.hous)
text(tree.hous, pretty=0)
# CV to find best k ---
cv.reg = cv.tree(tree.hous, K=10)
par(cex=0.9)
plot(cv.reg$size, cv.reg$dev, type="b")
# Prune tree 4 nodes ---
prune.reg1 = prune.tree(tree.hous, best=4); summary(prune.reg1)
plot(prune.reg1)
text(prune.reg1, pretty=0)
#make preds
preds.reg1 = predict(prune.reg1, newdata= test)
#plot residuals
plot(preds.reg1, test$Global_active_power)
abline(0, 1)
#check RMSE for performance
MSE1 = mean((preds.reg1 - test[, "Global_active_power"])^2)
RMSE1 = sqrt(MSE1); cat("The MSE is:", MSE1, "\nThe RMSE for this tree is:", RMSE1)
# 7 nodes ---
prune.reg3 = prune.tree(tree.hous, best=7); summary(prune.reg3)
plot(prune.reg3)
text(prune.reg3, pretty=0)
preds.reg3 = predict(prune.reg3, newdata= test)
plot(preds.reg3, test$Global_active_power)
abline(0, 1)
MSE3 = mean((preds.reg3 - test[, "Global_active_power"])^2)
RMSE3 = sqrt(MSE3); cat("The MSE is:", MSE3, "\nThe RMSE for this tree is:", RMSE3)
print(prune.reg1)


#####################
### DECISION TREE ###
#####################
# Set up df for classification ---
df_class = df
#create binary indicator for high power day (1=high, 0=no)
#thresh for high power day = 1909
Q3 = 1909.5
rows = dim(df_class)[1]
for (i in 1:rows) {
  if (df_class$Global_active_power[i] > Q3){
    df_class$pwr[i] <- 1
  } else {
    df_class$pwr[i] <- 0
  }
}
#remove global active power
df_class$Global_active_power = NULL

#split into train and test sets
train = subset(df_class, year != 2009)
test = subset(df_class, year == 2009)

tree.highpower = tree(as.factor(pwr) ~ year + as.factor(quarter) + as.factor(month) +
                        day_of_month + as.factor(iswkend) + as.factor(is_holiday) + 
                        as.factor(day_of_week), data=train)
#summarize
summary(tree.highpower)
missClassErrorRate = summary(tree.highpower)$misclass[1] / summary(tree.highpower)$misclass[2]
missClassErrorRate
cat("Accuracy:", 1-missClassErrorRate)
#plots
par(cex=0.9)
plot(tree.highpower)
text(tree.highpower, pretty=0)
#validation on test set
tree.pred = predict(tree.highpower, newdata= test, type="class")
mean(tree.pred != test$pwr)
table(test$pwr, tree.pred)
TP = 56
TN = 230
FP = 54
FN = 25
grandtotal = TP + TN + FP + FN
Accuracy = (TP + TN)/grandtotal; cat("\nAccuracy:", Accuracy)
Precision = TP / (TP + FP); cat("\nPrecision:", Precision)
Recall = TP/(TP+FN); cat("\nRecall:", Recall)

#10 fold cross validation the tree
cv.tree = cv.tree(tree.highpower, FUN=prune.misclass, K=10)
#CV error plots 
par(mfrow = c(1,2))
plot(cv.tree$size, cv.tree$dev, type="b")
plot(cv.tree$k, cv.tree$dev, type="b")

#prune the tree
par(mfrow= c(1,1))
pruned.tree = prune.misclass(tree.highpower, k=3)
par(cex=0.9)
plot(pruned.tree)
text(pruned.tree, pretty=0)
summary(pruned.tree)
pruned.tree
#misclasserrorrate
missClassErrorRate = summary(pruned.tree)$misclass[1] / summary(pruned.tree)$misclass[2]
missClassErrorRate

#vars used in one tree and not the other
c(setdiff(summary(pruned.tree)$used, summary(tree.highpower)$used),
  setdiff(summary(tree.highpower)$used, summary(pruned.tree)$used))

pruned.tree.pred = predict(pruned.tree, newdata= test, type="class")
table(test$pwr, pruned.tree.pred)
TP = 44
TN = 250
FP = 34
FN = 37
grandtotal = TP + TN + FP + FN
Accuracy = (TP + TN)/grandtotal; cat("\nAccuracy:", Accuracy)
Precision = TP / (TP + FP); cat("\nPrecision:", Precision)
Recall = TP/(TP+FN); cat("\nRecall:", Recall)

####################
# * Model 4: XGBoot# 
####################

days <- read.csv("HEC_days.csv")
days1 <- subset(days, select = -c(Voltage, Sub_metering_1, Sub_metering_2, Sub_metering_3, Global_intensity, Date, Global_reactive_power, X))
data = as.data.frame(days1)
head(days1)
ohe_feats = c('quarter', 'day_of_week')
dummies = dummyVars(~ quarter + day_of_week, data = data)
df_all_ohe <- as.data.frame(predict(dummies, newdata = data))
df_all_combined <- cbind(data[,-c(which(colnames(data) %in% ohe_feats))],df_all_ohe)


set.seed(12)
indexes = createDataPartition(df_all_combined$Global_active_power, p = .70, list = F)
train = df_all_combined[indexes, ]
test = df_all_combined[-indexes, ]

train_x = data.matrix(train[, -13])
train_y = train[,13]

test_x = data.matrix(test[, -13])
test_y = test[, 13]

xgb_train = xgb.DMatrix(data = train_x, label = train_y)

xgb_test = xgb.DMatrix(data = test_x, label = test_y)


modelc <-  xgboost(data = xgb_train, max_depth = 2,eta = 1, nthread = 2, nrounds = 2, objective = "reg:linear")
pred_y = predict(modelc, xgb_test)
pred_y
xgb.plot.tree(model = modelc)
xgb.plot.tree(model = modelc, trees =  1, show_node_id = TRUE)
importance_matrix1 <- xgb.importance(colnames(train$df_all_combined), model = modelc) 
xgb.plot.importance(importance_matrix1, rel_to_first = TRUE, xlab = "Relative importance")
xgb.importance(model = modelc)

mse = (mean((test_y - pred_y)^2))
mae = caret::MAE(test_y, pred_y)
rmse = caret::RMSE(test_y, pred_y)
cat("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse)


