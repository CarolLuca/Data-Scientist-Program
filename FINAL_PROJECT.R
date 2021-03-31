setwd("/home/clau/Documents/DSC/PROIECT_FINAL")
CCPP <- read.csv("/home/clau/Documents/DSC/PROIECT_FINAL/energy_float_values.csv")
library(ggplot2, quietly = TRUE)
library(knitr, quietly = TRUE)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggpubr)
library(tseries)
summary(CCPP)
class(CCPP)

# remove outliers for Inflow..HPP1
boxplot(CCPP$Inflow..HPP1)
outliers1 <- boxplot(CCPP$Inflow..HPP1, plot = FALSE)$out
length(outliers1)


# remove outliers for Inflow.HPP2
boxplot(CCPP$Inflow.HPP2)
outliers2 <- boxplot(CCPP$Inflow.HPP2, plot = FALSE)$out
length(outliers2)
CCPP[CCPP$Inflow.HPP2 %in% outliers2, "Inflow.HPP2"] = mean(CCPP$Inflow.HPP2)

# remove outliers for Production.HPP1
boxplot(CCPP$Production.HPP1)
outliers3 <- boxplot(CCPP$Production.HPP1, plot = FALSE)$out
length(outliers3)
CCPP[CCPP$Production.HPP1 %in% outliers3, "Production.HPP1"] = mean(CCPP$Production.HPP1)

# remove outliers for Production.HPP2
boxplot(CCPP$Production.HPP2)
outliers4 <- boxplot(CCPP$Production.HPP2, plot = FALSE)$out
length(outliers4)
CCPP[CCPP$Production.HPP2 %in% outliers4, "Production.HPP2"] = mean(CCPP$Production.HPP2)

boxplot(CCPP$Outflow.HHP1)
outliers1 <- boxplot(CCPP$Outflow.HHP1, plot = FALSE)$out
length(outliers1)
CCPP[CCPP$Outflow.HHP1 %in% outliers1, "Inflow..HPP1"] = mean(CCPP$Outflow.HHP1)

summary(CCPP)
sapply(CCPP, class)
head(CCPP,10)
tail(CCPP,10)
write.csv(CCPP,"/home/clau/Documents/DSC/PROIECT_FINAL/energy_clean_data.csv", row.names = FALSE)


attach(CCPP)
par(mfrow=c(2,3))
boxplot(Outflow.HHP1, main="Outflow.HHP1", horizontal = TRUE, col = "tomato3")
boxplot(Inflow..HPP1, main="Inflow..HPP1", horizontal = TRUE, col = "tomato3")
boxplot(Inflow.HPP2, main="Inflow.HPP2", horizontal = TRUE, col ="tomato3" )
boxplot(Production.HPP1, main="Production.HPP1", horizontal = TRUE, col = "tomato3")
boxplot(Production.HPP2, main="Production.HPP2", horizontal = TRUE, col = "tomato3")


par(mfrow=c(2,3))
hist(Outflow.HHP1, main="Outflow.HHP1", breaks=20, col="yellow", freq = FALSE)
lines(density(Outflow.HHP1, bw=10), type = "l", col="red", lwd=2)
hist(Inflow..HPP1, main="Inflow..HPP1", breaks=20, col="yellow", freq = FALSE)
lines(density(Inflow..HPP1, bw=10), type = "l", col="red", lwd=2)
hist(Inflow.HPP2, main="Inflow.HPP2", breaks=20, col="yellow", freq = FALSE)
lines(density(Inflow.HPP2, bw=10), type = "l", col="red", lwd=2)
hist(Production.HPP1, main="Production.HPP1", breaks=20, col="yellow", freq = FALSE)
lines(density(Production.HPP1, bw=10), type = "l", col="red", lwd=2)
hist(Production.HPP2, main="Production.HPP2", breaks=20, col="yellow", freq = FALSE)
lines(density(Production.HPP2, bw=10), type = "l", col="red", lwd=2)
par(mfrow=c(2,2))

ggplot(CCPP, aes(x=Inflow..HPP1, y=Production.HPP1))+ 
  geom_point(col="coral")+
  geom_smooth(model = lm, se = FALSE)+ 
  labs(x="Inflow..HPP1", y="Production.HPP1")
ggplot(CCPP, aes(x=Inflow.HPP2, y=Production.HPP2))+ 
  geom_point(col="coral")+
  geom_smooth(model = lm, se = FALSE)+ 
  labs(x="Inflow.HPP2", y="Production.HPP2")

# Creating a Correlation Matrix Heatmap

CCPP1 <- subset( CCPP, select = -(dateRep) )

matrix <- round(cor(CCPP1),2)
# Getting lower triangle of the correlation matrix
get_lower_tri<-function(matrix){
  matrix[upper.tri(matrix)] <- NA
  return(matrix)
}
# Getting upper triangle of the correlation matrix
get_upper_tri <- function(matrix){
  matrix[lower.tri(matrix)]<- NA
  return(matrix)
}
upper_tri <- get_upper_tri(matrix)
# Melt the correlation matrix
library(reshape2)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Heatmap
library(ggplot2)
#reordering the correlation matrix according to the correlation coefficient is useful to identify the hidden pattern in the matrix. hclust for hierarchical clustering order is used below in the funtion defined.
reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}
# Reorder the correlation matrix
cormat <- reorder_cormat(matrix)
upper_tri <- get_upper_tri(matrix)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
#Adding correlation coefficients on the heatmap
#Using geom_text() to add the correlation coefficients on the graph
#Using a blank theme (remove axis labels, panel grids and background, and axis ticks)
#Using guides() to change the position of the legend title
ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

# ScatterPlots OF Production.HPP1 vs Other Variables in the Dataset

attach(CCPP)
library(hexbin)
library(RColorBrewer)
bin1 <- hexbin(Outflow.HHP1, Production.HPP1, xbins = 40)
bin2 <- hexbin(Inflow..HPP1, Production.HPP1, xbins = 40)
bin3 <- hexbin(Inflow.HPP2, Production.HPP1, xbins = 40)
bin4 <- hexbin(Production.HPP2, Production.HPP1, xbins = 40)
my_colors=colorRampPalette(rev(brewer.pal(11,'Spectral')))
par(mfrow=c(2,2))
plot(bin1, main="", xlab= "Outflow.HHP1", ylab="Production.HPP1", colramp=my_colors) 
plot(bin2, main="", xlab= "Inflow..HPP1", ylab="Production.HPP1", colramp=my_colors) 
plot(bin3, main="", xlab= "Inflow.HPP2", ylab="Production.HPP1", colramp=my_colors) 
plot(bin4, main="", xlab= "Production.HPP2", ylab="Production.HPP1", colramp=my_colors) 

# Chi-squared Tests of Independence

tbl1 <- table(Outflow.HHP1, Production.HPP1)
tbl2 <- table(Inflow..HPP1, Production.HPP1)
tbl3 <- table(Inflow.HPP2, Production.HPP1)
tbl4 <- table(Production.HPP2, Production.HPP1)
chisq.test(tbl1)
chisq.test(tbl2)
chisq.test(tbl3)
chisq.test(tbl4)

# Regression Analysis

fit <- lm(Production.HPP1~Outflow.HHP1+Inflow..HPP1+Inflow.HPP2+Production.HPP2,CCPP)
library(sjPlot)
plot_model(fit,type = "slope" , show.header = TRUE, emph.p = TRUE, string.dv = "Production.HPP1")

# Component+Residual Plots
library(car)
library(broom)
crPlots(fit)

# Residual Plots
library(ggfortify)
autoplot(fit, label.size = 3)

# Testing Outliers
outlierTest(fit)

# Checking Multicollinearity
vif(fit)
sqrt(vif(fit)) > 2

# Testing Heteroscadasticity
ncvTest(fit)

require(astsa)
library(feasts)
summary(CCPP)
library(forecast)
library(fpp2)
ggseasonplot(ts(CCPP$Outflow.HHP1,start=2006,end=2013,frequency=12))
ggseasonplot(ts(CCPP$Inflow..HPP1,start=2006,end=2013,frequency=12))
ggseasonplot(ts(CCPP$Inflow.HPP2,start=2006,end=2013,frequency=12))
ggseasonplot(ts(CCPP$Production.HPP1,start=2006,end=2013,frequency=12))
ggseasonplot(ts(CCPP$Production.HPP2,start=2006,end=2013,frequency=12))

# Converting character object dateRep to object date and visualise the data
CCPP$dateRep <- as.Date(CCPP$dateRep)
class(CCPP$dateRep)

# Converting to a xts object daily series to preserve the date and visualise the data

Production.HPP1.xts.d <- xts(CCPP$Production.HPP1, CCPP$dateRep)
class(Production.HPP1.xts.d)
head(Production.HPP1.xts.d,10)
tail(Production.HPP1.xts.d,30)
tail(CCPP$Production.HPP1,30)
length(Production.HPP1.xts.d)

Production.HPP1.ts.d<-ts(Production.HPP1.xts.d,start=2006,frequency=365)
plot(decompose(Production.HPP1.ts.d))
plot(Production.HPP1.ts.d,ylab ="Production.HPP1")+abline(reg=lm(Production.HPP1.ts.d~time(Production.HPP1.ts.d)))


# Model 1- SARIMA model daily

train.production<-head(Production.HPP1.xts.d,2265)# 80% of observations
test.production<-tail(Production.HPP1.xts.d,566)# 20% of observation
library(tseries)
adf.test(train.production, alternative ="stationary", k=12)
tseries::kpss.test(train.production,null="Trend")

tail(train.production,1)
head(test.production,1)
sarima.f<-auto.arima(train.production)
summary(sarima.f)
forecast<-forecast(sarima.f, h=566)
forecast_dates <- seq(as.Date("2012-03-15"), length = 566, by = "day")

length(forecast_dates)
length(forecast$mean)

# forecasted values for the last 100 days
forecast_xts <- xts(forecast$mean, order.by = forecast_dates)
length(forecast_xts)
plot(test.production, main = 'Forecast Comparison')
lines(forecast_xts, col = "blue")

head(forecast_xts,100)
predicted_vs_measured<- cbind(head(forecast_xts,100),head(test.production,100))
colnames(predicted_vs_measured)<-c("Predicted values","Measured values")
predicted_vs_measured

accuracy(forecast(sarima.f,566), test.production)
checkresiduals(sarima.f)
Box.test(sarima.f$residuals)

ggdensity(sarima.f$residuals, fill = "lightgray")      
ggqqplot(sarima.f$residuals)
shapiro.test(sarima.f$residuals)# normality in residuals

# Converting to a xts object weekly series to preserve the date and visualise the data

Production.HPP1.xts.d <- xts(CCPP$Production.HPP1, CCPP$dateRep)
Production.HPP1.xts.w<-apply.weekly(Production.HPP1.xts.d, sum)
class(Production.HPP1.xts.w)
head(Production.HPP1.xts.w,10)
tail(Production.HPP1.xts.w,30)
length(Production.HPP1.xts.w)

Production.HPP1.ts.w<-ts(Production.HPP1.xts.w,start=2006,frequency=52)
plot(decompose(Production.HPP1.ts.w))
plot(Production.HPP1.ts.w,ylab ="Production.HPP1")+abline(reg=lm(Production.HPP1.ts.w~time(Production.HPP1.ts.w)))

# Model 1- SARIMA model weekly
train.production<-head(Production.HPP1.xts.w,326)# 80% of observations
test.production<-tail(Production.HPP1.xts.w,80)# 20% of observation

adf.test(train.production, alternative ="stationary", k=12)
tseries::kpss.test(train.production,null="Trend")

tail(train.production,1)
head(test.production,1)
sarima.f<-auto.arima(train.production)
summary(sarima.f)
forecast<-forecast(sarima.f, h=80)
forecast_dates <- seq(as.Date("2012-04-01"), length = 80, by = "week")

length(forecast_dates)
length(forecast$mean)

# forecasted values for the last 80 weeks
forecast_xts <- xts(forecast$mean, order.by = forecast_dates)
length(forecast_xts)
plot(test.production, main = 'Forecast Comparison')
lines(forecast_xts, col = "blue")
predicted_vs_measured<- cbind(head(forecast_xts,80),head(test.production,80))
colnames(predicted_vs_measured)<-c("Predicted weekly values","Measured weekly values")
predicted_vs_measured

accuracy(forecast$mean, test.production)
checkresiduals(sarima.f)
Box.test(sarima.f$residuals)


ggdensity(sarima.f$residuals, fill = "lightgray")      
ggqqplot(sarima.f$residuals)
shapiro.test(sarima.f$residuals)# normality in residuals

# Converting to a xts object daily series to preserve the date and visualise the data

Production.HPP1.xts.d <- xts(CCPP$Production.HPP1, CCPP$dateRep)
head(Production.HPP1.xts.d)
CCPP.Production.HPP1.xts.m<-apply.monthly(Production.HPP1.xts.d, sum)
head(CCPP.Production.HPP1.xts.m)
frequency(CCPP.Production.HPP1.xts.m)
start(CCPP.Production.HPP1.xts.m)
end(CCPP.Production.HPP1.xts.m)
plot(CCPP.Production.HPP1.xts.m)

#  Convert CCPP.Production.HPP1.xts.m to a matrix with 12 columns

head(CCPP.Production.HPP1.xts.m,10)
dateRep<-c(as.Date("2013-12-30"),as.Date("2013-11-30"))
Production.HPP1<-c(0,0)
test.df<-data.frame(dateRep,Production.HPP1)
class(test.df)
test.df$dateRep<-as.Date(test.df$dateRep)
class(test.df$dateRep)

xts1 <- xts(test.df$Production.HPP1, test.df$dateRep)
xts1
xts2<-rbind.xts(CCPP.Production.HPP1.xts.m,xts1)
xts2<-c(CCPP.Production.HPP1.xts.m,xts1)
tail(xts2)

class(xts2)
am <- matrix(xts2,nrow=12)
head(am,13)
# am[10,8] = c(rowMeans(am[,-8]))[10]
# am[11,8] = c(rowMeans(am[,-8]))[11]
# am[12,8] = c(rowMeans(am[,-8]))[12]
am[10,8] = mean(am[1:9,8])
am[11,8] = mean(am[1:9,8])
am[12,8] = mean(am[1:9,8])

class(am)

Production.HPP1.m.ts<-ts(c(am), start = 2006, freq = 12)
class(Production.HPP1.m.ts)
summary(Production.HPP1.m.ts)
head(Production.HPP1.m.ts,5)
frequency(Production.HPP1.m.ts)

plot(Production.HPP1.m.ts,ylab ="Production.HPP1")+abline(reg=lm(Production.HPP1.m.ts~time(Production.HPP1.m.ts)))
plot(diff(log(Production.HPP1.m.ts)))
plot(aggregate(Production.HPP1.m.ts,FUN = mean))
boxplot(Production.HPP1.m.ts~cycle(Production.HPP1.m.ts),outline = FALSE)
plot(decompose(Production.HPP1.m.ts))
ggmonthplot(ts(Production.HPP1.m.ts,start=2006,frequency=12),main="Production.HPP1")

# Model 1- SARIMA model
train.production<-head(Production.HPP1.m.ts,74)# 80% of observations
test.production<-Production.HPP1.m.ts[c(75:93)]# 20% of observation
library(tseries)
adf.test(train.production, alternative ="stationary", k=12)
tseries::kpss.test(train.production,null="Trend")
sarima.f<-auto.arima(ts(train.production,start=2006,frequency=12))
summary(sarima.f)
plot(forecast(sarima.f),19)

accuracy(forecast(sarima.f,19), test.production)
checkresiduals(sarima.f)
Box.test(sarima.f$residuals)

plot(forecast(sarima.f,19))
lines(ts(test.production,start=c(2012,3),frequency = 12),col="red")


ggdensity(sarima.f$residuals, fill = "lightgray")      
ggqqplot(sarima.f$residuals)
shapiro.test(sarima.f$residuals)# normality in residuals

fc<-forecast(sarima.f,19) 
fc # forecasted values for the last 19 months
ts(test.production,start=c(2012,3),frequency = 12) # real measured values from data set for the last 19 months
accuracy(fc,test.production)

# Model 2- ETS model
train.production<-head(Production.HPP1.m.ts,74)# 80% of observations
test.production<-Production.HPP1.m.ts[c(75:93)]# 20% of observation

ets.f<-ets(ts(train.production,start = 2006,frequency = 12))
summary(ets.f)
plot(forecast(ets.f,19))

accuracy(forecast(ets.f,19), test.production)
checkresiduals(ets.f)
Box.test(ets.f$residuals)

plot(forecast(ets.f,19))
lines(ts(test.production,start=c(2012,3),frequency = 12),col="red")

library(ggpubr)
ggdensity(ets.f$residuals, fill = "lightgray")      
ggqqplot(ets.f$residuals)
shapiro.test(ets.f$residuals)# normality in residuals

fc<-forecast(ets.f,19)
fc # forecasted values for the last 19 months
ts(test.production,start=c(2012,3),frequency = 12) # real measured values from data set for the last 19 months
accuracy(fc,test.production)

# TBATS model

msts.HPP1 <- msts(df$Production.HPP1, seasonal.periods = c(7,365.25),start = decimal_date(as.Date("2006-01-01")))
plot(msts.HPP1, main="Production HHP 1", xlab="Year", ylab="Daily Admissions")

tbats.HPP1 <- tbats(msts.HPP1)
plot(tbats.HPP1, main="Production.HHP1 Multiple Season Decomposition")

sp.HPP1<- predict(tbats.HPP1,h=14)
plot(sp.HPP1, main = "TBATS Forecast for HHP1", include=14)


train.HPP1 <- window(msts.HPP1,start=decimal_date(as.Date("2006-01-01")),end=c(decimal_date(as.Date("2012-08-06"))))
test.HPP1 <- window(msts.HPP1, start=c(decimal_date(as.Date("2012-08-06"))), end=c(decimal_date(as.Date("2013-10-01"))))

#accuracy test

s.HPP1 <- tbats(train.HPP1)
sp.HPP1<- predict(s.HPP1,h=14)
print(accuracy(sp.HPP1,test))
print(sp.HPP1$model)
plot(sp.HPP1, main = "TBATS Forecast for HHP1", include=100)
checkresiduals(sp.HPP1)
Box.test(sp.HPP2$residuals)
sp.HPP1
#HPP2
msts.HPP2 <- msts(df$Production.HPP2, seasonal.periods = c(7,365.25),start = decimal_date(as.Date("2006-01-01")))
plot(msts.HPP2, main="Production HHP 1", xlab="Year", ylab="Daily Admissions")

tbats.HPP2 <- tbats(msts.HPP2)
plot(tbats.HPP1, main="Production.HHP2 Multiple Season Decomposition")

sp.HPP1<- predict(tbats.HPP2,h=14)
plot(sp.HPP2, main = "TBATS Forecast for HHP2", include=14)


train.HPP2 <- window(msts.HPP2,start=decimal_date(as.Date("2006-01-01")),end=c(decimal_date(as.Date("2012-08-06"))))
test.HPP2 <- window(msts.HPP2, start=c(decimal_date(as.Date("2012-08-06"))), end=c(decimal_date(as.Date("2013-10-01"))))

#accuracy test

s.HPP2 <- tbats(train.HPP2)
sp.HPP2<- predict(s.HPP2,h=14)
print(accuracy(sp.HPP2,test))
print(sp.HPP2$model)
plot(sp.HPP2, main = "TBATS Forecast for HHP1", include=100)

#residue
checkresiduals(sp.HPP2)


Box.test(sp.HPP2$residuals)

for (i in 1:20)
{ nTest <- 14*i  
nTrain <- length(msts)- nTest 
train <- window(msts,start=decimal_date(as.Date("2012-04-01")),end=c(decimal_date(as.Date("2012-04-01")),nTrain))
test <- window(msts, start=c(decimal_date(as.Date("2012-04-01")),nTrain+1), end=c(decimal_date(as.Date("2013-10-01")),nTrain+14))

s <- tbats(train)
sp<- predict(s,h=14)

cat("----------------------------------
      
Data Partition",i,"
      
Training Set includes",nTrain," time periods. Observations 1 to", nTrain, "
Test Set includes 14 time periods. Observations", nTrain+1, "to", nTrain+14,"
      
")
print(accuracy(sp,test))
cat("
      
      ")
print(sp$model)
}


