# install packages
install.packages("tidyverse")
install.packages("ggfortify")
install.packages("lubridate")
install.packages("plotly")
install.packages("RMySQL")
install.packages("Hmisc")
install.packages("backports")
install.packages("cellranger")
install.packages("labeling")
install.packages("forecast")
install.packages("tibbletime")

# calling libraries
library(tidyverse)
library(tidyr)
library(ggfortify)
library(dplyr)
library(lubridate)
library(plotly)
library(RMySQL)
library(Hmisc)
library(forecast)
library(tibbletime)

# create an SQL database connection 
con = dbConnect(MySQL(), user='deepAnalytics', password='Sqltask1234!', 
                dbname='dataanalytics2018', 
                host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

# list the tables contained in the database  & attributes contained in a table
dbListTables(con)
dbListFields(con,'yr_2006')

# build list of tables from query, Use attribute names to specify specific attributes for download
tableList <- dbListTables(con)
tableList <- tableList[2:6]
dfList <- lapply(tableList, function(t) 
  dbGetQuery(con, paste0("SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM ",  t)))

# name each element to corresponding table name
dfList <- setNames(dfList, tableList)
                 
# examine each dataset
lapply(dfList, function(f){
  str(f)
  summary(f)
  head(f)
  tail(f)
  range(f$Date)
})

# combine tables into one dataframe using dplyr
newDF <- bind_rows(dfList[2:4])
str(newDF)
summary(newDF)
head(newDF)
tail(newDF)
range(newDF$Date)

# combine Date and Time attribute values in a new attribute column
newDF <-cbind(newDF,paste(newDF$Date,newDF$Time), stringsAsFactors=FALSE)
# give the new attribute in the 6th column a header name 
colnames(newDF)[6] <-"DateTime"
# move the DateTime attribute within the dataset
newDF <- newDF[,c(ncol(newDF), 1:(ncol(newDF)-1))]
head(newDF)

# convert DateTime from character to POSIXct 
newDF$DateTime <- as.POSIXct(newDF$DateTime, "%Y/%m/%d %H:%M:%S")
# add the time zone
attr(newDF$DateTime, "tzone") <- "Europe/Paris"
# inspect the data types
str(newDF)

# break DateTime attribute into separate attributes with lubridate
newDF$year <- year(newDF$DateTime)
newDF$quarter <- quarter(newDF$DateTime)
newDF$month <- month(newDF$DateTime)
newDF$week <- week(newDF$DateTime)
newDF$weekday <- weekdays(newDF$DateTime)
newDF$day <- day(newDF$DateTime)
newDF$hour <- hour(newDF$DateTime)
newDF$minute <- minute(newDF$DateTime)

# review basic statistics of submetering parameters
summary(newDF[4:6])
sapply(newDF[4:6], sd, na.rm=TRUE) 
sapply(newDF[4:6], var, na.rm=TRUE) 
sapply(newDF[4:6], quantile, na.rm=TRUE) 
sapply(newDF[4:6], range, na.rm=TRUE) 
describe(newDF)


# plot all of sub-meter 1
plot(newDF$Sub_metering_1)

# subset the second week of 2008 & plot submeter 1 - all Observations
houseWeek2 <- filter(newDF, year == 2008 & week == 2)
plot(houseWeek2$Sub_metering_1)

# subset the 9th day of January 2008 & plot submeter 1 - all observations
houseDay <- filter(newDF, year == 2008 & month == 1 & day == 9)
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, type = 'scatter', mode = 'lines')

# plot sub-meter 1, 2 and 3 with title, legend and labels - all observations 
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

# subset the 9th day of January 2008 & plot submeter 1 - 10 Minute frequency
houseDay10 <- filter(newDF, year == 2008 & month == 1 & day == 9 & (minute == 0 | minute == 10 | minute == 20 | minute == 30 | minute == 40 | minute == 50))
# Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseDay10, x = ~houseDay10$DateTime, y = ~houseDay10$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))


# plot sub-meter 1, 2 and 3 with title, legend and labels for week 2, 2018 - all observations 
plot_ly(houseWeek2, x = ~houseWeek2$DateTime, y = ~houseWeek2$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeek2$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeek2$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption Week 2, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

# subset week 2 of 2008 & plot - 2-hour frequency
houseWeek2hour <- filter(newDF, year == 2008 & week == 2 & 
                           (hour == 0 |  hour == 2 | hour == 4 | hour == 6 | hour == 8 | hour == 10 | hour == 12 | 
                              hour == 14 | hour == 16 | hour == 18 | hour == 20 | hour == 22 | hour ==24))
plot_ly(houseWeek2hour, x = ~houseWeek2hour$DateTime, y = ~houseWeek2hour$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeek2hour$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeek2hour$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption Week 2, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))


# subset Jan 2008 & plot - all Observations
houseMonth1 <- filter(newDF, year == 2008 & month == 1)
plot_ly(houseMonth1, x = ~houseMonth1$DateTime, y = ~houseMonth1$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseMonth1$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseMonth1$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption Jan. 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))


# subset Jan of 2008 & plot - 4-hout frequency
houseMonth1 <- as_tbl_time(houseMonth1, DateTime)
houseMonth1Day <- as_period(houseMonth1, "4 hours")
plot_ly(houseMonth1Day, x = ~houseMonth1Day$DateTime, y = ~houseMonth1Day$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseMonth1Day$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseMonth1Day$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption Jan. 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))


# Subset 2008 & plot
houseYear <- as_tbl_time(filter(newDF, year == 2008), DateTime)
houseYearDay<- as_period(houseYear, "1 day")
plot_ly(houseYearDay, x = ~houseYearDay$DateTime, y = ~houseYearDay$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseYearDay$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseYearDay$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption - 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))


# pie chart for power usage from 6am to 12pm on Jan. 9, 2008
houseAM <- filter(newDF, year == 2008 & month == 1 & day == 9 & (hour > 6 & hour < 12))

## pie chart using plotly
clr <- c("orange", "green", "blue")

houseAMsum <- data.frame("Name"=c("Kitchen", "Laundry Room", "AC/Water Heater"),
                         "Data"=c(sum(houseAM[4]), sum(houseAM[5]), sum(houseAM[6])))
plot_ly(houseAMsum, labels = ~Name, values = ~Data, type = 'pie', marker = list(colors = clr),
        textposition = 'inside',
        textinfo = 'label+percent'
) %>%
  
  layout(title = 'Pie Chart of Power Usage from 6am-12pm on Jan. 9, 2008',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


# pie chart for power usage from 12pm to 6 pm on Jan. 9, 2008
housePM <- filter(newDF, year == 2008 & month == 1 & day == 9 & (hour > 12 & hour < 18))
housePMsum <- data.frame("Name"=c("Kitchen", "Laundry Room", "AC/Water Heater"), 
                         "Data"=c(sum(housePM[4]), sum(housePM[5]), sum(housePM[6])))
plot_ly(housePMsum, labels = ~Name, values = ~Data, type = 'pie', marker = list(colors = clr),
        textposition = 'inside',
        textinfo = 'label+percent'
) %>%
  
  layout(title = 'Pie Chart of Power Usage from 12pm-6pm on Jan. 9, 2008',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

# pie chart for power usage from 6pm to midnight on Jan. 9, 2008
houseEve <- filter(newDF, year == 2008 & month == 1 & day == 9 & (hour > 18 & hour < 24))
houseEvesum <- data.frame("Name"=c("Kitchen", "Laundry Room", "AC/Water Heater"), 
                          "Data"=c(sum(houseEve[4]), sum(houseEve[5]), sum(houseEve[6])))
plot_ly(houseEvesum, labels = ~Name, values = ~Data, type = 'pie', marker = list(colors = clr),
        textposition = 'inside',
        textinfo = 'label+percent'
) %>%
  
  layout(title = 'Pie Chart of Power Usage from 6pm to Midnight on Jan. 9, 2008',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


# summarize total power usage by sub-meter for Jan. 9, 2008
sumDay <- data.frame("Name"=c("Kitchen", "Laundry Room", "AC/Water Heater"), 
                     "Data"=c(sum(houseDay$Sub_metering_1), sum(houseDay$Sub_metering_2), sum(houseDay$Sub_metering_3)))
plot_ly(sumDay, labels = ~Name, values = ~Data, type = 'pie', marker = list(colors = clr),
        textposition = 'inside',
        textinfo = 'label+percent'
) %>%
  
  layout(title = 'Pie Chart of Power Usage on Jan. 9, 2008',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

# summarize total power usage by sub-meter for Week 2, 2008
sumWeek <- data.frame("Name"=c("Kitchen", "Laundry Room", "AC/Water Heater"), 
                      "Data"=c(sum(houseWeek2$Sub_metering_1), sum(houseWeek2$Sub_metering_2), sum(houseWeek2$Sub_metering_3)))
plot_ly(sumWeek, labels = ~Name, values = ~Data, type = 'pie', marker = list(colors = clr),
        textposition = 'inside',
        textinfo = 'label+percent'
) %>%
  
  layout(title = 'Pie Chart of Power Usage for Week 2, 2008',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

# summarize total power usage by sub-meter for Jan. 2008
sumMonth <- data.frame("Name"=c("Kitchen", "Laundry Room", "AC/Water Heater"), 
                       "Data"=c(sum(houseMonth1$Sub_metering_1), sum(houseMonth1$Sub_metering_2), sum(houseMonth1$Sub_metering_3)))
plot_ly(sumMonth, labels = ~Name, values = ~Data, type = 'pie', marker = list(colors = clr),
        textposition = 'inside',
        textinfo = 'label+percent'
) %>%
  
  layout(title = 'Pie Chart of Power Usage for Jan. 2008',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


##summarize total power usage by sub-meter for 2008 by Quarter
houseQ1 <- filter(newDF, year == 2008 & quarter == 1)
sumQ1 <- data.frame("Name"=c("Kitchen", "Laundry Room", "AC/Water Heater"), 
                    "Data"=c(sum(houseQ1$Sub_metering_1), sum(houseQ1$Sub_metering_2), sum(houseQ1$Sub_metering_3)))
plot_ly(sumQ1, labels = ~Name, values = ~Data, type = 'pie', marker = list(colors = clr),
        textposition = 'inside',
        textinfo = 'label+percent'
) %>%
  
  layout(title = 'Pie Chart of Power Usage for Q1 2008',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


houseQ2 <- filter(newDF, year == 2008 & quarter == 2)
sumQ2 <- data.frame("Name"=c("Kitchen", "Laundry Room", "AC/Water Heater"), 
                    "Data"=c(sum(houseQ2$Sub_metering_1), sum(houseQ2$Sub_metering_2), sum(houseQ2$Sub_metering_3)))
plot_ly(sumQ2, labels = ~Name, values = ~Data, type = 'pie', marker = list(colors = clr),
        textposition = 'inside',
        textinfo = 'label+percent'
) %>%
  
  layout(title = 'Pie Chart of Power Usage for Q2 2008',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


houseQ3 <- filter(newDF, year == 2008 & quarter == 3)
sumQ3 <- data.frame("Name"=c("Kitchen", "Laundry Room", "AC/Water Heater"), 
                    "Data"=c(sum(houseQ3$Sub_metering_1), sum(houseQ3$Sub_metering_2), sum(houseQ3$Sub_metering_3)))
plot_ly(sumQ3, labels = ~Name, values = ~Data, type = 'pie', marker = list(colors = clr),
        textposition = 'inside',
        textinfo = 'label+percent'
) %>%
  
  layout(title = 'Pie Chart of Power Usage for Q3 2008',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


houseQ4 <- filter(newDF, year == 2008 & quarter == 4)
sumQ4 <- data.frame("Name"=c("Kitchen", "Laundry Room", "AC/Water Heater"), 
                    "Data"=c(sum(houseQ4$Sub_metering_1), sum(houseQ4$Sub_metering_2), sum(houseQ4$Sub_metering_3)))
plot_ly(sumQ4, labels = ~Name, values = ~Data, type = 'pie', marker = list(colors = clr),
        textposition = 'inside',
        textinfo = 'label+percent'
) %>%
  
  layout(title = 'Pie Chart of Power Usage for Q4 2008',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))



# summarize total power usage by sub-meter for 2007
house2007 <- filter(newDF, year == 2007)
sum2007 <- data.frame("Name"=c("Kitchen", "Laundry Room", "AC/Water Heater"), 
                      "Data"=c(sum(house2007$Sub_metering_1), sum(house2007$Sub_metering_2), sum(house2007$Sub_metering_3)))
plot_ly(sum2007, labels = ~Name, values = ~Data, type = 'pie', marker = list(colors = clr),
        textposition = 'inside',
        textinfo = 'label+percent'
) %>%
  
  layout(title = 'Pie Chart of Power Usage for 2007',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


# summarize total power usage by sub-meter for 2008
house2008 <- filter(newDF, year == 2008)
sum2008 <- data.frame("Name"=c("Kitchen", "Laundry Room", "AC/Water Heater"), 
                      "Data"=c(sum(house2008$Sub_metering_1), sum(house2008$Sub_metering_2), sum(house2008$Sub_metering_3)))
plot_ly(sum2008, labels = ~Name, values = ~Data, type = 'pie', marker = list(colors = clr),
        textposition = 'inside',
        textinfo = 'label+percent'
) %>%
  
  layout(title = 'Pie Chart of Power Usage for 2008',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


# summarize total power usage by sub-meter for 2009
house2009 <- filter(newDF, year == 2009)
sum2009 <- data.frame("Name"=c("Kitchen", "Laundry Room", "AC/Water Heater"), 
                      "Data"=c(sum(house2009$Sub_metering_1), sum(house2009$Sub_metering_2), sum(house2009$Sub_metering_3)))
plot_ly(sum2009, labels = ~Name, values = ~Data, type = 'pie', marker = list(colors = clr),
        textposition = 'inside',
        textinfo = 'label+percent'
) %>%
  
  layout(title = 'Pie Chart of Power Usage for 2009',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


# Subset to one observation per week on Mondays at 8:00pm for 2007, 2008 and 2009
house070809weekly3 <- filter(newDF, weekday == "Monday" & hour == 20 & minute == 1)
# Subset to one observation per week on Tuesdays at 12:00pm for 2007, 2008 and 2009
house070809weekly2 <- filter(newDF, weekday == "Tuesday" & (hour == 12 | hour == 17) & minute == 1)
# Subset to one observation per week on Tuesdays at 5:30pm for 2007, 2008 and 2009
house070809weekly1 <- filter(newDF, weekday == "Tuesday" & hour == 17 & 
                               (minute == 1 | minute == 31))

# Create TS object with SubMeter3
tsSM3_070809weekly <- ts(house070809weekly3$Sub_metering_3, frequency=52, start=c(2007,1))
tsSM2_070809weekly <- ts(house070809weekly2$Sub_metering_2, frequency=104, start=c(2007,1))
tsSM1_070809weekly <- ts(house070809weekly1$Sub_metering_1, frequency=104, start=c(2007,1))

# Plot sub-meter 3 with autoplot 
autoplot(tsSM3_070809weekly)
# Plot sub-meter 3 with autoplot - add labels, color
autoplot(tsSM3_070809weekly, ts.colour = 'red', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 3")
autoplot(tsSM2_070809weekly, ts.colour = 'red', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 3")
autoplot(tsSM1_070809weekly, ts.colour = 'red', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 3")
# Plot sub-meter 3 with plot.ts
plot.ts(tsSM3_070809weekly)
plot.ts(tsSM2_070809weekly)
plot.ts(tsSM1_070809weekly)


# Forecasting Time Series
# Apply time series linear regression to the sub-meter 3 ts object and use summary to obtain R2 and RMSE from the model you built
fitSM3 <- tslm(tsSM3_070809weekly ~ trend + season) 
summary(fitSM3)

fitSM2 <- tslm(tsSM2_070809weekly ~ trend + season) 
summary(fitSM2)

fitSM1 <- tslm(tsSM1_070809weekly ~ trend + season) 
summary(fitSM1)

# Create the forecast for sub-meter 3. Forecast ahead 20 time periods 
forecastfitSM3 <- forecast(fitSM3, h=20)
# Plot the forecast for sub-meter 3. 
plot(forecastfitSM3)

# Create sub-meter 3 forecast with confidence levels 80 and 90
forecastfitSM3c <- forecast(fitSM3, h=52, level=c(80,90))
forecastfitSM2c <- forecast(fitSM2, h=104, level=c(80,90))
forecastfitSM1c <- forecast(fitSM1, h=156, level=c(80,90))
# Plot sub-meter 3 forecast, limit y and add labels
plot(forecastfitSM3c, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time", sub="AC/Water Heater")
plot(forecastfitSM2c, ylim = c(0, 40), ylab= "Watt-Hours", xlab="Time", sub="Laundry Room")
plot(forecastfitSM1c, ylim = c(0, 40), ylab= "Watt-Hours", xlab="Time", sub="Kitchen")

#  calculate a correlogram of the in-sample forecast errors 
acf(forecastfitSM3c$residuals)
acf(forecastfitSM2c$residuals)
acf(forecastfitSM1c$residuals)
# test whether there is signiﬁcant evidence for non-zero correlations at lags 1-20
Box.test(forecastfitSM3c$residuals, lag=20, type="Ljung-Box")
Box.test(forecastfitSM2c$residuals, lag=20, type="Ljung-Box")
Box.test(forecastfitSM1c$residuals, lag=20, type="Ljung-Box")
# check whether the forecast errors are normally distributed with mean zero and constant variance
plot.ts(forecastfitSM3c$residuals)
plot.ts(forecastfitSM2c$residuals)
plot.ts(forecastfitSM1c$residuals)
#  check whether the forecast errors are normally distributed with mean zero
plotForecastErrors <- function(forecasterrors) { 
  # make a histogram of the forecast errors: 
  mybinsize <- IQR(forecasterrors)/4 
  mysd <- sd(forecasterrors) 
  mymin <- min(forecasterrors) - mysd*5 
  mymax <- max(forecasterrors) + mysd*3 
  # generate normally distributed data with mean 0 and standard deviation mysd 
  mynorm <- rnorm(10000, mean=0, sd=mysd) 
  mymin2 <- min(mynorm) 
  mymax2 <- max(mynorm) 
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed, data overlaid: 
  mybins <- seq(mymin, mymax, mybinsize) 
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins) 
  # freq=FALSE ensures the area under the histogram = 1 
  # generate normally distributed data with mean 0 and standard deviation mysd 
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins) 
  # plot the normal curve as a blue line on top of the histogram of forecasterrors: 
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2) 
}
plotForecastErrors(forecastfitSM3c$residuals)
plotForecastErrors(forecastfitSM2c$residuals)
plotForecastErrors(forecastfitSM1c$residuals)


# DECOMPOSING SEASONED TIME SERIES
# Decompose Sub-meter 3 into trend, seasonal and remainder
components070809SM3weekly <- decompose(tsSM3_070809weekly)
# Plot decomposed sub-meter 3 
plot(components070809SM3weekly)
# Check summary statistics for decomposed sub-meter 3 
summary(components070809SM3weekly)

# Decompose Sub-meter 2 into trend, seasonal and remainder
components070809SM2weekly <- decompose(tsSM2_070809weekly)
# Plot decomposed sub-meter 2
plot(components070809SM2weekly)
# Check summary statistics for decomposed sub-meter 3 
summary(components070809SM2weekly)

# Decompose Sub-meter 1 into trend, seasonal and remainder
components070809SM1weekly <- decompose(tsSM1_070809weekly)
# Plot decomposed sub-meter 1
plot(components070809SM1weekly)
# Check summary statistics for decomposed sub-meter 3 
summary(components070809SM1weekly)

# REMOVE SEASONAL COMPONENT - HOLT WINTERS FORECASTING
# Seasonal adjusting sub-meter 3 by subtracting the seasonal component & plot
tsSM3_070809Adjusted <- tsSM3_070809weekly - components070809SM3weekly$seasonal
autoplot(tsSM3_070809Adjusted)
# Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(tsSM3_070809Adjusted))
# Holt Winters Exponential Smoothing & Plot
tsSM3_HW070809 <- HoltWinters(tsSM3_070809Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM3_HW070809, ylim = c(0, 25))
# HoltWinters forecast & plot
tsSM3_HW070809for <- forecast(tsSM3_HW070809, h=52)
plot(tsSM3_HW070809for, ylim = c(0, ), ylab= "Watt-Hours", xlab="Time - Sub-meter 3")
# Forecast HoltWinters with diminished confidence levels
tsSM3_HW070809forC <- forecast(tsSM3_HW070809, h=52, level=c(10,25))
# Plot only the forecasted area
plot(tsSM3_HW070809forC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3", start(2010))

# Seasonal adjusting sub-meter 2 by subtracting the seasonal component & plot
tsSM2_070809Adjusted <- tsSM2_070809weekly - components070809SM2weekly$seasonal
autoplot(tsSM2_070809Adjusted)
# Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(tsSM2_070809Adjusted))
# Holt Winters Exponential Smoothing & Plot
tsSM2_HW070809 <- HoltWinters(tsSM2_070809Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM2_HW070809, ylim = c(0, 25))
# HoltWinters forecast & plot
tsSM2_HW070809for <- forecast(tsSM2_HW070809, h=104)
plot(tsSM2_HW070809for, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 2")
# Forecast HoltWinters with diminished confidence levels
tsSM2_HW070809forC <- forecast(tsSM2_HW070809, h=104, level=c(10,25))
# Plot only the forecasted area
plot(tsSM2_HW070809forC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 2", start(2010))

# Seasonal adjusting sub-meter 1 by subtracting the seasonal component & plot
tsSM1_070809Adjusted <- tsSM1_070809weekly - components070809SM1weekly$seasonal
autoplot(tsSM1_070809Adjusted)
# Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(tsSM1_070809Adjusted))
# Holt Winters Exponential Smoothing & Plot
tsSM1_HW070809 <- HoltWinters(tsSM1_070809Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM1_HW070809, ylim = c(0, 25))
# HoltWinters forecast & plot
tsSM1_HW070809for <- forecast(tsSM1_HW070809, h=104)
plot(tsSM1_HW070809for, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 1")
# Forecast HoltWinters with diminished confidence levels
tsSM1_HW070809forC <- forecast(tsSM1_HW070809, h=104, level=c(10,25))
# Plot only the forecasted area
plot(tsSM1_HW070809forC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 1", start(2010))

#  calculate a correlogram of the in-sample forecast errors 
acf(tsSM3_HW070809for$residuals, lag.max=25, na.action=na.pass)
acf(tsSM2_HW070809for$residuals, lag.max=25, na.action=na.pass)
acf(tsSM1_HW070809for$residuals, lag.max=25, na.action=na.pass)
# test whether there is signiﬁcant evidence for non-zero correlations at lags 1-25
Box.test(tsSM3_HW070809for$residuals, lag=25, type="Ljung-Box")
Box.test(tsSM2_HW070809for$residuals, lag=25, type="Ljung-Box")
Box.test(tsSM1_HW070809for$residuals, lag=25, type="Ljung-Box")
# check whether the forecast errors are normally distributed with mean zero and constant variance
plot.ts(tsSM3_HW070809for$residuals)
plot.ts(tsSM2_HW070809for$residuals)
plot.ts(tsSM1_HW070809for$residuals)
#  check whether the forecast errors are normally distributed with mean zero
plotForecastErrors <- function(forecasterrors) { 
  # make a histogram of the forecast errors: 
  mybinsize <- IQR(forecasterrors)/4 
  mysd <- sd(forecasterrors) 
  mymin <- min(forecasterrors) - mysd*5 
  mymax <- max(forecasterrors) + mysd*3 
  # generate normally distributed data with mean 0 and standard deviation mysd 
  mynorm <- rnorm(10000, mean=0, sd=mysd) 
  mymin2 <- min(mynorm) 
  mymax2 <- max(mynorm) 
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed, data overlaid: 
  mybins <- seq(mymin, mymax, mybinsize) 
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins) 
  # freq=FALSE ensures the area under the histogram = 1 
  # generate normally distributed data with mean 0 and standard deviation mysd 
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins) 
  # plot the normal curve as a blue line on top of the histogram of forecasterrors: 
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2) 
}
plotForecastErrors(na.exclude(tsSM3_HW070809for$residuals))
plotForecastErrors(na.exclude(tsSM2_HW070809for$residuals))
plotForecastErrors(na.exclude(tsSM1_HW070809for$residuals))
