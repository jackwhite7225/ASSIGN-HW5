install.packages(c("dplyr","ggplot2","olsrr","PerformanceAnalytics","forecast"))
install.packages('lubridate')
library(ggplot2)
library(dplyr)
library(PerformanceAnalytics)
library(olsrr)
library(forecast)
library(lubridate)

ETdat = read.csv("/cloud/project/ETdata.csv")

ghg = read.csv('/cloud/project/Deemer_GHG_Data.csv')

#Question 1

ghg$log.ch4 <- log(ghg$ch4+1)

ghg$co2_transf = 1/(ghg$co2 + 1000)

ghg$log.age <- log(ghg$age)
ghg$log.DIP <- log(ghg$DIP+1)
ghg$log.precip <- log(ghg$precipitation)
ghg$BorealV <- ifelse(ghg$Region == "Boreal",1,0)
ghg$TropicalV <- ifelse(ghg$Region == "Tropical",1,0)
ghg$AlpineV <- ifelse(ghg$Alpine == "yes",1,0)
ghg$HydroV <- ifelse(ghg$hydropower == "yes",1,0)

mod.full = lm(co2_transf ~ airTemp+
                 log.age+mean.depth+
                 log.DIP+
                 log.precip, data=ghg)

summary(mod.full)

#Interpret the variables for each variable to CO2 fluxes

res.full <- rstandard(mod.full)
fit.full <- fitted.values(mod.full)
qqnorm(res.full, pch=19, col="grey50")
qqline(res.full)
shapiro.test(res.full)
plot(fit.full,res.full, pch=19, col="grey50")
abline(h=0)
reg.data <- data.frame(ghg$airTemp,
                       ghg$log.age,ghg$mean.depth,
                       ghg$log.DIP,
                       ghg$log.precip)

# make a correlation matrix 
chart.Correlation(reg.data, histogram=TRUE, pch=19)

full.step <- ols_step_forward_aic(mod.full)
# view table
full.step 

full.step$model

plot(full.step)

#Question 2

ETall = ETdat %>%
  group_by(date, crop) %>%
  summarise(ET.in = mean(Ensemble.ET))

Pistachios = ETall %>%
  filter(crop == "Pistachios")

ggplot(Pistachios, aes(x=ymd(date),y=ET.in))+
  geom_point()+
  geom_line()+
  labs(x="year", y="Monthy evapotranspiration (in)")

Pistachio_ts = na.omit(ts(Pistachios$ET.in,
                  start = c(2016,1),
                  frequency = 12))

Pistachio_decompose = decompose(Pistachio_ts)

plot(Pistachio_decompose)

acf(na.omit(Pistachio_ts), lag.max = 24)

Almonds = ETall %>%
  filter(crop == "Almonds")

ggplot(Almonds, aes(x=ymd(date),y=ET.in))+
  geom_point()+
  geom_line()+
  labs(x="year", y="Monthy evapotranspiration (in)")

Almonds_ts = na.omit(ts(Almonds$ET.in,
                          start = c(2016,1),
                          frequency = 12))

Almonds_decompose = decompose(Almonds_ts)

plot(Almonds_decompose)

acf(na.omit(Almonds_ts), lag.max = 24)

Corn = ETall %>%
  filter(crop == "Corn")

ggplot(Corn, aes(x=ymd(date),y=ET.in))+
  geom_point()+
  geom_line()+
  labs(x="year", y="Monthy evapotranspiration (in)")

Corn_ts = na.omit(ts(Corn$ET.in,
                          start = c(2016,1),
                          frequency = 12))

Corn_decompose = decompose(Corn_ts)

plot(Corn_decompose)

acf(na.omit(Corn_ts), lag.max = 24)

Grapes = ETall %>%
  filter(crop == "Grapes (Table/Raisin)")

ggplot(Grapes, aes(x=ymd(date),y=ET.in))+
  geom_point()+
  geom_line()+
  labs(x="year", y="Monthy evapotranspiration (in)")

Grapes_ts = na.omit(ts(Grapes$ET.in,
                     start = c(2016,1),
                     frequency = 12))

Grapes_decompose = decompose(Grapes_ts)

plot(Grapes_decompose)

acf(na.omit(Grapes_ts), lag.max = 24)

Fallow = ETall %>%
  filter(crop == "Fallow/Idle Cropland")

ggplot(Fallow, aes(x=ymd(date),y=ET.in))+
  geom_point()+
  geom_line()+
  labs(x="year", y="Monthy evapotranspiration (in)")

Fallow_ts = na.omit(ts(Fallow$ET.in,
                     start = c(2016,1),
                     frequency = 12))

Fallow_decompose = decompose(Fallow_ts)

plot(Fallow_decompose)

acf(na.omit(Fallow_ts), lag.max = 24)

#Question 3

pacf(na.omit(Pistachio_ts))

pistachio_y = na.omit(Pistachio_ts)

model1 <- arima(pistachio_y , # data 
                order = c(1,0,0)) # first number is AR order all other numbers get a 0 to keep AR format
model1

model5 <- arima(pistachio_y , # data 
                order = c(5,0,0)) # first number is AR order all other numbers get a 0 to keep AR format
model5

AR_fit1 <- pistachio_y - residuals(model1)
AR_fit5 <- pistachio_y - residuals(model5)
plot(pistachio_y)
# plot fit
points(AR_fit1, type = "l", col = "tomato3", lty = 2, lwd=2)
points(AR_fit5, type = "l", col = "darkgoldenrod4", lty = 2, lwd=2)
legend("topleft", c("data","AR1","AR5"),
       lty=c(1,2,2), lwd=c(1,2,2), 
       col=c("black", "tomato3","darkgoldenrod4"),
       bty="n")

newPistachio <- forecast(model5)
newPistachio

newPistachioF <- data.frame(newPistachio)

# set up dates
years <- c(rep(2021,4),rep(2022,12), rep(2023,8))
month <- c(seq(9,12),seq(1,12), seq(1,8))
newPistachioF$dateF <- ymd(paste(years,"/",month,"/",1))
# make a plot with data and predictions including a prediction interval
ggplot() +
  geom_line(data = Pistachios, aes(x = ymd(date), y = ET.in))+
  xlim(ymd(Pistachios$date[1]), newPistachioF$dateF[24])+  # Plotting original data
  geom_line(data = newPistachioF, aes(x = dateF, y = Point.Forecast),
            col="red") +  # Plotting model forecasts
  geom_ribbon(data=newPistachioF, 
              aes(x=dateF,ymin=Lo.95,
                  ymax=Hi.95), fill=rgb(0.5,0.5,0.5,0.5))+ # uncertainty interval
  theme_classic()+
  labs(x="year", y="Evapotranspiration (in)")

pacf(na.omit(Corn_ts))

Corn_y = na.omit(Corn_ts)

model1 <- arima(Corn_y , # data 
                order = c(1,0,0)) # first number is AR order all other numbers get a 0 to keep AR format
model1

model5 <- arima(Corn_y , # data 
                order = c(5,0,0)) # first number is AR order all other numbers get a 0 to keep AR format
model5

AR_fit1 <- Corn_y - residuals(model1)
AR_fit5 <- Corn_y - residuals(model5)
plot(Corn_y)
# plot fit
points(AR_fit1, type = "l", col = "tomato3", lty = 2, lwd=2)
points(AR_fit5, type = "l", col = "darkgoldenrod4", lty = 2, lwd=2)
legend("topleft", c("data","AR1","AR5"),
       lty=c(1,2,2), lwd=c(1,2,2), 
       col=c("black", "tomato3","darkgoldenrod4"),
       bty="n")

newCorn <- forecast(model5)
newCorn

newCornF <- data.frame(newCorn)

# set up dates
years <- c(rep(2021,4),rep(2022,12), rep(2023,8))
month <- c(seq(9,12),seq(1,12), seq(1,8))
newCornF$dateF <- ymd(paste(years,"/",month,"/",1))
# make a plot with data and predictions including a prediction interval
ggplot() +
  geom_line(data = Corn, aes(x = ymd(date), y = ET.in))+
  xlim(ymd(Corn$date[1]), newCornF$dateF[24])+  # Plotting original data
  geom_line(data = newCornF, aes(x = dateF, y = Point.Forecast),
            col="red") +  # Plotting model forecasts
  geom_ribbon(data=newCornF, 
              aes(x=dateF,ymin=Lo.95,
                  ymax=Hi.95), fill=rgb(0.5,0.5,0.5,0.5))+ # uncertainty interval
  theme_classic()+
  labs(x="year", y="Evapotranspiration (in)")

pacf(na.omit(Fallow_ts))

Fallow_y = na.omit(Fallow_ts)

model1 <- arima(Fallow_y , # data 
                order = c(1,0,0)) # first number is AR order all other numbers get a 0 to keep AR format
model1

model5 <- arima(Fallow_y , # data 
                order = c(5,0,0)) # first number is AR order all other numbers get a 0 to keep AR format
model5

AR_fit1 <- Fallow_y - residuals(model1)
AR_fit5 <- Fallow_y - residuals(model5)
plot(Fallow_y)
# plot fit
points(AR_fit1, type = "l", col = "tomato3", lty = 2, lwd=2)
points(AR_fit5, type = "l", col = "darkgoldenrod4", lty = 2, lwd=2)
legend("topleft", c("data","AR1","AR5"),
       lty=c(1,2,2), lwd=c(1,2,2), 
       col=c("black", "tomato3","darkgoldenrod4"),
       bty="n")

newFallow <- forecast(model5)
newFallow

newFallowF <- data.frame(newFallow)

# set up dates
years <- c(rep(2021,4),rep(2022,12), rep(2023,8))
month <- c(seq(9,12),seq(1,12), seq(1,8))
newFallowF$dateF <- ymd(paste(years,"/",month,"/",1))
# make a plot with data and predictions including a prediction interval
ggplot() +
  geom_line(data = Fallow, aes(x = ymd(date), y = ET.in))+
  xlim(ymd(Fallow$date[1]), newFallowF$dateF[24])+  # Plotting original data
  geom_line(data = newFallowF, aes(x = dateF, y = Point.Forecast),
            col="red") +  # Plotting model forecasts
  geom_ribbon(data=newFallowF, 
              aes(x=dateF,ymin=Lo.95,
                  ymax=Hi.95), fill=rgb(0.5,0.5,0.5,0.5))+ # uncertainty interval
  theme_classic()+
  labs(x="year", y="Evapotranspiration (in)")
