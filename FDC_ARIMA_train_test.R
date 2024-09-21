library(zoo)
library(dplyr)
library(magrittr)
library(tidyverse)
library(lubridate)

## read in csv file of stream gages
df_gages <- read_csv("C:/Users/p739b253/OneDrive - University of Kansas/FEWtures/Bias correction_NHM/newNC.csv")

#look at column names
names(df_gages)

# get a vector of all ID numbers
all_gages <- df_gages$Site_no.
all_gages

gage <- all_gages[25]
gage

# get a vector of all ID numbers
all_gages2 <- df_gages$Segment
all_gages2

seg <- all_gages2[25]
seg
####################################################################
path_to_usgs_data <- "C:/Users/p739b253/OneDrive - University of Kansas/FEWtures/NHM_data/streamflowdata/"

# USGS data
mf <- read.csv(paste0(path_to_usgs_data, "Discharge_", gage, "_RawC.csv"))
#head(mf)

#changing the date format to standard unambigious standard
mf$Date <-mdy(mf$Date)
mf$Year <- year(mf$Date)
#mf <- subset(mf, Year >= 1980 & Year <= 2016)

mf1 <- subset(mf, Date >= "1980-10-01" & Year <= 2000)

##########################################################################

# NHM data
df <- read.csv("C:/Users/p739b253/Documents/nhm_sf.csv")
#head(df)
seg_colname <- paste0("X", seg)
df_seg <- df[,c("Date", seg_colname)]

df_seg$Date <-mdy(df_seg$Date)
df_seg$Year <- year(df_seg$Date)
#mf <- subset(mf, Year >= 1980 & Year <= 2016)

df_seg1 <- subset(df_seg, Date >= "1980-10-01" & Year <= 2000)

# rename one column name
colnames(df_seg1)[colnames(df_seg1)=="X36166"]<-"Sim"      #change seg ID

#JOIN data
df_joined <- left_join(df_seg1, mf1, by = "Date")

#converting data from daily to monthly
df_joined1 <- df_joined       #Duplicate data
df_joined1$year_month <- floor_date(df_joined1$Date, "month")  #Create year-month column

df_joined_aggr <- df_joined1 %>%   #Agrregate data
  group_by(year_month) %>%
  dplyr::summarize(Q= mean(Q)) %>%
  as.data.frame()

df_joined_aggr1 <- df_joined1 %>%   #Agrregate data
  group_by(year_month) %>%
  dplyr::summarize(Sim= mean(Sim)) %>%     
  as.data.frame()

#JOIN data again
df_joined <- left_join(df_joined_aggr, df_joined_aggr1, by = "year_month")

#change column name "year_month" to "Date"
colnames(df_joined)[1]<-"Date"

###################################################

library(EcoHydRology)

library(hydrostats)

library(hydroTSM)
library(gridExtra)

# FDC for Sim and Obs
#fdc(df_joined$Q,plot = T,lQ.thr=0.8,ylab='Q, [ft3/s]',main='Upstream monthly duration curve',thr.shw=TRUE)
probObs <- fdc(df_joined$Q,plot = T,lQ.thr=0.8,ylab='Q, [ft3/s]',main='Observed Flow Duration Curve',thr.shw=TRUE)
probSim <- fdc(df_joined$Sim,plot = T,lQ.thr=0.8,ylab='Q, [ft3/s]',main='Simulated Flow Duration Curve',thr.shw=TRUE)

df_sim_fdc <- df_joined[,c("Date", "Sim")]
df_sim_fdc$probSim <- probSim
df_sim_fdc$probSimRound <- round(df_sim_fdc$probSim, 3)
df_sim_fdc_summarize <-
  summarize(group_by(df_sim_fdc, probSimRound), Qsim_mean = mean(Sim))

#summarize(group_by(df_sim_fdc, probSimRound, Date), Qsim_mean = mean(Sim))
df_obs_fdc <- df_joined[,c("Date", "Q")]
df_obs_fdc$probObs <- probObs
df_obs_fdc$probObsRound <- round(df_obs_fdc$probObs, 3)
df_obs_fdc_summarize <-
  summarize(group_by(df_obs_fdc, probObsRound), Qobs_mean = mean(Q))

df_fdc_joined <- left_join(df_sim_fdc_summarize, df_obs_fdc_summarize, by = c("probSimRound" = "probObsRound"))

# interpolate to fill gaps in probObsRound
#interpolate missing values in 'Qobs_mean' column
df_fdc_joined$Qobs_meanInt <- na.approx(df_fdc_joined$Qobs_mean)
probObsInt <- fdc(df_fdc_joined$Qobs_meanInt,plot = T,lQ.thr=0.8,ylab='Q, [ft3/s]',main='Flow Duration Curve',thr.shw=TRUE)
df_fdc_joined$probObsInt <- probObsInt

# to correct df_joined, make a column in df_joined called probSimRound where probSim is rounded to 3 places
df_fdc_joined$probObsIntRound <- round(df_fdc_joined$probObsInt, 3)
df_corrected <- left_join(df_sim_fdc, df_fdc_joined[,c("probSimRound", "Qobs_meanInt")], by = "probSimRound")
df_corrected_with_obs <- left_join(df_corrected, df_obs_fdc[,c("Date", "Q")], by = "Date")

#change column name "Qobs_meanInt" to "SimBC"
colnames(df_corrected_with_obs)[5]<-"SimCorrected"

############## ARIMA ###############

#First, the residual series was obtained by subtracting the simulated flow from that of the observed.
df_corrected_with_obs$diff <- df_corrected_with_obs$Q - df_corrected_with_obs$SimCorrected


#checking if the dataset is a time series
class(df_corrected_with_obs)  #data is in "dataframe" form

#converting the data first to time series
df_joinedtime=ts(df_corrected_with_obs[,7],start=c(1980,10),frequency = 12)

#checking again if the dataset is a time series
class(df_joinedtime)  #data is in "ts" now

library(forecast)
library(tseries)
plot(df_joinedtime)

acf(df_joinedtime)
pacf(df_joinedtime)

adf.test(df_joinedtime)

#df_joinedModel= auto.arima(df_joinedtime,ic="aic",trace = TRUE)


#################
S = auto.arima(df_joinedtime, allowmean=FALSE, allowdrift=FALSE, trace=TRUE)
S
s <- S$residuals
s

d <- cbind(df_corrected_with_obs, s)

d1 <- transform(d, SimCorrectednew= s+SimCorrected)

SimCorrectednew1 <- d1 $SimCorrectednew                     # Duplicate data frame
SimCorrectednew1[d1 $SimCorrectednew  < 0] <- 0     # Set negative values to 0
SimCorrectednew1

#Add to dataframe
d1 <- cbind(d1, SimCorrectednew1)

library(hydroGOF)

##All Metrics
gof(sim=d1$SimCorrectednew1, obs=d1$Q)


#ANALYSIS FOR TESTING DATA

mf1 <- subset(mf, Date >= "2001-01-01" & Year <= 2016)

seg

##########################################################################

df_seg1 <- subset(df_seg, Date >= "2001-01-01" & Year <= 2016)

# rename one column name
colnames(df_seg1)[colnames(df_seg1)=="X36166"]<-"Sim"      #change seg ID

#JOIN data
df_joined <- left_join(df_seg1, mf1, by = "Date")

df_joined<- df_joined[c("Date", "Sim", "Q")]


#converting data from daily to monthly
df_joined1 <- df_joined       #Duplicate data
df_joined1$year_month <- floor_date(df_joined1$Date, "month")  #Create year-month column

df_joined1_aggr1 <- aggregate(Sim ~ year_month,
                              df_joined1,
                              FUN = mean)

df_joined1_aggr2 <- aggregate(Q ~ year_month,
                              df_joined1,
                              FUN = mean)


#JOIN data again
df_joinednew <- left_join(df_joined1_aggr1, df_joined1_aggr2, by = "year_month")
df_joinednew


# FDC for Sim and Obs
#fdc(df_joined$Q,plot = T,lQ.thr=0.8,ylab='Q, [ft3/s]',main='Upstream monthly duration curve',thr.shw=TRUE)
probObs <- fdc(df_joinednew$Q,plot = T,lQ.thr=0.8,ylab='Q, [ft3/s]',main='Observed Flow Duration Curve',thr.shw=TRUE)
probSim <- fdc(df_joinednew$Sim,plot = T,lQ.thr=0.8,ylab='Q, [ft3/s]',main='Simulated Flow Duration Curve',thr.shw=TRUE)

df_sim_fdc <- df_joinednew[,c("year_month", "Sim")]
df_sim_fdc$probSim <- probSim
df_sim_fdc$probSimRound <- round(df_sim_fdc$probSim, 3)
df_sim_fdc_summarize <-
  summarize(group_by(df_sim_fdc, probSimRound), Qsim_mean = mean(Sim))


#summarize(group_by(df_sim_fdc, probSimRound, Date), Qsim_mean = mean(Sim))
df_obs_fdc <- df_joinednew[,c("year_month", "Q")]
df_obs_fdc$probObs <- probObs
df_obs_fdc$probObsRound <- round(df_obs_fdc$probObs, 3)
df_obs_fdc_summarize <-
  summarize(group_by(df_obs_fdc, probObsRound), Qobs_mean = mean(Q))

df_fdc_joined <- left_join(df_sim_fdc_summarize, df_obs_fdc_summarize, by = c("probSimRound" = "probObsRound"))


# interpolate to fill gaps in probObsRound
#interpolate missing values in 'Qobs_mean' column
df_fdc_joined$Qobs_meanInt <- na.approx(df_fdc_joined$Qobs_mean)
probObsInt <- fdc(df_fdc_joined$Qobs_meanInt,plot = T,lQ.thr=0.8,ylab='Q, [ft3/s]',main='Flow Duration Curve',thr.shw=TRUE)
df_fdc_joined$probObsInt <- probObsInt

# to correct df_joined, make a column in df_joined called probSimRound where probSim is rounded to 3 places
df_fdc_joined$probObsIntRound <- round(df_fdc_joined$probObsInt, 3)
df_corrected <- left_join(df_sim_fdc, df_fdc_joined[,c("probSimRound", "Qobs_meanInt")], by = "probSimRound")
df_corrected_with_obs <- left_join(df_corrected, df_obs_fdc[,c("year_month", "Q")], by = "year_month")

#change column name "Qobs_meanInt" to "SimBC"
colnames(df_corrected_with_obs)[5]<-"SimCorrected"

############## ARIMA ###############

#First, the residual series was obtained by subtracting the simulated flow from that of the observed.
df_corrected_with_obs$diff <- df_corrected_with_obs$Q - df_corrected_with_obs$SimCorrected


#checking if the dataset is a time series
class(df_corrected_with_obs)  #data is in "dataframe" form

#converting the data first to time series
df_joinedtime=ts(df_corrected_with_obs[,7],start=c(1980,10),frequency = 12)

#checking again if the dataset is a time series
class(df_joinedtime)  #data is in "ts" now

library(forecast)
library(tseries)
plot(df_joinedtime)

acf(df_joinedtime)
pacf(df_joinedtime)

adf.test(df_joinedtime)

#df_joinedModel= auto.arima(df_joinedtime,ic="aic",trace = TRUE)


#################
S = auto.arima(df_joinedtime, allowmean=FALSE, allowdrift=FALSE, trace=TRUE)
S
s <- S$residuals
s

d <- cbind(df_corrected_with_obs, s)

d1 <- transform(d, SimCorrectednew= s+SimCorrected)

SimCorrectednew1 <- d1 $SimCorrectednew                     # Duplicate data frame
SimCorrectednew1[d1 $SimCorrectednew  < 0] <- 0     # Set negative values to 0
SimCorrectednew1

#Add to dataframe
d1 <- cbind(d1, SimCorrectednew1)

library(hydroGOF)

##All Metrics
gof(sim=d1$SimCorrectednew1, obs=d1$Q)


#SimCorrected vs Measured

ggplot(data=df_corrected_with_obs, mapping=aes( x = year_month))+
  geom_line(mapping = aes(y = SimCorrectednew1, color= "SimCorrected"))+
  #geom_line(mapping = aes(y = Sim, color= "Sim"))+     #change y value
  geom_line(mapping = aes(y = Q, color= "Q"))+
  #scale_x_continuous(breaks = seq(1980,2016,4))+
  scale_color_manual(labels=c('After Bias Correction', 'Measured'), values = c('SimCorrected'='red', 'Q'='black'))+
  labs(x= 'Time', y= 'Q (ft3/s)', title = 'Monthly Observations vs Simulations')+
  #theme_classic()+
  theme(text = element_text(size=15))+   #increase All font sizes
  theme(axis.text = element_text(size = 15))+ #changing axis text size
  guides(color= guide_legend(title = ''))

