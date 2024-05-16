install.packages("fpp2")
install.packages("urca")

setwd("C:/Users/jj_bu/Desktop/UTA/Spring 2022/ECON 4318/Semester Project/RCode")
#use tslm function instead of lm due to standard errors
library(fpp2)
library(urca)
library(dplyr)
library(readxl)
library(ivreg)
bitcoinData = read_excel("FullDatasetFinal.xlsx")
View(bitcoinData)

summary(bitcoinData)
#summary stats before first diffs
sd(bitcoinData$LogGDP,na.rm = T)

sd(bitcoinData$`Log-S&P-500-Historical-Data-Price`,na.rm = T)

sd(bitcoinData$`Log-XLK`,na.rm = T)

sd(bitcoinData$LogBitcoinHashRate,na.rm = T)

sd(bitcoinData$LogBitcoinTradeVolume,na.rm = T)

sd(bitcoinData$LogBitcoinMarketCap,na.rm = T)

sd(bitcoinData$LogNetworkingDifficulty,na.rm = T)

sd(bitcoinData$LogBitcoinPrice,na.rm = T)
#sd of nonlog below

sd(bitcoinData$`GDP(billions)`,na.rm = T)

sd(bitcoinData$`S&P-500-Historical-Data-Price`,na.rm = T)

sd(bitcoinData$`XLK-Historical-Data-Price`,na.rm = T)

sd(bitcoinData$BitcoinHashRate,na.rm = T)

sd(bitcoinData$BitcoinTradeVolume,na.rm = T)

sd(bitcoinData$BitcoinMarketCap,na.rm = T)

sd(bitcoinData$NetworkDifficulty,na.rm = T)

sd(bitcoinData$BitcoinPrice,na.rm = T)




bitcoinPlot1=ts(data.frame(bitcoinData["LogGDP"],bitcoinData[,4],bitcoinData[,6],bitcoinData[,8],bitcoinData[,10]
                          ,bitcoinData[,12],bitcoinData[,14],bitcoinData[,16],bitcoinData[,18]))
autoplot(bitcoinPlot1)

bitcoinPlot1=ts(data.frame(bitcoinData["LogGDP"],bitcoinData[,6],bitcoinData[,8],bitcoinData[,10]
                           ,bitcoinData[,12],bitcoinData[,14],bitcoinData[,16],bitcoinData[,18]))
autoplot(bitcoinPlot1)



#Null hypothesis THERE IS A UNIT ROOT
#Look for rejection of null hypothesis
#Interested in first test stat
#Compare that to critical values of tau2
#if R test stat > critical value then we fail to reject

unitRootGDP=ur.df(bitcoinData$LogGDP,type = "drift")
summary(unitRootGDP)
#rejected null hypothesis and we need to first difference the data
unitRootGDP=ur.df(diff(bitcoinData$LogGDP),type = "drift")
summary(unitRootGDP)


unitRootCPI=ur.df(bitcoinData$CPIAUCSL,type = "drift")
summary(unitRootCPI)

unitRoot500=ur.df(bitcoinData$`Log-S&P-500-Historical-Data-Price` ,type = "drift")
summary(unitRoot500)

unitRootXLK=ur.df(bitcoinData$`XLK-Historical-Data-Price`,type = "drift")
summary(unitRootXLK)



#test unit root for bit coin price
unitRootBitcoinPrice=ur.df(bitcoinData$LogBitcoinPrice,type = "drift")
summary(unitRootBitcoinPrice)
#rejected null hypothesis we must first difference EVERYTHING
unitRootBitcoinPrice=ur.df(diff(bitcoinData$LogBitcoinPrice),type = "drift")
summary(unitRootBitcoinPrice)

unitRootHashRate=ur.df(bitcoinData$LogBitcoinHashRate,type = "drift")
summary(unitRootHashRate)

unitRootTrade=ur.df(bitcoinData$LogBitcoinTradeVolume,type = "drift")
summary(unitRootTrade)

unitRootMarket=ur.df(bitcoinData$LogBitcoinMarketCap,type = "drift")
summary(unitRootMarket)

#prime superscript prime means first differences t subscript y t prime = b
bitcoinDataDiff = data.frame(matrix(ncol=8,nrow=44))
View(bitcoinDataDiff)
x = c("dlgdp","dlSP","dlxlk","dlbitcoinprice","dlhashrate","dltradevolume","dlmarketcap","dlnetworkdifficulty")
colnames(bitcoinDataDiff)=x




for(t in 2:length(bitcoinData$LogGDP)){
  
  bitcoinDataDiff$dlgdp[t]=bitcoinData$LogGDP[t]-bitcoinData$LogGDP[t-1]
  
  bitcoinDataDiff$dlSP[t]=bitcoinData$`Log-S&P-500-Historical-Data-Price`[t]-bitcoinData$`Log-S&P-500-Historical-Data-Price`[t-1]
  
  bitcoinDataDiff$dlxlk[t]=bitcoinData$`Log-XLK`[t]-bitcoinData$`Log-XLK`[t-1]
  
  bitcoinDataDiff$dlbitcoinprice[t]=bitcoinData$LogBitcoinPrice[t]-bitcoinData$LogBitcoinPrice[t-1]
  
  bitcoinDataDiff$dlhashrate[t]=bitcoinData$LogBitcoinHashRate[t]-bitcoinData$LogBitcoinHashRate[t-1]
  
  bitcoinDataDiff$dltradevolume[t]=bitcoinData$LogBitcoinTradeVolume[t]-bitcoinData$LogBitcoinTradeVolume[t-1]
  
  bitcoinDataDiff$dlmarketcap[t]=bitcoinData$LogBitcoinMarketCap[t]-bitcoinData$LogBitcoinMarketCap[t-1]
  
  bitcoinDataDiff$dlnetworkdifficulty[t]=bitcoinData$LogNetworkingDifficulty[t]-bitcoinData$LogNetworkingDifficulty[t-1]
  
  bitcoinDataDiff$dltotalbitcoin[t]=bitcoinData$LogTotalBitcoin[t]-bitcoinData$LogTotalBitcoin[t-1]
}


#Don't include CPI in the regression
#1 unit change in x 100 * beta percent change in y




#what is going on
#frequency
#turn into time series before first difference
#trend overarching changes over time represents a fixed change
#adds a variable that equals 1 in first period and 2 in next period and the affect
#show all information
y = ts(bitcoinDataDiff[,4],frequency =4,)
fit = tslm(y ~ dlgdp + dlSP + dlxlk + dlhashrate + dltradevolume 
           +dlmarketcap,data =bitcoinDataDiff)
summary(fit)

y = ts(bitcoinDataDiff[,4],frequency =4,)
fit = tslm(y ~ dlgdp + dlSP + dlxlk + dlhashrate + dltradevolume 
           +dlmarketcap + trend,data =bitcoinDataDiff)
summary(fit)

y = ts(bitcoinDataDiff[,4],frequency =4,)
fit = tslm(y ~ dlgdp + dlSP + dlxlk + dlhashrate + dltradevolume 
           +dlmarketcap+ trend+ season,data =bitcoinDataDiff)
summary(fit)


bitcoinPlot3=ts(data.frame(bitcoinDataDiff[,1],bitcoinDataDiff[,2],bitcoinDataDiff[,3],bitcoinDataDiff[,4]
                           ,bitcoinDataDiff[,5],bitcoinDataDiff[,6],bitcoinDataDiff[,7],bitcoinDataDiff[,8]))
autoplot(bitcoinPlot3)
















#Correlations, not sure if needed
bitcoinDataDiffInst2=slice(bitcoinDataDiffInst,-1)
View(bitcoinDataDiffInst2)
cor(bitcoinDataDiffInst[,2],bitcoinDataDiffInst[,4],use = "complete.obs",method = c("pearson"))

cor(bitcoinDataDiffInst[,3],bitcoinDataDiffInst[,4],use = "complete.obs",method = c("pearson"))



cor(bitcoinData[,5],bitcoinData[,9],method = c("pearson"))



#do ivreg on everything already differences

#ivreg two stage least sqaures difference, standard errors calculated incorrectly

#trend season easiy to create

#create trend variable and season dummies

#trend = period that it is

#season = 1 have season 2 3 4

#y value is logged trend and season not
#trend multiply by 100

#season multiply by 100


#we need to correct for market price using 

# high r^2 does not equal better model, first model is theoritically flawed
# 2nd model is better due to lack of endogenieity problem.

#Summary stats
#write in sci notation
#NEED min and max
#dont need log variable

#adding manual trend and season variables


#Creating Dataset for ivreg includign dummy variables for trend and season
write.csv(bitcoinDataDiff,"C:\\Users\\jj_bu\\Desktop\\UTA\\Spring 2022\\ECON 4318\\DataDiff.csv",row.names=FALSE)
library(readr)
bitcoinDataDiff2 <- read_csv("DataDiff.csv")
View(bitcoinDataDiff2)

bitcoinIVreg = ivreg(dlbitcoinprice ~ dlmarketcap + dlgdp + dlSP + dlxlk+  dlhashrate
                     +dltradevolume+trend+season2+season3+season4  
                     | dlnetworkdifficulty + dlhashrate + dlgdp + dlSP + dlxlk
                     +dltradevolume+trend+season2+season3+season4,data=bitcoinDataDiff2)
summary(bitcoinIVreg)

bitcoinlm = lm(dlbitcoinprice ~  dlgdp + dlSP + dlxlk
                 +dltradevolume,data=bitcoinDataDiff2)
summary(bitcoinlm)

y = ts(bitcoinDataDiff2[,4],frequency =4,)
bitcointslm = tslm(y ~  dlgdp + dlSP + dlxlk
               +dltradevolume+trend+season,data=bitcoinDataDiff2)
summary(bitcointslm)

#drop market cap

#include just S&P, just XLK, model with both S&P and XLK, market controls without market cap, 5th model with trend and season, try to stay on same table


#use table 2 as an example in prof paper to put all models on one table.
#() standard error

#can use word " specification" 

#explain market cap, believed there was an endogeneity problem , could not find sufficient 


#Final 5 Models

#Just S&P 
#TSLM and LM the same
 
bitcoinlmSP = lm(dlbitcoinprice ~  dlSP, data=bitcoinDataDiff2)
summary(bitcoinlmSP)

y = ts(bitcoinDataDiff2[,4],frequency =4,)
bitcointslmSPT = tslm(y ~  dlSP,data=bitcoinDataDiff2)
summary(bitcointslmSPT)


#Just XLK
bitcoinlmXLK = lm(dlbitcoinprice ~  dlxlk, data=bitcoinDataDiff2)
summary(bitcoinlmXLK)

#XLK AND S&P
bitcoinlmSPXLK = lm(dlbitcoinprice ~  dlSP+ dlxlk, data=bitcoinDataDiff2)
summary(bitcoinlmSPXLK)

#All Controls 
bitcoinlmCONTROL = lm(dlbitcoinprice ~  dlSP+ dlxlk+ dlgdp+ dlhashrate+dltradevolume, data=bitcoinDataDiff2)
summary(bitcoinlmCONTROL)

#All controls with trend + season
bitcoinlmALL = lm(dlbitcoinprice ~  dlSP+ dlxlk+ dlgdp+ dlhashrate+dltradevolume + trend + season2 + season3 + season4, data=bitcoinDataDiff2)
summary(bitcoinlmALL)

y = ts(bitcoinDataDiff2[,4],frequency =4,)
bitcointslm = tslm(y ~   dlSP + dlxlk + dlgdp + dlhashrate
                   +dltradevolume+trend+season,data=bitcoinDataDiff2)
summary(bitcointslm)




