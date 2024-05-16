data = read.csv("Phishing_Legitimate_full.csv")

legit_df <- subset(data, CLASS_LABEL == 0)
phish_df <- subset(data, CLASS_LABEL == 1)

hist(legit_df$UrlLength, main="Safe URL Length")
hist(phish_df$UrlLength, main="Phishing URL Length")

pacf(legit_df$UrlLength) 
pacf(phish_df$UrlLength) 

## assess dist for legit websites
library(fitdistrplus)
descdist(legit_df$UrlLength, discrete = TRUE)

fit.nbinom = fitdist(legit_df$UrlLength, "nbinom")
summary(fit.nbinom) ### size = 6.25 ### mu = 72.75

fit.pois = fitdist(legit_df$UrlLength, "pois")
summary(fit.pois)

gofstat(fit.nbinom) ## fail to reject NegBinomial distribution


## assess dist for phishing websites
descdist(phish_df$UrlLength, discrete = TRUE)

fit.nbinom2 = fitdist(phish_df$UrlLength, "nbinom") # greatest log likelihood
summary(fit.nbinom2)

fit.pois2 = fitdist(phish_df$UrlLength, "pois")
summary(fit.pois2)

gofstat(fit.nbinom2) # p-value = 0 



## assess dist for phishing websites - P value was 0 so now try continuous
descdist(phish_df$UrlLength, discrete = FALSE)

fit.gamma = fitdist(phish_df$UrlLength, "gamma") # greatest log likelihood
summary(fit.gamma)

fit.exp = fitdist(phish_df$UrlLength, "exp")
summary(fit.exp)

fit.weibull = fitdist(phish_df$UrlLength, "weibull")
summary(fit.weibull)

plot(fit.gamma) # lol

g <- gofstat(fit.gamma)
g$chisqpvalue # still 0 

## shape = 5.17
## rate = 0.0762




