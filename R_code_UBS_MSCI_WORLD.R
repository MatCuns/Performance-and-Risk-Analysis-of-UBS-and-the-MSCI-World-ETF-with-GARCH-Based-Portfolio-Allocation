#####SETUP#####

library(quantmod)

#Downloading tickers:UBSG.SW and IWDC.SW from yahoo

getSymbols("UBSG.SW", src="yahoo")
getSymbols("IWDC.SW",src="yahoo")

# Renaming the Dataset and removing not necessary columns, 
# after checking the columns name i keep only the adjusted prices
# to account for stock splits, dividends and reverse split.

UBS = UBSG.SW
rm(UBSG.SW)
colnames(UBS)
UBS <- UBS[, -c(1:5)]

IWDC = IWDC.SW
rm(IWDC.SW)
colnames(IWDC)
IWDC <- IWDC[,-c(1:5)]

#setting start and finish date
IWDC <- IWDC["2015-01-01/2025-12-31"]
UBS  <- UBS["2015-01-01/2025-12-31"]

tail(UBS)
tail(IWDC)

#checking if all the dates are the same
all(index(IWDC) == index(UBS))
#They are not the same so we keep only the same dates 
common_dates <- intersect(index(IWDC), index(UBS))
IWDC <- IWDC[common_dates]
UBS  <- UBS[common_dates]
# Last check
all(index(IWDC) == index(UBS))
rm(common_dates)
#given TRUE output, we now have the dataset with just the common dates

#plotting the adj. prices
plot(
  UBS,
  col = "red",
  lwd = 2,
  main = "UBS Group AG Adjusted Price (2015–2025)",
  ylab = "Price in CHF",
  xlab = "Date"
)

grid()

plot(
  IWDC,
  col = "blue",
  lwd = 2,
  main = "IWDC Adjusted Price (2015–2025)",
  ylab = "Price in CHF",
  xlab = "Date"
)

grid()

#Plotting the normalized prices together
plot(
  UBS/UBS[[1]],
  col = "red",
  lwd = 2,
  main = "Normalized Prices: UBS vs IWDC (2015–2025)",
  ylab = "Normalized Price (Start = 1)",
  xlab = "Date"
)

grid()

lines(
  IWDC/IWDC[[1]],
  col = "blue",
  lwd = 2
)

#calculating returns
UBS_returns=dailyReturn(UBS,type="log",leading=TRUE)
IWDC_returns=dailyReturn(IWDC,type="log",leading=TRUE)

#plotting returns
plot(UBS_returns,col="orange",main="UBS log Returns")
plot(IWDC_returns,col="black",main="IWDC log Returns")

#together
plot(
  UBS_returns,
  col="orange",
  lwd=2,
  main="Daily Returns: UBS vs MSCI World CHF ETF",
  ylab="Log Returns",
  xlab="Date"
)

grid()

lines(
  IWDC_returns,
  col="black",
  lwd=2
)



##### DESCRIPTIVE STATISTICS #####

#mean daily return
UBS_mean_returns=mean(UBS_returns)
IWDC_mean_returns=mean(IWDC_returns)
print(paste("UBS mean return -->",UBS_mean_returns))
print(paste("IWDC mean return -->",IWDC_mean_returns))


#standard deviation
UBS_sd=sd(UBS_returns)
IWDC_sd=sd(IWDC_returns)
print(paste("UBS standard deviation -->",UBS_sd))
print(paste("IWDC standard deviation -->",IWDC_sd))


#annualized volatility
UBS_ann_vol=UBS_sd*sqrt(252)
IWDC_ann_vol=IWDC_sd*sqrt(252)
print(paste("UBS annualized volatility -->",UBS_ann_vol))
print(paste("IWDC annualized volatility -->",IWDC_ann_vol))


#annualized return linear
UBS_ann_return=UBS_mean_returns*252
IWDC_ann_return=IWDC_mean_returns*252
print(paste("UBS annualized return -->",UBS_ann_return))
print(paste("IWDC annualized return -->",IWDC_ann_return))


#annualized return compounded
UBS_ann_return_comp=prod(1+UBS_returns)^(252/length(UBS_returns))-1
IWDC_ann_return_comp=prod(1+IWDC_returns)^(252/length(IWDC_returns))-1
print(paste("UBS annualized return compounded -->",UBS_ann_return_comp))
print(paste("IWDC annualized return compounded -->",IWDC_ann_return_comp))

#min and max returns with dates
UBS_min=min(UBS_returns)
IWDC_min=min(IWDC_returns)
UBS_max=max(UBS_returns)
IWDC_max=max(IWDC_returns)

UBS_min_date=index(UBS_returns)[which.min(UBS_returns)]
IWDC_min_date=index(IWDC_returns)[which.min(IWDC_returns)]
UBS_max_date=index(UBS_returns)[which.max(UBS_returns)]
IWDC_max_date=index(IWDC_returns)[which.max(IWDC_returns)]

print(paste("UBS min return -->",UBS_min,"on",UBS_min_date))
print(paste("IWDC min return -->",IWDC_min,"on",IWDC_min_date))
print(paste("UBS max return -->",UBS_max,"on",UBS_max_date))
print(paste("IWDC max return -->",IWDC_max,"on",IWDC_max_date))



#before calulating skewnewss an kurtosis
#i am installing a library to not manually calculate the results
#install.packages("moments")
library(moments)
#skewness
UBS_skew=skewness(UBS_returns)
IWDC_skew=skewness(IWDC_returns)
print(paste("UBS skewness -->",UBS_skew))
print(paste("IWDC skewness -->",IWDC_skew))


#kurtosis (NON EXCESS, so i have to detract 3)
UBS_kurt=kurtosis(UBS_returns)
IWDC_kurt=kurtosis(IWDC_returns)
print(paste("UBS kurtosis -->",UBS_kurt))
print(paste("IWDC kurtosis -->",IWDC_kurt))

#correlation between UBS and ETF
correlation = cor(UBS_returns, IWDC_returns)
print(paste("Correlation UBS vs MSCI World ETF -->", correlation))

#####Risk Adjusted Metrics ###### 

#risk free rate from Yahoo Finance 
#(13 week T bill of USA, because of global etf)

getSymbols("^IRX", src="yahoo")
rf_annual=Cl(IRX)/100
rf_daily=(1+rf_annual)^(1/252)-1

#align dates
data=na.omit(merge(UBS_returns, IWDC_returns, rf_daily))
UBS_r=data[,1]
IWDC_r=data[,2]
rf=data[,3]

#excess returns
UBS_excess=UBS_r-rf
IWDC_excess=IWDC_r-rf

#Sharpe ratio
UBS_sharpe=mean(UBS_excess)/sd(UBS_excess)*sqrt(252)
IWDC_sharpe=mean(IWDC_excess)/sd(IWDC_excess)*sqrt(252)
print(paste("UBS Sharpe ratio -->",UBS_sharpe))
print(paste("IWDC Sharpe ratio -->",IWDC_sharpe))

#Sortino ratio
UBS_downside=UBS_excess
UBS_downside[UBS_downside>0]=0
UBS_downside_dev=sqrt(mean(UBS_downside^2))

IWDC_downside=IWDC_excess
IWDC_downside[IWDC_downside>0]=0
IWDC_downside_dev=sqrt(mean(IWDC_downside^2))

UBS_sortino=mean(UBS_excess)/UBS_downside_dev*sqrt(252)
IWDC_sortino=mean(IWDC_excess)/IWDC_downside_dev*sqrt(252)
print(paste("UBS Sortino ratio -->",UBS_sortino))
print(paste("IWDC Sortino ratio -->",IWDC_sortino))

#####Drawdowns and Peaks and calmar######

#cumulative value (starting from 1)
UBS_value=exp(cumsum(UBS_returns))
IWDC_value=exp(cumsum(IWDC_returns))

#drawdown series
UBS_peak=cummax(UBS_value)
IWDC_peak=cummax(IWDC_value)

UBS_dd=(UBS_value-UBS_peak)/UBS_peak
IWDC_dd=(IWDC_value-IWDC_peak)/IWDC_peak

#maximum drawdown and dates
UBS_mdd=min(UBS_dd)
IWDC_mdd=min(IWDC_dd)

UBS_mdd_date=index(UBS_dd)[which.min(UBS_dd)]
IWDC_mdd_date=index(IWDC_dd)[which.min(IWDC_dd)]

print(paste("UBS max drawdown -->",UBS_mdd,"on",UBS_mdd_date))
print(paste("IWDC max drawdown -->",IWDC_mdd,"on",IWDC_mdd_date))

#####PLOTS PEAKS AND DRAWDOWNS

#UBS: cumulative value with running peak
plot(
  UBS_value,
  col="red",
  lwd=2,
  main="UBS cumulative value and running peak (2015–2025)",
  ylab="Cumulative Value (Start = 1)",
  xlab="Date"
)
grid()
lines(
  UBS_peak,
  col="black",
  lwd=2
)


#IWDC: cumulative value with running peak
plot(
  IWDC_value,
  col="blue",
  lwd=2,
  main="MSCI World CHF ETF cumulative value and running peak",
  ylab="Cumulative Value (Start = 1)",
  xlab="Date"
)
grid()
lines(
  IWDC_peak,
  col="black",
  lwd=2
)


#UBS drawdown
plot(
  UBS_dd*100,
  col="red",
  lwd=2,
  main="UBS drawdown (2015–2025)",
  ylab="Drawdown (%)",
  xlab="Date"
)
grid()
abline(h=0)


#IWDC drawdown
plot(
  IWDC_dd*100,
  col="blue",
  lwd=2,
  main="MSCI World CHF ETF drawdown",
  ylab="Drawdown (%)",
  xlab="Date"
)
grid()
abline(h=0)

#Calmar ratio using annual return
UBS_calmar=UBS_ann_return/abs(UBS_mdd)
IWDC_calmar=IWDC_ann_return/abs(IWDC_mdd)

print(paste("UBS Calmar ratio -->",UBS_calmar))
print(paste("IWDC Calmar ratio -->",IWDC_calmar))

#####Autocorrelation#####
#normal returns
acf(UBS_returns, lag=30, main="ACF UBS Returns",col="red")
acf(IWDC_returns, lag=30, main="ACF MSCI World ETF Returns",col="blue")

#absolute returns
acf(abs(UBS_returns), lag=250, main="ACF UBS ABSOLUTE Returns",col="red")
acf(abs(IWDC_returns), lag=250, main="ACF MSCI World ETF ABSOLUTE Returns",col="blue")

#####Leverage Effect#####

#IWDC
par(mfrow = c(2,1), mar = c(3,4,2,2))

plot(
  IWDC,
  col = "blue",
  lwd = 2,
  main = "IWDC Price and Returns (2015–2025)",
  ylab = "Price in CHF",
  xlab = ""
)
grid()

plot(
  IWDC_returns,
  main = "IWDC returns",
  col = "black",
  lwd = 1.5,
  ylab = "Log Returns",
  xlab = "Date"
)
grid()
abline(h = 0)

par(mfrow = c(1,1))

#UBS
par(mfrow = c(2,1), mar = c(3,4,2,2))

plot(
  UBS,
  col = "red",
  lwd = 2,
  main = "UBS Price and Returns (2015–2025)",
  ylab = "Price in CHF",
  xlab = ""
)
grid()

plot(
  UBS_returns,
  main="UBS returns",
  col = "orange",
  lwd = 1.5,
  ylab = "Log Returns",
  xlab = "Date"
)
grid()
abline(h = 0)

par(mfrow = c(1,1))



##### NON NORMALITY #####

# GAUSSIAN DENSITY UBS

mu_UBS    = UBS_mean_returns
sigma_UBS = UBS_sd

x_UBS = seq(-0.05,0.05,0.001)
Gaussian_density_UBS = dnorm(x_UBS, mu_UBS, sigma_UBS)

kernel_density_UBS = density(UBS_returns)

plot(
  kernel_density_UBS,
  main = "Non Normality: UBS log returns",
  xlab = "Log Returns",
  ylab = "Density",
  col="red"
)

grid()

lines(
  x_UBS,
  Gaussian_density_UBS,
  col = "orange",
  lwd = 2
)

legend(
  "topright",
  legend = c("Kernel density","Gaussian density"),
  col = c("red","orange"),
  lwd = 2,
  bty = "n"
)


# GAUSSIAN DENSITY IWDC

mu_IWDC    = IWDC_mean_returns
sigma_IWDC = IWDC_sd

x_IWDC = seq(-0.05,0.05,0.001)
Gaussian_density_IWDC = dnorm(x_IWDC, mu_IWDC, sigma_IWDC)

kernel_density_IWDC = density(IWDC_returns)

plot(
  kernel_density_IWDC,
  main = "Non Normality: IWDC log returns",
  xlab = "Log Returns",
  ylab = "Density",
  col="blue"
)

grid()

lines(
  x_IWDC,
  Gaussian_density_IWDC,
  col = "black",
  lwd = 2
)
legend(
  "topright",
  legend = c("Kernel density","Gaussian density"),
  col = c("blue","black"),
  lwd = 2,
  bty = "n")


#QQ Plots

#UBS QQ
qqnorm(
  UBS_returns,
  main = "QQ Plot UBS log returns",
  col  = "red",
  pch  = 16
)

grid()

qqline(UBS_returns,col = "orange",lwd = 2)


#IWDC QQ
qqnorm(
  IWDC_returns,
  main = "QQ Plot IWDC log returns",
  col  = "blue",
  pch  = 16
)

grid()

qqline(IWDC_returns,col = "black",lwd = 2)

#####Value at Risk sample data #####

#Historical Var

hist_var_UBS  = -quantile(UBS_returns, 0.05)
hist_var_IWDC = -quantile(IWDC_returns, 0.05)

print(paste("Hist VaR of UBS -->",  hist_var_UBS))
print(paste("Hist VaR of IWDC -->", hist_var_IWDC))

# Gaussian VaR
gauss_var_UBS  = -(mean(UBS_returns)  + sd(UBS_returns)  * qnorm(0.05))
gauss_var_IWDC = -(mean(IWDC_returns) + sd(IWDC_returns) * qnorm(0.05))

print(paste("Gaussian VaR of UBS  -->",  gauss_var_UBS))
print(paste("Gaussian VaR of IWDC -->", gauss_var_IWDC))

#Cornish-Fisher VaR

#alpha for 95% VaR
alpha = 0.05
z_alpha = qnorm(alpha)
#adjusted z
z_tilde_UBS = z_alpha +
  (1/6)*(z_alpha^2 - 1)*UBS_skew +
  (1/24)*(z_alpha^3 - 3*z_alpha)*(UBS_kurt - 3) -
  (1/36)*(2*z_alpha^3 - 5*z_alpha)*(UBS_skew^2)

z_tilde_IWDC = z_alpha +
  (1/6)*(z_alpha^2 - 1)*IWDC_skew +
  (1/24)*(z_alpha^3 - 3*z_alpha)*(IWDC_kurt - 3) -
  (1/36)*(2*z_alpha^3 - 5*z_alpha)*(IWDC_skew^2)

cf_var_UBS  = -(UBS_mean_returns  + UBS_sd  * z_tilde_UBS)
cf_var_IWDC = -(IWDC_mean_returns + IWDC_sd * z_tilde_IWDC)

print(paste("Cornish-Fisher VaR of UBS  -->",  cf_var_UBS))
print(paste("Cornish-Fisher VaR of IWDC -->", cf_var_IWDC))

# VaR Comparison

var_matrix = rbind(
  c(hist_var_UBS,  gauss_var_UBS,  cf_var_UBS),
  c(hist_var_IWDC, gauss_var_IWDC, cf_var_IWDC)
)

colnames(var_matrix) = c("Historical", "Gaussian", "Cornish-Fisher")
rownames(var_matrix) = c("UBS", "IWDC")
barplot(
  var_matrix * 100,
  beside = TRUE,
  col = c("red","blue"),
  ylim = c(0, max(var_matrix*100)*1.2),
  main = "Daily Value at Risk Comparison (95%)",
  ylab = "VaR (%)",
  legend.text = TRUE,
  args.legend = list(x = "topright", bty = "n")
)

grid()

##### Forecasting Volatility with Sample Volatility #####

#FIRST WE PLOT ACTUAL VOALTILITY

rw_size = 90

# UBS rolling volatility
UBS_sample_vol = UBS_returns*0
for(i in rw_size:length(UBS_returns)){
  sample_returns = UBS_returns[(i-rw_size+1):i]
  UBS_sample_vol[i] = sd(sample_returns)*sqrt(252)
}

# IWDC rolling volatility
IWDC_sample_vol = IWDC_returns*0
for(i in rw_size:length(IWDC_returns)){
  sample_returns = IWDC_returns[(i-rw_size+1):i]
  IWDC_sample_vol[i] = sd(sample_returns)*sqrt(252)
}


#UBS GRAPH

par(mfrow = c(2,1), mar = c(3,4,2,2))

plot(
  UBS_returns,
  col = "orange",
  lwd = 1.5,
  main = "UBS log returns",
  ylab = "Returns",
  xlab = ""
)
grid()
abline(h = 0)

plot(
  UBS_sample_vol,
  col = "red",
  lwd = 2,
  main = "UBS rolling volatility (90 days)",
  ylab = "Volatility",
  xlab = "Date"
)
grid()

par(mfrow = c(1,1))


# IWDC GRAPH

par(mfrow = c(2,1), mar = c(3,4,2,2))

plot(
  IWDC_returns,
  col = "black",
  lwd = 1.5,
  main = "IWDC log returns",
  ylab = "Returns",
  xlab = ""
)
grid()
abline(h = 0)

plot(
  IWDC_sample_vol,
  col = "blue",
  lwd = 2,
  main = "IWDC rolling volatility (90 days)",
  ylab = "Volatility",
  xlab = "Date"
)
grid()

par(mfrow = c(1,1))

##### Historical VaR forecast (rolling) + violations #####
rw_size = 90

##### UBS #####
VaR_Forecast_UBS_hist = UBS_returns * 0

for (i in rw_size:(length(UBS_returns) - 1)) {
  Sample_Returns = UBS_returns[(i - rw_size + 1):i]
  VaR_Forecast_UBS_hist[i + 1] = quantile(Sample_Returns, 0.05)
}

# plot returns + VaR forecast
plot(
  UBS_returns,
  col = "orange",
  lwd = 1,
  main = "UBS returns and Historical VaR forecast (95%)",
  ylab = "Log Returns",
  xlab = "Date"
)
grid()
lines(VaR_Forecast_UBS_hist, col = "red", lwd = 2)
abline(h = 0)

# count violations (FIX)
violations_UBS_hist = 0
for (i in rw_size:(length(UBS_returns) - 1)) {
  violations_UBS_hist = violations_UBS_hist +
    (as.numeric(UBS_returns[i + 1]) < as.numeric(VaR_Forecast_UBS_hist[i + 1]))
}
totals_UBS_hist = (length(UBS_returns) - 1) - rw_size + 1
violation_rate_UBS_hist = violations_UBS_hist / totals_UBS_hist

print(paste("UBS historical VaR violations -->", violations_UBS_hist))
print(paste("UBS historical VaR violation rate -->", violation_rate_UBS_hist))


##### IWDC #####
VaR_Forecast_IWDC_hist = IWDC_returns * 0

for (i in rw_size:(length(IWDC_returns) - 1)) {
  Sample_Returns = IWDC_returns[(i - rw_size + 1):i]
  VaR_Forecast_IWDC_hist[i + 1] = quantile(Sample_Returns, 0.05)
}

# plot returns + VaR forecast
plot(
  IWDC_returns,
  col = "black",
  lwd = 1,
  main = "IWDC returns and Historical VaR forecast (95%)",
  ylab = "Log Returns",
  xlab = "Date"
)
grid()
lines(VaR_Forecast_IWDC_hist, col = "blue", lwd = 2)
abline(h = 0)

# count violations (FIX)
violations_IWDC_hist = 0
for (i in rw_size:(length(IWDC_returns) - 1)) {
  violations_IWDC_hist = violations_IWDC_hist +
    (as.numeric(IWDC_returns[i + 1]) < as.numeric(VaR_Forecast_IWDC_hist[i + 1]))
}
totals_IWDC_hist = (length(IWDC_returns) - 1) - rw_size + 1
violation_rate_IWDC_hist = violations_IWDC_hist / totals_IWDC_hist

print(paste("IWDC historical VaR violations -->", violations_IWDC_hist))
print(paste("IWDC historical VaR violation rate -->", violation_rate_IWDC_hist))

##### Gaussian VaR forecast (rolling) + violations #####
rw_size = 90

## UBS

VaR_Forecast_UBS_gauss = UBS_returns*0

for (i in rw_size:(length(UBS_returns)-1)){
  Sample_Returns = UBS_returns[(i-rw_size+1):i]
  mu = mean(Sample_Returns)
  sigma = sd(Sample_Returns)
  VaR_Forecast_UBS_gauss[i+1] = mu + sigma*qnorm(0.05)
}

plot(
  UBS_returns,
  col = "orange",
  lwd = 1,
  main = "UBS returns and Gaussian VaR forecast (95%)",
  ylab = "Log Returns",
  xlab = "Date"
)
grid()
lines(VaR_Forecast_UBS_gauss, col = "red", lwd = 2)
abline(h = 0)

violations_UBS_gauss = 0
for (i in rw_size:(length(UBS_returns)-1)){
  violations_UBS_gauss = violations_UBS_gauss +
    (as.numeric(UBS_returns[i+1]) < as.numeric(VaR_Forecast_UBS_gauss[i+1]))
}
totals_UBS_gauss = (length(UBS_returns)-1) - rw_size + 1
violation_rate_UBS_gauss = violations_UBS_gauss / totals_UBS_gauss

print(paste("UBS gaussian VaR violations -->", violations_UBS_gauss))
print(paste("UBS gaussian VaR violation rate -->", violation_rate_UBS_gauss))


## IWDC

VaR_Forecast_IWDC_gauss = IWDC_returns*0

for (i in rw_size:(length(IWDC_returns)-1)){
  Sample_Returns = IWDC_returns[(i-rw_size+1):i]
  mu = mean(Sample_Returns)
  sigma = sd(Sample_Returns)
  VaR_Forecast_IWDC_gauss[i+1] = mu + sigma*qnorm(0.05)
}

plot(
  IWDC_returns,
  col = "black",
  lwd = 1,
  main = "IWDC returns and Gaussian VaR forecast (95%)",
  ylab = "Log Returns",
  xlab = "Date"
)
grid()
lines(VaR_Forecast_IWDC_gauss, col = "blue", lwd = 2)
abline(h = 0)

violations_IWDC_gauss = 0
for (i in rw_size:(length(IWDC_returns)-1)){
  violations_IWDC_gauss = violations_IWDC_gauss +
    (as.numeric(IWDC_returns[i+1]) < as.numeric(VaR_Forecast_IWDC_gauss[i+1]))
}
totals_IWDC_gauss = (length(IWDC_returns)-1) - rw_size + 1
violation_rate_IWDC_gauss = violations_IWDC_gauss / totals_IWDC_gauss

print(paste("IWDC gaussian VaR violations -->", violations_IWDC_gauss))
print(paste("IWDC gaussian VaR violation rate -->", violation_rate_IWDC_gauss))



##### Cornish-Fisher VaR forecast (rolling skewness and kurtosis) + violations #####
rw_size = 90
alpha = 0.05
z_alpha = qnorm(alpha)

##### UBS #####
VaR_Forecast_UBS_cf = UBS_returns * 0

for (i in rw_size:(length(UBS_returns) - 1)) {
  x = UBS_returns[(i - rw_size + 1):i]
  mu = mean(x)
  sigma = sd(x)
  
  s = mean((x - mu)^3) / sigma^3
  k = mean((x - mu)^4) / sigma^4
  
  z_tilde = z_alpha +
    (1/6)  * (z_alpha^2 - 1) * s +
    (1/24) * (z_alpha^3 - 3*z_alpha) * (k - 3) -
    (1/36) * (2*z_alpha^3 - 5*z_alpha) * (s^2)
  
  VaR_Forecast_UBS_cf[i + 1] = mu + sigma * z_tilde
}

plot(
  UBS_returns, col="orange", lwd=1,
  main="UBS returns and Cornish-Fisher VaR forecast (95%)",
  ylab="Log Returns", xlab="Date"
)
grid()
lines(VaR_Forecast_UBS_cf, col="red", lwd=2)
abline(h=0)

violations_UBS_cf = 0
for (i in rw_size:(length(UBS_returns) - 1)) {
  violations_UBS_cf = violations_UBS_cf +
    (as.numeric(UBS_returns[i + 1]) < as.numeric(VaR_Forecast_UBS_cf[i + 1]))
}

totals_UBS_cf = (length(UBS_returns) - 1) - rw_size + 1
violation_rate_UBS_cf = violations_UBS_cf / totals_UBS_cf

print(paste("UBS cornish-fisher VaR violations -->", violations_UBS_cf))
print(paste("UBS cornish-fisher VaR violation rate -->", violation_rate_UBS_cf))


##### IWDC #####
VaR_Forecast_IWDC_cf = IWDC_returns * 0

for (i in rw_size:(length(IWDC_returns) - 1)) {
  x = IWDC_returns[(i - rw_size + 1):i]
  mu = mean(x)
  sigma = sd(x)
  
  s = mean((x - mu)^3) / sigma^3
  k = mean((x - mu)^4) / sigma^4
  
  z_tilde = z_alpha +
    (1/6)  * (z_alpha^2 - 1) * s +
    (1/24) * (z_alpha^3 - 3*z_alpha) * (k - 3) -
    (1/36) * (2*z_alpha^3 - 5*z_alpha) * (s^2)
  
  VaR_Forecast_IWDC_cf[i + 1] = mu + sigma * z_tilde
}

plot(
  IWDC_returns,
  col="black",
  lwd=1,
  main="IWDC returns and Cornish-Fisher VaR forecast (95%)",
  ylab="Log Returns",
  xlab="Date"
)
grid()
lines(VaR_Forecast_IWDC_cf, col="blue", lwd=2)
abline(h=0)

violations_IWDC_cf = 0
for (i in rw_size:(length(IWDC_returns) - 1)) {
  violations_IWDC_cf = violations_IWDC_cf +
    (as.numeric(IWDC_returns[i + 1]) < as.numeric(VaR_Forecast_IWDC_cf[i + 1]))
}

totals_IWDC_cf = (length(IWDC_returns) - 1) - rw_size + 1
violation_rate_IWDC_cf = violations_IWDC_cf / totals_IWDC_cf

print(paste("IWDC cornish-fisher VaR violations -->", violations_IWDC_cf))
print(paste("IWDC cornish-fisher VaR violation rate -->", violation_rate_IWDC_cf))

############GARCHVAR###########################
library(rugarch)

rw_size = 250
alpha = 0.05
dist_choice = "sstd"

garch.setup = ugarchspec(
  mean.model     = list(armaOrder = c(0,0), include.mean = TRUE),
  variance.model = list(model = "gjrGARCH", garchOrder = c(1,1)),
  distribution.model = dist_choice
)

##### UBS #####
VaR_Forecast_UBS_garch = rep(0, length(UBS_returns))
valid_UBS = 0
violations_UBS_garch = 0

for (i in rw_size:(length(UBS_returns)-1)){
  Sample_Returns = UBS_returns[(i-rw_size+1):i]
  
  fit = tryCatch(
    ugarchfit(spec = garch.setup, data = Sample_Returns, solver = "hybrid"),
    error = function(e) NULL
  )
  
  if (!is.null(fit)){
    fc = ugarchforecast(fit, n.ahead = 1)
    
    mu_hat = as.numeric(fitted(fc))[1]
    sigma_hat = as.numeric(sigma(fc))[1]
    
    # get distribution parameters from the fitted model
    pars = coef(fit)
    shape = if ("shape" %in% names(pars)) pars["shape"] else NA
    skew  = if ("skew"  %in% names(pars)) pars["skew"]  else NA
    
    # alpha-quantile of standardized innovations
    if (dist_choice == "norm"){
      q_alpha = qnorm(alpha)
    } else if (dist_choice == "std"){
      q_alpha = qdist("std", p = alpha, mu = 0, sigma = 1, shape = shape)
    } else if (dist_choice == "sstd"){
      q_alpha = qdist("sstd", p = alpha, mu = 0, sigma = 1, skew = skew, shape = shape)
    } else {
      stop("Unsupported distribution choice")
    }
    
    VaR_Forecast_UBS_garch[i+1] = mu_hat + sigma_hat * q_alpha
    
    valid_UBS = valid_UBS + 1
    violations_UBS_garch = violations_UBS_garch +
      (as.numeric(UBS_returns[i+1]) < as.numeric(VaR_Forecast_UBS_garch[i+1]))
  }
  
  if(i %% 100 == 0) print(paste("Iteration:", i))
}

VaR_plot = VaR_Forecast_UBS_garch
VaR_plot[VaR_plot == 0] = NA

plot(
  UBS_returns,
  col = "orange",
  lwd = 1,
  main = paste("UBS returns and", dist_choice, "GJR-GARCH VaR forecast (95%)"),
  ylab = "Log Returns",
  xlab = "Date"
)
grid()
lines(VaR_plot, col = "red", lwd = 2)
abline(h = 0)

violation_rate_UBS_garch = violations_UBS_garch / valid_UBS

print(paste("UBS", dist_choice, "GARCH VaR violations -->", violations_UBS_garch))
print(paste("UBS", dist_choice, "GARCH VaR valid forecasts -->", valid_UBS))
print(paste("UBS", dist_choice, "GARCH VaR violation rate -->", round(violation_rate_UBS_garch, 4)))

##### VAR BASED PORTOFOLIO
##### GARCH VaR BASED PORTFOLIO (UBS vs IWDC) #####
library(rugarch)

window <- 750
alpha  <- 0.05
rebalance_every <- 1

UBS_r  <- na.omit(UBS_returns)
IWDC_r <- na.omit(IWDC_returns)

data_all <- na.omit(merge(UBS_r, IWDC_r))
UBS_r  <- data_all[,1]
IWDC_r <- data_all[,2]
rm(data_all)

n <- length(UBS_r)

VaR_UBS  <- rep(NA, n)
VaR_IWDC <- rep(NA, n)

w_UBS  <- rep(NA, n)
w_IWDC <- rep(NA, n)

spec <- ugarchspec(
  variance.model = list(model="sGARCH", garchOrder=c(1,1)),
  mean.model     = list(armaOrder=c(0,0), include.mean=TRUE),
  distribution.model = "std")

##### PROGRESS + ERROR COUNTERS #####
start_time <- Sys.time()
total_iter <- n - window
fail_UBS   <- 0
fail_IWDC  <- 0
fail_any   <- 0

for(i in (window+1):n){
  
  iter <- i - window
  
  if(iter %% 50 == 0 || i == n){
    
    now <- Sys.time()
    elapsed_min <- as.numeric(difftime(now, start_time, units = "mins"))
    progress <- iter / total_iter
    eta_min <- if(progress > 0) elapsed_min * (1 - progress) / progress else NA
    
    cat(sprintf(
      "i = %d | %4.1f %% done | elapsed %4.1f min | ETA %4.1f min | fails UBS %d IWDC %d any %d\n",
      i, 100 * progress, elapsed_min, eta_min, fail_UBS, fail_IWDC, fail_any
    ))
  }
  
  if(((i-(window+1)) %% rebalance_every) != 0){
    w_UBS[i]  <- w_UBS[i-1]
    w_IWDC[i] <- w_IWDC[i-1]
    next
  }
  
  UBS_window  <- as.numeric(UBS_r[(i-window):(i-1)])
  IWDC_window <- as.numeric(IWDC_r[(i-window):(i-1)])
  
  fit_UBS <- tryCatch(
    ugarchfit(spec=spec, data=UBS_window, solver="hybrid"),
    error=function(e){ fail_UBS <<- fail_UBS + 1; NULL }
  )
  
  fit_IWDC <- tryCatch(
    ugarchfit(spec=spec, data=IWDC_window, solver="hybrid"),
    error=function(e){ fail_IWDC <<- fail_IWDC + 1; NULL }
  )
  
  if(is.null(fit_UBS) || is.null(fit_IWDC)){
    fail_any <- fail_any + 1
    w_UBS[i]  <- w_UBS[i-1]
    w_IWDC[i] <- w_IWDC[i-1]
    next
  }
  
  fc_UBS  <- ugarchforecast(fit_UBS,  n.ahead=1)
  fc_IWDC <- ugarchforecast(fit_IWDC, n.ahead=1)
  
  mu_UBS     <- as.numeric(fitted(fc_UBS))
  sigma_UBS  <- as.numeric(sigma(fc_UBS))
  mu_IWDC    <- as.numeric(fitted(fc_IWDC))
  sigma_IWDC <- as.numeric(sigma(fc_IWDC))
  
  nu_UBS  <- coef(fit_UBS)["shape"]
  nu_IWDC <- coef(fit_IWDC)["shape"]
  
  q_UBS  <- qt(alpha, df=nu_UBS)
  q_IWDC <- qt(alpha, df=nu_IWDC)
  
  VaR_UBS[i]  <- max(-(mu_UBS  + sigma_UBS  * q_UBS),  1e-6)
  VaR_IWDC[i] <- max(-(mu_IWDC + sigma_IWDC * q_IWDC), 1e-6)
  
  inv1 <- 1 / VaR_UBS[i]
  inv2 <- 1 / VaR_IWDC[i]
  
  w_UBS[i]  <- inv1 / (inv1 + inv2)
  w_IWDC[i] <- inv2 / (inv1 + inv2)
}

w_UBS[is.na(w_UBS)]   <- 0.5
w_IWDC[is.na(w_IWDC)] <- 0.5

port_r <- w_UBS * UBS_r + w_IWDC * IWDC_r
port_r <- na.omit(port_r)
port_value <- exp(cumsum(port_r))

plot(
  port_value,
  col="darkgreen",
  lwd=2,
  main="GARCH VaR Based Portfolio Value",
  ylab="Portfolio Value (Start = 1)",
  xlab="Date"
)
grid()




#####Portfolio vs single assets comparisons #####
##### PERFORMANCE COMPARISON PLOT #####

#check if its still ok
port_r <- na.omit(port_r)

port_value <- xts(
  exp(cumsum(as.numeric(port_r))),
  order.by = index(port_r)
)
colnames(port_value) <- "PORT"

# dates alinged
UBS_plot  <- UBS[index(port_value)]
IWDC_plot <- IWDC[index(port_value)]

# normalize
UBS_norm  <- UBS_plot  / as.numeric(first(UBS_plot))
IWDC_norm <- IWDC_plot / as.numeric(first(IWDC_plot))
PORT_norm <- port_value / as.numeric(first(port_value))

# y lim on all series because i cant see more
ymin <- min(UBS_norm, IWDC_norm, PORT_norm, na.rm = TRUE)
ymax <- max(UBS_norm, IWDC_norm, PORT_norm, na.rm = TRUE)

#   plot
par(mar = c(5,5,4,2))
plot(
  PORT_norm,
  col="darkgreen",
  lwd=2,
  ylim=c(ymin, ymax),
  main="Performance Comparison: UBS vs IWDC vs GARCH-VaR Portfolio",
  ylab="Normalized Value (Start = 1)",
  xlab="Date"
)
grid()

lines(UBS_norm,  col="red",  lwd=2)
lines(IWDC_norm, col="blue", lwd=2)


