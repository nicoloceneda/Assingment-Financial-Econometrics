### Finacial Econometrics

### Frohmolz Claire, Ceneda Nicolò, Gnecco Pietro

### Homework 4



# Preliminary procedure

library(ggplot2)        # package needed for plotting
library(xts)            # package needed for creating and handling time series
library(forecast)       # package needed for time series forecasts
library(np)             # package needed for nonparametric regression
library(stats4)         # package needed for mle estimation
library(rugarch)        # package needed for GARCH models
library(L1pack)         # package needed for Laplace distribution
library(rmutil)         # package needed for Laplace distribution

rm(list=ls())           # empty R environment

# set the working directory to source file location



# QUESTION 1:  Load s4 data.txt into RStudio. We'll start with estimating the functional relation between
#              the expected return on the telcm portfolio and the dollar appreciation rate:
#              Estimate it using the Nadaraya-Watson estimator and plot the fitted values vs. the regressor,
#              along with an OLS line. Is the absolute effect on telcm larger when the dollar appreciates
#              by 1% or when it depreciates by the same amount?

data <- read.table("s4_data.txt",header = TRUE,sep="\t")        # load s4 data
ts <- xts(x = data[-1], order.by = as.Date(data$date))          # create time series
remove(data)                                                    # remove object data

bw <- 1.06*sd(ts$dol)*nrow(ts)^(-0.2)                           # rule-of-thumb bandwidth
npr<- npreg(telcm ~ dol,as.data.frame(ts),bws=bw)
summary(npr)
ts$fit.telcm <- fitted(npr)                                     # 6826 point estimates from the regression

plot <- ggplot(ts, aes(x= dol,y= telcm)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_smooth(method="lm",se=FALSE) +                           # linear regression line
  geom_point(size=1.2,aes(y=fit.telcm,colour = "kernel reg")) + # kernel regression estimates (points)
  scale_x_continuous(breaks=pretty(ts$dol, n=10)) +             # increase the number of axis ticks
  scale_y_continuous(breaks=pretty(ts$telcm, n=40)) +           # increase the number of axis ticks
  theme_bw() 
plot



# QUESTION 2:  Fill in the table in the answer sheet, indicating if we should see positive or negative returns
#              for different values of the dollar return.



# QUESTION 3:  If we chose a higher (wider) bandwidth, what will happen to our plot?



# QUESTION 4: Let's switch to MLE. Say, you believe that the true model of the relation between telcm and
#             mkt is as follows: where L(0; s) is the zero-mean Laplace distribution (link to wiki) parametrized with the scale parameter s:
#             Estimate the three parameters with MLE, using the log-liklihood. Report 0, 1, and their
#             respective standard errors.

negLL <- function (b0, b1, s){
  logdens = suppressWarnings (dlaplace(y= ts$telcm, m= b0+b1*ts$mkt, s= s, log=TRUE))
  -sum(logdens)
}

fit.mle <- mle(negLL, start = list(b0=0, b1=0, s=1), lower=c(-Inf,-Inf,0), method ="L-BFGS-B")
summary(fit.mle)



# QUESTION 5: In order to test for ARCH effects, perform a Ljung-Box test with 12 lags on the squared
#             returns of telcm. Give your conclusion at the 5% significance level.

Box.test(ts$telcm^2, lag=12, type="Lj")



# QUESTION 6: Assuming that innovations follow a normal distribution, estimate an ARCH(5) model on the
#             returns of telcm. We will name it model 1. Comment on the normal QQ plot (plot 9 using
#             the uGARCHfit plot method with the which argument). 
 
spec1 <- ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(5,0)), 
                    mean.model = list(armaOrder=c(0,0)), 
                    distribution.model = "norm")
model1 <- ugarchfit(spec = spec1, data = ts$telcm)
model1
plot(model1, which=9) # qqplot



# QUESTION 7: Re-estimate the same model but with a t-distribuition for the residuals instead of a normal
#           one (model 2). Report the shape coefficient from the estimation output. Relate it to your
#           observation in the previous question. (2 points)

spec2 <- ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(5,0)), 
                    mean.model = list(armaOrder=c(0,0)), 
                    distribution.model = "std")
model2 <-ugarchfit(spec = spec2, data = ts$telcm)
model2
plot(model2, which=9) # qqplot



# QUESTION 8: Finally, estimate a GJR(1,1) model assuming a t-distribution for the innovations (model 3).
#             Does the variance go up or down after a negative return on the previous day? Is this move
#             statistically significant at the 95% level? (1 point)

spec3 <- ugarchspec(variance.model = list(model="gjrGARCH",garchOrder=c(1,1)),
                    mean.model = list(armaOrder=c(0,0)),
                    distribution.model = "std")

model3 <- ugarchfit(spec = spec3, data = ts$telcm)

max_sd <- max (sigma (ugarchfit(spec = spec3, data = ts$telcm))) # maximum conditional sigma value



# QUESTION 9: You want to assess whether the ARCH effects were correctly modeled. Ideally, the stan-
#             dardized squared residuals should not exhibit autocorrelation. Why? Compare the ACF of
#             squared standardized residuals plots (plot 11) from models 2 and 3.

plot(model2, which=11) # ACF of Squared Standardized Residuals - Model 2

plot(model3, which=11) # ACF of Squared Standardized Residuals - Model 3



# QUESTION 10: Taking a look at the conditional volatility plot from model 3, what is approximately the
#              maximum daily 95% Value-at-Risk you could have calculated since 1990 based on the as-
#              sumption that the returns are conditionally normally distributed with zero mean and the
#              volatility is the one estimated in model 3?

plot (model3, which=3) # conditional volatility plot - model 3



# QUESTION 11: Compare your three models in terms of the AIC. Comment on the ranking obtained.

model1 # Akaike       2.9267

model2 # Akaike       2.8771

model3 # Akaike       2.8276



