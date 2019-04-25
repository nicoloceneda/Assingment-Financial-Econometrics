### Finacial Econometrics

### Frohmolz Claire, Ceneda Nicol?, Gnecco Pietro
rm(list=ls())
### Homework 3



# Preliminary procedure

library(ggplot2)        # package needed for plotting
library(xts)            # package needed for creating and handling time series
library(forecast)       # package needed for time series forecasts
library(urca)           # package needed for unit root tests

rm(list=ls())           # empty R environment

# set the working directory to source file location



# QUESTION 0: Load s3 data.txt into RStudio. Calculate the excess returns 
#             of SP500 and FINANC. We will try to forecast the latter (financ).  
#             Calculate also the (logarithm of) price-dividend ratio, a popular 
#             variable among predictors.Calculate this ratio using SP500 and SPDIV.
#             Get rid of the 1999 data. We will only consider observations from 2000 onwards.

data <- read.table("s3_data.txt",header = TRUE,sep="\t")    # import dataset
ts <- xts(x = data[-1], order.by = as.Date(data$date))      # define the time series, ts
remove(data)                                                # remove data

ts$return_SP500 <- 100*diff(log(ts$SP500))-ts$rf            # monthly continuous excess return: S&P500
ts$return_FINANC <- 100*diff(log(ts$FINANC))-ts$rf          # monthly continuous excess return: FINANC

ts$pdr_SP500 <- log (ts$SP500/ts$SPDIV)                     # logarithm of price-dividend ratio for S&P500

ts <- ts['2000-01/',]                                       # subsample the dataset, starting in 2000-01



# QUESTION 1: Let's first consider an AR model. Using data from 2000:01 to 2009:12, construct a correlo-
#             gram of financ with 12 lags to see which lags are individually significant at the 95% level. Do
#             the same for the PACF (using this time the ggPacf() function). Interpret your two plots.

ts_sub <- ts
ts_sub <- ts_sub['/2009-12',]

correlogram_acf_FINANC <- ggAcf(ts_sub$return_FINANC,lag.max=12,calc.ci = TRUE, level = 95)+    # plot correlogram of FINANC (PACF) - 12 lags
                           ggtitle("Correlogram of FINANC - ACF")+
                           labs(x="Lag", y= 'Partial Autocorrelation')+
                           theme_bw()
correlogram_acf_FINANC
# ggsave("correlogram_acf_FINANC.pdf")                                 # save the plot in the source file location 

correlogram_pacf_FINANC <- ggPacf(ts_sub$return_FINANC,lag.max=12,calc.ci = TRUE, level = 95)+    # plot correlogram of FINANC (PACF) - 12 lags
                           ggtitle("Correlogram of FINANC - PACF")+
                           labs(x="Lag", y= 'Partial Autocorrelation')+
                           theme_bw()
correlogram_pacf_FINANC
# ggsave("correlogram_pacf_FINANC.pdf")                                # save the plot in the source file location 



# QUESTION 2: The function ar.mle() allows us to select the order of an AR(p) model according to Akaike
#             information criterion (AIC). Report your result and relate it to your previous answer.
#             Time span: 31.01.2000 -  31.12.2009

fit <- ar.mle(as.ts (ts_sub$return_FINANC),  aic = TRUE)        # Fit an autoregressive time series model to the data

fit_order <- fit$order                                          #  Order of the fitted model



# QUESTION 3: Using data from 2000:01 to 2009:12, estimate a demeaned AR(p) where p is the order derived
#             in the previous question. Doing some model checking, comment on the checkresiduals() output.

fit_ar_11 <- Arima(ts_sub$return_FINANC,                        # autoregressive model with order 6
                   order = c(11,0,0))         

checkresiduals(fit_ar_11)                                       # model checking



# QUESTION 4: Based on your AR estimation, make a forecast of financ in 2010:01. Report your forecast
#             and compute a 90% confidence interval around it

forecast(fit_ar_11, h=1,level = 90)                             # forecast values of FINANC return in 2010:01

as.numeric( forecast(fit_ar_11, h=1,level = 90)$mean)           # extract forecast of FINANC return in 2010:01
as.numeric( forecast(fit_ar_11, h=1,level = 90)$lower)          # extract 90% confidence interval upper limit
as.numeric( forecast(fit_ar_11, h=1,level = 90)$upper)          # extract 90% confidence interval lower limit



# QUESTION 5: Gradually increasing the estimation window length, keep on forecasting till the end of the
#           RMSE  sample; calculate the RMSE of this model. (2010:01 ~ 2017:12)

ts$f_ar11 <- NA                                                 # creating forecast variable

for (i in (nrow(ts['/2010-01'])) : nrow(ts))                    # for loop to forecast till the end of the sample
{
 fit_ar_11 <- Arima (ts$return_FINANC[1:(i-1)], order = c(11,0,0))
 ts$f_ar11[i] <- as.numeric( forecast(fit_ar_11, h=1,level = 90)$mean)           
}


forecast_error_ar11 <- ts$return_FINANC-ts$f_ar11               # RMSE of the model.
RMSE_ar11 <- sqrt(mean((forecast_error_ar11)^2,na.rm=TRUE))



# QUESTION 6: Price-dividend ratios are often assumed to be stationary, although in small samples this could
#             be difficult to prove. Conduct two stationarity tests (include a constant but no trend): the
#             ADF test and the KPSS test. Report the test statistics and the critical values at the 1%
#             level. Give your conclusion.
#             time span considered: 2000-01-31 to 2017-12-31 

adf_aic <- ur.df(ts$pdr_SP500, type = "drift", selectlags = "AIC")   # Augmented Dickey-Fuller unit root test.
                                                                     # Lag selection can be achieved according to the Akaike "AIC"
summary(adf_aic)

adf_bic <- ur.df(ts$pdr_SP500, type = "drift", selectlags = "BIC")   # Augmented Dickey-Fuller unit root test.
                                                                     # Lag selection can be achieved according to Bayes "BIC" information criterion
summary(adf_bic)

adf_fix <- ur.df(ts$pdr_SP500, type = "drift", selectlags = "Fixed") # Augmented Dickey-Fuller unit root test.
                                                                     # Lag selection can be achieved according to "fixed" lag length set by lags
summary(adf_fix)

kpss <- ur.kpss(ts$pdr_SP500 ,type="mu")                             # KPSS unit root test, where the Null hypothesis is stationarity
                                                                     # type = "mu" and not type = "tau" since we do not include a linear trend "tau"
summary(kpss)



# QUESTION 7: Estimate by OLS the following factor model3, using the data from 2000:01 to 2009:12:
#             Come up with a forecast for 2010:01. Report the forecast and the 95% confidence interval around it.
#             ts_sub: Monthly periodicity from 2000-01-31 to 2009-12-31 

X <- cbind(ts$return_SP500, ts$eurusd, ts$VIX,           # Regressors' matrix. Dim: 120 x 6
           ts$pdr_SP500, ts$return_FINANC,               # 6 regressors with 120 observations 
           lag(ts$return_FINANC))                        # Monthly periodicity from 2000-01-31 to 2009-12-31 

fit_ols <- lm(ts$return_FINANC['/2009-12'] ~ lag(X['/2009-12']))

summary(fit_ols)

forecast(fit_ols, as.data.frame(X['2009-12']), level = 95 )



# QUESTION 8: Gradually increasing the estimation window length, repeat the forecasting till the end of the
#             sample. Plot predicted and realized values on the same chart and report the RMSE of this
#             model.

ts$f_ols <- NA                                                  # creating forecast variable

for (i in (nrow(ts['/2010-01'])) : nrow(ts))                    # for loop to forecast till the end of the sample
{
  fit_ols <- lm( ts$return_FINANC[1:i-1] ~ lag( X[1:(i-1)]))    # OLS linear model 
  ts$f_ols[i] <- as.numeric(forecast(fit_ols,                   # fille the forecast variable
                            as.data.frame(X[i-1]),  
                            h =1)$mean)           
}

series <- cbind(ts$return_FINANC,ts$f_ols)                      # predicted and realized values 
autoplot.zoo(series,facets=NULL)+
ggtitle("FINANC: forecasted VS actual returns")+
scale_x_date(limits = c(as.Date("2010-01-31 "), NA))+
ylim(-15,15)+
theme_bw()  

forecast_error_ols <- ts$return_FINANC-ts$f_ols                 # RMSE of the model.
RMSE_ols <- sqrt(mean((forecast_error_ols)^2,na.rm=TRUE))



# QUESTION 9: Perform a two-sided Diebold-Mariano test, comparing the forecast errors of the two models.
#             Adopt a quadratic loss function. Based on your result, determine whether the two forecasts
#             have the same accuracy. Give your conclusion at a 5% significance level.


dm.test(forecast_error_ar11, forecast_error_ols,                # DM test 
alternative = "two.sided", power = 2)                           # two-sided and quadratic loss


# DM = 2.4803, Forecast horizon = 1, Loss function power = 2, p-value = 0.0139

# Under the null hypothesis, the test statistics DM is asymptotically N(0, 1) distributed. 

# The null hypothesis of no difference will be rejected if the computed DM statistic falls outside the range of -1.96 to 1.96.

# Reject the null hypothesis!



# QUESTION 10: Finally, combine your two forecasts, computing a simple mean at each observation (forecast
#             averaging). Derive the RMSE and comment.

ts$f_mean <- (0.5* ts$f_ar11) + (0.5* ts$f_ols)                 # forecast averaging

forecast_error_mean <- ts$return_FINANC - ts$f_mean             # RMSE of the model.
RMSE_mean <- sqrt(mean((forecast_error_mean)^2,na.rm=TRUE))









