### Finacial Econometrics

### Frohmolz Claire, Ceneda Nicolò, Gnecco Pietro

### Homework 2


# Preliminary procedure

library(ggplot2)    # package needed for plotting
library(xts)        # package needed for creating and handling time series
library(tseries)    # package needed for performing JB test
library(forecast)   # package needed for plotting: gghistogram(), ggAcf(), checkresiduals()
library(sandwich)   # package needed for calculating HAC standard errors: eyWest()
library(lmtest)     # package needed for regressions with HAC errors: coeftest()
library(systemfit)  # package needed for seemingly unrelated regressions (SUR)

rm(list=ls())       # empty R environment

#set the working directory to source file location



# QUESTION 0: Load s2 data.txt into RStudio if you have not done so yet.
#             Calculate the excess returns of the hedge fund indices. 
#             Then take out the first observation (2000-12-31).


data <- read.table("s2_data.txt",header = TRUE,sep= "\t")   # import dataset
ts <- xts(x = data[-1], order.by = as.Date(data$date))      # define the time series, ts
remove(data)                                                # remove data

ts$DIST_return <- 100*(ts$DIST/lag(ts$DIST)-1)-ts$rf        # Hedge funds index - Distressed Securities: simple monthly excess return 

ts$FI_return   <- 100*(ts$FI/lag(ts$FI)-1)-ts$rf            # Hedge funds index - Fixed Income Arbitrage: simple monthly excess return 

ts$MAC_return  <- 100*(ts$MAC/lag(ts$MAC)-1)-ts$rf          # Hedge funds index - Global Macro: simple monthly excess return 

ts$ELS_return  <- 100*(ts$ELS/lag(ts$ELS)-1)-ts$rf          # Hedge funds index - Equity Long-Short: simple monthly excess return 

ts$MULT_return <- 100*(ts$MULT/lag(ts$MULT)-1)-ts$rf        # Hedge funds index - Multi-strategy: simple monthly excess return

ts <- ts[-1,]                                               # Leave out the first observation (2000-12-31)



# QUESTION 1: Estimate the market model on FI returns. Report the alpha and the beta as well as the R2.
#             Is the alpha statistically significant at the 10% level? 


fit <- lm(data=ts, FI_return ~ mktrf)                       # regress FI returns on market returns 
summary (fit)                                               # report the regression results

alpha <- fit$coefficients[1]                                # interecept of the regression
beta <-  fit$coefficients[2]                                # slope of the regression

res <- as.xts(resid(fit))                                   # Residuals of the linear regression model 

T <- length(res)                                            # number observations
k <- length(coef(fit))                                      # number coefficients
y <- ts$FI_return                                           # dependent variable of the regression, i.e. FI_return
y_hat <- fitted(fit)                                        # predicted dependenedt variable       
y_bar <- mean(y)                                            # mean of the actual dependent variable                                
rss <- sum(res^2)                                           # residual sum of squares
tss <- sum((y - y_bar)^2)                                   # total sum of squares 
                                                            
R2 <- 1 - (rss/T)/(tss/T)                                   #'Multiple R-squared' in summary(fit):

scatter_lm <- ggplot(data=ts,                               # scatterplot: Linear regression of FI on the Market
              mapping = aes(x = mktrf, y = FI_return) )+
              ggtitle("Regression of FI on Market")+
              geom_point(col= "dodgerblue2", size = 0.3)+
              geom_vline(xintercept = 0, col="gray2")+
              geom_hline(yintercept = 0, col="gray2")+
              geom_smooth(method="lm", col='dimgray', size =0.5)+
              labs(x="Market return", y= 'Fixed Income Arbitrage return')+
              theme_bw() 

# ggsave("scatter_lm.pdf")                                  # save the plot in the source file location 
 


# QUESTION 2: Plot the residuals from the model above on the time line. Highlight (e.g. with a circle) two
#             areas of different apparent volatility of residuals or leave the plot blank if you do not see
#             distinct differences. Test for heteroskedasticity, reporting the test you use, the p-value and
#             your conclusion?          

res_tl <- autoplot.zoo(res)+                                          
          ggtitle("Residuals")+
          labs(x="Years", y= 'Residuals')+
          theme_bw()

# ggsave("res_tl_lm.pdf")                                   # save the plot in the source file location 

res2_tl <- autoplot.zoo(res^2)+                                          
           ggtitle("Residuals squared")+
           theme_bw()

# ggsave("res2_tl_lm.png")                                  # save the plot in the source file location 

bptest(fit)                                                 # Breusch-Pagan test

bptest(fit, ~ mktrf + I(mktrf^2), data = ts)                # Breusch-Pagan test adding the squared regressor 
                                                            # obtainig the equivalent of the White test 


# QUESTION 3: Plot a correlogram of residuals with 4 lags. Test for autocorrelation at the 1st lag using the
#             Durbin-Watson test; report the test statistic and your conclusion. 
#             Hint: A Durbin-Watson test can notably be performed with the dwtest() function from the package lmtest.

correlogram <- ggAcf (res, lag.max = 4)+                    # plot the Correlogram of residuals
               ggtitle("Correlogram of Residuals")+           
               theme_bw()

# ggsave("correlogram.pdf")                                 # save the plot in the source file location 

dwtest(fit)                                                 # Perform the Durbin-Watson test for autocorrelation of disturbances



# QUESTION 4:  Do you expect the regression coefficients to change when errors are corrected for 
#              heteroskedasticity and autocorrelation? Why? 

#              See the answer sheet



# QUESTION 5:  Calculate HAC standard errors of the coefficients. Report the results and determine 
#              whether the alpha is statistically significant at the 10% level. 

m <- floor(0.75*T^(1/3))                                    # number of lags

fit1 <- coeftest(fit, 
        vcov = NeweyWest(fit, lag=m, prewhite = FALSE))     # test of coefficients, with eyWest var-cov matrix
fit1



# QUESTION 6: Estimate the CAPM on the returns of FI, MULT and ELS in a SURE fashion, using a
#             non-HAC covariance estimator. Report the correlation matrix of residuals.

r1 <- ts$FI_return ~ ts$mktrf                               # regression to test CAPM for FI
r2 <- ts$MULT_return ~ ts$mktrf                             # regression to test CAPM for MULT
r3 <- ts$ELS_return ~ ts$mktrf                              # regression to test CAPM for ELS

eqSystem <- list(FIreg = r1,                                # system of the regression equations 
            MULTreg = r2, ELSreg= r3)

fitsur <- systemfit(eqSystem, method = "SUR")               # Fits a set of linear structural equations
                                                            # Seemingly Unrelated Regression (SUR)

summary(fitsur)                                             # summary of the estimation



# QUESTION 7: Test for the joint significance of the three alphas at the 10% level. Report the test-statistic
#             and the critical value at the 10% level as well as your conclusion.

# For that we perform a chi-square test of the form

restriction <- c("FIreg_(Intercept)","MULTreg_(Intercept)", # vector with the regression intercepts
                 "ELSreg_(Intercept)") 
linearHypothesis (fitsur, restriction, test = "Chisq")      # testing a linear null hypothesis:
                                                            # alpha_1 = alpha_2 = alpha_3 = 0

qchisq(0.90, df = 3)                                        # critical chi-squared quantile at the 10% level, 3 degrees of freedom
                                                            # to be compared with the chi-sq. obtained: 26.443



# QUESTION 8: Create a  time series object in order to subsample the dataset, starting in 2009:07 (that's
#             the end of the recent recession). Re-estimate the CAPM on FI with robust standard errors.
#             Test if the post-crisis alpha is statistically different from 0.4 at the 5% level, reporting the
#             t-statistic, the critical value and your conclusion. (2 points)

ts_sub <- ts                                                 
ts_sub <- ts_sub['2009-07/',]                               # subsample the dataset, starting in 2009:07

fit2 <- lm(data=ts_sub, FI_return ~ mktrf)                  # regress FI returns on market returns (within the  subsample)

bptest(fit2, ~ mktrf + I(mktrf^2), data = ts_sub)           # Breusch-Pagan test adding the squared regressor 
                                                            # obtainig the equivalent of the White test 
#result: p-value = 0.2015                                   # We do not reject the null hypothesis of residuals' homoskedasticity

dwtest(fit2)                                                # Perform the Durbin-Watson test for autocorrelation of residuals

# Result: DW = 1.18, p-value = 1.812e-05                    # We reject the null hypothesis, in favour of positive autcorrelation

res_sub <- as.xts(resid(fit2))                              # Residuals of the linear regression model 
T_sub <- length(res_sub)                                    # number residuals' observations
m_sub <- floor(0.75*T_sub^(1/3))                            # number of lags
fit.rob <- coeftest(fit2,                                   # test of coefficients, with eyWest var-cov matrix
           vcov = NeweyWest(fit2, lag= m_sub, 
           prewhite = FALSE))                               
fit.rob

# Std. Error  (Intercept) = 0.095644
# coefficient (Intercept)=  0.472113

(0.472113-0.4)/0.095644                                     # Test if alpha is statistically different from 0.4 at the 5% level



# QUESTION 9: Consider again the full range. Looking at Equity Long-Short funds (ELS), compare the
#             significance of alpha under CAPM and Fama-French five-factor model. Use HAC standard
#             errors in both models. Report the two alphas and your conclusion.

# CAPM:

fit_els_capm <- lm(data=ts, ELS_return ~ mktrf)             # CAPM model
bptest(fit_els_capm, ~ mktrf + I(mktrf^2), data = ts)       # Breusch-Pagan test adding the squared regressor 
dwtest(fit_els_capm)                                        # Perform the Durbin-Watson test for autocorrelation of residuals

res_els_capm <- as.xts(resid(fit_els_capm))                 # Residuals of the linear regression CAPM 
T_els        <- length(res_els_capm)                        # Number residuals' observations
m_els_capm <- 0                                             # number of lags

fit.rob_els_capm <- coeftest(fit_els_capm,                  # test of coefficients, with NeweyWest var-cov matrix
                    vcov = NeweyWest(fit_els_capm, 
                    lag=m_els_capm,prewhite = FALSE))                               
fit.rob_els_capm

# FAMA-FRENCH:

fit_els_ff <- lm (data=ts, ELS_return ~ mktrf+smb+hml+rmw+cma)         # FF model

bptest(fit_els_ff, ~   mktrf + smb+hml+rmw+cma+                        # Breusch-Pagan test adding the levels, the squared regressors and the cross products
                       I(mktrf^2)+I(smb^2)+I(hml^2)+I(rmw^2)+I(cma^2)+
                       mktrf*smb+mktrf*hml+mktrf*rmw+mktrf*cma+
                       smb* hml+smb* rmw+smb* cma+
                       hml*rmw+ hml*cma+
                       rmw*cma
                      ,data = ts)       

dwtest(fit_els_ff)                                         # Perform the Durbin-Watson test for autocorrelation of residuals

m_els_ff <- 0                                                       
fit.rob_els_ff <- coeftest(fit_els_ff, 
                           vcov = NeweyWest(fit_els_ff,
                           lag=m_els_ff,prewhite = FALSE))
fit.rob_els_ff  



# QUESTION 10: Estimate the Fama-French five-factor model on the returns of FI, MULT and ELS in a
#              SURE fashion, using a non-HAC covariance estimator. Report the variance of the residuals.
# 

r1_ff <- ts$FI_return   ~ ts$mktrf +ts$smb +ts$hml +ts$rmw +ts$cma  #Fama-French equation for FI
r2_ff <- ts$MULT_return ~ ts$mktrf +ts$smb +ts$hml +ts$rmw +ts$cma  #Fama-French equation for MULT
r3_ff <- ts$ELS_return  ~ ts$mktrf +ts$smb +ts$hml +ts$rmw +ts$cma  #Fama-French equation for ELS

eqSystem <- list(FIregff = r1_ff, MULTregff = r2_ff, ELSregff = r3_ff)

fitsur_ff <- systemfit(eqSystem, "SUR")
summary(fitsur_ff)

summary(lm(r1_ff))                                                  # summary of FF regression for FI
summary(lm(r2_ff))                                                  # summary of FF regression for MULT
summary(lm(r3_ff))                                                  # summary of FF regression for ELS



# QUESTION 11: Now we have one very significant alpha and two insignificant ones, so it is not straightforward
#              to tell if they will be jointly significant.
#              Anyway, perform a chi-square test of joint significance of the three alphas at the 5% level. 
#              Report the test-statistic, the critical value at the 5% level as well as your conclusion.




restrictionff <- c("FIregff_(Intercept)","MULTregff_(Intercept)",    # vector with the regression intercepts
                 "ELSregff_(Intercept)") 

linearHypothesis (fitsur_ff, restrictionff, test = "Chisq")          # testing a linear null hypothesis:
                                                                     # alpha_1 = alpha_2 = alpha_3 = 0

qchisq(0.95, df = 3)                                                 # critical chi-squared quantile at the 5% level, 3 degrees of freedom


