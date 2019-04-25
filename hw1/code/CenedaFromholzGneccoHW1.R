### Finacial Econometrics: Homework 1 --- Frohmolz Claire, Ceneda Nicolo, Gnecco Pietro



# PRELIMINARY: Prepare Rstudio by clearing the environment and importing the relevant packages.

rm(list=ls())                                                     # Clear the environment

library(ggplot2)                                                  # Plotting system
library(xts)                                                      # Creating/using time series
library(fBasics)                                                  # Exploring basic properties of financial returns
library(tseries)                                                  # Analyzing time series


# QUESTION 1:Load s1 data.txt into RStudio.

data<- read.table("s1_data.txt",header = TRUE,sep= "\t")          # Import the data, containing a header and separated by a tab
date <- as.Date(data$date)                                        # Transform "date" which is categorical into a date
time_series <- xts(x = data[-1], order.by = date)                 # Transform the dataframe into time series, excluding the first column "date"
remove(date)                                                      # Delete "date" since it has been included into "time_series"

start(time_series)                                                # Display start date of "time_series"
end(time_series)                                                  # Display end date of "time_series"
periodicity(time_series)                                          # Display the frequency of the data in "time_series"


# QUESTION 2: Calculate the monthly logarithmic returns of SP500, SMIUSD, GOLD, BORD and MSCIE. Convert the risk-free rate to monthly values. 
#             Subsample your data to start in January 2004. Report the mean logarithmic return for GOLD and SMIUSD.

time_series$return_SP500 <- 100*diff(log(time_series$SP500))      # Monthly logarithmic returns of SP500 in percentage
time_series$return_SMIUSD <- 100*diff(log(time_series$SMIUSD))    # Monthly logarithmic returns of SMIUSD in percentage
time_series$return_MSCIE <- 100*diff(log(time_series$MSCIE))      # Monthly logarithmic returns of MSCIE in percentage
time_series$return_GOLD <- 100*diff(log(time_series$GOLD))        # Monthly logarithmic returns of GOLD in percentage
time_series$return_BORD <- 100*diff(log(time_series$BORD))        # Monthly logarithmic returns of BORD in percentage

time_series$monthly_USRF <- time_series$USRF/12                   # Monthly risk free rate in percentage

subset_time_series <- time_series['2004-01/']                     # Subsampling data to start in January 2004

mean_return_GOLD <- mean(subset_time_series$return_GOLD)          # Mean logarithmic return for GOLD in percentage
print(paste0("The mean log return of GOLD is ",                   # Display the mean value
             round(mean_return_GOLD,3),"%"))
mean_return_SMIUSD <- mean(subset_time_series$return_SMIUSD)      # Mean logarithmic return for SMIUSD in percentage
print(paste0("The mean log return of SMIUSD is ",                 # Display the mean value
             round(mean_return_SMIUSD,3),"%"))


# QUESTION 3: In separate graphs, plot the time series of BORD index and of BORD returns, using ggplot2.

plot1 <- ggplot(subset_time_series,                               # BORD time series graph
         aes(x=time(subset_time_series))) +                       # X axis   
         geom_line(aes(y=BORD,colour="BORD")) +                   # Y axis
         labs(x="Date", y="Price") +                              # Labels
         scale_color_discrete(name="Legend") +                    # Legend label
         ggtitle("BORD") +                                        # Graph title
         theme_bw()                                               # Theme
plot1                                                             # Display the graph

plot2 <- ggplot(subset_time_series,                               # BORD returns time series graph
         aes(x=time(subset_time_series))) +                       # X axis  
         geom_line(aes(y=return_BORD,colour="Bord returns")) +    # Y axis 
         labs(x="Date", y="Return") +                             # Labels
         scale_color_discrete(name="Legend") +                    # Legend label
         ggtitle("BORD Return")+                                  # Graph title
         theme_bw()                                               # Theme
plot2                                                             # Display the graph


# QUESTION 4: Test the null hypothesis that the average monthly return of BORD is significantly different from zero at the 5% level. Report 
#             the t-stat and whether you reject or cannot reject the null.

t_test_return_BORD=suppressWarnings(t.test(subset_time_series$    # Two sided t-test on BORD average monthly return
                        return_BORD, alternative = "two.sided", 
                        mu = 0, conf.level = 0.95))
print(paste0("The t-statistic of return_BOARD is ",               # Display the t-stat
             round(t_test_return_BORD$statistic,3)))      

# Check the result obtained
T_return_BORD <- nrow(subset_time_series$return_BORD)             # Number of observations
st_er_return_BORD <- sd(subset_time_series$return_BORD)/          # Standard error
                     sqrt(T_return_BORD)  
mean_return_BORD <- mean(subset_time_series$return_BORD)          # Mean value: repeated but it had already been computed
t_stat_return_BORD_test <- (mean_return_BORD-0)/                  # t-statistic
                           st_er_return_BORD         
t_stat_return_BORD_test                                           # Display the t-stat


# QUESTION 5: Calculate the 95% confidence interval around the mean value of GOLD. Can you be 95% sure that GOLD has been a better (in terms
#             of average return) investment than the risk-free rate?

t_test_return_GOLD=suppressWarnings(t.test(subset_time_series$    # Two sided t-test on GOLD average monthly return
                      return_GOLD, alternative = "two.sided",
                      mu = 0, conf.level = 0.95))
print(paste0("The upper bound of the confidence interval is ",    # Display the confidence interval
             round(t_test_return_GOLD$conf.int[2],3), 
             ". The lower bound is ",
             round(t_test_return_GOLD$conf.int[1],3))) 

mean_return_USRF <- mean(subset_time_series$monthly_USRF)         # Average return of the risk free asset

suppressWarnings(t.test(subset_time_series$return_GOLD,           # Two sided paired t-test on GOLD and risk free rate returns
                        subset_time_series$monthly_USRF, 
                        alternative = "two.sided",
                        conf.level = 0.95, paired = TRUE))


# QUESTION 6: Calculate the correlation matrix of the five risky assets' returns. With which assets are BORD and GOLD most correlated, 
#             respectively?

return_matrix <- cbind(subset_time_series$return_SP500,           # Matrix where each column contains an asset's return series
                       subset_time_series$return_SMIUSD,                   
                       subset_time_series$return_MSCIE,
                       subset_time_series$return_GOLD, 
                       subset_time_series$return_BORD) 

cor(return_matrix)                                                # Correlation matrix

cor_BORD <- cor(return_matrix)[,"return_BORD"]                    # We define which asset is correlated the most with BORD
which.max( cor_BORD [cor_BORD<1] )                             
cor_GOLD <- cor(return_matrix)[,"return_GOLD"]                    # We define which asset is correlated the most with GOLD
which.max( cor_GOLD [cor_GOLD<1] )                         


# QUESTION 7: Among the five risky assets, report the asset with the lowest excess kurtosis and the corresponding excess kurtosis coeficient. 
#             Use ggplot2 to plot a histogram for this asset's returns, fitting a normal distribution using the sample mean and variance. 
#             Is the histogram of the returns approximately normal?

excess_kurtosis <- (basicStats(return_matrix)["Kurtosis",])       # Excess kurtosis of each asset
lowest_excess_kurtosis <- which.min(abs(basicStats(return_matrix) # asset with the lowest excess kurtosis
                          ["Kurtosis",]))   

print(paste0(colnames(return_matrix[,lowest_excess_kurtosis]),    # Display the name of the asset with the lowest excess kurtosis and the value
             " has the lowest excess kurtosis ", 
             "with a value of ", round(min(excess_kurtosis),3)))

return_GOLD <- subset_time_series$return_GOLD                     # Rename the time series of GOLD returns
histogram <- ggplot(return_matrix, aes (x=return_GOLD)) +         # Plot the histogram and define its aesthetic
             geom_histogram(aes(y = ..density.. ), 
             bins = 20, color = "gray65", fill = "dodgerblue3") +
             theme_bw() +
             stat_function(fun=dnorm,
             args = list(mean(return_GOLD), sd(return_GOLD)))
histogram

jarque.bera.test(return_GOLD)                                     #Jarque-Bera test


# QUESTION 8: Report which of the five risky assets is farthest from a normal distribution and report its Jarque-Bera test statistic.

worst_kurtosis <- which.max(abs(basicStats(return_matrix)         # Which asset has the highest absolute value of excess Kurtosis
                   ["Kurtosis",]))            
worst_skewness <- which.max(abs(basicStats(return_matrix)         # Which asset has the highest absolute value of Skewness
                   ["Skewness",]))         

JB_stat <- jarque.bera.test(subset_time_series$return_SP500)      # Jarque-Bera test for SP500 

print(paste0("The asset which is farthes from normal is ",        # Display the name of the asset farthest from a normal distribution and its JB 
             colnames(return_matrix[1,lowest_excess_kurtosis]), 
             " with a JB value of ",
             round(jarque.bera.test(subset_time_series$
             return_SP500)$statistic[1],3)))


# QUESTION 9: Calculate the expected return of an equally-weighted portfolio of all five risky assets and its Sharpe ratio. Report the former 
#             in percent per month and the latter in fractions of 1.

portfolio_results <- data.frame(0,0)                              # Set a blank dataframe
return_matrix$portfolio_return <- rowMeans(return_matrix)         # Equally weighted portfolio monthly returns             
portfolio_expected_retrun<-mean(return_matrix$portfolio_return)   # Equally wieghted portfolio expected return
portfolio_results[1,1] <- portfolio_expected_retrun               # Assign the expected return value to the dataframe
portfolio_sharpe_ratio <- mean(return_matrix$portfolio_return-    # Equally wieghted portfolio sharpe ratio
                          subset_time_series$monthly_USRF)/
                          sd(return_matrix$portfolio_return)
portfolio_results[1,2] <- portfolio_sharpe_ratio                  # Assign sharpe ratio value to the dataframe
colnames(portfolio_results) <- c("expected_return_portfolio",     # Set output vector column names
                               "sharpe_ratio_portfolio")
portfolio_results                                                 # Print portfolio result vector


# QUESTION 10: Test if the above average return is statistically different from zero by calculating a 95% confidence interval around the 
#              expected return. Report the interval and the inference conclusion.

t_test_return_portfolio <- suppressWarnings(t.test
                           (return_matrix$portfolio_return,        # two sided t test at 95% confidence level
                           alternative = "two.sided",                            
                           conf.level = 0.95))

print(paste0("The upper bound of the confidence interval is ",     # Display the confidence interval
             round(t_test_return_portfolio$conf.int[2],3), 
             ". The lower bound is ",
             round(t_test_return_portfolio$conf.int[1],3))) 


# QUESTION 11: In this question, we reconsider the whole sample (1962:01 - 2017:12). 

# QUESTION 11 a: The file_potus_by_party.txt has one single dummy variable taking a value of 1 if the President was a Democrat in a particular
#                year, and a value of 0 otherwise. Import this variable.

data2<-read.table("potus_by_party.txt",header = TRUE,sep= "\t")   # Import the data, containing a header and separated by a tab
date2 <- as.Date(data2$date)                                      # Transform "date" which is categorical into a date
time_series2 <- xts(x = data2[-1], order.by = date2)              # Transform the dataframe into time series, excluding the first column "date"
remove(date2)                                                     # Delete "date2" since it has been included into "time_series"

start(time_series2)                                               # Display start date of "time_series2"
end(time_series2)                                                 # Display end date of "time_series2"
periodicity(time_series2)                                         # Display the frequency of the data in "time_series2"


# QUESTION 11 b: Create a new series by summing the dividends and the index levels. Calculate the total return by dividing this new series by 
#                lagged prices.

time_series$monthly_SPDIV <- time_series$SPDIV/12                 # Monthly dividend
time_series$price_total_SP500 <- time_series$SP500 +              # Price of SP500 with dividend time series
                                 time_series$monthly_SPDIV        
time_series$return_total_SP500<-log(time_series$price_total_SP500/# Total return time series for S&P500
                                lag(time_series$SP500))
mean(time_series$return_total_SP500, na.rm=TRUE)                  # Expected total return for S&P500 is indeed 0.7904%


# QUESTION 11 c: Now, select only those dates when a Democrat president was in power and calculate the average total return.

time_series$party <- time_series2$party                           # Add the "party" column to the original time series
mean_return_democrat <- mean (time_series[time_series$party == 1, # S&P500 average return during under a Democart president
                        "return_total_SP500"],na.rm=TRUE)              


# QUESTION 11 d: Repeat the same exercise for the months under a Republican president.

mean_return_republican <- mean(time_series[time_series$party == 0,# S&P500 average return during under a Republican president
                          "return_total_SP500"],na.rm=TRUE)


# QUESTION 11 e: Use the t.test function of R to perform a two-sample t-test (not a paired one), comparing the two means. Report and interpret 
#                the results of this test. Are the two means significantly differen at the 5% level?

var.test((time_series[time_series$party == 0,                     # Test whether the variances of the two groups
        "return_total_SP500"]),(time_series
        [time_series$party == 1,"return_total_SP500"]))


suppressWarnings(t.test(time_series[time_series$party == 0,       # Two-sample test compairing the two means obtained in 11c - 11d
                                    "return_total_SP500"],   
                        time_series[time_series$party == 1,
                                    "return_total_SP500"], 
                        alternative = 'two.sided',                # Alternative hypothesis: the avergae returns differ from each other
                        conf.level = 0.95))

suppressWarnings(t.test(time_series[time_series$party == 0,       # Two-sample test compairing the two means obtained in 11c - 11d
                                    "return_total_SP500"],   
                        time_series[time_series$party == 1,
                                    "return_total_SP500"], 
                        alternative = 'less',                     # Alternative hypothesis: the mean difference is smaller than zero
                        conf.level = 0.95))

suppressWarnings(t.test(time_series[time_series$party == 0,       # Two-sample test compairing the two means obtained in 11c - 11d
                                    "return_total_SP500"],   
                        time_series[time_series$party == 1,
                                    "return_total_SP500"], 
                        alternative = 'greater',                  # Alternative hypothesis: the mean difference is greater than zero
                        conf.level = 0.95))

##
