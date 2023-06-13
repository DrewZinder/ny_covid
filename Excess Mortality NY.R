######################################
###          Drew Zinder           ###
###      DrewZinder@gmail.com      ###
###        January 2, 2021         ###
###                                ###
###     Excess Mortality Model     ###
###  Data: Weekly Counts of Death  ###
###      Data Source: CDC.gov      ###
######################################

rm(list = ls())
library(data.table)
library(tidyverse)
library(stringr)
library(dplyr)
library(ggplot2)
library(forecast)
library(magrittr)


########################################
### Import raw data from CDC, Census ###
########################################

# Weekly death totals are broken into two files by the CDC: 2014 - 2018, and 2019 - 2020
raw_deaths_2014 <- fread('https://data.cdc.gov/api/views/3yf8-kanr/rows.csv?accessType=DOWNLOAD')
raw_deaths_2019 <- fread('https://data.cdc.gov/api/views/muzy-jte6/rows.csv?accessType=DOWNLOAD')
raw_population <- fread('http://www2.census.gov/programs-surveys/popest/datasets/2010-2019/national/totals/nst-est2019-alldata.csv')


############################################
### Subset NY data and relevant columns  ###
############################################

# Subset data; correcting double-space typo in 2014 dataset column name
ny_deaths_2014  <- raw_deaths_2014[`Jurisdiction of Occurrence` == 'New York', 
                                   .(`Week Ending Date`, `All Cause` = `All  Cause`, `MMWR Week`)] 

ny_deaths_2019  <- raw_deaths_2019[`Jurisdiction of Occurrence` == 'New York', 
                                   .(`Week Ending Date`, `All Cause`, `MMWR Week`)]

# Format date columns 
ny_deaths_2014$`Week Ending Date` %<>% as.Date(., '%m/%d/%Y')
ny_deaths_2019$`Week Ending Date` %<>% as.Date()

# Create a single dataframe with all weekly death counts
ny_deaths_total <- rbind(ny_deaths_2014, ny_deaths_2019)

### July 2021 Update: remove data not originally available in January analysis ###
ny_deaths_total %<>% .[`Week Ending Date` < '2021-01-02',]

# Subset data
ny_population <- raw_population[NAME == 'New York', 
                                POPESTIMATE2010:POPESTIMATE2019] %>%
  
                  # Convert data to long format
                  pivot_longer(., POPESTIMATE2010:POPESTIMATE2019) %>%
  
                  # Rename variables, for ease of use
                  rename('Year' = 'name', 'Population Estimate' = 'value')
  

# Add in current best estimate for NY 2020 population
# Data from World Population Review: https://worldpopulationreview.com/states/new-york-population 
ny_population %<>% rbind(., c('POPESTIMATE2020', 19440500))

# Create Year variable
ny_population$Year %<>% str_sub(., -4) %>% as.numeric()


########################################
### Join deaths & population tables  ###
########################################

# Generate Year variable, coerce to numeric
ny_deaths_total$Year <- as.Date(ny_deaths_total$`Week Ending Date`) %>% 
  
                          format(., '%Y') %>% 
  
                          as.numeric()


# Join relevant dataframes
ny_data <- data.table(ny_deaths_total, key = 'Year')[
              data.table(ny_population, key = 'Year')[5:11,], 
              allow.cartesian = FALSE]

# Coerce population data to numeric
ny_data$`Population Estimate` %<>% as.numeric()

# Create deaths per 100,000
ny_data$`Deaths Per 100,000` <- ((ny_data$'All Cause'/ny_data$'Population Estimate')*100000)


###########################################
### Inspect deaths per 100,000 variable ###
###########################################

# Examine trend line, look for stationarity
ggplot(ny_data, 
       aes(`Week Ending Date`, `Deaths Per 100,000`)) + 
       geom_line(color = 'steelblue') + 
       theme_update()

# Visually, there seems to be a slowly increasing trend, as well as seasonality effects.

# Look at ACF & PACF functions
plot.new()
frame()
par(mfcol = c(2, 1))
acf(ny_data$`Deaths Per 100,000`,
    type = 'correlation',
    lag.max = 200,
    plot = TRUE,
    main = 'Autocorrelation Function: Deaths Per 100,000')
acf(ny_data$`Deaths Per 100,000`,
    type = 'partial',
    lag.max = 200,
    plot = TRUE,
    main = 'Partial Autocorrelation Function: Deaths Per 100,000')


###############################
### Split data into buckets ###
###############################

# 2014 - 2018 data for training, 2019 for testing, and 2020 to measure excess mortality

# Set columns to keep
data_cols <- c('MMWR Week', 'Week Ending Date', 'Year', 'Deaths Per 100,000')

train <- subset(ny_data,
                as.numeric(ny_data$Year) < 2019,
                select = data_cols)

test <- subset(ny_data,
               as.numeric(ny_data$Year) == 2019,
               select = data_cols)

mortality <- subset(ny_data,
                    as.numeric(ny_data$Year) == 2020,
                    select = data_cols)


################################
### Build final SARIMA model ###
################################

# Estimate a (2,2,1)(1,1,0) SARIMA model
# (Iterative process by which this was determined not included in final code)
ny_sarima <- Arima(train$`Deaths Per 100,000`, 
                   order = c(2,0,2), 
                   seasonal = list(order = c(1,0,0), 
                                   period = 52, 
                                   method = 'ML'))

# Inspect model estimates
ny_sarima

# Look at residual ACF & PACF functions, to see if there's any remaining unexplained trends
plot.new()
frame()
par(mfcol = c(2, 1))
acf(ny_sarima$residuals,
    type = 'correlation',
    lag.max = 200,
    plot = TRUE,
    main = 'Autocorrelation Function: SARMIA Residuals')
acf(ny_sarima$residuals,
    type = 'partial',
    lag.max = 200,
    plot = TRUE,
    main = 'Partial Autocorrelation Function: SARMIA Residuals')

# Generate predicted values, and coerce into dataframe
predicted <- as.data.frame(predict(ny_sarima, n.ahead = 104))

predict_2019 <- as.data.frame(predicted$pred[1:52])
predict_2019$`Week Ending Date` <- test$`Week Ending Date`
names(predict_2019)[1] <- 'Predicted Deaths Per 100,000'

predict_2020 <- as.data.frame(predicted$pred[53:104])
predict_2020$`Week Ending Date` <- mortality$`Week Ending Date`
names(predict_2020)[1] <- 'Predicted Deaths Per 100,000'

# Look at actual vs predicted deaths for 2019
plot.new()
frame()
par(mfcol = c(2, 1))
plot(predict_2019$`Week Ending Date`, 
     predict_2019$`Predicted Deaths Per 100,000`,
     main = 'Actual vs Predicted Deaths Per 100,000, 2019',
     xlab = 'Date',
     ylab = 'Deaths Per 100,000',
     type = 'l', 
     col = 'red')
lines(test$`Week Ending Date`, 
      test$`Deaths Per 100,000`, 
      col = 'blue')
legend(x = 'topright', 
       lty = 1, 
       lwd = 1, 
       legend = c('Actual', 'Predicted'), 
       col = c('blue', 'red'))

# Calculate  total prediction mismatch
predict_2019$`Prediction Mismatch` <- (predict_2019$`Predicted Deaths Per 100,000` - test$`Deaths Per 100,000`)
mismatch_2019 <- sum(predict_2019$`Prediction Mismatch`)

mismatch_2019/52
mismatch_2019*as.numeric(ny_population$`Population Estimate`[10])/as.numeric(100000)

# We're underpredicting mortality by about 0.22 deaths per 100,000, per week, for a total of ~2200 underpredicted deaths

# Look at actual vs predicted deaths for 2020
plot(predict_2020$`Week Ending Date`, 
     predict_2020$`Predicted Deaths Per 100,000`,
     main = 'Actual vs Predicted Deaths Per 100,000, 2020',
     xlab = 'Date',
     ylab = 'Deaths Per 100,000',
     type = 'l', 
     col = 'red',
     ylim = c(8, 25))
lines(mortality$`Week Ending Date`, 
      mortality$`Deaths Per 100,000`, 
      col = 'blue')
legend(x = 'topright', 
       lty = 1, 
       lwd = 1, 
       legend = c('Actual', 'Predicted'), 
       col = c('blue', 'red'))

# Calculate 2020 excess mortality, based on SARIMA predictions
predict_2020$`Prediction Mismatch` <- (predict_2020$`Predicted Deaths Per 100,000` - mortality$`Deaths Per 100,000`)
mismatch_2020 <- sum(predict_2020
                     [predict_2020$`Week Ending Date` >= as.Date('2020-03-11', '%Y-%m-%d') &  
                      predict_2020$`Week Ending Date` <= as.Date('2020-05-02', '%Y-%m-%d'),]
                    $`Prediction Mismatch`)

mismatch_2020/52
mismatch_2020*as.numeric(ny_population$`Population Estimate`[11])/as.numeric(100000)

# This suggests excess mortality of ~1.12 per 100,000 per week, for a total of ~11,350 over this period. 

# CDC calculates excess mortality of 24,172 for March 11 - May 2: https://www.cdc.gov/mmwr/volumes/69/wr/mm6919e5.htm

# With that said, total mortality for that period was 26,421, so the claim of over 90% excess mortality seems agressive.

# As the CDC determined excess mortality to be more than double that number, this model 
#     could likely benefit from significant further development.

# However, as mentioned above, the CDC has set the "baseline" mortality for that period
#      to roughly 15% of what it was for that period in 2019, which should also warrant further attention.  
