#install.packages("quantmod")
#install.packages("zoo")
library(quantmod)
library(forecast)
library(zoo) 
library(xts)
library(tsibble)
library(fpp3)


#pulling the stock ticker for SP500 Food Products Index - Same time range as other dataset
getSymbols("^SP500-302020",
           src="yahoo",
           from = "2003-01-01",
           to = "2020-12-31")


# Retrieve the data from quantmod (assuming it's already in an xts object)
stock_data <- Cl(`SP500-302020`)  # Get the closing prices as an xts object

# Create a complete time sequence including weekends
all_dates <- seq.Date(from = as.Date("2003-01-02"), to = as.Date("2020-12-31"), by = "day")
stock_data_full <- merge(stock_data, xts(, all_dates))  # Merge with full date sequence

#adjusting the column names 
colnames(stock_data_full) <- c("Close")

# Check the result
head(stock_data_full)

# Forward fill the missing values
stock_data_filled <- na.locf(stock_data_full)

# View the data after filling missing values
head(stock_data_filled)

#plotting xts object
plot(stock_data_filled)

#decomposition plot
SP500_ts <- ts(stock_data_filled,frequency=365)
plot(decompose(SP500_ts))

#visualization 1 - Trend
SP500_ts.decom <- decompose(SP500_ts, type = "mult")
plot(SP500_ts.decom)
Trend <- SP500_ts.decom$trend
Seasonal <- SP500_ts.decom$seasonal
ts.plot(cbind(Trend, Trend * Seasonal), lty=1:2)


#visualization 2 - ACF Plot
acf(SP500_ts)


#visualization 3 - Seasonality
# Convert xts to tsibble
df <- data.frame(Date = zoo::index(stock_data_filled), Close = zoo::coredata(stock_data_filled))
tsibble_data <- as_tsibble(df, index = Date)
# Check the result
#print(tsibble_data)

tsibble_data %>%
  gg_season(Close, labels = "both") +
  labs(y = "Closing Price ($)",
       title = "Seasonal plot: S&P500 Food Price Index")


#visualization 4 - 5 and 7 Moving Average
tsibble_data_subset <- tail(tsibble_data,100)
tsibble_data_ma <- tsibble_data_subset |>
  mutate(
    `5-MA` = slider::slide_dbl(Close, mean,
                               .before = 2, .after = 2, .complete = TRUE),
    '7-MA' = slider::slide_dbl(Close, mean,
                               .before = 3, .after = 3, .complete = TRUE)            
    
  )

tsibble_data_ma |>
  autoplot(Close) +
  geom_line(aes(y = `5-MA`), colour = "orange") +
  geom_line(aes(y = `7-MA`), colour = "green") +
  labs(y = "Closing Price ($)",
       x = "Month",
       title = "Closing Price for S&P500 Food Price Index")



