library(tidyverse)
library(readxl)
library(PerformanceAnalytics)
library(writexl)
library(timeSeries)
library(fPortfolio)
library(ggplot2)
library(dplyr)
library(quantmod)
library(zoo)

## DRAWING A CANDLE CHART AND ADDING VARIOUS TECHNICAL INDICATORS

icici<-getSymbols("ICICIBANK.NS",from="2022-08-15",auto.assign = FALSE)
class(icici)

candleChart(icici)

# add sma50,sma100,sma200,ema50,macd,rsi,smi,wpr,volatility indicator(bbands)and volume indicator (obv)to chart
addSMA(50,with.col = Ad,col = "green")
addSMA(100,with.col = Ad,col="yellow")
addSMA(200,with.col = Ad,col="pink")
addEMA(50,with.col = Ad,col="blue")
addMACD(fast=12,slow=26,signal=9,type="EMA",histogram = TRUE)
addRSI(n=14,maType = "EMA")
addSMI(n=13,slow=25,fast=2,signal = 9,ma.type = "EMA")
addWPR(n=14)
addBBands(n=20,sd=2)
addOBV()

# storing the above mentioned indicators in icici file or separate user defined variables
icici$sma50<-SMA(Ad(icici),50)
icici$sma100<-SMA(Ad(icici),100)
icici$sma200<-SMA(Ad(icici),200)
icici$ema50<-EMA(Ad(icici),50)
macd<-MACD(Ad(icici),nFast = 12,nSlow = 26,nSig = 9)
rsi<-RSI(Ad(icici),n=14,maType = "EMA")
smi<-SMI(HLC(icici),n=13,nSlow =25,nFast=2,nSig = 9,ma.type = "EMA")
wpr<-WPR(HLC(icici),n=14)
bbands<-BBands(HLC(icici),n=20,sd=2)
obv<-OBV(Cl(icici),volume = Vo(icici))

# writing the above icici file to excel
icici<-as.data.frame(icici)
class(icici)
head(icici)
icici$date=row.names(icici)
head(icici)
icici<-relocate(icici,date,.before="ICICIBANK.NS.Open")
head(icici)
write_xlsx(icici,"technical_indicators.xlsx")


# COMPONENTS OF CANDLE CHART BELOW


# Candlesticks: Represent the opening and closing prices for a specified time period.
# The body is green or white if the price rises, and red or black if it falls.

# Wicks (Shadows): Vertical lines extending above and below the body, showing the highest and lowest prices during the period.

# Color Coding: Green/white indicates rising prices, while red/black indicates falling prices.

# Patterns: Formations such as a hammer can indicate potential price direction changes.

# Time Frame: Each candlestick represents a specific time period (e.g., 1 minute, 1 hour, daily).


## INTERPRETATION AND TRADING RULES OF VARIOUS TECHNICAL INDICATORS BELOW

# SMA50
# Interpretation: Represents the average price over the last 50 periods, providing a smoother trend line.
# Trading Rule: Buy when the price crosses above the SMA50, indicating possible upward momentum.
# Sell when the price crosses below the SMA50, indicating potential downward movement.

# EMA50
# Interpretation: Places more emphasis on recent prices, making it more responsive to price changes compared to SMA.
# Trading Rule: Buy when the price crosses above EMA50 for a bullish signal, and sell when it crosses below EMA50 for a bearish signal.

# MACD
# Interpretation: Consists of the MACD line (difference between 12-period EMA and 26-period EMA) 
# and the signal line (9-period EMA of the MACD line). It shows trend direction and momentum.
# Trading Rule: Buy when the MACD line crosses above the signal line (bullish crossover). 
# Sell when the MACD line crosses below the signal line (bearish crossover).

# RSI
# Interpretation: Measures the speed and change of price movements, identifying overbought (above 70) and oversold (below 30) conditions.
# Trading Rule: Buy when RSI crosses above 30 from below, suggesting a potential upward reversal. 
# Sell when RSI crosses below 70 from above, indicating a potential downward reversal.

# SMI
# Interpretation: A combination of the Stochastic Oscillator and Momentum Index, measuring the closing price relative to the range of highs and lows.
# Trading Rule: Buy when SMI crosses above -50 from below, indicating possible upward momentum. 
# Sell when SMI crosses below +50 from above, suggesting possible downward momentum.

# WPR (Williams %R)
# Interpretation: Evaluates the current closing price in relation to the highest high over a specific lookback period (typically 14).
# Trading Rule: Buy when WPR crosses above -80 from below, indicating oversold conditions. 
# Sell when WPR crosses below -20 from above, indicating overbought conditions.

# Bollinger Bands (BBands)
# Interpretation: Comprises a middle band (SMA or EMA), an upper band (2 standard deviations above the middle band), 
# and a lower band (2 standard deviations below the middle band). It helps identify volatility and potential reversal points.
# Trading Rule: Buy when the price touches or drops below the lower band, suggesting potential oversold conditions. 
# Sell when the price touches or rises above the upper band, indicating potential overbought conditions.

# OBV (On-Balance Volume)
# Interpretation: Measures buying and selling pressure as a cumulative indicator, adding volume on up days and subtracting on down days.
# Trading Rule: Buy when OBV begins to rise from a base or shows divergence (rising OBV while price falls). 
# Sell when OBV falls sharply from a peak or shows divergence (falling OBV while price rises).


## DEMONSTRATING HOW A TRADING STRATEGY WORKS, BACKTESTING IT AND PRINTING IT IN EXCEL FORMAT
## TAKING EMA STRATEGY

# Load required libraries
library(quantmod)
library(TTR)
library(PerformanceAnalytics)
library(dplyr)
library(writexl)
library(ggplot2)

# Define stock symbol and date range
symbol <- "ICICIBANK.NS"
start_date <- "2022-01-01"

# Download stock data
stock <- getSymbols(symbol, from = start_date, auto.assign = FALSE)
stock <- na.locf(stock)  # Replace NAs with last observation

# Calculate EMAs
n1 <- 12  # Short EMA
n2 <- 26  # Long EMA
ema_short <- EMA(Ad(stock), n1)
ema_long <- EMA(Ad(stock), n2)

# Generate trading signal
ema_signal <- lag(ifelse((ema_short > ema_long) & lag(ema_short) < lag(ema_long), 1, 
                         ifelse((ema_short < ema_long) & lag(ema_short) > lag(ema_long), -1, 0)))

# Replace NA values with 0
ema_signal[is.na(ema_signal)] <- 0

# Create trading positions using a for loop
positions <- numeric(length(ema_signal))
changeover <- 0

for (i in 1:length(ema_signal)) {
  if (ema_signal[i] == 1) {
    positions[i] <- 1
    changeover <- 1
  } else if (ema_signal[i] == -1) {
    positions[i] <- 0
    changeover <- 0
  } else {
    positions[i] <- changeover
  }
}

# Calculate returns
daily_returns <- Return.calculate(Ad(stock), method = "discrete")
daily_returns[is.na(daily_returns)] <- 0
strategy_returns <- daily_returns * positions
cum_strategy_returns <- cumprod(1 + strategy_returns) - 1

# Output results
cat("Strategy Annualized Return:", Return.annualized(strategy_returns), "\n")
cat("Buy & Hold Annualized Return:", Return.annualized(daily_returns), "\n")
cat("Maximum Drawdown:", maxDrawdown(strategy_returns), "\n")

# Prepare data for Excel output
output_data <- data.frame(
  Date = index(stock),
  ICICIBANK_Open = Op(stock),
  ICICIBANK_Close = Cl(stock),
  EMA_Short = ema_short,
  EMA_Long = ema_long,
  Signal = ema_signal,
  Positions = positions,
  Daily_Returns = daily_returns,
  Strategy_Returns = strategy_returns,
  Cum_Strategy_Returns = cum_strategy_returns
)

# Write results to Excel
write_xlsx(output_data, "backtesting_ema_strategy.xlsx")

# Plotting cumulative returns
ggplot() +
  geom_line(aes(x = index(stock), y = cumprod(1 + daily_returns) - 1), color = 'blue') +
  geom_line(aes(x = index(stock), y = cum_strategy_returns), color = 'red') +
  labs(title = paste(symbol, "EMA Strategy Backtest"),
       x = 'Date',
       y = 'Cumulative Returns',
       caption = 'Blue: Buy & Hold, Red: EMA Strategy') +
  theme_minimal()


## MAKING A FUNCTION FOR BACKTESTING OF VARIOUS TRADING STRATEGIES


# Load required libraries
library(quantmod)
library(TTR)
library(PerformanceAnalytics)
library(dplyr)
library(writexl)
library(ggplot2)

# Function for EMA Strategy
EMA_strategy <- function(symbol, n1 = 12, n2 = 26) {
  stock <- getSymbols(symbol, from = "2022-01-01", auto.assign = FALSE)
  stock <- na.locf(stock)  # Replace NAs with last observation
  
  ema_short <- EMA(Ad(stock), n1)
  ema_long <- EMA(Ad(stock), n2)
  
  # Generate trading signal
  ema_signal <- lag(ifelse((ema_short > ema_long) & lag(ema_short) < lag(ema_long), 1, 
                           ifelse((ema_short < ema_long) & lag(ema_short) > lag(ema_long), -1, 0)))
  
  # Replace NA values with 0
  ema_signal[is.na(ema_signal)] <- 0
  
  # Create trading positions using a for loop
  positions <- numeric(length(ema_signal))
  changeover <- 0
  
  for (i in 1:length(ema_signal)) {
    if (ema_signal[i] == 1) {
      positions[i] <- 1
      changeover <- 1
    } else if (ema_signal[i] == -1) {
      positions[i] <- 0
      changeover <- 0
    } else {
      positions[i] <- changeover
    }
  }
  
  # Calculate returns
  daily_returns <- Return.calculate(Ad(stock), method = "discrete")
  daily_returns[is.na(daily_returns)] <- 0
  strategy_returns <- daily_returns * positions
  cum_strategy_returns <- cumprod(1 + strategy_returns) - 1
  
  # Output results
  cat("Strategy Annualized Return:", Return.annualized(strategy_returns), "\n")
  cat("Buy & Hold Annualized Return:", Return.annualized(daily_returns), "\n")
  cat("Maximum Drawdown:", maxDrawdown(strategy_returns), "\n")
  
  # Plotting
  ggplot() +
    geom_line(aes(x = index(stock), y = cumprod(1 + daily_returns) - 1), color = 'blue') +
    geom_line(aes(x = index(stock), y = cum_strategy_returns), color = 'red') +
    labs(title = paste(symbol, "EMA Strategy Backtest"),
         x = 'Date',
         y = 'Cumulative Returns',
         caption = 'Blue: Buy & Hold, Red: EMA Strategy') +
    theme_minimal()
}

# Function for SMA Strategy
SMA_strategy <- function(symbol, n = 50) {
  stock <- getSymbols(symbol, from = "2022-01-01", auto.assign = FALSE)
  stock <- na.locf(stock)
  
  sma <- SMA(Ad(stock), n)
  
  # Generate trading signal
  sma_signal <- lag(ifelse((Ad(stock) > sma) & (lag(Ad(stock)) < lag(sma)), 1, 
                           ifelse((Ad(stock) < sma) & (lag(Ad(stock)) > lag(sma)), -1, 0)))
  sma_signal[is.na(sma_signal)] <- 0
  
  # Create trading positions using a for loop
  positions <- numeric(length(sma_signal))
  changeover <- 0
  
  for (i in 1:length(sma_signal)) {
    if (sma_signal[i] == 1) {
      positions[i] <- 1
      changeover <- 1
    } else if (sma_signal[i] == -1) {
      positions[i] <- 0
      changeover <- 0
    } else {
      positions[i] <- changeover
    }
  }
  
  # Calculate returns
  daily_returns <- Return.calculate(Ad(stock), method = "discrete")
  daily_returns[is.na(daily_returns)] <- 0
  strategy_returns <- daily_returns * positions
  cum_strategy_returns <- cumprod(1 + strategy_returns) - 1
  
  # Output results
  cat("Strategy Annualized Return:", Return.annualized(strategy_returns), "\n")
  cat("Buy & Hold Annualized Return:", Return.annualized(daily_returns), "\n")
  cat("Maximum Drawdown:", maxDrawdown(strategy_returns), "\n")
  
  # Plotting
  ggplot() +
    geom_line(aes(x = index(stock), y = cumprod(1 + daily_returns) - 1), color = 'blue') +
    geom_line(aes(x = index(stock), y = cum_strategy_returns), color = 'orange') +
    labs(title = paste(symbol, "SMA Strategy Backtest"),
         x = 'Date',
         y = 'Cumulative Returns',
         caption = 'Blue: Buy & Hold, Orange: SMA Strategy') +
    theme_minimal()
}

# Function for MACD Strategy
MACD_strategy <- function(symbol) {
  stock <- getSymbols(symbol, from = "2022-01-01", auto.assign = FALSE)
  stock <- na.locf(stock)
  
  macd <- MACD(Ad(stock), 12, 26, 9, type = "EMA")
  
  # Generate trading signal
  macd_signal <- lag(ifelse(macd$macd > macd$signal, 1, ifelse(macd$macd < macd$signal, -1, 0)))
  macd_signal[is.na(macd_signal)] <- 0
  
  # Create trading positions using a for loop
  positions <- numeric(length(macd_signal))
  changeover <- 0
  
  for (i in 1:length(macd_signal)) {
    if (macd_signal[i] == 1) {
      positions[i] <- 1
      changeover <- 1
    } else if (macd_signal[i] == -1) {
      positions[i] <- 0
      changeover <- 0
    } else {
      positions[i] <- changeover
    }
  }
  
  # Calculate returns
  daily_returns <- Return.calculate(Ad(stock), method = "discrete")
  daily_returns[is.na(daily_returns)] <- 0
  strategy_returns <- daily_returns * positions
  cum_strategy_returns <- cumprod(1 + strategy_returns) - 1
  
  # Output results
  cat("Strategy Annualized Return:", Return.annualized(strategy_returns), "\n")
  cat("Buy & Hold Annualized Return:", Return.annualized(daily_returns), "\n")
  cat("Maximum Drawdown:", maxDrawdown(strategy_returns), "\n")
  
  # Plotting
  ggplot() +
    geom_line(aes(x = index(stock), y = cumprod(1 + daily_returns) - 1), color = 'blue') +
    geom_line(aes(x = index(stock), y = cum_strategy_returns), color = 'green') +
    labs(title = paste(symbol, "MACD Strategy Backtest"),
         x = 'Date',
         y = 'Cumulative Returns',
         caption = 'Blue: Buy & Hold, Green: MACD Strategy') +
    theme_minimal()
}

# Function for RSI Strategy
RSI_strategy <- function(symbol, n = 14) {
  stock <- getSymbols(symbol, from = "2022-01-01", auto.assign = FALSE)
  stock <- na.locf(stock)
  
  rsi <- RSI(Ad(stock), n)
  
  # Generate trading signal
  rsi_signal <- lag(ifelse(rsi < 30, 1, ifelse(rsi > 70, -1, 0)))
  rsi_signal[is.na(rsi_signal)] <- 0
  
  # Create trading positions using a for loop
  positions <- numeric(length(rsi_signal))
  changeover <- 0
  
  for (i in 1:length(rsi_signal)) {
    if (rsi_signal[i] == 1) {
      positions[i] <- 1
      changeover <- 1
    } else if (rsi_signal[i] == -1) {
      positions[i] <- 0
      changeover <- 0
    } else {
      positions[i] <- changeover
    }
  }
  
  # Calculate returns
  daily_returns <- Return.calculate(Ad(stock), method = "discrete")
  daily_returns[is.na(daily_returns)] <- 0
  strategy_returns <- daily_returns * positions
  cum_strategy_returns <- cumprod(1 + strategy_returns) - 1
  
  # Output results
  cat("Strategy Annualized Return:", Return.annualized(strategy_returns), "\n")
  cat("Buy & Hold Annualized Return:", Return.annualized(daily_returns), "\n")
  cat("Maximum Drawdown:", maxDrawdown(strategy_returns), "\n")
  
  # Plotting
  ggplot() +
    geom_line(aes(x = index(stock), y = cumprod(1 + daily_returns) - 1), color = 'blue') +
    geom_line(aes(x = index(stock), y = cum_strategy_returns), color = 'purple') +
    labs(title = paste(symbol, "RSI Strategy Backtest"),
         x = 'Date',
         y = 'Cumulative Returns',
         caption = 'Blue: Buy & Hold, Purple: RSI Strategy') +
    theme_minimal()
}

# Main function to prompt user input and execute chosen strategy
select_trading_strategy <- function() {
  cat("Choose a trading strategy (EMA, SMA, MACD, RSI): ")
  strategy <- tolower(readline())
  
  cat("Enter the stock symbol (e.g., BPCL.NS): ")
  symbol <- toupper(readline())
  
  switch(strategy,
         ema = EMA_strategy(symbol),
         sma = SMA_strategy(symbol),
         macd = MACD_strategy(symbol),
         rsi = RSI_strategy(symbol),
         cat("Invalid strategy. Please choose from EMA, SMA, MACD, RSI.\n"))
}

# Run the main function
select_trading_strategy()

