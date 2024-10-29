# Application of Fourier-Transform to identify dominant-frequencies (cyclical pattern)
Application of Fourier Transform to identify dominant frequencies in various economic variables
#Adil shah- 28th-10-24
##applation of Fourier Transform to identify dominant frequencies in various economic variables

#Load necessary libraries
library(tidyverse)
library(lubridate)
data <- read.csv("business cycle data.csv")
setwd("C:/Users/adil/OneDrive/Desktop/research papers")
library(tidyverse)
library(lubridate)
data <- read.csv("business cycle data.csv")
view(data)
#Load necessary libraries
library(tidyverse)
print(data)
data$Date <- as.Date(data$Date, format="%Y-%m-%d")
str(data)
names(data)
sum(is.na(data$Output.growth))
unique(data$Output.growth)
data$Output.growth <- as.numeric(data$Output.growth)
Inflation_ts <- ts(data$Inflation, start=c(2000,1), frequency=12)
interest.rate_ts <- ts(data$Interest.rate, start=c(2000,1), frequency=12)
unemployment_ts <- ts(data$Unemployment, start=c(2000,1), frequency=12)
output.growth_ts <- ts(data$Output.growth, start=c(2000,1), frequency=12)
S.P_ts <- ts(data$S.P, start=c(2000,1), frequency=12)
Gold_ts <- ts(data$Gold, start=c(2000,1), frequency=12)
Treasury_ts <- ts(data$Treasury, start=c(2000,1), frequency=12)
par(mfrow=c(7,1))
plot(interest.rate_ts, main="Interest Rate Over Time", ylab="Interest Rate (%)", xlab="Year")
plot(interest.rate_ts, main="Interest Time", ylab="Interest Rate (%)", xlab="Year")
plot(interest.rate_ts, main="Interest Time", ylab="Interest Rate (%)", xlab="Year")
par(mar=c(4, 4, 2, 1))
plot(interest.rate_ts, main="Interest Time", ylab="Interest Rate (%)", xlab="Year")
dev.off()
par(mar=c(4, 4, 2, 1))
plot(interest.rate_ts, main="Interest Time", ylab="Interest Rate (%)", xlab="Year")
# Plot each variable
dev.off()  # This will close the current graphics device and reset it
par(mfrow=c(7,1))
par(mar=c(4, 4, 2, 1))
plot(interest.rate_ts, main="Interest Time", ylab="Interest Rate (%)", xlab="Year")
plot(interest.rate_ts, main="Interest Time", ylab="Interest Rate (%)", xlab="Year")
plot(interest.rate_ts, main="Interest Time", ylab="Interest Rate (%)", xlab="Year")
dev.off()
par(mar=c(4, 4, 2, 1))
plot(interest.rate_ts, main="Interest Time", ylab="Interest Rate (%)", xlab="Year")
plot(interest.rate_ts, main="Interest Rate", ylab="Interest Rate (%)", xlab="Year")
plot(unemployment_ts, main="Unemployment Rate Over Time", ylab="Unemployment (%)", xlab="Year")
plot(output.growth_ts, main="Output Growth Over Time", ylab="Growth (%)", xlab="Year")
plot(output.growth_ts, main="PPI", ylab="Growth (%)", xlab="Year")
plot(Inflation_ts, main="Inflation rate Over Time", ylab="Infation rate (%)", xlab="Year")
plot(Gold_ts, main="Gold Price Over Time", ylab="Gold Price (USD per Ounce)", xlab="Year")
plot(S.P_ts, main="S&P Index Over Time", ylab="Equity Index (USD)", xlab="Year")
library(tidyverse)
library(lubridate)
data <- read.csv("business cycle data.csv")
view(data)
data$Date <- as.Date(data$Date, format="%Y-%m-%d")
str(data)
names(data)
sum(is.na(data$Output.growth))
unique(data$Output.growth)
data$Output.growth <- as.numeric(data$Output.growth)
Inflation_ts <- ts(data$Inflation, start=c(2000,1), frequency=12)
interest.rate_ts <- ts(data$Interest.rate, start=c(2000,1), frequency=12)
unemployment_ts <- ts(data$Unemployment, start=c(2000,1), frequency=12)
output.growth_ts <- ts(data$Output.growth, start=c(2000,1), frequency=12)
S.P_ts <- ts(data$S.P, start=c(2000,1), frequency=12)
Gold_ts <- ts(data$Gold, start=c(2000,1), frequency=12)
Treasury_ts <- ts(data$Treasury, start=c(2000,1), frequency=12)
#previous margins delete
dev.off()
par(mar=c(4, 4, 2, 1))
plot(interest.rate_ts, main="Interest Rate", ylab="Interest Rate (%)", xlab="Year")
plot(unemployment_ts, main="Unemployment Rate Over Time", ylab="Unemployment (%)", xlab="Year")
plot(output.growth_ts, main="PPI", ylab="Growth (%)", xlab="Year")
plot(Inflation_ts, main="Inflation rate Over Time", ylab="Infation rate (%)", xlab="Year")
plot(Gold_ts, main="Gold Price Over Time", ylab="Gold Price (USD per Ounce)", xlab="Year")
plot(S.P_ts, main="S&P Index Over Time", ylab="Equity Index (USD)", xlab="Year")
plot(Treasury_ts, main="Bond Price index Over Time", ylab="Bond Price Index (USD)", xlab="Year")
par(mfrow=c(1,1))
interest.rate_fft <- fft(interest.rate_ts)
unemployment_fft <- fft(unemployment_ts)
output.growth_fft <- fft(output.growth_ts)
Inflation_fft <- fft(Inflation_ts)
Gold_fft <- fft(Gold_ts)
S.P_fft <- fft(S.P_ts)
Treasury_fft <- fft(Treasury_ts)
plot(Mod(interest.rate_fft), type="h", main="FFT of Interest Rate Time Series", xlab="Frequency", ylab="Magnitude")
interest.rate_amp <- Mod(interest.rate_fft)
interest.rate_amp <- Mod(interest.rate_fft)
unemployment_fft <- fft(unemployment_ts)
output.growth_fft <- fft(output.growth_ts)
Inflation_fft <- fft(Inflation_ts)
Gold_fft <- fft(Gold_ts)
S.P_fft <- fft(S.P_ts)
Treasury_fft <- fft(Treasury_ts)
interest.rate_amp <- Mod(interest.rate_fft)
unemployment_amp <- Mod(unemployment_fft)
unemployment_amp <- Mod(unemployment_fft)
unemployment_fft <- fft(unemployment_ts)
plot(Mod(unemployment_fft), type="h", main="FFT of unemployment Time Series", xlab="Frequency", ylab="Magnitude")
output.growth_fft <- fft(output.growth_ts)
plot(Mod(output.growth_fft), type="h", main="FFT of output.growth Time Series", xlab="Frequency", ylab="Magnitude")
Inflation_fft <- fft(Inflation_ts)
plot(Mod(Inflation_fft), type="h", main="FFT of Inflation rate Time Series", xlab="Frequency", ylab="Magnitude")
Gold_fft <- fft(Gold_ts)
plot(Mod(Gold_fft), type="h", main="FFT of Gold Time Series", xlab="Frequency", ylab="Magnitude")
S.P_fft <- fft(S.P_ts)
plot(Mod(S.P_fft), type="h", main="FFT of S&P Time Series", xlab="Frequency", ylab="Magnitude")
Treasury_fft <- fft(Treasury_ts)
plot(Mod(Treasury_fft), type="h", main="FFT of Treasury index Time Series", xlab="Frequency", ylab="Magnitude")
freq <- (0:(length(interest.rate_amp)-1)) / length(interest.rate_amp)
plot(freq, interest.rate_amp, type="h", main="Interest Rate Frequency Spectrum", xlab="Frequency (cycles/month)", ylab="Amplitude")
cycle_interest.rate <- 1 / freq[which.max(interest.rate_amp[2:length(interest.rate_amp)])]
cat("Interest Rate Dominant Cycle:", cycle_interest.rate, "months\n")
#Reconstruct with Dominant Frequencies
interest.rate_reconstructed <- Re(fft(interest.rate_fft, inverse=TRUE)/length(interest.rate_fft))
plot(interest.rate_reconstructed, type="l", main="Reconstructed Interest Rate Cycle")
combined_data <- data.frame(Date=data$Date, Gold=data$Gold, Equity=data$Equity, Bonds=data$Bonds,
Inflation=data$Inflation, Unemployment=data$Unemployment, Interest_Rate=data$Interest_Rate)
combined_data <- data.frame(Date=data$Date, Gold=data$Gold, S.P=data$S.P, Treasury=data$Treasury,
Inflation=data$Inflation, Unemployment=data$Unemployment, Interest.rate=data$Interest.rate)
cor_matrix <- cor(combined_data[, -1])  # Exclude Date column
print(cor_matrix)
cor_matrix <- cor(combined_data[, -1])
sapply(combined_data[, -1], is.numeric)
cor_matrix <- cor(combined_data[, -1])
numeric_data <- combined_data[, sapply(combined_data[, -1], is.numeric)]
cor_matrix <- cor(numeric_data)
combined_data$your_column <- as.numeric(as.factor(combined_data$your_column))
# Function to determine portfolio weights based on economic indicators
get_weights <- function(inflation, unemployment, interest.rate) {
if (interest.rate < 2 && unemployment < 5) {  # Expansion phase
return(c(0.6, 0.3, 0.1))  # 60% Equity, 30% Gold, 10% Bonds
} else if (interest.rate >= 2 && unemployment < 5) {  # Peak phase
return(c(0.4, 0.4, 0.2))  # 40% Equity, 40% Gold, 20% Bonds
} else if (unemployment >= 5) {  # Recession phase
return(c(0.1, 0.6, 0.3))  # 10% Equity, 60% Gold, 30% Bonds
} else {  # Recovery phase
return(c(0.5, 0.3, 0.2))  # 50% Equity, 30% Gold, 20% Bonds
}
}
# Initialize portfolio
portfolio_returns <- numeric(length(gold_ts))
# Calculate monthly portfolio returns
for (i in 2:length(gold_ts)) {
weights <- get_weights(inflation_ts[i-1], unemployment_ts[i-1], interest.rate_ts[i-1])
portfolio_returns[i] <- (weights[1] * (S.P_ts[i] - S.P_ts[i-1]) / S.P_ts[i-1]) +
(weights[2] * (gold_ts[i] - gold_ts[i-1]) / gold_ts[i-1]) +
(weights[3] * (Treasury_ts[i] - Treasury_ts[i-1]) / Treasury_ts[i-1])
}
# Initialize portfolio
portfolio_returns <- numeric(length(Gold_ts))
# Calculate monthly portfolio returns
for (i in 2:length(gold_ts)) {
weights <- get_weights(inflation_ts[i-1], unemployment_ts[i-1], interest.rate_ts[i-1])
portfolio_returns[i] <- (weights[1] * (S.P_ts[i] - S.P_ts[i-1]) / S.P_ts[i-1]) +
(weights[2] * (Gold_ts[i] - Gold_ts[i-1]) / Gold_ts[i-1]) +
(weights[3] * (Treasury_ts[i] - Treasury_ts[i-1]) / Treasury_ts[i-1])
}
# Initialize portfolio
portfolio_returns <- numeric(length(Gold_ts))
# Calculate monthly portfolio returns
for (i in 2:length(Gold_ts)) {
weights <- get_weights(inflation_ts[i-1], unemployment_ts[i-1], interest.rate_ts[i-1])
portfolio_returns[i] <- (weights[1] * (S.P_ts[i] - S.P_ts[i-1]) / S.P_ts[i-1]) +
(weights[2] * (Gold_ts[i] - Gold_ts[i-1]) / Gold_ts[i-1]) +
(weights[3] * (Treasury_ts[i] - Treasury_ts[i-1]) / Treasury_ts[i-1])
}
# Convert to time series
portfolio_returns_ts <- ts(portfolio_returns, start=c(2000,2), frequency=12)
# Calculate cumulative returns
cumulative_returns_portfolio <- cumprod(1 + portfolio_returns_ts) - 1
cumulative_returns_Gold <- cumprod(1 + (diff(gold_ts) / lag(Gold_ts, k=1))) - 1
cumulative_returns_S.P <- cumprod(1 + (diff(S.P_ts) / lag(S.P, k=1))) - 1
cumulative_returns_Treasury <- cumprod(1 + (diff(Treasury_ts) / lag(Treasury_ts, k=1))) - 1
cumulative_returns_portfolio <- cumprod(1 + portfolio_returns_ts) - 1
cumulative_returns_Gold <- cumprod(1 + (diff(Gold_ts) / lag(Gold_ts, k=1))) - 1
portfolio_returns_ts <- ts(portfolio_returns, start=c(2000,2), frequency=12)
Inflation_ts <- ts(data$Inflation, start=c(2005,9), frequency=12)
portfolio_returns_ts <- ts(portfolio_returns, start=c(2005,3), frequency=12)
cumulative_returns_portfolio <- cumprod(1 + portfolio_returns_ts) - 1
cumulative_returns_portfolio <- cumprod(1 + portfolio_returns_ts) - 1
cumulative_returns_Gold <- cumprod(1 + (diff(Gold_ts) / lag(Gold_ts, k=1))) - 1
cumulative_returns_Gold <- cumprod(1 + (diff(Gold_ts) / stats::lag(Gold_ts, k=1)[-1])) - 1
Gold_lagged <- stats::lag(Gold_ts, k=1)
cumulative_returns_Gold <- cumprod(1 + (diff(Gold_ts) / Gold_lagged[-1])) - 1
cumulative_returns_portfolio <- cumprod(1 + portfolio_returns_ts) - 1
cumulative_returns_Gold <- cumprod(1 + (diff(Gold_ts) / stats::lag(Gold_ts, k=1)[-1])) - 1
Gold_lagged <- stats::lag(Gold_ts, k=1)
cumulative_returns_Gold <- cumprod(1 + (diff(Gold_ts) / Gold_lagged[-1])) - 1
library(stats)
Gold_lagged <- stats::lag(Gold_ts, k=1)
cumulative_returns_Gold <- cumprod(1 + (diff(Gold_ts) / Gold_lagged[-1])) - 1
cumulative_returns_Gold <- cumprod(1 + (diff(Gold_ts) / lag(Gold_ts, k=1))) - 1
cumulative_returns_S.P <- cumprod(1 + (diff(S.P_ts) / lag(S.P, k=1))) - 1
cumulative_returns_Treasury <- cumprod(1 + (diff(Treasury_ts) / lag(Treasury_ts, k=1))) - 1
# Load necessary libraries
library(dplyr)
# Assuming Gold_ts, S.P_ts, and Treasury_ts are your time series data
# Cumulative returns for Gold
cumulative_returns_Gold <- cumprod(1 + (diff(Gold_ts) / Gold_ts[-length(Gold_ts)])) - 1
# Cumulative returns for S&P
cumulative_returns_S.P <- cumprod(1 + (diff(S.P_ts) / S.P_ts[-length(S.P_ts)])) - 1
# Cumulative returns for Treasury
cumulative_returns_Treasury <- cumprod(1 + (diff(Treasury_ts) / Treasury_ts[-length(Treasury_ts)])) - 1
plot(cumulative_returns_portfolio, type="l", col="blue", main="Cumulative Returns", ylab="Cumulative Return", xlab="Time")
lines(cumulative_returns_gold, col="gold")
lines(cumulative_returns_equity, col="green")
lines(cumulative_returns_bonds, col="brown")
legend("topleft", legend=c("Portfolio", "Gold", "Equity", "Bonds"), col=c("blue", "gold", "green", "brown"), lty=1)
print(cumulative_returns_Gold)
print(cumulative_returns_S.P)
print(cumulative_returns_Treasury)
cumulative_returns_Gold <- cumprod(1 + (diff(Gold_ts) / lag(Gold_ts, k=1))) - 1
cumulative_returns_S.P <- cumprod(1 + (diff(S.P_ts) / lag(S.P, k=1))) - 1
cumulative_returns_Treasury <- cumprod(1 + (diff(Treasury_ts) / lag(Treasury_ts, k=1))) - 1
# Plot cumulative returns
plot(cumulative_returns_portfolio, type="l", col="blue", main="Cumulative Returns", ylab="Cumulative Return", xlab="Time")
lines(cumulative_returns_Gold, col="Gold")
lines(cumulative_returns_S.P, col="green")
lines(cumulative_returns_Treasury, col="brown")
legend("topleft", legend=c("Portfolio", "Gold", "S.P", "Treasury"), col=c("blue", "gold", "green", "brown"), lty=1)
lines(cumulative_returns_Gold, col="Gold")
lines(cumulative_returns_Gold, col="Gold")
lines(cumulative_returns_Gold, col="Gold")
# Plot cumulative returns for the overall portfolio
plot(cumulative_returns_portfolio, type="l", col="blue",
main="Cumulative Returns", ylab="Cumulative Return", xlab="Time")
legend("topleft", legend="Portfolio", col="blue", lty=1)
# Add cumulative returns for Gold
lines(cumulative_returns_Gold, col="gold")
legend("topleft", legend=c("Portfolio", "Gold"), col=c("blue", "gold"), lty=1)
plot(cumulative_returns_Gold, type="l", col="blue",
main="Cumulative Returns", ylab="Cumulative Return", xlab="Time")
legend("topleft", legend="Portfolio", col="blue", lty=1)
plot(cumulative_returns_S.P, type="l", col="blue",
main="Cumulative Returns", ylab="Cumulative Return", xlab="Time")
legend("topleft", legend="Portfolio", col="blue", lty=1)
plot(cumulative_returns_Treasury, type="l", col="blue",
main="Cumulative Returns", ylab="Cumulative Return", xlab="Time")
legend("topleft", legend="Portfolio", col="blue", lty=1)
# Plot cumulative returns
plot(cumulative_returns_portfolio, type="l", col="blue", main="Cumulative Returns", ylab="Cumulative Return", xlab="Time")
lines(cumulative_returns_Gold, col="Gold")
lines(cumulative_returns_S.P, col="green")
lines(cumulative_returns_Treasury, col="brown")
legend("topleft", legend=c("Portfolio", "Gold", "S.P", "Treasury"), col=c("blue", "gold", "green", "brown"), lty=1)
# Plot cumulative returns for the overall portfolio
plot(cumulative_returns_portfolio, type="l", col="blue",
main="Cumulative Returns", ylab="Cumulative Return", xlab="Time")
legend("topleft", legend="Portfolio", col="blue", lty=1)
# Plot cumulative returns for the overall portfolio
plot(cumulative_returns_portfolio, type="l", col="blue",
main="Cumulative Returns for overall portfolio", ylab="Cumulative Return", xlab="Time")
legend("topleft", legend="Portfolio", col="blue", lty=1)
plot(cumulative_returns_Gold, type="l", col="blue",
main="Cumulative Returns for Gold", ylab="Cumulative Return", xlab="Time")
legend("topleft", legend="Portfolio", col="blue", lty=1)
