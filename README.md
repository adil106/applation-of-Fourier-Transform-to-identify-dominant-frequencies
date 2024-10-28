# Application of Fourier-Transform to identify dominant-frequencies (cyclical pattern)
Application of Fourier Transform to identify dominant frequencies in various economic variables
#Adil shah- 28th-10-24
##applation of Fourier Transform to identify dominant frequencies in various economic variables

#Load necessary libraries
library(tidyverse)
library(lubridate)

# Load the data
data <- read.csv("business cycle data.csv")
view(data)
print(data)

# Convert Date column to Date format
data$Date <- as.Date(data$Date, format="%Y-%m-%d")

# View the data structure
str(data)
# Check if the column exists in the dataset
names(data)

# Check for missing values in Output_Growth
sum(is.na(data$Output.growth))


# View unique values in Output growth to identify any non-numeric entries
unique(data$Output.growth)

data$Output.growth <- as.numeric(data$Output.growth)

# Convert data to time series (monthly frequency)
interest.rate_ts <- ts(data$Interest.rate, start=c(2000,1), frequency=12)
unemployment_ts <- ts(data$Unemployment, start=c(2000,1), frequency=12)
output.growth_ts <- ts(data$Output.growth, start=c(2000,1), frequency=12)

# Plot each variable
par(mfrow=c(3,1))
plot(interest.rate_ts, main="Interest Rate Over Time", ylab="Interest Rate (%)", xlab="Year")
plot(unemployment_ts, main="Unemployment Rate Over Time", ylab="Unemployment (%)", xlab="Year")
plot(output.growth_ts, main="Output Growth Over Time", ylab="Growth (%)", xlab="Year")
par(mfrow=c(1,1))

# Apply Fourier Transform to each time series- to identify dominant frequencies in each variable
interest.rate_fft <- fft(interest.rate_ts)
unemployment_fft <- fft(unemployment_ts)
output.growth_fft <- fft(output.growth_ts)

# Calculate magnitudes (amplitudes) of frequencies- Extract and Interpret Frequencies
#To interpret, we need to extract the frequencies and magnitudes.In Fourier analysis,
#the Mod function gives us the amplitude (strength) of each frequency component.
interest.rate_amp <- Mod(interest.rate_fft)
unemployment_amp <- Mod(unemployment_fft)
output.growth_amp <- Mod(output.growth_fft)

# Frequencies (cycles per month)
freq <- (0:(length(interest.rate_amp)-1)) / length(interest.rate_amp)

# Plot amplitude vs frequency
plot(freq, interest.rate_amp, type="h", main="Interest Rate Frequency Spectrum", xlab="Frequency (cycles/month)", ylab="Amplitude")
plot(freq, unemployment_amp, type="h", main="Unemployment Frequency Spectrum", xlab="Frequency (cycles/month)", ylab="Amplitude")
plot(freq, output.growth_amp, type="h", main="Output Growth Frequency Spectrum", xlab="Frequency (cycles/month)", ylab="Amplitude")


# Find the cycle duration in months for the highest amplitudes
# Calculate the cycle duration in months for the top frequencies
#Filter Dominant Cycles- Focus on the most prominent (highest amplitude) frequencies that match business cycle lengths 
#(e.g., 5–10 years) by filtering lower amplitudes.
cycle_interest.rate <- 1 / freq[which.max(interest.rate_amp[2:length(interest.rate_amp)])]
cycle_unemployment <- 1 / freq[which.max(unemployment_amp[2:length(unemployment_amp)])]
cycle_output.growth <- 1 / freq[which.max(output.growth_amp[2:length(output.growth_amp)])]

cat("Interest Rate Dominant Cycle:", cycle_interest.rate, "months\n")
cat("Unemployment Dominant Cycle:", cycle_unemployment, "months\n")
cat("Output Growth Dominant Cycle:", cycle_output.growth, "months\n")

# Use Inverse FFT with dominant cycles (keeping only significant components)
#Reconstruct with Dominant Frequencies
interest.rate_reconstructed <- Re(fft(interest.rate_fft, inverse=TRUE)/length(interest.rate_fft))
unemployment_reconstructed <- Re(fft(unemployment_fft, inverse=TRUE)/length(unemployment_fft))
output.growth_reconstructed <- Re(fft(output.growth_fft, inverse=TRUE)/length(output.growth_fft))

# Plot reconstructed cycles
par(mfrow=c(3,1))
plot(interest.rate_reconstructed, type="l", main="Reconstructed Interest Rate Cycle")
plot(unemployment_reconstructed, type="l", main="Reconstructed Unemployment Cycle")
plot(output.growth_reconstructed, type="l", main="Reconstructed Output Growth Cycle")
par(mfrow=c(1,1))
