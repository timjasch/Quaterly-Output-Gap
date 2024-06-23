library(readxl)
library(tidyverse)
library(dplyr)
library(zoo)
library(tempdisagg)
library(svglite)

#### Read in the Excel files ####
# Real GDP (2015 Mrd-MarkEuro, Quarterly). Source: DESTATIS
destatis_data <- read_excel("DESTATIS_Data.xlsx")
# Potential, Real GDP (2015 Mrd-MarkEuro, Yearly).
# Output Gap (in % of Potential GDP, Yearly). Source: AMECO.
ameco_data <- read_excel("AMECO_Data.xlsx")
# Delete the first 5 rows
ameco_data <- ameco_data[-c(1:5), ]
ameco_data <- as.data.frame(lapply(ameco_data, as.numeric))
ameco_data <- ameco_data %>% mutate(output_gap = output_gap/100)

# Output Gap (Percent, Yearly). Source: WEO. 
# output_gap_weo <- read_excel("WEO_Real_GDP.xlsx")
# output_gap_weo$output_gap <- output_gap_weo$'Output Gap'/100

# Create Time-Series Objects
real_gdp_quarterly_ts <- ts(destatis_data$real_gdp, start = c(1991, 1), frequency = 4)
potential_gdp_yearly_ts <- ts(ameco_data$potential_gdp, start = c(1991, 1), frequency = 1)
output_gap_yearly_ameco_ts <- ts(ameco_data$output_gap[1:33], start = c(1991, 1), frequency = 1)
real_gdp_yearly_ameco_ts <- ts(ameco_data$real_gdp[1:33], start = c(1991, 1), frequency = 1)

#### Interpolation & Dataframe ####
potential_interpolated <- td(potential_gdp_yearly_ts ~ 1, to = "quarterly", method = "denton-cholette", conversion = "sum")
# Create Time-Series Object for Interpolated Series
potential_gdp_quarterly_ts <- ts(predict(potential_interpolated), start = c(1991, 1), frequency = 4)

# Create a dataframe with the interpolated data
potential_gdp_vector <- as.vector(potential_gdp_quarterly_ts)[1:133]
real_gdp_vector <- as.vector(real_gdp_quarterly_ts)
quarters <- seq(as.Date("1991-01-01"), as.Date("2024-01-01"), by = "quarter")

quarterly_data <- tibble(
  date = quarters,
  potential_gdp = (potential_gdp_quarterly_ts)[1:133],
  real_gdp = real_gdp_quarterly_ts,
  output_gap = (real_gdp_quarterly_ts - (potential_gdp_quarterly_ts)[1:133])/(potential_gdp_quarterly_ts)[1:133]
)

output_gap_quaterly_ts <- ts(quarterly_data$output_gap, start = c(1991, 1), frequency = 4)

# Data Investigation
mean(quarterly_data$potential_gdp)
mean(quarterly_data$real_gdp)
mean(quarterly_data$output_gap)
# Potential and Real GDP have similar means, the output gap is close to zero.

#### Graphs ####

# Define colors
red <- "#8B0000"
blue <- "#00008B"
orange <- "#cc6e1b"

# Plot the real and potential GDP over time (1991-2024)
potential_real_plot <- quarterly_data %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = potential_gdp, color = "Potential GDP")) +
  geom_line(aes(y = real_gdp, color = "Real GDP")) +
  theme_minimal() +
  scale_color_manual(values = c("Potential GDP" = blue, "Real GDP" = red)) +
  labs(title = "Real and Potential GDP 1991-2024 (Potential Interpolated), quarterly Frequency",
    x = "Year",
    y = "GDP (2015 Billion Euro)") +
  theme(legend.position = "bottom") +
  labs(color = NULL)

# Plot the output gap over time (1991-2024)
output_gap_plot <- quarterly_data %>%
  ggplot(aes(x = date, y = (100*output_gap))) +
  geom_line(color = orange) +
  theme_minimal() +
  labs(title = "Output Gap 1991-2024 (Interpolated), quarterly Frequency",
    x = "Year",
    y = "Output Gap in %") +
  theme(legend.position = "none") +
  ylim(-10, 10)

# Filter the data for the years 2019-2024
quarterly_data_recent <- quarterly_data %>%
    filter(date >= as.Date("2019-01-01"))

# Plot the real and potential GDP over time (2019-2024)
potential_real_plot_recent <- quarterly_data_recent %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = potential_gdp, color = "Potential GDP")) +
  geom_line(aes(y = real_gdp, color = "Real GDP")) +
  theme_minimal() +
  scale_color_manual(values = c("Potential GDP" = blue, "Real GDP" = red)) +
  labs(title = "Real and Potential GDP 2019-2024 (Potential Interpolated), quarterly Frequency",
    x = "Year",
    y = "GDP (2015 Billion Euro)") +
  theme(legend.position = "bottom") +
  labs(color = NULL)

# Plot the output gap over time (2019-2024)
output_gap_plot_recent <- quarterly_data_recent %>%
  ggplot(aes(x = date, y = (100*output_gap))) +
  geom_line(color = orange) +
  theme_minimal() +
  labs(title = "Output Gap 2019-2024 (Interpolated), quarterly Frequency",
    x = "Year",
    y = "Output Gap in %") +
  theme(legend.position = "none") +
  ylim(-10, 10)

#### Comparing the output gap from Ameco with the interpolated output gap ####
# Plot the output gap from the Europe Comission over time (1991-2024)
output_gap_plot_ameco <- ameco_data %>%
  ggplot(aes(x = Year, y = (100*output_gap))) +
  geom_line(color = orange) +
  theme_minimal() +
  labs(title = "Output Gap 1991-2024 (Ameco-Estimates), Yearly Frequency",
    x = "Year",
    y = "Output Gap in %") +
  theme(legend.position = "none") +
  ylim(-10, 10)

# Plot the output gap from the Europe Comission over time (2019-2024)
output_gap_plot_ameco_recent <- ameco_data %>%
  filter(Year >= 2019) %>%
  ggplot(aes(x = Year, y = (100*output_gap))) +
  geom_line(color = orange) +
  theme_minimal() +
  labs(title = "Output Gap 2019-2024 (Ameco-Estimates), Yearly Frequency",
    x = "Year",
    y = "Output Gap in %") +
  theme(legend.position = "none") +
  ylim(-10, 10)

# Compare the plots of the interpolated output gap and the output gap from Ameco for the years 1991-2024
comparison_plot <- cowplot::plot_grid(output_gap_plot, output_gap_plot_ameco, nrow = 2)
comparison_plot

# Compare the plots of the interpolated output gap and the output gap from the Ameco for the years 2019-2024
comparison_plot_recent <- cowplot::plot_grid(output_gap_plot_recent, output_gap_plot_ameco_recent, nrow = 2)
comparison_plot_recent

# Export the plots to a Svg file
ggsave("potential_real_plot.svg", plot = potential_real_plot, device = "svg")
ggsave("comparison_plot.svg", plot = comparison_plot, device = "svg")
ggsave("comparison_plot_recent.svg", plot = comparison_plot_recent, device = "svg")

# Investigating the validity of using Real GDP from Destatis and Potential GDP from AMECO
# R^(2)_Measure for intercept = 0 and coefficient = 1

Rsqr <- 1 - sum(((potential_gdp$'Real GDP (Ameco)')[1:33] - (potential_gdp$'Real GDP (Destatis)')[1:33])^(2))/sum(((potential_gdp$'Real GDP (Ameco)') - mean(potential_gdp$'Real GDP (Ameco)'))^(2))

#### Comparing the output gap from Ameco and the ECB with the interpolated output gap ####

# Aggregate quarterly real output to yearly data
real_gdp_destatis_yearly_ts <- aggregate(real_gdp_quarterly_ts, FUN = sum)
real_gdp_destatis_yearly_ts <- ts(real_gdp_destatis_yearly_ts[1:33], start = c(1991, 1), frequency = 1)
potential_gdp_interpolated_yearly_ts <- aggregate(potential_gdp_quarterly_ts, FUN = sum)
potential_gdp_interpolated_yearly_ts_short <- ts(potential_gdp_interpolated_yearly_ts[1:33], start = c(1991, 1), frequency = 1)

output_gap_yearly_interpolated_ts <- (real_gdp_destatis_yearly_ts - potential_gdp_interpolated_yearly_ts_short)/potential_gdp_interpolated_yearly_ts_short

# Combine the two ts objects into a data frame
compare_output_gap <- data.frame(date = time(output_gap_yearly_interpolated_ts), output_gap_ameco = as.vector(output_gap_yearly_ameco_ts), output_gap_interpolated = as.vector(outputgap_yearly_interpolated_ts))

# Plot the output gap in comparison
compare_output_gap_plot <- compare_output_gap %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = output_gap_ameco, color = "Ameco Output Gap")) +
  geom_line(aes(y = output_gap_interpolated, color = "Interpolated Output Gap")) +
  theme_minimal() +
  scale_color_manual(values = c("Ameco Output Gap" = blue, "Interpolated Output Gap" = red)) +
  labs(title = "Yearly Ameco and Interpolated Output Gap",
    x = "Year b",
    y = "Output Gap in Percent") +
  theme(legend.position = "bottom") +
  labs(color = NULL)

# Import the ECB_Data.xlsx file
ecb_data <- read_excel("ECB_Data.xlsx")
# Convert it to date format
ecb_data <- ecb_data %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y"))
ecb_data <- ecb_data %>%
  mutate(ecb_output_gap = ecb_output_gap/100)
ecb_data$quarters = seq(as.Date("2013-04-01"), as.Date("2023-07-01"), by = "quarter")

combined_ouput_gap <- left_join(quarterly_data, ecb_data, by = c("date" = "quarters"))
combined_ouput_gap <- combined_ouput_gap %>%
  filter(date >= as.Date("2010-01-01"))

output_ecb_plot_recent <- combined_ouput_gap %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = output_gap, color = "Output Gap (Germany, Estimate)")) +
  geom_line(aes(y = ecb_output_gap, color = "Output Gap (EU, ECB)")) +
  theme_minimal() +
  scale_color_manual(values = c("Output Gap (Germany, Estimate)" = orange, "Output Gap (EU, ECB)" = "blue")) +
  labs(title = "Output Gap, estimated German and estimated EU, quarterly Frequency",
    x = "Year",
    y = "Output Gap") +
  theme(legend.position = "bottom") +
  labs(color = NULL)

#### Taylor Rule Implied Interest Rate ####

# Convert quarterly data to daily data
daily_data <- quarterly_data %>%
  complete(date = seq(min(date), max(date), by = "day")) %>%
  fill(everything(), .direction = "down") %>%
  mutate(date = as.Date(date))