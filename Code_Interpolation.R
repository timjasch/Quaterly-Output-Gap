library(readxl)
library(tidyverse)
library(dplyr)
library(zoo)
library(tempdisagg)
library(svglite)
 
#### Read in the Excel files ####
# Real GDP (2015 Mrd-MarkEuro, Quarterly). Source: AMECO.
real_gdp <- read_excel("DESTATIS_Real_GDP.xlsx")
# Potential GDP (2015 Mrd-MarkEuro, Yearly). Source: AMECO.
potential_gdp <- read_excel("AMECO_Potential_GDP.xlsx")
# Output Gap (Percent, Yearly). Source: WEO. 
output_gap_weo <- read_excel("WEO_Real_GDP.xlsx")
output_gap_weo$output_gap <- output_gap_weo$'Output Gap'/100

# Create Time-Series Objects
real_gdp_quarterly_ts <- ts(real_gdp$`Real GDP`, start = c(1991, 1), frequency = 4)
potential_gdp_yearly_ts <- ts(potential_gdp$`Potential GDP (Ameco)`, start = c(1991, 1), frequency = 1)
output_gap_weo_ts <- ts(output_gap_weo$`Output Gap`, start = c(1991, 1), frequency = 1)

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

#### Comparing the output gap from the WEO with the interpolated output gap ####

# Plot the output gap from the WEO over time (1991-2024)
output_gap_plot_weo <- output_gap_weo %>%
  ggplot(aes(x = Year, y = (100*output_gap))) +
  geom_line(color = orange) +
  theme_minimal() +
  labs(title = "Output Gap 1991-2024 (WEO-Estimates), Yearly Frequency",
    x = "Year",
    y = "Output Gap in %") +
  theme(legend.position = "none") +
  ylim(-10, 10)

# Plot the output gap from the WEO over time (2019-2024)
output_gap_plot_weo_recent <- output_gap_weo %>%
  filter(Year >= 2019) %>%
  ggplot(aes(x = Year, y = (100*output_gap))) +
  geom_line(color = orange) +
  theme_minimal() +
  labs(title = "Output Gap 2019-2024 (WEO-Estimates), Yearly Frequency",
    x = "Year",
    y = "Output Gap in %") +
  theme(legend.position = "none") +
  ylim(-10, 10)

# Compare the plots of the interpolated output gap and the output gap from the WEO for the years 1991-2024
combined_plot <- cowplot::plot_grid(output_gap_plot, output_gap_plot_weo, nrow = 2)
combined_plot

# Compare the plots of the interpolated output gap and the output gap from the WEO for the years 2019-2024
combined_plot_recent <- cowplot::plot_grid(output_gap_plot_recent, output_gap_plot_weo_recent, nrow = 2)
combined_plot_recent

# Export the plots to a Svg file
ggsave("potential_real_plot.svg", plot = potential_real_plot, device = "svg")
ggsave("combined_plot.svg", plot = combined_plot, device = "svg")
ggsave("combined_plot_recent.svg", plot = combined_plot_recent, device = "svg")

# Investigating the validity of using Real GDP from Destatis and Potential GDP from AMECO
# R^(2)_Measure for intercept = 0 and coefficient = 1

Rsqr <- 1 - sum(((potential_gdp$'Real GDP (Ameco)')[1:33] - (potential_gdp$'Real GDP (Destatis)')[1:33])^(2))/sum(((potential_gdp$'Real GDP (Ameco)') - mean(potential_gdp$'Real GDP (Ameco)'))^(2))

#### Comparing the output gap from the ECB with the interpolated output gap ####

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