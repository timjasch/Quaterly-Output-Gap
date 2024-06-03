library(readxl)
library(tidyverse)
library(dplyr)
library(zoo)
library(tempdisagg)
 
#### Read in the Excel files ####
# Real GDP (2015 Mrd-MarkEuro, Quaterly). Source: AMECO.
real_gdp <- read_excel("AMECO_Real_GDP.xlsx")
# Potential GDP (2015 Mrd-MarkEuro, Yearly). Source: AMECO.
potential_gdp <- read_excel("AMECO_Potential_GDP.xlsx")
# Output Gap (Percent, Yearly). Source: WEO. 
output_gap_weo <- read_excel("WEO_Real_GDP.xlsx")
output_gap_weo$'Output Gap' <- output_gap_weo$'Output Gap'/100

# Create Time-Series Objects
real_gdp_quarterly_ts <- ts(real_gdp$`Real GDP`, start = c(1991, 1), frequency = 4)
potential_gdp_yearly_ts <- ts(potential_gdp$`Potential GDP (Ameco)`, start = c(1991, 1), frequency = 1)

#### Interpolation & Dataframe ####
potential_interpolated <- td(potential_gdp_yearly_ts ~ 1, to = "quarterly", method = "denton-cholette", conversion = "sum")
potential_gdp_quaterly_ts <- ts(predict(potential_interpolated), start = c(1991, 1), frequency = 4)

potential_gdp_vector <- as.vector(potential_gdp_quaterly_ts)[1:133]
real_gdp_vector <- as.vector(real_gdp_quarterly_ts)
quaters <- seq(as.Date("1991-01-01"), as.Date("2024-01-01"), by = "quarter")

data <- tibble(
  date = quaters,
  potential_gdp = potential_gdp_vector,
  real_gdp = real_gdp_vector,
  output_gap = (real_gdp_vector-potential_gdp_vector)/potential_gdp_vector
)

mean(data$potential_gdp)
mean(data$real_gdp)
mean(data$output_gap)

#### Graphs ####

potential_real_plot <- data %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = potential_gdp, color = "Potential GDP")) +
  geom_line(aes(y = real_gdp, color = "Real GDP")) +
  theme_minimal() +
  scale_color_manual(values = c("Potential GDP" = "blue", "Real GDP" = "red")) +
  labs(title = "Real GDP vs Potential GDP",
    x = "Year",
    y = "GDP (Billion Euro)") +
  theme(legend.position = "top")

output_gap_plot <- data %>%
  ggplot(aes(x = date, y = output_gap)) +
  geom_line(color = "blue") +
  theme_minimal() +
  labs(title = "Output Gap over Time (Interpolated)",
    x = "Year",
    y = "Output Gap") +
  theme(legend.position = "none") +
  ylim(-0.10, 0.10)

data_recent <- data %>%
    filter(date >= as.Date("2019-01-01"))

potential_real_plot_recent <- data_recent %>%
    ggplot(aes(x = date)) +
    geom_line(aes(y = log(potential_gdp), color = "Potential GDP")) +
    geom_line(aes(y = log(real_gdp), color = "Real GDP")) +
    theme_minimal() +
    scale_color_manual(values = c("Potential GDP" = "blue", "Real GDP" = "red")) +
    labs(title = "Real GDP vs Potential GDP (Starting 2019)",
        x = "Year",
        y = "Log GDP (Billion Euro)") +
    theme(legend.position = "top")

output_gap_plot_recent <- data_recent %>%
    ggplot(aes(x = date, y = output_gap)) +
    geom_line(color = "blue") +
    theme_minimal() +
    labs(title = "Output Gap over Time (Interpolated, Starting 2019)",
        x = "Year",
        y = "Output Gap") +
    theme(legend.position = "none") +
  ylim(-0.10, 0.10)

# Plot output gap data from WEO
output_gap_plot_weo <- output_gap_weo %>%
  ggplot(aes(x = Year, y = `Output Gap`)) +
  geom_line(color = "blue") +
  theme_minimal() +
  labs(title = "Output Gap over Time (WEO-Estimates)",
       x = "Year",
       y = "Output Gap") +
  theme(legend.position = "none") +
  ylim(-0.10, 0.10)

# Plot output gap data from WEO
output_gap_plot_recent_weo <- output_gap_weo %>%
  filter(Year >= 2019) %>%
  ggplot(aes(x = Year, y = `Output Gap`)) +
  geom_line(color = "blue") +
  theme_minimal() +
  labs(title = "Output Gap over Time (WEO-Estimates, Starting 2019)",
       x = "Year",
       y = "Output Gap") +
  theme(legend.position = "none") +
  ylim(-0.10, 0.10)

output_gap_weo_ts <- ts(output_gap_weo$`Output Gap`, start = c(1991, 1), frequency = 1)
output_gap_interpolated <- ts(data$output_gap, start = c(1991, 1), frequency = 4)
#plot
plot(output_gap_interpolated, type = "l", col = "#db4343", xlab = "Year", ylab = "Output Gap", main = "Output Gap over Time (Red = Interpolated, Blue = WEO)")
lines(output_gap_weo_ts, col = "#6e6fae")

# Combine the plots
combined_plot <- cowplot::plot_grid(output_gap_plot, output_gap_plot_weo, nrow = 2)
combined_plot

combined_plot_after2019 <- cowplot::plot_grid(output_gap_plot_recent, output_gap_plot_recent_weo, nrow = 2)
combined_plot_after2019

# Calculate the change rate of real GDP, add it to the dataframe
data$real_gdp_change <- c(diff(real_gdp_vector) / lag(real_gdp_vector))
# Plot the change rate of real GDP and interpolated output gap over time
real_gdp_change_plot <- ggplot(data = data, aes(x = date)) +
  geom_line(aes(y = real_gdp_change, color = "Change Rate of Real GDP")) +
  geom_line(aes(y = output_gap, color = "Interpolated Output Gap")) +
  geom_line(aes(y = output_gap_interpolated, color = "Interpolated Output Gap")) +
  theme_minimal() +
  scale_color_manual(values = c("Change Rate of Real GDP" = "blue", "Interpolated Output Gap" = "red", "Interpolated Output Gap" = "green")) +
  labs(title = "Change Rate of Real GDP and Interpolated Output Gap over Time",
       x = "Year",
       y = "Change Rate / Output Gap") +
  ylim(-0.10, 0.10)

combined_plot_after2019 <- cowplot::plot_grid(output_gap_plot_recent, output_gap_plot_recent_weo, nrow = 2)
