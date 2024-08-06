#### Preambel ####

library(readxl)
library(tidyverse)
library(dplyr)
library(zoo)
library(tempdisagg)
library(svglite)
# Using the CM Serif font, yes or no?
CM <- TRUE
# If CM = Yes, load the showtext library
if (CM == TRUE) {
  library(showtext)
  font_add(family = "CM", regular = "computer-modern/cmunrm.ttf", italic = "computer-modern/cmunsl.ttf")
  showtext_auto()
}

# Define colors
red <- "#8B0000"
blue <- "#00008B"
orange <- "#FF8C27"
yellow <- "#F0D22B"
green <- "#3B9934"
darkgrey <- "#212121"

#### Read in the Excel files ####

# Real GDP (2015 Mrd-MarkEuro, Quarterly). Source: DESTATIS
destatis_data <- read_excel("DESTATIS_Data.xlsx")

# Real and Potential GDP and Output Gap (2015 Mrd-MarkEuro, in % of Potential GDP; Yearly). Source: AMECO.
ameco_data <- read_excel("AMECO_Data.xlsx")
# Delete the first 5 rows, clean the data and convert it to numeric
ameco_data <- ameco_data[-c(1:5), ]
ameco_data <- as.data.frame(lapply(ameco_data, as.numeric))
ameco_data <- ameco_data %>% mutate(output_gap = (output_gap / 100))

# Create Time-Series Objects
real_gdp_quarterly_destatis_ts <- ts(destatis_data$real_gdp[1:133], start = c(1991, 1), frequency = 4)
potential_gdp_ameco_yearly_ts <- ts(ameco_data$potential_gdp, start = c(1991, 1), frequency = 1)
output_gap_yearly_ameco_ts <- ts(ameco_data$output_gap[1:33], start = c(1991, 1), frequency = 1)
real_gdp_yearly_ameco_ts <- ts(ameco_data$real_gdp[1:33], start = c(1991, 1), frequency = 1)

#### Interpolation and Dataframe ####
potential_gdp_interpolated <- td(potential_gdp_ameco_yearly_ts ~ 1, to = "quarterly", method = "denton-cholette", conversion = "sum")
# Create Time-Series Object for Interpolated Series
potential_gdp_quarterly_interpolated_ts <- ts(predict(potential_gdp_interpolated), start = c(1991, 1), frequency = 4)

# Create a dataframe with the interpolated data, calculate Output gap, cut them down to 2024Q1
quarterly_data <- tibble(
  date = seq(as.Date("1991-01-01"), as.Date("2024-01-01"), by = "quarter"),
  potential_gdp = as.vector(potential_gdp_quarterly_interpolated_ts)[1:133],
  real_gdp = as.vector(real_gdp_quarterly_destatis_ts)[1:133],
  output_gap = (real_gdp_quarterly_destatis_ts - (potential_gdp_quarterly_interpolated_ts))/(potential_gdp_quarterly_interpolated_ts)
)

output_gap_quaterly_interpolated_ts <- ts(quarterly_data$output_gap, start = c(1991, 1), frequency = 4)

# Data Investigation
mean(quarterly_data$potential_gdp)
mean(quarterly_data$real_gdp)
mean(quarterly_data$output_gap)
# Potential and Real GDP have similar means, the output gap is close to zero.

#### Graphs ####

# Plot the real and potential GDP over time (1991-2024)
potential_real_plot <- quarterly_data %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = potential_gdp, color = "Potential GDP")) +
  geom_line(aes(y = real_gdp, color = "Real GDP")) +
  theme_minimal() +
  scale_color_manual(values = c("Potential GDP" = blue, "Real GDP" = red)) +
  labs(title = "Real and Potential GDP 1991-2024 (Potential Interpolated), Quarterly Frequency",
    x = "Date",
    y = "GDP (2015 Billion Euro)") +
  theme(legend.position = "bottom",
        text = element_text(size = 18)) +
  labs(color = NULL)

# Plot the output gap over time (1991-2024)
output_gap_plot <- quarterly_data %>%
  ggplot(aes(x = date, y = (100*output_gap))) +
  geom_line(color = orange) +
  theme_minimal() +
  labs(title = "Output Gap 1991-2024 (Interpolated), Quarterly Frequency",
    x = "Date",
    y = "Output Gap in %") +
  theme(legend.position = "none",
        text = element_text(size = 18)) +
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
  labs(title = "Real and Potential GDP 2019-2024 (Potential Interpolated), Quarterly Frequency",
    x = "Date",
    y = "GDP (2015 Billion Euro)") +
  theme(legend.position = "bottom",
        text = element_text(size = 18)) +
  labs(color = NULL)

# Plot the output gap over time (2019-2024)
output_gap_plot_recent <- quarterly_data_recent %>%
  ggplot(aes(x = date, y = (100*output_gap))) +
  geom_line(color = orange) +
  theme_minimal() +
  labs(title = "Output Gap 2019-2024 (Interpolated), Quarterly Frequency",
    x = "Date",
    y = "Output Gap in %") +
  theme(legend.position = "none",
        text = element_text(size = 18)) +
  ylim(-10, 10)

#### Comparing output gap: yearly AMECO, quarterly Interpolated ####

# Plot the output gap from AMECO over time (1991-2024)
output_gap_plot_ameco <- ameco_data %>%
  ggplot(aes(x = Year, y = (100*output_gap))) +
  geom_line(color = orange) +
  theme_minimal() +
  labs(title = "Output Gap 1991-2024 (Ameco-Estimates), Yearly Frequency",
    x = "Date",
    y = "Output Gap in %") +
  theme(legend.position = "none",
        text = element_text(size = 18)) +
  ylim(-10, 10)

# Plot the output gap from AMECO over time (2019-2024)
output_gap_plot_ameco_recent <- ameco_data %>%
  filter(Year >= 2019) %>%
  ggplot(aes(x = Year, y = (100*output_gap))) +
  geom_line(color = orange) +
  theme_minimal() +
  labs(title = "Output Gap 2019-2024 (Ameco-Estimates), Yearly Frequency",
    x = "Date",
    y = "Output Gap in %"
  ) +
  theme(legend.position = "none",
        text = element_text(size = 18)) +
  ylim(-10, 10)

#### Comparing output gap: yearly AMECO, yearly Interpolated ####

# Aggregate quarterly real output to yearly data; cut it down to 2023
real_gdp_yearly_destatis_ts <- ts(aggregate(
  real_gdp_quarterly_destatis_ts, FUN = sum
)[1:33], start = c(1991, 1), frequency = 1
)

# Aggregate quarterly potential output to yearly data; cut it down to 2023 
potential_gdp_yearly_interpolated_ts <- ts(aggregate(
  potential_gdp_quarterly_interpolated_ts, FUN = sum
)[1:33], start = c(1991, 1), frequency = 1
)
# This time series is identical to ameco_data$potential_gdp (as it should be)

# Calculate the output gap for the yearly interpolated data
output_gap_yearly_interpolated_ts <- (real_gdp_yearly_destatis_ts-potential_gdp_yearly_interpolated_ts)/potential_gdp_yearly_interpolated_ts

# Combine the two ts objects into a data frame
compare_output_gap <- data.frame(
  date = time(output_gap_yearly_interpolated_ts),
  output_gap_ameco = as.vector(output_gap_yearly_ameco_ts),
  output_gap_interpolated = as.vector(output_gap_yearly_interpolated_ts)
  #output_gap_ameco_own = (ameco_data$real_gdp - ameco_data$potential_gdp)/ameco_data$potential_gdp[1:33]
)

# Plot the output gap in comparison
compare_output_gap_plot <- compare_output_gap %>%
#  filter(date >= 2019) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = 100*output_gap_ameco, color = "Ameco Output Gap")) +
  geom_line(aes(y = 100*output_gap_interpolated, color = "Interpolated Output Gap")) +
  theme_minimal() +
  scale_color_manual(values = c("Ameco Output Gap" = blue, "Interpolated Output Gap" = red)) +
  labs(title = "Yearly Ameco and Interpolated Output Gap",
    x = "Date",
    y = "Output Gap in Percent") +
  theme(legend.position = "bottom",
        text = element_text(size = 18)) +
  labs(color = NULL) +
  ylim(-10, 10)

# Investigating the validity of using Real GDP from Destatis and Potential GDP from AMECO

# Squared differences between the two series
sum((real_gdp_yearly_ameco_ts - real_gdp_yearly_destatis_ts)^(2))

# Relative to whole variation (R^(2) Measure, for intercept = 0 and coefficient = 1)
Rsqr = 1 - sum((real_gdp_yearly_ameco_ts - real_gdp_yearly_destatis_ts)^(2)) / sum((real_gdp_yearly_ameco_ts - mean(real_gdp_yearly_ameco_ts))^(2))

#### Comparing the output gap from the ECB with the interpolated output gap ####

# Import the ECB_Data.xlsx file; it holds the output gap for the EU as a whole
ecb_data <- read_excel("ECB_Data.xlsx")

# Convert it to date format
ecb_data <- ecb_data %>% mutate(date = as.Date(date, format = "%d/%m/%Y"), ecb_output_gap = ecb_output_gap/100)

# The source encodes the date for a quarter as the last day, we want the first day of the quarter
ecb_data <- ecb_data %>% mutate(quarters = seq(as.Date("2013-04-01"), as.Date("2023-07-01"), by = "quarter"))

# Combine the two data frames by the date
combined_ouput_gap <- left_join(quarterly_data, ecb_data, by = c("date" = "quarters"))

# Only take data from 2010 onwards
combined_ouput_gap <- combined_ouput_gap %>%
  filter(date >= as.Date("2010-01-01"))

output_ecb_plot_recent <- combined_ouput_gap %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = output_gap, color = "Output Gap (Germany, Own Estimate)")) +
  geom_line(aes(y = ecb_output_gap, color = "Output Gap (EU, ECB)")) +
  theme_minimal() +
  scale_color_manual(values = c("Output Gap (Germany, Estimate)" = orange, "Output Gap (EU, ECB)" = "blue")) +
  labs(title = "Output Gap, estimated German and estimated EU, quarterly Frequency",
    x = "Date",
    y = "Output Gap") +
  theme(legend.position = "bottom",
        text = element_text(size = 18)) +
  labs(color = NULL)

#### Taylor Rule Implied Interest Rate ####

# Convert quarterly data to daily data
daily_data <- quarterly_data %>%
  complete(date = seq(min(date), as.Date("2024-03-31"), by = "day")) %>%
  fill(everything(), .direction = "down") %>%
  mutate(date = as.Date(date))

# Read in the .RData file
load("CPI_measures_daily.RData")

daily_data <- left_join(daily_data, CPI_measures_d, by = c("date" = "Day"))
interest_daily_data <- daily_data %>%
  select(date, output_gap, pi_yoy) %>%
  filter(!is.na(output_gap) & !is.na(pi_yoy))

# Fit the Value of ouput_gap into percent format
interest_daily_data <- interest_daily_data %>%
  mutate(
    output_gap = 100 * output_gap
  )

# Calculate the Taylor Rule Implied Interest Rate
interest_daily_data <- interest_daily_data %>%
  mutate(
    taylor_rate = 0 + 1.5 * (pi_yoy - 2) + 0.5 * output_gap
  )
# Calculate the overshoot of the inflation rate
interest_daily_data <- interest_daily_data %>% mutate(pi_overshoot = pi_yoy - 2)

taylor_rule_plot <- interest_daily_data %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = taylor_rate, color = "Taylor Rate")) +
  geom_line(aes(y = pi_yoy, color = "CPI"), linetype = "dashed") +
  geom_line(aes(y = output_gap, color = "Output Gap"), linetype = "dashed") +
  theme_minimal() +
  scale_color_manual(values = c("Taylor Rate" = green,
                                "CPI" = yellow,
                                "Output Gap" = blue)) +
  labs(title = "Taylor Rule Implied Interest Rate, Daily Frequency",
    x = "Date",
    y = "Interest Rate, CPI, Output Gap in %",
    color = "Variables") +
  theme(legend.position = "bottom",
        text = element_text(size = 18))

# Actual Interest Rate
interest_rate <- read_excel("Policy_Rate_ECB.xlsx")

interest_daily_data <- left_join(interest_daily_data, interest_rate, by = c("date" = "Day"))

interest_rate_comparision <- interest_daily_data %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = taylor_rate, color = "Taylor Rate")) +
  geom_line(aes(y = fixed_rate, color = "Fixed Rate"), linetype = "dashed") +
  geom_line(aes(y = deposit_rate, color = "Deposit Rate"), linetype = "dashed") +
  geom_line(aes(y = pi_overshoot, color = "Inflation Overshoot")) +
  theme_minimal() +
  scale_color_manual(values = c("Taylor Rate" = green, "Fixed Rate" = yellow, "Deposit Rate" = blue, "Inflation Overshoot" = red)) +
  labs(title = "Taylor Rule Implied and ECB Interest Rates, Daily Frequency",
    x = "Date",
    y = "Interest Rate in %",
    color = "Variables") +
  theme(legend.position = "bottom",
        text = element_text(size = 18))

interest_rate_gap <- interest_daily_data %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = taylor_rate, color = "Taylor Rate"), linetype = "dashed") +
  geom_line(aes(y = deposit_rate, color = "Deposit Rate"), linetype = "dashed") +
  geom_line(aes(y = taylor_rate - deposit_rate, color = "Taylor-Gap")) +
  theme_minimal() +
  scale_color_manual(values = c("Taylor Rate" = green, "Taylor-Gap" = yellow, "Deposit Rate" = blue, "Pi Overshoot" = red)) +
  labs(title = "Implied Taylor Rule, ECB Deposit Rate and Gap between them, Daily Frequency",
    x = "Date",
    y = "Interest Rate in %",
    color = "Variables") +
  theme(legend.position = "bottom",
        text = element_text(size = 18))

#### Using a shadow rate to compare to the Taylor rate ####

# Read in the shadow rate data
shadow_rate_zlb <- read_excel("Shadow_Rate.xlsx")
# Convert the date to a date format, the date is in Format YYYYMM
shadow_rate_zlb <- shadow_rate_zlb %>% mutate(date = paste0(date, "01"))
shadow_rate_zlb <- shadow_rate_zlb %>% 
  mutate(date = as.Date(date, format = "%Y%m%d")) %>%
  complete(date = seq(min(date),  as.Date("2022-08-31"), by = "day")) %>%
  fill(everything(), .direction = "down")
shadow_rate_zlb <- shadow_rate_zlb %>% filter(date >= as.Date("2019-01-01"))

# take from the daily data the deposit rate from the 01.09.2020 onwards
shadow_rate_deposit <- interest_daily_data %>%
  filter(date >= as.Date("2022-09-01")) %>%
  select(date, shadow_rate = deposit_rate)

shadow_rate <- bind_rows(shadow_rate_zlb, shadow_rate_deposit)

#### Remake the comparision plots with the shadow rate ####

interest_daily_data <- left_join(interest_daily_data, shadow_rate, by = c("date" = "date"))

interest_rate_comparision_shadow <- interest_daily_data %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = taylor_rate, color = "Taylor Rate")) +
  geom_line(aes(y = fixed_rate, color = "Fixed Rate"), linetype = "dashed") +
  geom_line(aes(y = deposit_rate, color = "Deposit Rate"), linetype = "dashed") +
  geom_line(aes(y = pi_overshoot, color = "Inflation Overshoot")) +
  geom_line(aes(y = shadow_rate, color = "Shadow Rate"), linetype = "dashed") +
  theme_minimal() +
  scale_color_manual(values = c("Taylor Rate" = green, "Fixed Rate" = yellow, "Deposit Rate" = blue, "Inflation Overshoot" = red, "Shadow Rate" = darkgrey)) +
  labs(title = "Taylor Rule Implied and ECB Interest Rates, Daily Frequency",
    x = "Date",
    y = "Interest Rate in %",
    color = "Variables") +
  theme(legend.position = "bottom",
        text = element_text(size = 18))

interest_rate_gap_shadow <- interest_daily_data %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = taylor_rate, color = "Taylor Rate"), linetype = "dashed") +
  geom_line(aes(y = deposit_rate, color = "Deposit Rate"), linetype = "dashed") +
  geom_line(aes(y = taylor_rate - deposit_rate, color = "Taylor-Gap")) +
  geom_line(aes(y = shadow_rate, color = "Shadow Rate"), linetype = "dashed") +
  geom_line(aes(y = taylor_rate - shadow_rate, color = "Taylor-Shadow-Gap")) +
  theme_minimal() +
  scale_color_manual(values = c("Taylor Rate" = green, "Taylor-Gap" = yellow, "Deposit Rate" = blue, "Pi Overshoot" = red, "Shadow Rate" = darkgrey, "Taylor-Shadow-Gap" = darkgrey)) +
  labs(title = "Implied Taylor Rule, ECB Deposit Rate and Gap between them, Daily Frequency",
    x = "Date",
    y = "Interest Rate in %",
    color = "Variables") +
  theme(legend.position = "bottom",
        text = element_text(size = 18))

#### Save the data ####

# Export the Taylor Rate and the Gap between the Taylor Rate and the ECB Deposit Rate as RData
save(interest_daily_data, file = "interest_daily_data.RData")

#### Generate the plots ####

# Apply conditional theme modification for CM font
if (CM == TRUE) {
  potential_real_plot <- potential_real_plot + theme(text = element_text(family = "CM"))
  # The four plots that will get combined
  output_gap_plot <- output_gap_plot + theme(text = element_text(family = "CM"))
  output_gap_plot_ameco <- output_gap_plot_ameco + theme(text = element_text(family = "CM"))
  output_gap_plot_recent <- output_gap_plot_recent + theme(text = element_text(family = "CM"))
  output_gap_plot_ameco_recent <- output_gap_plot_ameco_recent + theme(text = element_text(family = "CM"))
  #
  taylor_rule_plot <- taylor_rule_plot + theme(text = element_text(family = "CM"))
  interest_rate_comparision <- interest_rate_comparision + theme(text = element_text(family = "CM"))
  interest_rate_gap <- interest_rate_gap + theme(text = element_text(family = "CM"))
  compare_output_gap_plot <- compare_output_gap_plot + theme(text = element_text(family = "CM"))
  interest_rate_comparision_shadow <- interest_rate_comparision_shadow + theme(text = element_text(family = "CM"))
  interest_rate_gap_shadow <- interest_rate_gap_shadow + theme(text = element_text(family = "CM"))
}

# Compare the plots of the interpolated output gap and the output gap from Ameco for the years 1991-2024
comparison_plot <- cowplot::plot_grid(output_gap_plot, output_gap_plot_ameco, nrow = 2)
# Compare the plots of the interpolated output gap and the output gap from the Ameco for the years 2019-2024
comparison_plot_recent <- cowplot::plot_grid(output_gap_plot_recent, output_gap_plot_ameco_recent, nrow = 2)

ggsave("SVGs/potential_real_plot.svg", plot = potential_real_plot, device = "svg")
ggsave("SVGs/comparison_plot.svg", plot = comparison_plot, device = "svg")
ggsave("SVGs/comparison_plot_recent.svg", plot = comparison_plot_recent, device = "svg")
ggsave("SVGs/taylor_rule_plot.svg", plot = taylor_rule_plot, device = "svg")
ggsave("SVGs/comparison_interest_rates.svg", plot = interest_rate_comparision, device = "svg")
ggsave("SVGs/interest_rate_gap.svg", plot = interest_rate_gap, device = "svg")
ggsave("SVGs/compare_output_gap_plot.svg", plot = compare_output_gap_plot, device = "svg")
ggsave("SVGs/interest_rate_comparision_shadow.svg", plot = interest_rate_comparision_shadow, device = "svg")
ggsave("SVGs/interest_rate_gap_shadow.svg", plot = interest_rate_gap_shadow, device = "svg")

ggsave("PDFs/potential_real_plot.pdf", plot = potential_real_plot, width = 16, height = 9, units = "in")
ggsave("PDFs/comparison_plot.pdf", plot = comparison_plot, width = 16, height = 9, units = "in")
ggsave("PDFs/comparison_plot_recent.pdf", plot = comparison_plot_recent, width = 16, height = 9, units = "in")
ggsave("PDFs/taylor_rule_plot.pdf", plot = taylor_rule_plot, width = 16, height = 9, units = "in")
ggsave("PDFs/comparison_interest_rates.pdf", plot = interest_rate_comparision, width = 16, height = 9, units = "in")
ggsave("PDFs/interest_rate_gap.pdf", plot = interest_rate_gap, width = 16, height = 9, units = "in")
ggsave("PDFs/compare_output_gap_plot.pdf", plot = compare_output_gap_plot, width = 16, height = 9, units = "in")
ggsave("PDFs/interest_rate_comparision_shadow.pdf", plot = interest_rate_comparision_shadow, width = 16, height = 9, units = "in")
ggsave("PDFs/interest_rate_gap_shadow.pdf", plot = interest_rate_gap_shadow, width = 16, height = 9, units = "in")