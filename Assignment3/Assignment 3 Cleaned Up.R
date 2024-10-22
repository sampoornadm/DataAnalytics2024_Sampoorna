library(ggplot2)
library(dplyr)
library(lubridate)
library(MASS)

# Define a function to load and prepare data
load_data <- function(file_path) {
  data <- read.csv(file_path)
  data$date <- as.Date(data$date)
  data$logcases <- log10(data$cases)
  data$logdeaths <- log10(data$deaths)
  return(data)
}

# Define a function to create boxplots comparing years
create_boxplot <- function(data1, data2, title, y_label) {
  boxplot(data1[data1 != -Inf], data2[data2 != -Inf], 
          outline = FALSE,
          names = c("2020", "2021"),
          main = title,
          ylab = y_label)
}

# Function to summarize cases by month
summarize_by_month <- function(data, column) {
  data %>%
    mutate(month = floor_date(date, "month")) %>%
    group_by(month) %>%
    summarise(total = sum({{column}}, na.rm = TRUE))
}

# Function to create a bar plot for monthly totals
plot_monthly_totals <- function(summary_data, title, y_label, fill_color) {
  ggplot(summary_data, aes(x = month, y = total)) +
    geom_col(fill = fill_color) +
    labs(title = title, x = "Month", y = y_label) +
    theme_minimal() +
    scale_x_date(date_labels = "%B", date_breaks = "1 month")
}

# Define a function to filter data within the IQR range
filter_iqr <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)  # 1st quartile
  Q3 <- quantile(x, 0.75, na.rm = TRUE)  # 3rd quartile
  IQR_value <- IQR(x, na.rm = TRUE)      # Interquartile range
  filtered <- x[x >= (Q1 - 1.5 * IQR_value) & x <= (Q3 + 1.5 * IQR_value)]
  return(filtered)
}
# Function to plot histogram with density overlay
plot_hist_density <- function(data, title, binwidth, x_label = "Values") {
  ggplot(data.frame(data), aes(x = data)) +
    geom_histogram(aes(y = ..density..), binwidth = binwidth, color = "black", fill = "lightblue") + 
    geom_density(color = "red", size = 1) +
    ggtitle(title) +
    xlab(x_label) +
    theme_minimal()
}

# Function to fit and plot gamma distribution
plot_gamma_fit <- function(data, title, binwidth, x_label = "Values") {
  fit_gamma <- fitdistr(data, "gamma")
  shape_gamma <- fit_gamma$estimate[1]
  rate_gamma <- fit_gamma$estimate[2]
  
  ggplot(data.frame(data), aes(x = data)) +
    geom_histogram(aes(y = ..density..), binwidth = binwidth, color = "black", fill = "lightblue") +
    stat_function(fun = dgamma, args = list(shape = shape_gamma, rate = rate_gamma), color = "red", size = 1) +
    ggtitle(title) +
    xlab(x_label) +
    ylab("Density") +
    theme_minimal()
}

# Function to plot ECDF
plot_ecdf <- function(data, title, x_label = "Values") {
  ggplot(data.frame(data), aes(x = data)) +
    stat_ecdf(geom = "step", color = "blue") +
    ggtitle(title) +
    xlab(x_label) +
    ylab("ECDF") +
    theme_minimal()
}

# Function to create Q-Q plots against Gamma distribution
plot_qq_gamma <- function(data, title) {
  fit_gamma <- fitdistr(data, "gamma")
  shape <- fit_gamma$estimate[1]
  rate <- fit_gamma$estimate[2]
  n <- length(data)
  theoretical_quantiles <- qgamma(ppoints(n), shape = shape, rate = rate)
  qq_data <- data.frame(Observed = sort(data), Theoretical = theoretical_quantiles)
  
  ggplot(qq_data, aes(x = Theoretical, y = Observed)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    ggtitle(title) +
    xlab("Theoretical Quantiles (Gamma)") +
    ylab("Observed Quantiles") +
    theme_minimal()
}

# Define a function to filter data by state
filter_by_state <- function(data, states) {
  filtered_data <- subset(data, state %in% states)
  return(filtered_data)
}
# Load the data
Covid2020 <- load_data('Assignment3/us-counties-2020.csv')
Covid2021 <- load_data('Assignment3/us-counties-2021.csv')


# Create boxplots comparing 2020 and 2021 for cases and deaths
create_boxplot(Covid2020$logcases, Covid2021$logcases, "Comparison of COVID-19 Cases in 2020 and 2021", "Log of Cases")
create_boxplot(Covid2020$logdeaths, Covid2021$logdeaths, "Comparison of COVID-19 Deaths in 2020 and 2021", "Log of Deaths")

summary(Covid2020$cases)
summary(Covid2020$logcases[Covid2020$logcases!=-Inf])
summary(Covid2021$cases)
summary(Covid2021$logcases[Covid2021$logcases!=-Inf])


summary(Covid2020$deaths)
summary(Covid2020$logdeaths[Covid2020$logdeaths!=-Inf])
summary(Covid2021$deaths)
summary(Covid2021$logdeaths[Covid2021$logdeaths!=-Inf])

Covid2020$date <- as.Date(Covid2020$date)
Covid2021$date <- as.Date(Covid2021$date)

# Summarize Cases for 2020 and 2021
cases_by_month_2020 <- summarize_by_month(Covid2020, cases)
cases_by_month_2021 <- summarize_by_month(Covid2021, cases)

# Plot total cases by month for 2020 and 2021
plot_monthly_totals(cases_by_month_2020, "Total COVID-19 Cases by Month (2020)", "Total Cases", "lightblue")
plot_monthly_totals(cases_by_month_2021, "Total COVID-19 Cases by Month (2021)", "Total Cases", "lightpink")

# Summarize deaths for 2020 and 2021
deaths_by_month_2020 <- summarize_by_month(Covid2020, deaths)
deaths_by_month_2021 <- summarize_by_month(Covid2021, deaths)

# Plot total deaths by month for 2020 and 2021
plot_monthly_totals(deaths_by_month_2020, "Total COVID-19 Deaths by Month (2020)", "Total Deaths", "darkblue")
plot_monthly_totals(deaths_by_month_2021, "Total COVID-19 Deaths by Month (2021)", "Total Deaths", "darkgreen")

# Filtering IQR for both cases and deaths for 2020
cases_iqr_2020 <- filter_iqr(Covid2020$cases)
deaths_iqr_2020 <- filter_iqr(Covid2020$deaths)

# Plot histogram and density for cases and deaths in 2020
plot_hist_density(cases_iqr_2020, "Histogram of COVID 2020 Cases with Density Overlay", binwidth = 10)
plot_hist_density(deaths_iqr_2020, "Histogram of COVID 2020 Deaths with Density Overlay", binwidth = 1)

# Remove non-positive values and fit gamma distribution for 2020 cases and deaths
cases_iqr_2020 <- cases_iqr_2020[cases_iqr_2020 > 0]
deaths_iqr_2020 <- deaths_iqr_2020[deaths_iqr_2020 > 0 & !is.na(deaths_iqr_2020)]

plot_gamma_fit(cases_iqr_2020, "Histogram of COVID Cases (IQR Filtered) with Gamma Distribution Overlay (2020)", binwidth = 10)
plot_gamma_fit(deaths_iqr_2020, "Histogram of COVID Deaths (IQR Filtered) with Gamma Distribution Overlay (2020)", binwidth = 1)

# Repeat for 2021
cases_iqr_2021 <- filter_iqr(Covid2021$cases)
deaths_iqr_2021 <- filter_iqr(Covid2021$deaths)

# Plot histogram and density for cases and deaths in 2021
plot_hist_density(cases_iqr_2021, "Histogram of COVID 2021 Cases with Density Overlay", binwidth = 100)
plot_hist_density(deaths_iqr_2021, "Histogram of COVID 2021 Deaths with Density Overlay", binwidth = 10)

# Remove non-positive values and fit gamma distribution for 2021 cases and deaths
cases_iqr_2021 <- cases_iqr_2021[cases_iqr_2021 > 0 & !is.na(cases_iqr_2021) & is.finite(cases_iqr_2021)]
deaths_iqr_2021 <- deaths_iqr_2021[deaths_iqr_2021 > 0 & !is.na(deaths_iqr_2021) & is.finite(deaths_iqr_2021)]

plot_gamma_fit(cases_iqr_2021, "Histogram of COVID Cases (IQR Filtered) with Gamma Distribution Overlay (2021)", binwidth = 100)
plot_gamma_fit(deaths_iqr_2021, "Histogram of COVID Deaths (IQR Filtered) with Gamma Distribution Overlay (2021)", binwidth = 1)

# Plot ECDF for 2020 Cases and Deaths
plot_ecdf(cases_iqr_2020, "ECDF of COVID 2020 Cases (IQR Filtered)", "Cases")
plot_ecdf(deaths_iqr_2020, "ECDF of COVID 2020 Deaths (IQR Filtered)", "Deaths")

# Plot ECDF for 2021 Cases and Deaths
plot_ecdf(cases_iqr_2021, "ECDF of COVID 2021 Cases (IQR Filtered)", "Cases")
plot_ecdf(deaths_iqr_2021, "ECDF of COVID 2021 Deaths (IQR Filtered)", "Deaths")

# Q-Q Plot against Gamma distribution for 2020 Cases and Deaths
plot_qq_gamma(cases_iqr_2020, "Q-Q Plot of COVID 2020 Cases Against Fitted Gamma Distribution")
plot_qq_gamma(deaths_iqr_2020, "Q-Q Plot of COVID 2020 Deaths Against Fitted Gamma Distribution")

# Q-Q Plot against Gamma distribution for 2021 Cases and Deaths
plot_qq_gamma(cases_iqr_2021, "Q-Q Plot of COVID 2021 Cases Against Fitted Gamma Distribution")
plot_qq_gamma(deaths_iqr_2021, "Q-Q Plot of COVID 2021 Deaths Against Fitted Gamma Distribution")


# Filter data for specific states
states <- c("New York", "Washington", "Illinois", "California")
Covid2020StateFiltered <- filter_by_state(Covid2020, states)
Covid2021StateFiltered <- filter_by_state(Covid2021, states)

# Create boxplots comparing 2020 and 2021 for cases and deaths
create_boxplot(Covid2020StateFiltered$logcases, Covid2021StateFiltered$logcases, "Comparison of COVID-19 Cases in 2020 and 2021", "Log of Cases")
create_boxplot(Covid2020StateFiltered$logdeaths, Covid2021StateFiltered$logdeaths, "Comparison of COVID-19 Deaths in 2020 and 2021", "Log of Deaths")

summary(Covid2020StateFiltered$cases)
summary(Covid2020StateFiltered$logcases[Covid2020StateFiltered$logcases!=-Inf])
summary(Covid2021StateFiltered$cases)
summary(Covid2021StateFiltered$logcases[Covid2021StateFiltered$logcases!=-Inf])

summary(Covid2020StateFiltered$deaths)
summary(Covid2020StateFiltered$logdeaths[Covid2020StateFiltered$logdeaths!=-Inf])
summary(Covid2021StateFiltered$deaths)
summary(Covid2021StateFiltered$logdeaths[Covid2021StateFiltered$logdeaths!=-Inf])


# Filtering IQR for both cases and deaths for 2020
cases_iqr_2020_new <- filter_iqr(Covid2020StateFiltered$cases)
deaths_iqr_2020_new <- filter_iqr(Covid2020StateFiltered$deaths)

# Plot histogram and density for cases and deaths in 2020
plot_hist_density(cases_iqr_2020_new, "Histogram of COVID 2020 Cases with Density Overlay", binwidth = 100)
plot_hist_density(deaths_iqr_2020_new, "Histogram of COVID 2020 Deaths with Density Overlay", binwidth = 1)

# Remove non-positive values and fit gamma distribution for 2020 cases and deaths
cases_iqr_2020_new <- cases_iqr_2020_new[cases_iqr_2020_new > 0]
deaths_iqr_2020_new <- deaths_iqr_2020_new[deaths_iqr_2020_new > 0 & !is.na(deaths_iqr_2020_new)]

plot_gamma_fit(cases_iqr_2020_new, "Histogram of COVID Cases (IQR Filtered) with Gamma Distribution Overlay (2020)", binwidth = 10)
plot_gamma_fit(deaths_iqr_2020_new, "Histogram of COVID Deaths (IQR Filtered) with Gamma Distribution Overlay (2020)", binwidth = 1)

# Repeat for 2021
cases_iqr_2021_new <- filter_iqr(Covid2021$cases)
deaths_iqr_2021_new <- filter_iqr(Covid2021$deaths)

# Plot histogram and density for cases and deaths in 2021
plot_hist_density(cases_iqr_2021_new, "Histogram of COVID 2021 Cases with Density Overlay", binwidth = 100)
plot_hist_density(deaths_iqr_2021_new, "Histogram of COVID 2021 Deaths with Density Overlay", binwidth = 10)

# Remove non-positive values and fit gamma distribution for 2021 cases and deaths
cases_iqr_2021_new <- cases_iqr_2021_new[cases_iqr_2021_new > 0 & !is.na(cases_iqr_2021_new) & is.finite(cases_iqr_2021_new)]
deaths_iqr_2021_new <- deaths_iqr_2021_new[deaths_iqr_2021_new > 0 & !is.na(deaths_iqr_2021_new) & is.finite(deaths_iqr_2021_new)]

plot_gamma_fit(cases_iqr_2021_new, "Histogram of COVID Cases (IQR Filtered) with Gamma Distribution Overlay (2021)", binwidth = 100)
plot_gamma_fit(deaths_iqr_2021_new, "Histogram of COVID Deaths (IQR Filtered) with Gamma Distribution Overlay (2021)", binwidth = 1)

# Plot ECDF for 2020 Cases and Deaths
plot_ecdf(cases_iqr_2020_new, "ECDF of COVID 2020 Cases (IQR Filtered)", "Cases")
plot_ecdf(deaths_iqr_2020_new, "ECDF of COVID 2020 Deaths (IQR Filtered)", "Deaths")

# Plot ECDF for 2021 Cases and Deaths
plot_ecdf(cases_iqr_2021_new, "ECDF of COVID 2021 Cases (IQR Filtered)", "Cases")
plot_ecdf(deaths_iqr_2021_new, "ECDF of COVID 2021 Deaths (IQR Filtered)", "Deaths")

# Q-Q Plot against Gamma distribution for 2020 Cases and Deaths
plot_qq_gamma(cases_iqr_2020_new, "Q-Q Plot of COVID 2020 Cases Against Fitted Gamma Distribution")
plot_qq_gamma(deaths_iqr_2020_new, "Q-Q Plot of COVID 2020 Deaths Against Fitted Gamma Distribution")

# Q-Q Plot against Gamma distribution for 2021 Cases and Deaths
plot_qq_gamma(cases_iqr_2021_new, "Q-Q Plot of COVID 2021 Cases Against Fitted Gamma Distribution")
plot_qq_gamma(deaths_iqr_2021_new, "Q-Q Plot of COVID 2021 Deaths Against Fitted Gamma Distribution")

