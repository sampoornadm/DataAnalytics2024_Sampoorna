library(ggplot2)
library(dplyr)
library(lubridate)
library(MASS)

getwd()
Covid2020 <- read.csv('Assignment3/us-counties-2020.csv')
attach(Covid2020)
Covid2021 <- read.csv('Assignment3/us-counties-2021.csv')
attach(Covid2021)

colSums(Covid2020==0)

Covid2020$logcases <- log10(Covid2020$cases)
Covid2020$logdeaths <- log10(Covid2020$deaths)

colSums(Covid2021==0)
Covid2021$logcases <- log10(Covid2021$cases)
Covid2021$logdeaths <- log10(Covid2021$deaths)




boxplot(Covid2020$logcases[Covid2020$logcases!=-Inf],Covid2021$logcases[Covid2021$logcases!=-Inf], 
        outline=FALSE,
        names = c("2020 Cases", "2021 Cases"),
        main = "Comparison of COVID-19 Cases in 2020 and 2021",
        ylab = "Number of Cases")
boxplot(Covid2020$logdeaths[Covid2020$logdeaths!=-Inf],Covid2021$logdeaths[Covid2021$logdeaths!=-Inf],
        outline=FALSE,
        names = c("2020 Deaths", "2021 Deaths"),
        main = "Comparison of COVID-19 Deaths in 2020 and 2021",
        ylab = "Number of Deaths")

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

# Summarize cases by month for both years
cases_by_month_2020 <- Covid2020 %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(total_cases = sum(cases, na.rm = TRUE))

cases_by_month_2021 <- Covid2021 %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(total_cases = sum(cases, na.rm = TRUE))



ggplot(cases_by_month_2020, aes(x = month, y = total_cases)) +
  geom_col(fill = "lightblue") +
  labs(title = "Total COVID-19 Cases by Month (2020)", x = "Month", y = "Total Cases") +
  geom_density(aes(x = month, y = ..density..)) + theme_minimal() +
  scale_x_date(date_labels = "%B", date_breaks = "1 month")





ggplot(cases_by_month_2021, aes(x = month, y = total_cases)) +
  geom_col(fill = "lightpink") +
  geom_density(color = "red", size = 1)+
  labs(title = "Total COVID-19 Cases by Month (2021)", x = "Month", y = "Total Cases") +
  theme_minimal() +
  scale_x_date(date_labels = "%B", date_breaks = "1 month") 


deaths_by_month_2020 <- Covid2020 %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(total_deaths = sum(deaths, na.rm = TRUE))

deaths_by_month_2021 <- Covid2021 %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(total_deaths = sum(deaths, na.rm = TRUE))


ggplot(deaths_by_month_2020, aes(x = month, y = total_deaths)) +
  geom_col(fill = "darkblue") +
  labs(title = "Total COVID-19 Deaths by Month (2020)", x = "Month", y = "Total Cases") +
  theme_minimal() +
  scale_x_date(date_labels = "%B", date_breaks = "1 month") 



ggplot(deaths_by_month_2021, aes(x = month, y = total_deaths)) +
  geom_col(fill = "darkgreen") +
   labs(title = "Total COVID-19 Deaths by Month (2021)", x = "Month", y = "Total Cases") +
  theme_minimal() +
  scale_x_date(date_labels = "%B", date_breaks = "1 month") 


#############################

# Function to filter data within the IQR range
filter_iqr <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)  # 1st quartile
  Q3 <- quantile(x, 0.75, na.rm = TRUE)  # 3rd quartile
  IQR_value <- IQR(x, na.rm = TRUE)      # Interquartile range
  
  # Keep data within the IQR range
  x[x >= (Q1 - 1.5 * IQR_value) & x <= (Q3 + 1.5 * IQR_value)]
}
filter_last_quartile <- function(x) {
  Q3 <- quantile(x, 0.75, na.rm = TRUE)  # 3rd quartile
  
  # Return data in the last quartile (cases >= Q3)
  return(x[x >= Q3])
}
cases_iqr <- filter_iqr(Covid2020$cases)
deaths_iqr <- filter_iqr(Covid2020$deaths)

# Histogram and Distribution for 'cases'
ggplot(data.frame(cases_iqr), aes(x = cases_iqr)) +
  geom_histogram(aes(y = ..density..), binwidth = 10, color = "black", fill = "lightblue") + 
  geom_density(color = "red", size = 1) +
  ggtitle("Histogram of COVID 2020 Cases with Density Overlay") +
  theme_minimal()

# Histogram and Distribution for 'deaths'
ggplot(data.frame(deaths_iqr), aes(x = deaths_iqr)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, color = "black", fill = "lightblue") + 
  geom_density(color = "red", size = 1) +
  ggtitle("Histogram of COVID 2020 Deaths with Density Overlay") +
  theme_minimal()


cases_iqr <- cases_iqr[cases_iqr > 0]  # Keep only positive cases
summary(cases_iqr)
# Fit gamma distribution to cases using fitdistr
fit_gamma <- fitdistr(cases_iqr, "gamma")
shape_gamma <- fit_gamma$estimate[1]
rate_gamma <- fit_gamma$estimate[2]

# Create the histogram and overlay gamma distribution
ggplot(data.frame(cases_iqr), aes(x = cases_iqr)) +
  geom_histogram(aes(y = ..density..), binwidth = 10, color = "black", fill = "lightblue") +
  stat_function(fun = dgamma, args = list(shape = shape_gamma, rate = rate_gamma), color = "red", size = 1) +
  ggtitle("Histogram of COVID Cases (IQR Filtered) with Gamma Distribution Overlay") +
  xlab("Cases") +
  ylab("Density") +
  theme_minimal()


deaths_iqr <- deaths_iqr[deaths_iqr > 0]  # Keep only positive cases
deaths_iqr<-na.omit(deaths_iqr)
#summary(deaths_iqr2)
# Fit gamma distribution to deaths using fitdistr
fit_gamma1 <- fitdistr(deaths_iqr, "gamma")
shape_gamma1 <- fit_gamma1$estimate[1]
rate_gamma1 <- fit_gamma1$estimate[2]

# Create the histogram and overlay gamma distribution
ggplot(data.frame(deaths_iqr), aes(x = deaths_iqr)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, color = "black", fill = "lightblue") +
  stat_function(fun = dgamma, args = list(shape = shape_gamma1, rate = rate_gamma1), color = "red", size = 1) +
  ggtitle("Histogram of COVID 2020 Cases (IQR Filtered) with Gamma Distribution Overlay") +
  xlab("Cases") +
  ylab("Density") +
  theme_minimal()


#2021

cases_iqr2 <- filter_iqr(Covid2021$cases)
deaths_iqr2 <- filter_iqr(Covid2021$deaths)

# Histogram and Distribution for 'cases'
ggplot(data.frame(cases_iqr2), aes(x = cases_iqr2)) +
  geom_histogram(aes(y = ..density..), binwidth = 100, color = "black", fill = "lightblue") + 
  geom_density(color = "red", size = 1) +
  ggtitle("Histogram of COVID 2021 Cases with Density Overlay") +
  theme_minimal()

# Histogram and Distribution for 'deaths'
ggplot(data.frame(deaths_iqr2), aes(x = deaths_iqr2)) +
  geom_histogram(aes(y = ..density..), binwidth = 10, color = "black", fill = "lightblue") + 
  geom_density(color = "red", size = 1) +
  ggtitle("Histogram of COVID 2021 Deaths with Density Overlay") +
  theme_minimal()


cases_iqr2 <- cases_iqr2[cases_iqr2 > 0 & !is.na(cases_iqr2) & is.finite(cases_iqr2)]  # Keep only positive, finite, non-NA cases

summary(cases_iqr2)
# Fit gamma distribution to cases using fitdistr
fit_gamma2 <- fitdistr(cases_iqr2, "gamma")
shape_gamma2 <- fit_gamma2$estimate[1]
rate_gamma2 <- fit_gamma2$estimate[2]

# Create the histogram and overlay gamma distribution
ggplot(data.frame(logcases), aes(x = logcases)) +
  geom_histogram(aes(y = ..density..), binwidth = 100, color = "black", fill = "lightblue") +
  stat_function(fun = dgamma, args = list(shape = shape_gamma2, rate = rate_gamma2), color = "red", size = 1) +
  ggtitle("Histogram of COVID 2021 Cases (IQR Filtered) with Gamma Distribution Overlay") +
  xlab("Cases") +
  ylab("Density") +
  theme_minimal()


deaths_iqr2 <- deaths_iqr2[deaths_iqr2 > 0 & !is.na(deaths_iqr2) & is.finite(deaths_iqr2)]  # Keep only positive, finite, non-NA cases


# Fit gamma distribution to deaths using fitdistr
fit_gamma3 <- fitdistr(deaths_iqr2, "gamma")
shape_gamma3 <- fit_gamma3$estimate[1]
rate_gamma3 <- fit_gamma3$estimate[2]

# Create the histogram and overlay gamma distribution
ggplot(data.frame(deaths_iqr2), aes(x = deaths_iqr2)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, color = "black", fill = "lightblue") +
  stat_function(fun = dgamma, args = list(shape = shape_gamma3, rate = rate_gamma3), color = "red", size = 1) +
  ggtitle("Histogram of COVID 2021 Cases (IQR Filtered) with Gamma Distribution Overlay") +
  xlab("Cases") +
  ylab("Density") +
  theme_minimal()


####################################

#Question 1 c : Plot the ECDFs (Empirical Cumulative Distribution Function) for the two variables in both datasets. Plot the quantile-quantile distribution using a suitable parametric distribution you chose in 1b. Describe features of these plots. min. 2-3 sentences


ggplot(data.frame(cases_iqr), aes(x = cases_iqr)) +
  stat_ecdf(geom = "step", color = "blue") +
  ggtitle("ECDF of COVID 2020 Cases (IQR Filtered)") +
  xlab("Cases") +
  ylab("ECDF") +
  theme_minimal()



ggplot(data.frame(deaths_iqr), aes(x = deaths_iqr)) +
  stat_ecdf(geom = "step", color = "blue") +
  ggtitle("ECDF of COVID 2020 Deaths (IQR Filtered)") +
  xlab("Deaths") +
  ylab("ECDF") +
  theme_minimal()

ggplot(data.frame(cases_iqr2), aes(x = cases_iqr2)) +
  stat_ecdf(geom = "step", color = "blue") +
  ggtitle("ECDF of COVID 2021 Cases (IQR Filtered)") +
  xlab("Cases") +
  ylab("ECDF") +
  theme_minimal()



ggplot(data.frame(deaths_iqr2), aes(x = deaths_iqr2)) +
  stat_ecdf(geom = "step", color = "blue") +
  ggtitle("ECDF of COVID 2021 Deaths (IQR Filtered)") +
  xlab("Deaths") +
  ylab("ECDF") +
  theme_minimal()




ggplot(data.frame(cases_iqr), aes(sample = cases_iqr)) +
  stat_qq(distribution = qnorm, dparams = list(mean = mean(cases_iqr), sd = sd(cases_iqr))) +
  stat_qq_line(distribution = qnorm, dparams = list(mean = mean(cases_iqr), sd = sd(cases_iqr)), color = "red") +
  ggtitle("Q-Q Plot of COVID Cases (IQR Filtered) against Normal Distribution") +
  theme_minimal()

# Generate quantiles for the theoretical gamma distribution
n <- length(cases_iqr)
theoretical_quantiles <- qgamma(ppoints(n), shape = shape_gamma, rate = rate_gamma)

# Create a Q-Q plot
qq_data <- data.frame(Observed = sort(cases_iqr), Theoretical = theoretical_quantiles)

ggplot(qq_data, aes(x = Theoretical, y = Observed)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  ggtitle("Q-Q Plot of COVID 2020 Cases Against Fitted Gamma Distribution") +
  xlab("Theoretical Quantiles (Gamma)") +
  ylab("Observed Quantiles") +
  theme_minimal()

# Create a Q-Q plot
n1 <- length(deaths_iqr)
theoretical_quantiles1 <- qgamma(ppoints(n1), shape = shape_gamma1, rate = rate_gamma1)

# Create a Q-Q plot
qq_data1 <- data.frame(Observed = sort(deaths_iqr), Theoretical = theoretical_quantiles1)

ggplot(qq_data1, aes(x = Theoretical, y = Observed)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  ggtitle("Q-Q Plot of COVID 2020 Deaths Against Fitted Gamma Distribution") +
  xlab("Theoretical Quantiles (Gamma)") +
  ylab("Observed Quantiles") +
  theme_minimal()

n2 <- length(cases_iqr2)
theoretical_quantiles2 <- qgamma(ppoints(n2), shape = shape_gamma2, rate = rate_gamma2)

# Create a Q-Q plot
qq_data2 <- data.frame(Observed = sort(cases_iqr2), Theoretical = theoretical_quantiles2)

ggplot(qq_data2, aes(x = Theoretical, y = Observed)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  ggtitle("Q-Q Plot of COVID 2021 Cases Against Fitted Gamma Distribution") +
  xlab("Theoretical Quantiles (Gamma)") +
  ylab("Observed Quantiles") +
  theme_minimal()

n3 <- length(deaths_iqr2)
theoretical_quantiles3 <- qgamma(ppoints(n3), shape = shape_gamma3, rate = rate_gamma3)

# Create a Q-Q plot
qq_data3 <- data.frame(Observed = sort(deaths_iqr2), Theoretical = theoretical_quantiles3)

ggplot(qq_data3, aes(x = Theoretical, y = Observed)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  ggtitle("Q-Q Plot of COVID 2020 Deaths Against Fitted Gamma Distribution") +
  xlab("Theoretical Quantiles (Gamma)") +
  ylab("Observed Quantiles") +
  theme_minimal()

Covid2020StateFilteredData <- subset(Covid2020, state %in% c("New York", "Washington", "Illinois", "California"))
Covid2021StateFilteredData <- subset(Covid2021, state %in% c("New York", "Washington", "Illinois", "California"))

boxplot(Covid2020StateFilteredData$logcases[Covid2020StateFilteredData$logcases!=-Inf],Covid2021StateFilteredData$logcases[Covid2021StateFilteredData$logcases!=-Inf], 
        outline=FALSE,
        names = c("2020 Cases", "2021 Cases"),
        main = "Comparison of COVID-19 Cases in 2020 and 2021 in New York, Washington, Illinois, California",
        ylab = "Number of Cases")

boxplot(Covid2021StateFilteredData$logdeaths[Covid2020StateFilteredData$logdeaths!=-Inf],Covid2021StateFilteredData$logdeaths[Covid2021StateFilteredData$logdeaths!=-Inf],
        outline=FALSE,
        names = c("2020 Deaths", "2021 Deaths"),
        main = "Comparison of COVID-19 Deaths in 2020 and 2021 in New York, Washington, Illinois, California",
        ylab = "Number of Deaths")

summary(Covid2020StateFilteredData$cases)
summary(Covid2020StateFilteredData$logcases[Covid2020StateFilteredData$logcases!=-Inf])

summary(Covid2021StateFilteredData$cases)
summary(Covid2021StateFilteredData$logcases[Covid2021StateFilteredData$logcases!=-Inf])

summary(Covid2020StateFilteredData$deaths)
summary(Covid2020StateFilteredData$logdeaths[Covid2020StateFilteredData$logdeaths!=-Inf])

summary(Covid2021StateFilteredData$deaths)
summary(Covid2021StateFilteredData$logdeaths[Covid2021StateFilteredData$logdeaths!=-Inf])

# Histogram and Distribution for 'cases'
ggplot(data.frame(Covid2020StateFilteredData$cases), aes(x = Covid2020StateFilteredData$cases)) +
  geom_histogram(aes(y = ..density..), binwidth = 10, color = "black", fill = "lightblue") + 
  geom_density(color = "red", size = 1) +
  ggtitle("Histogram of COVID 2020 Cases with Density Overlay") +
  theme_minimal()

# Histogram and Distribution for 'deaths'
ggplot(data.frame(deaths_iqr), aes(x = deaths_iqr)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, color = "black", fill = "lightblue") + 
  geom_density(color = "red", size = 1) +
  ggtitle("Histogram of COVID 2020 Deaths with Density Overlay") +
  theme_minimal()


