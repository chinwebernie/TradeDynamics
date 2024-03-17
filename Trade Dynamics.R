#Installing Libraries
install.packages("dplyr")
install.packages("psych")
install.packages("DescTools")
install.packages("ggplot2")
install.packages("mosaic")
install.packages("datarium")
install.packages("tidyverse")
install.packages("corrplot")
install.packages("rcompanion")
install.packages("qqplotr")
install.packages("RVAideMemoire")
install.packages("car")
install.packages("caret")
install.packages("TTR")
install.packages("forecast")

#Loading Libraries
library(dplyr)
library(psych)
library(DescTools)
library(ggplot2)
library(mosaic)
library(datarium)
library(tidyverse)
library(corrplot)
library(rcompanion)
library(qqplotr)
library(RVAideMemoire)
library(car)
library(caret)
library(TTR)
library(forecast)


#Loading dataset into dataframe (df)
df <- read.csv("Trade Dynamics.csv")

View(df)

summary(df)

# Identifying missing values
missing_values <- colSums(is.na(df))

# Detecting outliers

# Creating a boxplot for the Agricultural Raw Materials Exports variable
boxplot(df$ARME, main = "Boxplot for ARME", col = "skyblue", border = "black", horizontal = TRUE)

# Creating a boxplot for the Agricultural Raw Materials Imports variable
boxplot(df$ARMI, main = "Boxplot for ARMI", col = "skyblue", border = "black", horizontal = TRUE)

# Creating a boxplot for the Exports of Goods and Services  variable
boxplot(df$EGSG, main = "Boxplot for EGSG", col = "skyblue", border = "black", horizontal = TRUE)

# Creating a boxplot for the Imports of Goods and Services variable
boxplot(df$IGSG, main = "Boxplot for IGSG", col = "skyblue", border = "black", horizontal = TRUE)

# Creating a boxplot for the Food Exports  variable
boxplot(df$FEME, main = "Boxplot for FEME", col = "skyblue", border = "black", horizontal = TRUE)

# Creating a boxplot for the Food Imports variable
boxplot(df$FIME, main = "Boxplot for FIME", col = "skyblue", border = "black", horizontal = TRUE)

# Creating a boxplot for the Goods Exports  variable
boxplot(df$GEBC, main = "Boxplot for GEBC", col = "skyblue", border = "black", horizontal = TRUE)

# Creating a boxplot for the Goods Imports variable
boxplot(df$GIBC, main = "Boxplot for GIBC", col = "skyblue", border = "black", horizontal = TRUE)

# Creating a boxplot for the International Tourism, Receipts variable
boxplot(df$ITRC, main = "Boxplot for ITRC", col = "skyblue", border = "black", horizontal = TRUE)

# Creating a boxplot for the Merchandise Exports variable
boxplot(df$MECU, main = "Boxplot for MECU", col = "skyblue", border = "black", horizontal = TRUE)

# Creating a boxplot for the Merchandise Imports variable
boxplot(df$MICU, main = "Boxplot for MICU", col = "skyblue", border = "black", horizontal = TRUE)

# Creating a boxplot for the Merchandise Trade variable
boxplot(df$MTGD, main = "Boxplot for MTGD", col = "skyblue", border = "black", horizontal = TRUE)

# Creating a boxplot for the Net Trade in Goods and Services  variable
boxplot(df$NTGS, main = "Boxplot for NTGS", col = "skyblue", border = "black", horizontal = TRUE)

# Creating a boxplot for the Trade variable
boxplot(df$TGDP, main = "Boxplot for TGDP", col = "skyblue", border = "black", horizontal = TRUE)

# Creating a boxplot for the Trade-in Services  variable
boxplot(df$TISG, main = "Boxplot for TISG", col = "skyblue", border = "black", horizontal = TRUE)


# Visual Inspection of dataset (ds)
ds_variables <- c("ARME", "ARMI", "EGSG", "IGSG", "FEME",
                  "FIME", "GEBC", "GIBC", "ITRC", "MECU",
                  "MICU", "MTGD", "NTGS", "TGDP", "TISG")

# Creating a layout for the plots
par(mfrow = c(6, 5))  # 6 rows, 5 columns

# Looping through each variable
for (variable in ds_variables) {
  # Histogram
  hist(ds[[variable]], main = paste("Histogram of", variable), col = "lightblue")
  
  # Q-Q plot
  qqnorm(ds[[variable]], main = paste("Q-Q Plot of", variable))
  qqline(ds[[variable]])
}

# Reset the layout to default
par(mfrow = c(100, 15))


#Making a copy of the dataset as ds
ds <- data.frame(df)

# Drop columns
ds <- subset(ds, select=-c(Country, Code, Year, Class))

view(ds)

# Splitting data into 2 sections, developed and developing countries using the copy dataset (ds)
developed <- ds %>% slice(1:50) #developed countries
developing <- ds %>%slice(51:100) #developing countries


######################################################################################################################
#4.1
######################################################################################################################

# Carrying out a comprehensive descriptive statistical analysis on the dataset to view the Mean, Median, Mode,Standard deviation, Skewness and Kurtosis
summary_stats <- describe(developed)
print(summary_stats)

summary_stats <- describe(developing)
print(summary_stats)

#calculating mode for developed countries
Mode(developed$ARME)
Mode(developed$ARMI)
Mode(developed$EGSG)
Mode(developed$IGSG)
Mode(developed$FEME)
Mode(developed$FIME)
Mode(developed$GEBC)
Mode(developed$GIBC)
Mode(developed$ITRC)
Mode(developed$MECU)
Mode(developed$MICU)
Mode(developed$MTGD)
Mode(developed$NTGS)
Mode(developed$TGDP)
Mode(developed$TISG)

#calculating mode for developing countries
Mode(developing$ARME)
Mode(developing$ARMI)
Mode(developing$EGSG)
Mode(developing$IGSG)
Mode(developing$FEME)
Mode(developing$FIME)
Mode(developing$GEBC)
Mode(developing$GIBC)
Mode(developing$ITRC)
Mode(developing$MECU)
Mode(developing$MICU)
Mode(developing$MTGD)
Mode(developing$NTGS)
Mode(developing$TGDP)
Mode(developing$TISG)

######################################################################################################################
# 4.2
######################################################################################################################
# correlation analysis for the indicators

# 4.2.1 Correlation matrix of objective 3 for developed countries
continuous_developed <- developed %>% select(-ARME, -ARMI, -FEME, -FIME, -GEBC, -GIBC, -ITRC, -MECU, -MICU, -NTGS, -TISG)

# display 5 first obs. of new dataset
head(continuous_developed, 5)

# correlation for developed countries (objective 3) and rounded to 2 decimals
round(cor(continuous_developed), digits = 2)

# Improved correlation matrix
corrplot(cor(continuous_developed), method = "number", type = "upper")

# Correlation matrix of objective 3 for developing countries
continuous_developing <- developing %>% select(-ARME, -ARMI, -FEME, -FIME, -GEBC, -GIBC, -ITRC, -MECU, -MICU, -NTGS, -TISG)

# display 5 first obs. of new dataset
head(continuous_developing, 5)

# correlation for developing countries (objective 3) and rounded to 2 decimals
round(cor(continuous_developing), digits = 2)

# Improved correlation matrix
corrplot(cor(continuous_developing), method = "number", type = "upper")


# 4.2.2 Correlation Analysis of objective 4 for developed countries
continuous_developeda <- developed %>% 
  select(-c("ARME", "ARMI", "EGSG", "IGSG", "FEME", "FIME", "GEBC", "GIBC", "MTGD", "NTGS", "TGDP", "TISG"))

# display 5 first obs. of new dataset
head(continuous_developeda, 5)

# correlation matrix for developed countries (objective 4) and rounded to 2 decimals
round(cor(continuous_developeda), digits = 2)

# Improved correlation matrix
corrplot(cor(continuous_developeda), method = "number", type = "upper")

# Correlation matrix of objective 4 for developing countries
continuous_developinga <- developing %>% 
  select(-c("ARME", "ARMI", "EGSG", "IGSG", "FEME", "FIME", "GEBC", "GIBC", "MTGD", "NTGS", "TGDP", "TISG"))

# display 5 first obs. of new dataset
head(continuous_developinga, 5)

# correlation for developing countries (objective 4) and rounded to 2 decimals
round(cor(continuous_developinga), digits = 2)

# Improved correlation matrix
corrplot(cor(continuous_developinga), method = "number", type = "upper")



# 4.2.3 Correlation matrix of objective 5 for developed countries
continuous_developedb <- developed %>% 
  select(-c("EGSG", "IGSG", "GEBC", "GIBC", "ITRC", "MECU", "MICU", "MTGD", "NTGS", "TGDP", "TISG"))

# display 5 first obs. of new dataset
head(continuous_developedb, 5)

# correlation for developed countries (objective 5) and rounded to 2 decimals
round(cor(continuous_developedb), digits = 2)

# Improved correlation matrix
corrplot(cor(continuous_developedb), method = "number", type = "upper")

# Correlation matrix of objective 5 for developing countries
continuous_developingb <- developing %>% 
  select(-c("EGSG", "IGSG", "GEBC", "GIBC", "ITRC", "MECU", "MICU", "MTGD", "NTGS", "TGDP", "TISG"))

# display 5 first obs. of new dataset
head(continuous_developingb, 5)

# correlation for developing countries (objective 5) and rounded to 2 decimals
round(cor(continuous_developingb), digits = 2)

# Improved correlation matrix
corrplot(cor(continuous_developingb), method = "number", type = "upper")


######################################################################################################################
# 4.3
######################################################################################################################
# hypotheses testing related to the objectives 

# 4.3.1 Wilcoxon Signed-Rank Hypothesis Test for objective 1
wilcoxon_test_result <- wilcox.test(df$MECU, df$MICU, paired = TRUE)

# Print the results
print(wilcoxon_test_result)

# 4.3.2 Chi-squared hypothesis testing for objective 2

# Creating a contigency table
contingency_table <- table(df$Class, df$MTGD)
print(contingency_table)

# Performing chi-square test
chi_squared_test <- chisq.test(contingency_table)

# Printing the results
print(chi_squared_test)


# 4.3.3 Pearson Correlation coefficient hypothesis testing for objective 3

# For developed countries
cor_test_TGDP_MTGD <- cor.test(continuous_developed$TGDP, continuous_developed$MTGD)
cor_test_TGDP_IGSG <- cor.test(continuous_developed$TGDP, continuous_developed$IGSG)
cor_test_TGDP_EGSG <- cor.test(continuous_developed$TGDP, continuous_developed$EGSG)

# Printing the results
print(cor_test_TGDP_MTGD)
print(cor_test_TGDP_IGSG)
print(cor_test_TGDP_EGSG)


# For developing countries
cor_testa_TGDP_MTGD <- cor.test(continuous_developing$TGDP, continuous_developing$MTGD)
cor_testa_TGDP_IGSG <- cor.test(continuous_developing$TGDP, continuous_developing$IGSG)
cor_testa_TGDP_EGSG <- cor.test(continuous_developing$TGDP, continuous_developing$EGSG)

# Printing the results
print(cor_testa_TGDP_MTGD)
print(cor_testa_TGDP_IGSG)
print(cor_testa_TGDP_EGSG)


#4.3.4 Hypothesis Testing of objective 4

# Pair plot of the 3 variables for developed countries
pairs(~ITRC + MECU + MICU, data = continuous_developeda)

# Spearman correlation test
cor_test_spearman <- cor.test(continuous_developeda$ITRC, continuous_developeda$MECU, method = "spearman")
print(cor_test_spearman)

cor_test_spearman <- cor.test(continuous_developeda$ITRC, continuous_developeda$MICU, method = "spearman")
print(cor_test_spearman)


# Pair plot of the 3 variables for developing countries
pairs(~ITRC + MECU + MICU, data = continuous_developinga)

# Spearman correlation test
cor_test_spearman <- cor.test(continuous_developinga$ITRC, continuous_developinga$MECU, method = "spearman")
print(cor_test_spearman)

cor_test_spearman <- cor.test(continuous_developinga$ITRC, continuous_developinga$MICU, method = "spearman")
print(cor_test_spearman)


# 4.3.5 Pearson Correlation coefficient hypothesis testing for objective 5

# For developed countries

cor_test_ARMI_ARME <- cor.test(continuous_developedb$ARMI, continuous_developedb$ARME)
cor_test_ARME_FEME <- cor.test(continuous_developedb$ARME, continuous_developedb$FEME)
cor_test_ARMI_FIME <- cor.test(continuous_developedb$ARMI, continuous_developedb$FIME)

# Printing the results
print(cor_test_ARMI_ARME)
print(cor_test_ARME_FEME)
print(cor_test_ARMI_FIME)


# For developing countries

cor_test_ARMI_ARME <- cor.test(continuous_developingb$ARMI, continuous_developingb$ARME)
cor_test_ARME_FEME <- cor.test(continuous_developingb$ARME, continuous_developingb$FEME)
cor_test_ARMI_FIME <- cor.test(continuous_developingb$ARMI, continuous_developingb$FIME)

# Printing the results
print(cor_test_ARMI_ARME)
print(cor_test_ARME_FEME)
print(cor_test_ARMI_FIME)

##################################################################################################
# 4.4
##################################################################################################
# Regression analysis

# 4.4.1 Single Linear Analysis on Goods Exports of developed countries
cor_matrix <- cor(developed)

# Print the correlation matrix
print(cor_matrix)

corrplot(cor(developed))

# Running the lm command
model_1 <-lm(GEBC ~ MECU, developed)
summary.lm(model_1)

# Drawing scatter plot to visualise the fitted regression line
  plot(GEBC ~ MECU, developed,
       col = "blue",
       main = "Regression: Goods Exports & Merchandise Exports",
       xlab = "Merchandise Exports",
       ylab = "Goods Exports")

  abline(model_1, col="red")
  
  plot(model_1, 1)
  
  plot(model_1, 2)
  
  plot(model_1, 3)

  
# 4.4.2 Single Linear Analysis on Goods Exports of developed countries
model_2 <-lm(GEBC ~ MECU + MICU, developed)
summary.lm(model_2)  

model_3 <-lm(GEBC ~ MECU + MICU + IGSG, developed)
summary.lm(model_3)  

model_4 <-lm(GEBC ~ MECU + MICU + IGSG + TGDP, developed)
summary.lm(model_4)

model_5 <-lm(GEBC ~ MECU + TGDP + EGSG + ITRC, developed)
summary.lm(model_5)

model_6 <-lm(GEBC ~ MECU + EGSG + ITRC, developed)
summary.lm(model_6)

model_7 <-lm(GEBC ~ MECU + ITRC, developed)
summary.lm(model_7)

model_8 <-lm(GEBC ~ MICU + IGSG + TGDP, developed)
summary.lm(model_8)

model_9 <-lm(GEBC ~ MICU + IGSG + TGDP + ITRC, developed)
summary.lm(model_9)

model_10 <-lm(GEBC ~ MICU + IGSG + TGDP, developed)
summary.lm(model_10)

model_11 <-lm(GEBC ~ MICU + IGSG, developed)
summary.lm(model_11)

# Finding each variable's index
data.frame(colnames(developed))

# Drawing a scatter plot matrix
pairs(developed[,c(7,11,4)], lower.panel = NULL, pch = 19,cex = 0.2)

plot(model_11, 1)

plot(model_11, 2)

plot(model_11, 3)

vif(model_11)


#################################################################################################
# 4.5
#################################################################################################

# Creating a time series object from 2009-2018 for the merchandise exports variable with an annual frequency
data_ts <- ts(df$MECU, frequency = 1, start=c(2009,1), end=c(2018,1))
data_ts
print(data_ts)

# Plotting time series data
plot.ts(data_ts)

# Selecting an ARIMA model for the merchandise exports time series based on the Bayesian Information Criterion (BIC)
auto.arima(df$MECU,ic='bic')

# Fitting an ARIMA(0,1,0) model to the merchandise exports time series to achieve stationarity
data_ts_arima <- arima(data_ts, order=c(0,1,0)) # fit an ARIMA(0,1,0) model
data_ts_arima

# Generating forecasts for the next 10 time points using the fitted ARIMA model
data_ts_forecasts <- forecast(data_ts_arima, h=10)
data_ts_forecasts 
plot(data_ts_forecasts)

# Checking the autocorrelation function (ACF) of the residuals and performing the Ljung-Box test for autocorrelation in the residuals.
acf(data_ts_forecasts$residuals, lag.max=20)
Box.test(data_ts_forecasts$residuals, lag=20, type="Ljung-Box")

# Checking the autocorrelation function (ACF) of the residuals and performing the Ljung-Box test for autocorrelation in the residuals.
acf(data_ts_forecasts$residuals, lag.max=12)
Box.test(data_ts_forecasts$residuals, lag=12, type="Ljung-Box")

# Checking the autocorrelation function (ACF) of the residuals and performing the Ljung-Box test for autocorrelation in the residuals.
acf(data_ts_forecasts$residuals, lag.max=24)
Box.test(data_ts_forecasts$residuals, lag=24, type="Ljung-Box")

# Checking the autocorrelation function (ACF) of the residuals and performing the Ljung-Box test for autocorrelation in the residuals.
acf(data_ts_forecasts$residuals, lag.max=36)
Box.test(data_ts_forecasts$residuals, lag=36, type="Ljung-Box")