# Pre-analyses (reliability, power analysis, assumption test)

# load packages
library(haven)
library(InteractionPoweR)
library(ggplot2)
library(dplyr)

# Daten einlesen
df <- read_sav("/Users/janaberger/Desktop/UniBasel/Masterprojekt/Bundesministerium Gewalt gegen Frauen/Dataprep_01_03.sav")

##### reliability analysis for the construct of depression (computing cronbachs alpha)
# -> Zuerst mit allen items, danach mit den selektierten items (depression = mit allen items, depression1 = mit selektierten items)
# Spaltennamen erstellen
column_depr <- paste0("f610_", 1:13)

# Schleife durch die Spaltennamen
for (col in column_depr) {
  df[[col]] <- as.numeric(df[[col]])
}

depression <- data.frame(df$f610_1, df$f610_2, df$f610_3, 
                         df$f610_4, df$f610_5, df$f610_6, 
                         df$f610_7, df$f610_8, df$f610_9, 
                         df$f610_10, df$f610_11, df$f610_12, 
                         df$f610_13)

alpha_result = psych::alpha(depression)
print(alpha_result)

# cronbachs alpha mit vorher selektierten items für depression
depression1 <- data.frame(df$f610_2, df$f610_3, 
                          df$f610_4, df$f610_8, df$f610_9, 
                          df$f610_11, df$f610_12)

alpha_result1 = psych::alpha(depression1)

print(alpha_result1) # Conclusion: Das Cronbach's alpha ist bei Einbezug aller items höher als das Cronbach's alpha mit den selektierten items, jedoch sind beide Cronbach's alpha ausreichend und die Faktorenanalyse stützt die Verwendung der selektierten items als Skala für Depression

# ------ Assumption testing -----
# Hypothesis 1.1
# Conducting power analysis with package InteractionPoweR 
df_clean <- na.omit(df[c("IPV_phy", "Loneliness_sum", "Dep_sum")]) # mir stehen 8683 zur Verfügung, weshalb ich 4341 für die Berechnung verwende
test_power <- InteractionPoweR::power_interaction_r2(
  alpha = 0.05, # alpha, for the power analysis
  N = 4341,   # sample size
  r.x1x2.y = .05, # interaction effect to test (correlation between x1*x2 and y) (keine empirische Begründung)
  r.x1.y = .29,# correlation between x1 and y (physical IPV and Depression) (aus Free 2020)
  r.x2.y = .12,# correlation between x2 and y (Loneliness and Depression) (aus Free 2020)
  r.x1.x2 = .14 # correlation between x1 and x2 (Physical IPV and Loneliness) aus Free (2020)
)
test_power # power 0.932

## assumption test
modell1 <- lm(Dep_sum ~ IPV_phy * Loneliness_sum, data = df_clean)

# Set up the layout for a 2x2 plot
par(mfrow = c(2, 2))

# Plot 1: Residuals vs Fitted without highlighting outliers
std_res <- rstandard(modell1)
plot(fitted(modell1), std_res, xlab = "Fitted Values", ylab = "Standardized Residuals", 
     main = "Residuals vs Fitted")
abline(h = 0, col = "red", lwd = 2)

# Plot 2: Normal Q-Q Plot
qqnorm(std_res, main = "Normal Q-Q", xlab = "Theoretical Quantiles", ylab = "Standardized Residuals")
qqline(std_res, col = "red")

# Plot 3: Scale-Location Plot without highlighting outliers
plot(modell1, which = 3)

# Plot 4: Histogram of Residuals with Density Curve (bottom right)
hist(residuals(modell1), breaks = "Scott", freq = FALSE, main = "Histogram of Residuals", xlab = "Residuals")
lines(density(residuals(modell1)), col = "blue")



# Hypothesis 1.2
df_clean1 <- na.omit(df[c("IPV_sx", "Loneliness_sum", "Dep_sum")]) # 9383 stehen mir zur Verfügung, weshalb ich 4692 für die Poweranalyse nehme
# Poweranalyse für sexual IPV (moderator loneliness)
test_power <- InteractionPoweR::power_interaction_r2(
  alpha = 0.05, # alpha, for the power analysis
  N = 4691,   # sample size
  r.x1x2.y = .05, # interaction effect to test (correlation between x1*x2 and y)
  r.x1.y = .24,# correlation between x1 and y
  r.x2.y = .12,# correlation between x2 and y
  r.x1.x2 = .12# correlation between x1 and x2
)
test_power # power = 0.943

## assumption test
modell3 <- lm(Dep_sum ~ IPV_sx * Loneliness_sum, data = df_clean1)

# Set up the layout for a 2x2 plot
par(mfrow = c(2, 2))

# Plot 1: Residuals vs Fitted without highlighting outliers
std_res <- rstandard(modell3)
plot(fitted(modell3), std_res, xlab = "Fitted Values", ylab = "Standardized Residuals", 
     main = "Residuals vs Fitted")
abline(h = 0, col = "red", lwd = 2)

# Plot 2: Normal Q-Q Plot
qqnorm(std_res, main = "Normal Q-Q", xlab = "Theoretical Quantiles", ylab = "Standardized Residuals")
qqline(std_res, col = "red")

# Plot 3: Scale-Location Plot without highlighting outliers
plot(modell3, which = 3)

# Plot 4: Histogram of Residuals with Density Curve (bottom right)
hist(residuals(modell3), breaks = "Scott", freq = FALSE, main = "Histogram of Residuals", xlab = "Residuals")
lines(density(residuals(modell3)), col = "blue")


#Hypothesis 1.3
# Poweranalyse für psychological IPV (moderator: loneliness)
df_clean2 <- na.omit(df[c("IPV_psy", "Loneliness_sum", "Dep_sum")]) # 7244 stehen mir zur Verfügung, weshalb ich 3622 für die Poweranalyse nehme
# Poweranalyse für psychologicall IPV (moderator loneliness)
test_power <- InteractionPoweR::power_interaction_r2(
  alpha = 0.05, # alpha, for the power analysis
  N = 3622,   # sample size
  r.x1x2.y = .05, # interaction effect to test (correlation between x1*x2 and y)
  r.x1.y = -0.26,# correlation between x1 and y
  r.x2.y = 0.12,# correlation between x2 and y
  r.x1.x2 = -0.02# correlation between x1 and x2
)
test_power # power = 0.88

## assumption test
modell5 <- lm(Dep_sum ~ IPV_psy * Loneliness_sum, data = df_clean2)

# Set up the layout for a 2x2 plot
par(mfrow = c(2, 2))

# Plot 1: Residuals vs Fitted without highlighting outliers
std_res <- rstandard(modell5)
plot(fitted(modell5), std_res, xlab = "Fitted Values", ylab = "Standardized Residuals", 
     main = "Residuals vs Fitted")
abline(h = 0, col = "red", lwd = 2)

# Plot 2: Normal Q-Q Plot
qqnorm(std_res, main = "Normal Q-Q", xlab = "Theoretical Quantiles", ylab = "Standardized Residuals")
qqline(std_res, col = "red")

# Plot 3: Scale-Location Plot without highlighting outliers
plot(modell5, which = 3)


# Plot 4: Histogram of Residuals with Density Curve (bottom right)
hist(residuals(modell5), breaks = "Scott", freq = FALSE, main = "Histogram of Residuals", xlab = "Residuals")
lines(density(residuals(modell5)), col = "blue")


# Hypothesis 2.1
df_clean3 <- na.omit(df[c("IPV_phy", "ses_cat", "Dep_sum")]) # mir stehen 6123 zur Verfügung

df_clean3$ses_cat <- factor(df_clean3$ses_cat,
                           levels = c(1, 2, 3),
                           labels = c("low", "medium", "high"))

df_clean3$ses_cat <- factor(df_clean3$ses_cat)

test_power<-InteractionPoweR::power_interaction(
  n.iter = 1000,            # number of simulations per unique combination of input parameters
  alpha = 0.05,             # alpha, for the power analysis
  N = 6123,                  # sample size
  r.x1x2.y = .05,           # interaction effect to test (correlation between x1*x2 and y)
  r.x1.y = .24,              # correlation between x1 and y
  r.x2.y = -0.24,              # correlation between x2 and y
  r.x1.x2 = -0.15,             # correlation between x1 and x2
  k.x1 = 3,                 # x1 is ordinal and has 3 categories.!
  adjust.correlations = TRUE)     # Adjust correlations
test_power # power 0.984
## assumption test
modell7 <- lm(Dep_sum ~ IPV_phy * ses_cat, data = df_clean3)

# Set up the layout for a 2x2 plot
par(mfrow = c(2, 2))

# Plot 1: Residuals vs Fitted without highlighting outliers
std_res <- rstandard(modell7)
plot(fitted(modell7), std_res, xlab = "Fitted Values", ylab = "Standardized Residuals", 
     main = "Residuals vs Fitted")
abline(h = 0, col = "red", lwd = 2)

# Plot 2: Normal Q-Q Plot
qqnorm(std_res, main = "Normal Q-Q", xlab = "Theoretical Quantiles", ylab = "Standardized Residuals")
qqline(std_res, col = "red")

# Plot 3: Scale-Location Plot without highlighting outliers
plot(modell7, which = 3)


# Plot 4: Histogram of Residuals with Density Curve (bottom right)
hist(residuals(modell7), breaks = "Scott", freq = FALSE, main = "Histogram of Residuals", xlab = "Residuals")
lines(density(residuals(modell7)), col = "blue")



# Hypothesis 2.2
df_clean4 <- na.omit(df[c("IPV_sx", "ses_cat", "Dep_sum")]) # mir stehen 6543 zur Verfügung

df_clean4$ses_cat <- factor(df_clean4$ses_cat,
                            levels = c(1, 2, 3),
                            labels = c("low", "medium", "high"))

df_clean4$ses_cat <- factor(df_clean4$ses_cat)


test_power<-InteractionPoweR::power_interaction(
  n.iter = 1000,            # number of simulations per unique combination of input parameters
  alpha = 0.05,             # alpha, for the power analysis
  N = 6543,                  # sample size
  r.x1x2.y = .05,           # interaction effect to test (correlation between x1*x2 and y)
  r.x1.y = .26,              # correlation between x1 and y
  r.x2.y = -0.24,              # correlation between x2 and y
  r.x1.x2 = -0.18,             # correlation between x1 and x2
  k.x1 = 3,                 # x1 is ordinal and has 3 categories.
  adjust.correlations = TRUE)     # Adjust correlations
test_power # power = 0.99

## assumption test
modell8 <- lm(Dep_sum ~ IPV_sx * ses_cat, data = df_clean4)

# Set up the layout for a 2x2 plot
par(mfrow = c(2, 2))

# Plot 1: Residuals vs Fitted without highlighting outliers
std_res <- rstandard(modell8)
plot(fitted(modell8), std_res, xlab = "Fitted Values", ylab = "Standardized Residuals", 
     main = "Residuals vs Fitted")
abline(h = 0, col = "red", lwd = 2)

# Plot 2: Normal Q-Q Plot
qqnorm(std_res, main = "Normal Q-Q", xlab = "Theoretical Quantiles", ylab = "Standardized Residuals")
qqline(std_res, col = "red")

# Plot 3: Scale-Location Plot without highlighting outliers
plot(modell8, which = 3)

# Plot 4: Histogram of Residuals with Density Curve (bottom right)
hist(residuals(modell8), breaks = "Scott", freq = FALSE, main = "Histogram of Residuals", xlab = "Residuals")
lines(density(residuals(modell8)), col = "blue")


# Hypothese 2.3
df_clean5 <- na.omit(df[c("IPV_psy", "ses_cat", "Dep_sum")]) # mir stehen 5103 zur Verfügung, weshalb ich 4341 für die Berechnung verwende

df_clean5$ses_cat <- factor(df_clean5$ses_cat,
                            levels = c(1, 2, 3),
                            labels = c("low", "medium", "high"))

df_clean5$ses_cat <- factor(df_clean5$ses_cat)

test_power<-InteractionPoweR::power_interaction(
  n.iter = 1000,            # number of simulations per unique combination of input parameters
  alpha = 0.05,             # alpha, for the power analysis
  N = 6543,                  # sample size
  r.x1x2.y = .05,           # interaction effect to test (correlation between x1*x2 and y)
  r.x1.y = .35,              # correlation between x1 and y
  r.x2.y = -0.10,              # correlation between x2 and y
  r.x1.x2 = -0.11,             # correlation between x1 and x2
  k.x1 = 3,                 # x1 is ordinal and has 3 categories.
  adjust.correlations = TRUE)     # Adjust correlations
test_power # power = 0.99

## assumption test
modell9 <- lm(Dep_sum ~ IPV_psy * ses_cat, data = df_clean5)

# Set up the layout for a 2x2 plot
par(mfrow = c(2, 2))

# Plot 1: Residuals vs Fitted without highlighting outliers
std_res <- rstandard(modell9)
plot(fitted(modell9), std_res, xlab = "Fitted Values", ylab = "Standardized Residuals", 
     main = "Residuals vs Fitted")
abline(h = 0, col = "red", lwd = 2)

# Plot 2: Normal Q-Q Plot
qqnorm(std_res, main = "Normal Q-Q", xlab = "Theoretical Quantiles", ylab = "Standardized Residuals")
qqline(std_res, col = "red")

# Plot 3: Scale-Location Plot without highlighting outliers
plot(modell9, which = 3)

# Plot 4: Histogram of Residuals with Density Curve (bottom right)
hist(residuals(modell9), breaks = "Scott", freq = FALSE, main = "Histogram of Residuals", xlab = "Residuals")
lines(density(residuals(modell9)), col = "blue")






